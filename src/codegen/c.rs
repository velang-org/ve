use crate::ast::Type;
use crate::{
    ast,
    codegen::{CodegenConfig, CompileError},
};
use codespan::{FileId, Span};
use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::path::Path;

pub struct CBackend {
    config: CodegenConfig,
    header: String,
    body: String,
    file_id: FileId,
    includes: RefCell<BTreeSet<String>>,
    variables: RefCell<HashMap<String, Type>>,
    functions_map: HashMap<String, Type>,
    imported_functions: HashMap<String, (Vec<Type>, Type)>,
    ffi_functions: HashSet<String>,
    struct_defs: HashMap<String, Vec<(String, Type)>>,
    imported_structs: Vec<ast::StructDef>,
    enum_defs: HashMap<String, ast::EnumDef>,
    memory_analysis: MemoryAnalysis,
}

#[derive(Debug, Default)]
struct MemoryAnalysis {
    estimated_arena_size: usize,
    string_allocations: usize,
    array_allocations: usize,
    struct_allocations: usize,
    max_function_depth: usize,
    total_functions: usize,
}

impl CBackend {
    pub fn new(
        config: CodegenConfig,
        file_id: FileId,
        imported_functions: HashMap<String, (Vec<Type>, Type)>,
        imported_structs: Vec<ast::StructDef>,
        imported_ffi_vars: Vec<ast::FfiVariable>,
    ) -> Self {
        let backend = Self {
            config,
            header: String::new(),
            body: String::new(),
            file_id,
            includes: RefCell::new(BTreeSet::new()),
            variables: RefCell::new(HashMap::new()),
            functions_map: HashMap::new(),
            imported_functions,
            ffi_functions: HashSet::new(),
            struct_defs: HashMap::new(),
            imported_structs,
            enum_defs: HashMap::new(),
            memory_analysis: MemoryAnalysis::default(),
        };

        for ffi_var in imported_ffi_vars {
            backend
                .variables
                .borrow_mut()
                .insert(ffi_var.name, ffi_var.ty);
        }

        backend
    }

    fn generate_struct_to_str_functions<'a, I>(&mut self, structs: I)
    where
        I: IntoIterator<Item = &'a ast::StructDef>,
    {
        for struct_def in structs {
            let struct_name = &struct_def.name;
            self.header.push_str(&format!(
                "static char* ve_{}_to_str(const ve_{} *obj) {{\n",
                struct_name, struct_name
            ));
            self.header
                .push_str("    char *buffer = ve_arena_alloc(256);\n");
            self.header.push_str(&format!(
                "    if (!buffer) return \"<failed to allocate memory for {}>\";\n",
                struct_name
            ));
            let mut format_parts = Vec::new();
            let mut args = Vec::new();
            format_parts.push(format!("{}{{", struct_name));
            for (i, field) in struct_def.fields.iter().enumerate() {
                let (fmt, arg) = match field.ty {
                    Type::I32 => ("%d", format!("obj->{}", field.name)),
                    Type::Bool => (
                        "%s",
                        format!("(obj->{} ? \"true\" : \"false\")", field.name),
                    ),
                    Type::String => (
                        "%s",
                        format!("(obj->{} ? obj->{} : \"null\")", field.name, field.name),
                    ),
                    Type::Pointer(_) | Type::RawPtr => {
                        ("%p", format!("(void*)obj->{}", field.name))
                    }
                    Type::Struct(ref s) => ("%s", format!("ve_{}_to_str(&obj->{})", s, field.name)),
                    _ => ("?", "\"<unknown type>\"".to_string()),
                };
                if i > 0 {
                    format_parts.push(format!(" {}:{}", field.name, fmt));
                } else {
                    format_parts.push(format!("{}:{}", field.name, fmt));
                }
                args.push(arg);
            }
            format_parts.push("}".to_string());
            let format_str = format_parts.join("");
            self.header
                .push_str(&format!("    sprintf(buffer, \"{}\"", format_str));
            for arg in args {
                self.header.push_str(&format!(", {}", arg));
            }
            self.header.push_str(");\n");
            self.header.push_str("    return buffer;\n");
            self.header.push_str("}\n\n");
        }
    }
    pub fn compile(
        &mut self,
        program: &ast::Program,
        output_path: &Path,
    ) -> Result<(), CompileError> {
        self.analyze_memory_requirements(program);
        self.emit_header();
        self.generate_ffi_declarations(program)?;
        let imported_structs = self.imported_structs.clone();
        for struct_def in &imported_structs {
            self.emit_struct(struct_def)?;
        }
        for struct_def in &program.structs {
            let fields: Vec<(String, Type)> = struct_def
                .fields
                .iter()
                .map(|f| (f.name.clone(), f.ty.clone()))
                .collect();
            self.struct_defs.insert(struct_def.name.clone(), fields);
        }
        for enum_def in &program.enums {
            self.enum_defs.insert(enum_def.name.clone(), enum_def.clone());
        }
        for struct_def in &program.structs {
            self.emit_struct(struct_def)?;
        }
        for enum_def in &program.enums {
            self.emit_enum(enum_def)?;
        }
        use crate::ast::Expr;
        fn collect_generic_enum_instances(expr: &Expr, out: &mut Vec<Type>) {
            match expr {
                Expr::EnumConstruct(_, _, _, info) => {
                    if let Type::GenericInstance(_, _) = &info.ty {
                        out.push(info.ty.clone());
                    }
                }
                Expr::Call(_, args, info) => {
                    if let Type::GenericInstance(_, _) = &info.ty {
                        out.push(info.ty.clone());
                    }
                    for arg in args { collect_generic_enum_instances(arg, out); }
                }
                Expr::Match(_, arms, info) => {
                    if let Type::GenericInstance(_, _) = &info.ty {
                        out.push(info.ty.clone());
                    }
                    for arm in arms {
                        match &arm.body {
                            crate::ast::MatchArmBody::Expr(e) => collect_generic_enum_instances(e, out),
                            crate::ast::MatchArmBody::Block(stmts) => {
                                for s in stmts {
                                    if let crate::ast::Stmt::Expr(e, _) = s {
                                        collect_generic_enum_instances(e, out);
                                    }
                                }
                            }
                        }
                    }
                }
                Expr::SafeBlock(stmts, _) => {
                    for s in stmts {
                        if let crate::ast::Stmt::Expr(e, _) = s {
                            collect_generic_enum_instances(e, out);
                        }
                    }
                }
                Expr::TemplateStr(parts, _) => {
                    for part in parts {
                        if let crate::ast::TemplateStrPart::Expression(e) = part {
                            collect_generic_enum_instances(e, out);
                        }
                    }
                }
                Expr::BinOp(l, _, r, _) => {
                    collect_generic_enum_instances(l, out);
                    collect_generic_enum_instances(r, out);
                }
                Expr::UnaryOp(_, e, _) => collect_generic_enum_instances(e, out),
                Expr::StructInit(_, fields, _) => {
                    for (_, e) in fields { collect_generic_enum_instances(e, out); }
                }
                Expr::ArrayInit(elems, _) => {
                    for e in elems { collect_generic_enum_instances(e, out); }
                }
                Expr::ArrayAccess(a, b, _) => {
                    collect_generic_enum_instances(a, out);
                    collect_generic_enum_instances(b, out);
                }
                Expr::Cast(e, _, _) => collect_generic_enum_instances(e, out),
                Expr::Assign(a, b, _) => {
                    collect_generic_enum_instances(a, out);
                    collect_generic_enum_instances(b, out);
                }
                Expr::Deref(e, _) => collect_generic_enum_instances(e, out),
                Expr::Range(a, b, _) => {
                    collect_generic_enum_instances(a, out);
                    collect_generic_enum_instances(b, out);
                }
                Expr::FieldAccess(e, _, _) => collect_generic_enum_instances(e, out),
                _ => {}
            }
        }
        let mut generic_enum_instances = Vec::new();
        
        for func in &program.functions {
            if let Type::GenericInstance(_, _) = &func.return_type {
                generic_enum_instances.push(func.return_type.clone());
            }
            for stmt in &func.body {
                if let crate::ast::Stmt::Expr(e, _) = stmt {
                    collect_generic_enum_instances(e, &mut generic_enum_instances);
                }
                if let crate::ast::Stmt::Let(_, Some(ty), _, _, _) = stmt {
                    if let Type::GenericInstance(_, _) = ty {
                        generic_enum_instances.push(ty.clone());
                    }
                }
            }
        }
        
        for stmt in &program.stmts {
            if let crate::ast::Stmt::Expr(e, _) = stmt {
                collect_generic_enum_instances(e, &mut generic_enum_instances);
            }
            if let crate::ast::Stmt::Let(_, Some(ty), _, _, _) = stmt {
                if let Type::GenericInstance(_, _) = ty {
                    generic_enum_instances.push(ty.clone());
                }
            }
        }
        generic_enum_instances.sort_by(|a, b| format!("{:?}", a).cmp(&format!("{:?}", b)));
        generic_enum_instances.dedup();

        for ty in &generic_enum_instances {
            if let Type::GenericInstance(name, args) = ty {
                if let Some(enum_def) = program.enums.iter().find(|e| &e.name == name) {
                    self.emit_generic_enum_instance(enum_def, args)?;
                }
            }
        }

        self.functions_map = program
            .functions
            .iter()
            .map(|f| (f.name.clone(), f.return_type.clone()))
            .collect();
        for ffi in &program.ffi_functions {
            self.functions_map.insert(ffi.name.clone(), ffi.return_type.clone());
        }
        self.emit_globals(program)?;
        self.emit_functions(program)?;
        let imported_structs = self.imported_structs.clone();
        let all_structs: Vec<&ast::StructDef> = program
            .structs
            .iter()
            .chain(imported_structs.iter())
            .collect();
        self.generate_struct_to_str_functions(all_structs);
        self.emit_main_if_missing(program)?;
        self.write_output(output_path)?;
        Ok(())
    }

    fn generate_ffi_declarations(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        let mut ffi_decls = String::new();
        for ffi in &program.ffi_functions {
            self.ffi_functions.insert(ffi.name.clone());

            if ffi.name.starts_with("ve_") {
                continue;
            }

            let ret = self.type_to_c_ffi(&ffi.return_type);
            let params = ffi
                .params
                .iter()
                .map(|ty| self.type_to_c_ffi(ty))
                .collect::<Vec<String>>()
                .join(", ");

            let param_str = if params.is_empty() { "void" } else { &params };

            if let Some(header) = ffi.metadata.as_ref().and_then(|m| m.get("header")) {
                self.includes.borrow_mut().insert(format!("<{}>", header));
            }

            if let Some(link) = ffi.metadata.as_ref().and_then(|m| m.get("link")) {
                self.header
                    .push_str(&format!("#pragma comment(lib, \"{}\")\n", link));
            }

            ffi_decls.push_str(&format!("extern {} {}({});\n", ret, ffi.name, param_str));
        }

        if !ffi_decls.is_empty() {
            self.header.push_str(&ffi_decls);
            self.header.push_str("\n");
        }

        Ok(())
    }
    fn emit_header(&mut self) {
        self.header.push_str(&format!(
            "// Generated by Velang Compiler (target: {})\n",
            self.config.target_triple
        ));
        self.header
            .push_str("#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n");
        self.header.push_str("#include <stdbool.h>\n");
        self.header.push_str("#include <math.h>\n");
        self.header.push_str("#include <stdint.h>\n");

        self.header.push_str("typedef unsigned char u8;\n");
        self.header.push_str("typedef unsigned short u16;\n");
        self.header.push_str("typedef unsigned int u32;\n");
        self.header.push_str("typedef unsigned long long u64;\n");
        self.header.push_str("typedef signed char i8;\n");
        self.header.push_str("typedef signed short i16;\n");
        self.header.push_str("typedef signed int i32;\n");
        self.header.push_str("typedef signed long long i64;\n");

        self.header.push_str("typedef unsigned char ve_u8;\n");
        self.header.push_str("typedef unsigned short ve_u16;\n");
        self.header.push_str("typedef unsigned int ve_u32;\n");
        self.header.push_str("typedef unsigned long long ve_u64;\n");
        self.header.push_str("typedef signed char ve_i8;\n");
        self.header.push_str("typedef signed short ve_i16;\n");
        self.header.push_str("typedef signed int ve_i32;\n");
        self.header.push_str("typedef signed long long ve_i64;\n");
        self.header.push_str("typedef float ve_f32;\n");
        self.header.push_str("typedef double ve_f64;\n");
        self.header.push_str("typedef size_t ve_size_t;\n\n");

        for include in self.includes.borrow().iter() {
            self.header.push_str(&format!("#include {}\n", include));
        }

        self.header.push('\n');

        self.header.push_str("typedef struct {\n");
        self.header.push_str("    char* memory;\n");
        self.header.push_str("    size_t used;\n");
        self.header.push_str("    size_t capacity;\n");
        self.header.push_str("} ve_Arena;\n\n");

        self.header
            .push_str("static void** ve_malloc_ptrs = NULL;\n");
        self.header.push_str("static size_t ve_malloc_count = 0;\n");
        self.header
            .push_str("static size_t ve_malloc_capacity = 0;\n");
        self.header
            .push_str("static void ve_track_malloc(void* ptr) {\n");
        self.header
            .push_str("    if (ve_malloc_count >= ve_malloc_capacity) {\n");
        self.header.push_str(
            "        ve_malloc_capacity = ve_malloc_capacity ? ve_malloc_capacity * 2 : 16;\n",
        );
        self.header.push_str("        ve_malloc_ptrs = realloc(ve_malloc_ptrs, sizeof(void*) * ve_malloc_capacity);\n");
        self.header.push_str("    }\n");
        self.header
            .push_str("    ve_malloc_ptrs[ve_malloc_count++] = ptr;\n");
        self.header.push_str("}\n\n");

        let thread_local_keyword = if cfg!(target_os = "windows") && cfg!(target_env = "msvc") {
            "__declspec(thread)"
        } else if cfg!(any(target_env = "gnu", target_env = "musl"))
            || cfg!(target_os = "linux")
            || cfg!(target_os = "macos")
        {
            "__thread"
        } else {
            "_Thread_local"
        };

        self.header.push_str(&format!(
            "static {} ve_Arena ve_temp_arena = {{0}};\n",
            thread_local_keyword
        ));
        self.header.push_str(&format!(
            "static {} int ve_arena_depth = 0;\n\n",
            thread_local_keyword
        ));
        self.header.push_str("static void ve_arena_enter() {\n");
        self.header.push_str("    ve_arena_depth++;\n");
        self.header
            .push_str("    if (ve_arena_depth == 1 && !ve_temp_arena.memory) {\n");
        self.header.push_str(&format!(
            "        size_t arena_size = {};\n",
            self.memory_analysis.estimated_arena_size
        ));
        self.header
            .push_str("        ve_temp_arena.memory = malloc(arena_size);\n");
        self.header
            .push_str("        if (!ve_temp_arena.memory) {\n");
        self.header
            .push_str("            fprintf(stderr, \"Failed to allocate arena memory\\n\");\n");
        self.header.push_str("            exit(1);\n");
        self.header.push_str("        }\n");
        self.header
            .push_str("        ve_temp_arena.capacity = arena_size;\n");
        self.header.push_str("        ve_temp_arena.used = 0;\n");
        self.header.push_str("    }\n");
        self.header.push_str("}\n\n");

        self.header.push_str("static void ve_arena_exit() {\n");
        self.header.push_str("    ve_arena_depth--;\n");
        self.header.push_str("    if (ve_arena_depth == 0) {\n");
        self.header.push_str("        ve_temp_arena.used = 0;\n");
        self.header.push_str("    }\n");
        self.header.push_str("}\n\n");
        self.header
            .push_str("char* ve_arena_alloc(size_t size) {\n");
        self.header.push_str("    if (!ve_temp_arena.memory) {\n");
        self.header.push_str("        ve_arena_enter();\n");
        self.header.push_str("    }\n");
        self.header.push_str("    \n");
        self.header.push_str("    size = (size + 7) & ~7;\n");
        self.header.push_str("    \n");
        self.header
            .push_str("    if (ve_temp_arena.used + size >= ve_temp_arena.capacity) {\n");
        self.header
            .push_str("        if (size <= ve_temp_arena.capacity / 2) {\n");
        self.header
            .push_str("            size_t new_capacity = ve_temp_arena.capacity * 2;\n");
        self.header.push_str(
            "            char* new_memory = realloc(ve_temp_arena.memory, new_capacity);\n",
        );
        self.header.push_str("            if (new_memory) {\n");
        self.header
            .push_str("                ve_temp_arena.memory = new_memory;\n");
        self.header
            .push_str("                ve_temp_arena.capacity = new_capacity;\n");
        self.header.push_str("            } else {\n");
        self.header
            .push_str("                char* ptr = malloc(size);\n");
        self.header
            .push_str("                if (ptr) ve_track_malloc(ptr);\n");
        self.header.push_str("                return ptr;\n");
        self.header.push_str("            }\n");
        self.header.push_str("        } else {\n");
        self.header
            .push_str("            char* ptr = malloc(size);\n");
        self.header
            .push_str("            if (ptr) ve_track_malloc(ptr);\n");
        self.header.push_str("            return ptr;\n");
        self.header.push_str("        }\n");
        self.header.push_str("    }\n");
        self.header.push_str("    \n");
        self.header
            .push_str("    char* result = ve_temp_arena.memory + ve_temp_arena.used;\n");
        self.header.push_str("    ve_temp_arena.used += size;\n");
        self.header.push_str("    return result;\n");
        self.header.push_str("}\n\n");

        self.header.push_str("static void ve_arena_cleanup() {\n");
        self.header
            .push_str("    for (size_t i = 0; i < ve_malloc_count; i++) {\n");
        self.header.push_str("        free(ve_malloc_ptrs[i]);\n");
        self.header.push_str("    }\n");
        self.header.push_str("    free(ve_malloc_ptrs);\n");
        self.header.push_str("    ve_malloc_ptrs = NULL;\n");
        self.header.push_str("    ve_malloc_count = 0;\n");
        self.header.push_str("    ve_malloc_capacity = 0;\n");
        self.header.push_str("    if (ve_temp_arena.memory) {\n");
        self.header
            .push_str("        free(ve_temp_arena.memory);\n");
        self.header
            .push_str("        ve_temp_arena.memory = NULL;\n");
        self.header.push_str("        ve_temp_arena.used = 0;\n");
        self.header
            .push_str("        ve_temp_arena.capacity = 0;\n");
        self.header.push_str("    }\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_int_to_str(int num) {\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(12);\n");
        self.header.push_str("    sprintf(buffer, \"%d\", num);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");
        self.header
            .push_str("static char* ve_float_to_str(float num) {\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(32);\n");
        self.header.push_str("    sprintf(buffer, \"%g\", num);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_double_to_str(double num) {\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(32);\n");
        self.header.push_str("    sprintf(buffer, \"%g\", num);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");
        self.header
            .push_str("static char* ve_i8_to_str(ve_i8 num) {\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(8);\n");
        self.header
            .push_str("    sprintf(buffer, \"%d\", (int)num);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_i16_to_str(ve_i16 num) {\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(8);\n");
        self.header
            .push_str("    sprintf(buffer, \"%d\", (int)num);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_i64_to_str(ve_i64 num) {\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(24);\n");
        self.header
            .push_str("    sprintf(buffer, \"%lld\", num);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_u8_to_str(ve_u8 num) {\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(8);\n");
        self.header
            .push_str("    sprintf(buffer, \"%u\", (unsigned int)num);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_u16_to_str(ve_u16 num) {\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(8);\n");
        self.header
            .push_str("    sprintf(buffer, \"%u\", (unsigned int)num);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_u32_to_str(ve_u32 num) {\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(16);\n");
        self.header.push_str("    sprintf(buffer, \"%u\", num);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_u64_to_str(ve_u64 num) {\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(24);\n");
        self.header
            .push_str("    sprintf(buffer, \"%llu\", num);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_bool_to_str(bool b) {\n");
        self.header
            .push_str("    const char* val = b ? \"true\" : \"false\";\n");
        self.header.push_str("    size_t len = strlen(val) + 1;\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(len);\n");
        self.header
            .push_str("    if (buffer) memcpy(buffer, val, len);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header
            .push_str("static char* ve_ptr_to_str(void* ptr) {\n");
        self.header
            .push_str("    char* buffer = ve_arena_alloc(20);\n");
        self.header.push_str("    sprintf(buffer, \"%p\", ptr);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");
        let (strcpy_fn, strcat_fn) = if cfg!(target_os = "windows") && cfg!(target_env = "msvc") {
            (
                "strcpy_s(result, len1 + len2 + 1, s1)",
                "strcat_s(result, len1 + len2 + 1, s2)",
            )
        } else {
            ("strcpy(result, s1)", "strcat(result, s2)")
        };

        self.header
            .push_str("static char* ve_concat(const char* s1, const char* s2) {\n");
        self.header.push_str("    size_t len1 = strlen(s1);\n");
        self.header.push_str("    size_t len2 = strlen(s2);\n");
        self.header
            .push_str("    char* result = ve_arena_alloc(len1 + len2 + 1);\n");
        self.header.push_str("    if (result) {\n");
        self.header.push_str(&format!("        {};\n", strcpy_fn));
        self.header.push_str(&format!("        {};\n", strcat_fn));
        self.header.push_str("    }\n");
        self.header.push_str("    return result;\n");
        self.header.push_str("}\n\n");

        #[cfg(debug_assertions)]
        {
            self.header.push_str("#ifdef VE_DEBUG_MEMORY\n");
            self.header.push_str("static void ve_arena_stats() {\n");
            self.header
                .push_str("    printf(\"Arena Statistics:\\n\");\n");
            self.header
                .push_str("    printf(\"  Capacity: %zu bytes\\n\", ve_temp_arena.capacity);\n");
            self.header
                .push_str("    printf(\"  Used: %zu bytes\\n\", ve_temp_arena.used);\n");
            self.header.push_str("    printf(\"  Free: %zu bytes\\n\", ve_temp_arena.capacity - ve_temp_arena.used);\n");
            self.header
                .push_str("    printf(\"  Utilization: %.1f%%\\n\", \n");
            self.header.push_str("           ve_temp_arena.capacity > 0 ? (100.0 * ve_temp_arena.used / ve_temp_arena.capacity) : 0.0);\n");
            self.header
                .push_str("    printf(\"  Malloc fallbacks: %zu\\n\", ve_malloc_count);\n");
            self.header.push_str("}\n");
            self.header.push_str("#endif\n\n");
        }
    }
    fn emit_globals(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        for stmt in &program.stmts {
            if let ast::Stmt::Let(name, ty, expr, _, _) = stmt {
                if self.is_constant_expr(expr) {
                    let c_ty = self.type_to_c(ty.as_ref().unwrap_or(&Type::I32));
                    let value = self.emit_expr(expr)?;
                    self.body
                        .push_str(&format!("{} {} = {};\n", c_ty, name, value));
                } else {
                    return Err(CompileError::CodegenError {
                        message: format!("Non-constant initializer for global '{}'", name),
                        span: Some(expr.span()),
                        file_id: self.file_id,
                    });
                }
            }
        }
        Ok(())
    }

    fn is_constant_expr(&self, expr: &ast::Expr) -> bool {
        matches!(
            expr,
            ast::Expr::Int(..) | ast::Expr::Str(..) | ast::Expr::Bool(..) | ast::Expr::F32(..)
        )
    }

    fn emit_main_if_missing(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        if !program.functions.iter().any(|f| f.name == "main") {
            self.body.push_str("\nint main() {\n");

            for stmt in &program.stmts {
                if !matches!(stmt, ast::Stmt::Let(..)) {
                    self.emit_stmt(stmt)?;
                }
            }

            self.body.push_str("#ifdef VE_DEBUG_MEMORY\n    ve_arena_stats();\n#endif\n");
            self.body.push_str("    ve_arena_cleanup();\n");
            self.body.push_str("    return 0;\n}\n");
        }
        Ok(())
    }

    fn emit_functions(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        for func in &program.functions {
            let return_type = if func.name == "main" {
                "int".to_string()
            } else {
                self.type_to_c(&func.return_type)
            };
            let func_name = if func.name == "main" {
                func.name.clone()
            } else {
                format!("ve_{}", func.name)
            };
            

            
            let params = func
                .params
                .iter()
                .map(|(name, ty)| format!("{} {}", self.type_to_c(ty), name))
                .collect::<Vec<_>>()
                .join(", ");
            self.body
                .push_str(&format!("{} {}({});\n", return_type, func_name, params));
        }

        for func in &program.functions {
            self.emit_function(func)?;
        }

        Ok(())
    }

    fn emit_function(&mut self, func: &ast::Function) -> Result<(), CompileError> {
        let return_type = if func.name == "main" {
            "int".to_string()
        } else {
            self.type_to_c(&func.return_type)
        };

        let func_name = if func.name == "main" {
            func.name.clone()
        } else {
            format!("ve_{}", func.name)
        };

        let mut param_strings = Vec::new();
        for (name, ty) in &func.params {
            let c_ty = self.type_to_c(ty);
            param_strings.push(format!("{} {}", c_ty, name));
            self.variables.borrow_mut().insert(name.clone(), ty.clone());
        }
        let params = param_strings.join(", ");

        self.body
            .push_str(&format!("{} {}({}) {{\n", return_type, func_name, params));

        let mut stmts = func.body.iter().peekable();
        while let Some(stmt) = stmts.next() {
            let is_last = stmts.peek().is_none();
            if is_last {
                if let ast::Stmt::Expr(expr, _) = stmt {
                    if expr.get_info().is_tail {
                        let expr_code = self.emit_expr(expr)?;
                        self.body.push_str(&format!("return {};\n", expr_code));
                        continue;
                    }
                }
            }
            self.emit_stmt(stmt)?;
        }

        if func.name == "main" {
            let last_is_return = func
                .body
                .last()
                .is_some_and(|s| matches!(s, ast::Stmt::Return(..)));

            if !last_is_return {
                self.body.push_str("#ifdef VE_DEBUG_MEMORY\n    ve_arena_stats();\n#endif\n");
                self.body.push_str("    ve_arena_cleanup();\n");
                self.body.push_str("    return 0;\n");
            }
        } else if func.return_type == Type::Void {
            self.body.push_str("    return;\n");
        }

        self.body.push_str("}\n\n");
        Ok(())
    }

    fn emit_stmt(&mut self, stmt: &ast::Stmt) -> Result<(), CompileError> {
        match stmt {
            ast::Stmt::Let(name, ty, expr, _, _) => {
                if let ast::Expr::Match(pattern, arms, info) = expr {
                    let var_type = ty.clone().unwrap_or_else(|| info.ty.clone());
                    let c_ty = self.type_to_c(&var_type);
                    let temp_var = format!("_match_result_{}", info.span.start());
                    self.body.push_str(&format!("{} {} = 0;\n", c_ty, temp_var));
                    let mut match_code = String::new();
                    self.emit_match_switch_with_result(
                        &match pattern.as_ref() {
                            ast::Pattern::Variable(var_name, _) => var_name.clone(),
                            _ => "_match_input".to_string(),
                        },
                        &temp_var,
                        arms,
                        &mut match_code,
                    )?;
                    self.body.push_str(&match_code);
                    self.body
                        .push_str(&format!("{} {} = {};\n", c_ty, name, temp_var));
                    self.variables.borrow_mut().insert(name.clone(), var_type);
                    return Ok(());
                }
                let var_type = if let Some(ty) = ty {
                    ty.clone()
                } else {
                    match expr {
                        ast::Expr::Int(_, _) => Type::I32,
                        ast::Expr::F32(_, _) => Type::F32,
                        ast::Expr::Bool(_, _) => Type::Bool,
                        ast::Expr::Str(_, _) => Type::String,
                        ast::Expr::Call(func_name, _, _) => {
                            self.functions_map.get(func_name).cloned().ok_or_else(|| {
                                CompileError::CodegenError {
                                    message: format!("Undefined function '{}'", func_name),
                                    span: Some(expr.span()),
                                    file_id: self.file_id,
                                }
                            })?
                        }
                        _ => expr.get_type(),
                    }
                };
                let c_ty = self.type_to_c(&var_type);
                let expr_code = self.emit_expr(expr)?;
                self.body
                    .push_str(&format!("{} {} = {};\n", c_ty, name, expr_code));
                self.variables.borrow_mut().insert(name.clone(), var_type);
            }
            ast::Stmt::Return(expr, _) => {
                if let ast::Expr::Void(_) = expr {
                    let current_func = self.body.rsplit_once("(").and_then(|(before, _)| {
                        before.rsplit_once(' ').map(|(_, name)| name.trim())
                    });
                    let func_name = current_func.unwrap_or("");
                    let ret_type = self.functions_map.get(func_name);
                    if func_name == "main" || ret_type == Some(&Type::I32) {
                        self.body.push_str("return 0;\n");
                        return Ok(());
                    } else if ret_type == Some(&Type::Void) {
                        self.body.push_str("return;\n");
                        return Ok(());
                    } else {
                        return Ok(());
                    }
                }
                let expr_code = self.emit_expr(expr)?;
                self.body.push_str(&format!("return {};\n", expr_code));
            }
            ast::Stmt::Expr(expr, _) => {
                let needs_arena = match expr {
                    ast::Expr::Call(name, _, _) => !self.ffi_functions.contains(name),
                    _ => false,
                };

                if needs_arena {
                    self.body.push_str("ve_arena_enter();\n");
                }

                let expr_code = self.emit_expr(expr)?;
                if expr_code.starts_with('{') {
                    self.body.push_str(&expr_code);
                } else if !expr_code.ends_with(';') {
                    self.body.push_str(&format!("{};\n", expr_code));
                } else {
                    self.body.push_str(&format!("{}\n", expr_code));
                }

                if needs_arena {
                    self.body.push_str("ve_arena_exit();\n");
                }
            }
            ast::Stmt::Block(stmts, _) => {
                for s in stmts {
                    self.emit_stmt(s)?;
                }
            }
            ast::Stmt::While(cond, body, _) => {
                let cond_code = self.emit_expr(cond)?;
                self.body.push_str(&format!("while ({}) {{\n", cond_code));
                for stmt in body {
                    self.emit_stmt(stmt)?;
                }
                self.body.push_str("}\n");
            }
            ast::Stmt::For(var_name, range, body, _) => {
                let range_code = self.emit_expr(range)?;
                let parts: Vec<&str> = range_code.split("..").collect();
                let start = parts[0].trim();
                let end = parts[1].trim();
                self.body.push_str(&format!(
                    "for (int {var} = {start}; {var} < {end}; {var}++) {{\n",
                    var = var_name
                ));

                for stmt in body {
                    self.emit_stmt(stmt)?;
                }
                self.body.push_str("}\n");
            }
            ast::Stmt::If(cond, then_branch, else_branch, _) => {
                let cond_code = self.emit_expr(cond)?;
                self.body.push_str(&format!("if ({}) {{\n", cond_code));

                for stmt in then_branch {
                    self.emit_stmt(stmt)?;
                }
                self.body.push('}');

                if let Some(else_body) = else_branch {
                    self.body.push_str(" else {\n");
                    for stmt in else_body {
                        self.emit_stmt(stmt)?;
                    }
                    self.body.push('}');
                }

                self.body.push('\n');
            }
            _ => unimplemented!(),
        }
        Ok(())
    }

    fn emit_expr(&mut self, expr: &ast::Expr) -> Result<String, CompileError> {
        match expr {
            ast::Expr::Int(n, _) => Ok(n.to_string()),
            ast::Expr::Int64(n, _) => Ok(n.to_string()),
            ast::Expr::F32(f, _) => Ok(f.to_string()),
            ast::Expr::Bool(b, _) => {
                self.includes.borrow_mut().insert("<stdbool.h>".to_string());
                Ok(if *b { "true" } else { "false" }.to_string())
            }
            ast::Expr::BinOp(left, op, right, _) => {
                let left_code = self.emit_expr(left)?;
                let right_code = self.emit_expr(right)?;

                let left_type = left.get_type();
                let right_type = right.get_type();
                let is_string_cmp =
                    matches!(left_type, Type::String) || matches!(right_type, Type::String);
                if is_string_cmp {
                    match op {
                        ast::BinOp::Eq => {
                            self.includes.borrow_mut().insert("<string.h>".to_string());
                            let left_conv = self.convert_to_c_str(&left_code, &left_type);
                            let right_conv = self.convert_to_c_str(&right_code, &right_type);
                            return Ok(format!("(strcmp({}, {}) == 0)", left_conv, right_conv));
                        }
                        ast::BinOp::NotEq => {
                            self.includes.borrow_mut().insert("<string.h>".to_string());
                            let left_conv = self.convert_to_c_str(&left_code, &left_type);
                            let right_conv = self.convert_to_c_str(&right_code, &right_type);
                            return Ok(format!("(strcmp({}, {}) != 0)", left_conv, right_conv));
                        }
                        _ => {
                            let left_conv = self.convert_to_c_str(&left_code, &left_type);
                            let right_conv = self.convert_to_c_str(&right_code, &right_type);
                            return Ok(format!("ve_concat({}, {})", left_conv, right_conv));
                        }
                    }
                }

                let result_type = expr.get_type();
                match result_type {
                    Type::String => match op {
                        ast::BinOp::Eq => {
                            self.includes.borrow_mut().insert("<string.h>".to_string());
                            let left_conv = self.convert_to_c_str(&left_code, &left.get_type());
                            let right_conv = self.convert_to_c_str(&right_code, &right.get_type());
                            Ok(format!("(strcmp({}, {}) == 0)", left_conv, right_conv))
                        }
                        ast::BinOp::NotEq => {
                            self.includes.borrow_mut().insert("<string.h>".to_string());
                            let left_conv = self.convert_to_c_str(&left_code, &left.get_type());
                            let right_conv = self.convert_to_c_str(&right_code, &right.get_type());
                            Ok(format!("(strcmp({}, {}) != 0)", left_conv, right_conv))
                        }
                        _ => {
                            let left_conv = self.convert_to_c_str(&left_code, &left.get_type());
                            let right_conv = self.convert_to_c_str(&right_code, &right.get_type());
                            Ok(format!("ve_concat({}, {})", left_conv, right_conv))
                        }
                    },
                    _ => {
                        let c_op = match op {
                            ast::BinOp::Add => "+",
                            ast::BinOp::Sub => "-",
                            ast::BinOp::Mul => "*",
                            ast::BinOp::Div => "/",
                            ast::BinOp::Mod => "%",
                            ast::BinOp::Eq => "==",
                            ast::BinOp::NotEq => "!=",
                            ast::BinOp::Gt => ">",
                            ast::BinOp::Lt => "<",
                            ast::BinOp::GtEq => ">=",
                            ast::BinOp::LtEq => "<=",
                            ast::BinOp::And => "&&",
                            ast::BinOp::Or => "||",
                            ast::BinOp::Pow | ast::BinOp::Pow2 => "",
                        };

                        if matches!(op, ast::BinOp::Pow | ast::BinOp::Pow2) {
                            self.includes.borrow_mut().insert("<math.h>".to_string());
                            return Ok(format!("pow({}, {})", left_code, right_code));
                        }

                        Ok(format!("({} {} {})", left_code, c_op, right_code))
                    }
                }
            }
            ast::Expr::Assign(target, value, _) => {
                let target_code = self.emit_expr(target)?;
                let value_code = self.emit_expr(value)?;
                Ok(format!("({} = {})", target_code, value_code))
            }
            ast::Expr::Str(s, _) => Ok(format!(
                "\"{}\"",
                s.replace("\n", "\\n").replace("\"", "\\\"")
            )),
            ast::Expr::Var(name, info) => {
                if name == "true" || name == "false" {
                    self.includes.borrow_mut().insert("<stdbool.h>".to_string());
                    return Ok(name.clone());
                }

                let var_type = self
                    .variables
                    .borrow()
                    .get(name)
                    .cloned()
                    .unwrap_or(Type::Unknown);
                match var_type {
                    Type::I32 | Type::String | Type::Pointer(_) | Type::RawPtr | Type::Struct(_)| 
                    Type::Array(_) | Type::SizedArray(_, _) | Type::F32 | Type::F64 | Type::I8 | 
                    Type::I16 | Type::I64 | Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::Unknown 
                    => Ok(name.clone()),
                    Type::Bool => {
                        self.includes.borrow_mut().insert("<stdbool.h>".to_string());
                        Ok(name.clone())
                    }
                    _ => Err(CompileError::CodegenError {
                        message: format!("Cannot print type (in var) {:?}", var_type),
                        span: Some(info.span),
                        file_id: self.file_id,
                    }),
                }
            }
            ast::Expr::Call(name, args, _expr_info) => {
                let mut args_code = Vec::new();

                let param_types = self
                    .imported_functions
                    .get(name)
                    .map(|(params, _)| params.clone());

                for (i, arg) in args.iter().enumerate() {
                    let value = self.emit_expr(arg)?;
                    let arg_type = arg.get_type();

                    let expected_type =
                        param_types.as_ref().and_then(|types| types.get(i)).cloned();

                    if let Some(expected) = expected_type {
                        if expected == Type::String && arg_type != Type::String {
                            args_code.push(self.convert_to_c_str(&value, &arg_type));
                        } else {
                            args_code.push(value);
                        }
                    } else {
                        args_code.push(value);
                    }
                }
                let final_name = if self.ffi_functions.contains(name) {
                    name.clone()
                } else {
                    format!("ve_{}", name)
                };

                Ok(format!("{}({})", final_name, args_code.join(", ")))
            }
            ast::Expr::Void(_) => Ok("".to_string()),
            ast::Expr::SafeBlock(stmts, _) => {
                let mut code = String::new();
                code.push_str("{\n");
                let mut defers = Vec::new();

                for stmt in stmts {
                    match stmt {
                        ast::Stmt::Defer(expr, _) => {
                            let expr_code = self.emit_expr(expr)?;
                            defers.push(expr_code);
                        }
                        _ => {
                            let stmt_code = self.emit_stmt_to_string(stmt)?;
                            code.push_str(&stmt_code);
                        }
                    }
                }

                for deferred in defers.into_iter().rev() {
                    code.push_str(&format!("{};\n", deferred));
                }

                code.push_str("}\n");
                Ok(code)
            }
            ast::Expr::Deref(expr, _) => {
                let inner = self.emit_expr(expr)?;
                Ok(format!("(*{})", inner))
            }
            ast::Expr::Cast(expr, target_ty, _) => {
                let expr_code = self.emit_expr(expr)?;
                let expr_type = expr.get_type();
                match (&expr_type, target_ty) {
                    (Type::I32, Type::String) => Ok(format!("ve_int_to_str({})", expr_code)),
                    (Type::I64, Type::String) => Ok(format!("ve_i64_to_str({})", expr_code)),
                    (Type::F32, Type::String) => Ok(format!("ve_float_to_str({})", expr_code)),
                    (Type::F64, Type::String) => Ok(format!("ve_double_to_str({})", expr_code)),
                    (Type::Bool, Type::String) => Ok(format!("ve_bool_to_str({})", expr_code)),
                    (Type::RawPtr, Type::String) => Ok(format!("(const char*)({})", expr_code)),
                    (Type::Pointer(inner), Type::String) => {
                        if matches!(**inner, Type::U8) {
                            Ok(format!("(const char*)({})", expr_code))
                        } else {
                            Ok(format!("ve_ptr_to_str({})", expr_code))
                        }
                    }
                    (Type::String, Type::I32) => {
                        self.includes.borrow_mut().insert("<stdlib.h>".to_string());
                        Ok(format!("atoi({})", expr_code))
                    }
                    (_, Type::Pointer(inner_ty)) => Ok(format!("({}*)({})", self.type_to_c(inner_ty), expr_code)),
                    (_, Type::RawPtr) => Ok(format!("(void*)({})", expr_code)),
                    _ => Ok(format!("({})({})", self.type_to_c(target_ty), expr_code)),
                }
            }
            ast::Expr::Range(start, end, _) => {
                let start_code = self.emit_expr(start)?;
                let end_code = self.emit_expr(end)?;
                Ok(format!("{} .. {}", start_code, end_code))
            }
            ast::Expr::StructInit(name, fields, _) => {
                let mut field_inits = Vec::new();
                for (field_name, field_expr) in fields {
                    let field_code = self.emit_expr(field_expr)?;
                    field_inits.push(format!(".{} = {}", field_name, field_code));
                }

                Ok(format!("(ve_{}){{ {} }}", name, field_inits.join(", ")))
            }
            ast::Expr::FieldAccess(obj, field_name, _info) => {
                let obj_code = self.emit_expr(obj)?;
                Ok(format!("{}.{}", obj_code, field_name))
            }
            ast::Expr::ArrayInit(elements, info) => {
                let element_type = match &info.ty {
                    Type::Array(inner) => inner.clone(),
                    _ => {
                        return Err(CompileError::CodegenError {
                            message: "Array initialization with non-array type".to_string(),
                            span: Some(info.span),
                            file_id: self.file_id,
                        });
                    }
                };

                let element_type_str = self.type_to_c(&element_type);
                let mut element_exprs = Vec::new();

                for elem in elements {
                    element_exprs.push(self.emit_expr(elem)?);
                }

                let elements_str = element_exprs.join(", ");

                self.includes.borrow_mut().insert("<stdlib.h>".to_string());

                let temp_var = format!("_array_stack_{}", info.span.start());
                let temp_var_ptr = format!("_array_ptr_{}", info.span.start());
                let size = elements.len();
                Ok(format!(
                    "({{ \
                    {} {}[] = {{ {} }}; \
                    {}* {} = ve_arena_alloc({} * sizeof({})); \
                    if ({}) {{ \
                        for (int _i = 0; _i < {}; _i++) {{{}[_i] = {}[_i];}} \
                    }} \
                    {}; \
                }})",
                    element_type_str,
                    temp_var,
                    elements_str,
                    element_type_str,
                    temp_var_ptr,
                    size,
                    element_type_str,
                    temp_var_ptr,
                    size,
                    temp_var_ptr,
                    temp_var,
                    temp_var_ptr
                ))
            }
            ast::Expr::ArrayAccess(array, index, _info) => {
                let array_expr = self.emit_expr(array)?;
                let index_expr = self.emit_expr(index)?;
                let array_type = array.get_type();

                match array_type {
                    Type::Pointer(inner_ty) => Ok(format!(
                        "(({}*){})[{}]",
                        self.type_to_c(&inner_ty),
                        array_expr,
                        index_expr
                    )),
                    Type::RawPtr => Ok(format!("((unsigned char*){})[{}]", array_expr, index_expr)),
                    _ => Ok(format!("{}[{}]", array_expr, index_expr)),
                }
            }
            ast::Expr::TemplateStr(parts, _info) => {
                if parts.is_empty() {
                    return Ok("\"\"".to_string());
                }

                let mut result = String::new();
                let mut is_first = true;

                for part in parts {
                    let part_code = match part {
                        ast::TemplateStrPart::Literal(text) => {
                            format!("\"{}\"", text.replace("\n", "\\n").replace("\"", "\\\""))
                        }
                        ast::TemplateStrPart::Expression(expr) => {
                            let expr_code = self.emit_expr(expr)?;
                            let expr_type = expr.get_type();

                            match &**expr {
                                ast::Expr::ArrayAccess(array, _, _) => {
                                    let array_type = array.get_type();
                                    match array_type {
                                        Type::Array(element_type)
                                        | Type::SizedArray(element_type, _) => {
                                            match *element_type {
                                                Type::I32 => {
                                                    format!("ve_int_to_str({})", expr_code)
                                                }
                                                Type::Bool => {
                                                    format!("ve_bool_to_str({})", expr_code)
                                                }
                                                Type::String => expr_code,
                                                _ => {
                                                    format!("\"[unsupported array element type]\"")
                                                }
                                            }
                                        }
                                        _ => format!("\"[not an array]\""),
                                    }
                                }
                                _ => self.convert_to_c_str(&expr_code, &expr_type),
                            }
                        }
                    };

                    if is_first {
                        result = part_code;
                        is_first = false;
                    } else {
                        result = format!("ve_concat({}, {})", result, part_code);
                    }
                }

                Ok(result)
            }
            ast::Expr::FfiCall(name, args, _) => {
                let mut args_code = Vec::new();
                for arg in args {
                    let arg_code = self.emit_expr(arg)?;
                    args_code.push(arg_code);
                }
                Ok(format!("{}({})", name, args_code.join(", ")))
            }
            ast::Expr::UnaryOp(op, expr, _) => {
                let inner_code = self.emit_expr(expr)?;
                match op {
                    ast::UnOp::Neg => Ok(format!("-{}", inner_code)),
                    ast::UnOp::Plus => Ok(format!("{}", inner_code)),
                    ast::UnOp::Not => Ok(format!("!{}", inner_code)),
                }
            }

            ast::Expr::EnumConstruct(enum_name, variant_name, args, info) => {
                let mut args_code = Vec::new();
                for arg in args {
                    args_code.push(self.emit_expr(arg)?);
                }

                let enum_type_name = match &info.ty {
                    Type::GenericInstance(_, _) => self.type_to_c_name(&info.ty),
                    _ => enum_name.clone()
                };
                let prefixed_enum = format!("ve_{}", enum_type_name);
                if args_code.is_empty() {
                    Ok(format!("{}_{}_new()", prefixed_enum, variant_name))
                } else {
                    Ok(format!(
                        "{}_{}_new({})",
                        prefixed_enum,
                        variant_name,
                        args_code.join(", ")
                    ))
                }
            }

        ast::Expr::Match(pattern, arms, info) => {
            let matched_var = match pattern.as_ref() {
                ast::Pattern::Variable(var_name, _) => var_name.clone(),
                _ => {
                    return Err(CompileError::CodegenError {
                        message: "Only variable patterns supported in match".to_string(),
                        span: None,
                        file_id: self.file_id,
                    });
                }
            };

            let matched_type = self.variables.borrow().get(&matched_var).cloned().unwrap_or(Type::Unknown);

            let switch_expr = match matched_type {
                Type::Enum(_) | Type::GenericInstance(_, _) => format!("{}.tag", matched_var),
                _ => matched_var.clone(),
            };

            let mut code = String::new();
            code.push_str(&format!("switch ({}) {{\n", switch_expr));

            for arm in arms {
                match &arm.pattern {
                    ast::Pattern::EnumVariant(enum_name, variant_name, patterns, _) => {
                        let case_value = if let Some(matched_type) = self.variables.borrow().get(&matched_var) {
                            match matched_type {
                                Type::GenericInstance(_, _) => {
                                    format!("ve_{}_{}", self.type_to_c_name(matched_type), variant_name)
                                }
                                _ => format!("ve_{}_{}", enum_name, variant_name)
                            }
                        } else {
                            format!("ve_{}_{}", enum_name, variant_name)
                        };
                        code.push_str(&format!("case {}: {{\n", case_value));

                        for (i, pattern) in patterns.iter().enumerate() {
                            if let ast::Pattern::Variable(var_name, _) = pattern {
                                let mut field_type = "int".to_string();
                                if let Some(matched_type) = self.variables.borrow().get(&matched_var) {
                                    if let Type::GenericInstance(enum_name, args) = matched_type {
                                        if let Some(enum_def) = self.enum_defs.get(enum_name) {
                                            enum_def.variants.iter().find(|v| v.name == *variant_name).map(|variant| {
                                                if let Some(data_types) = &variant.data {
                                                    if let Some(ty) = data_types.get(i) {
                                                        if let Type::Generic(param_name) = ty {
                                                            if let Some(generic_idx) = enum_def.generic_params.iter().position(|p| p == param_name) {
                                                                if let Some(concrete_type) = args.get(generic_idx) {
                                                                    field_type = self.type_to_c(concrete_type);
                                                                }
                                                            }
                                                        } else {
                                                            field_type = self.type_to_c(ty);
                                                        }
                                                    }
                                                }
                                            });
                                        }
                                    }
                                }
                                code.push_str(&format!(
                                    "    {} {} = {}.data.{}.field{};\n",
                                    field_type,
                                    var_name,
                                    matched_var,
                                    variant_name.to_lowercase(),
                                    i
                                ));
                            }
                        }

                        let body_code = match &arm.body {
                            ast::MatchArmBody::Expr(expr) => self.emit_expr(expr)?,
                            ast::MatchArmBody::Block(stmts) => {
                                let mut block_code = String::new();
                                for stmt in stmts {
                                    block_code.push_str(&self.emit_stmt_to_string(stmt)?);
                                }
                                block_code
                            }
                        };
                        
                        if info.is_tail {
                            code.push_str(&format!("    return {};\n", body_code));
                        } else {
                            code.push_str(&format!("    {};\n", body_code));
                            code.push_str("    break;\n");
                        }
                        code.push_str("}\n");
                    }
                    ast::Pattern::Literal(expr, _) => {
                        let literal_code = self.emit_expr(expr)?;
                        code.push_str(&format!("case {}: {{\n", literal_code));
                        let body_code = match &arm.body {
                            ast::MatchArmBody::Expr(expr) => self.emit_expr(expr)?,
                            ast::MatchArmBody::Block(stmts) => {
                                let mut block_code = String::new();
                                for stmt in stmts {
                                    block_code.push_str(&self.emit_stmt_to_string(stmt)?);
                                }
                                block_code
                            }
                        };
                        if info.is_tail {
                            code.push_str(&format!("    return {};\n", body_code));
                        } else {
                            code.push_str(&format!("    {};\n", body_code));
                            code.push_str("    break;\n");
                        }
                        code.push_str("}\n");
                    }
                    ast::Pattern::Wildcard(_) => {
                        code.push_str("default: {\n");
                        let body_code = match &arm.body {
                            ast::MatchArmBody::Expr(expr) => self.emit_expr(expr)?,
                            ast::MatchArmBody::Block(stmts) => {
                                let mut block_code = String::new();
                                for stmt in stmts {
                                    block_code.push_str(&self.emit_stmt_to_string(stmt)?);
                                }
                                block_code
                            }
                        };
                        if info.is_tail {
                            code.push_str(&format!("    return {};\n", body_code));
                        } else {
                            code.push_str(&format!("    {};\n", body_code));
                            code.push_str("    break;\n");
                        }
                        code.push_str("}\n");
                    }
                    _ => {
                        return Err(CompileError::CodegenError {
                            message: "Unsupported pattern in match".to_string(),
                            span: Some(arm.span),
                            file_id: self.file_id,
                        });
                    }
                }
            }
            code.push_str("}\n");
            Ok(code)
        }
                }
    }

    fn emit_stmt_to_string(&mut self, stmt: &ast::Stmt) -> Result<String, CompileError> {
        let original_body = std::mem::take(&mut self.body);
        self.emit_stmt(stmt)?;
        let result = std::mem::replace(&mut self.body, original_body);
        Ok(result)
    }

    fn type_to_c_name(&self, ty: &Type) -> String {
        match ty {
            Type::I32 => "i32",
            Type::Bool => "bool",
            Type::String => "string",
            Type::Void => "void",
            Type::F32 => "f32",
            Type::F64 => "f64",
            Type::I8 => "i8",
            Type::I16 => "i16",
            Type::I64 => "i64",
            Type::U8 => "u8",
            Type::U16 => "u16",
            Type::U32 => "u32",
            Type::U64 => "u64",
            Type::Struct(name) | Type::Enum(name) | Type::Generic(name) => name.as_str(),
            Type::GenericInstance(name, args) => {
                let mut s = name.clone();
                for arg in args {
                    s.push('_');
                    s.push_str(&self.type_to_c_name(arg));
                }
                return s;
            }
            _ => "unknown",
        }.to_string()
    }

    fn type_to_c(&self, ty: &Type) -> String {
        match ty {
            Type::GenericInstance(_, _) | Type::Struct(_) | Type::Enum(_) => format!("ve_{}", self.type_to_c_name(ty)),
            Type::I32 => "int".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "const char*".to_string(),
            Type::Void => "void".to_string(),
            Type::Ellipsis => "...".to_string(),
            Type::Function(args, ret) => {
                let args_str = args.iter().map(|t| self.type_to_c(t)).collect::<Vec<_>>().join(", ");
                let ret_str = self.type_to_c(ret);
                format!("{}(*)({})", ret_str, args_str)
            }
            Type::Pointer(inner) | Type::Array(inner) | Type::SizedArray(inner, _) => {
                format!("{}*", self.type_to_c(inner))
            }
            Type::F32 => "float".to_string(),
            Type::I8 => "ve_i8".to_string(),
            Type::I16 => "ve_i16".to_string(),
            Type::I64 => "ve_i64".to_string(),
            Type::U8 => "ve_u8".to_string(),
            Type::U16 => "ve_u16".to_string(),
            Type::U32 => "ve_u32".to_string(),
            Type::U64 => "ve_u64".to_string(),
            Type::F64 => "double".to_string(),
            Type::CChar => "char".to_string(),
            Type::CInt => "int".to_string(),
            Type::CSize => "ve_size_t".to_string(),
            Type::RawPtr => "void*".to_string(),
            Type::Any => "void*".to_string(),
            Type::Unknown | Type::Generic(_) | _ => "void*".to_string(),
        }
    }

    fn type_to_c_ffi(&self, ty: &Type) -> String {
        match ty {
            Type::GenericInstance(_, _) => format!("ve_{}", self.type_to_c_name(ty)),
            Type::Struct(_) | Type::Enum(_) => format!("ve_{}", self.type_to_c_name(ty)),
            Type::I32 => "int".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "const char*".to_string(),
            Type::Void => "void".to_string(),
            Type::Ellipsis => "...".to_string(),
            Type::Unknown => "void*".to_string(),
            Type::Function(args, ret) => {
                let args_str = args
                    .iter()
                    .map(|t| self.type_to_c_ffi(t))
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret_str = self.type_to_c_ffi(ret);
                format!("{}(*)({})", ret_str, args_str)
            }
            Type::Pointer(inner) => format!("{}*", self.type_to_c_ffi(inner)),
            Type::RawPtr => "void*".to_string(),
            Type::Array(inner) => format!("{}*", self.type_to_c_ffi(inner)),
            Type::SizedArray(inner, _) => format!("{}*", self.type_to_c_ffi(inner)),
            Type::Any => "void*".to_string(),
            Type::F32 => "float".to_string(),
            Type::I8 => "int8_t".to_string(),
            Type::I16 => "int16_t".to_string(),
            Type::I64 => "int64_t".to_string(),
            Type::U8 => "unsigned char".to_string(),
            Type::U16 => "uint16_t".to_string(),
            Type::U32 => "uint32_t".to_string(),
            Type::U64 => "uint64_t".to_string(),
            Type::F64 => "double".to_string(),
            Type::CChar => "char".to_string(),
            Type::CInt => "int".to_string(),
            Type::CSize => "size_t".to_string(),
            Type::Generic(_) => "void*".to_string(),
            _ => "void*".to_string(),
        }
    }

    fn write_output(&self, c_file_path: &Path) -> Result<(), CompileError> {
        std::fs::write(c_file_path, format!("{}{}", self.header, self.body))
            .map_err(CompileError::IOError)?;
        Ok(())
    }
    fn convert_to_c_str(&mut self, code: &str, ty: &Type) -> String {
        self.includes.borrow_mut().insert("<string.h>".to_string());
        match ty {
            Type::I32 => format!("ve_int_to_str({})", code),
            Type::Bool => format!("ve_bool_to_str({})", code),
            Type::RawPtr => format!("(const char*)({})", code),
            Type::Pointer(_) => format!("ve_ptr_to_str({})", code),
            Type::String => code.to_string(),
            Type::Struct(name) => format!("ve_{}_to_str(&{})", name, code),
            Type::F32 => format!("ve_float_to_str({})", code),
            Type::F64 => format!("ve_double_to_str({})", code),
            Type::I8 => format!("ve_i8_to_str({})", code),
            Type::I16 => format!("ve_i16_to_str({})", code),
            Type::I64 => format!("ve_i64_to_str({})", code),
            Type::U8 => format!("ve_u8_to_str({})", code),
            Type::U16 => format!("ve_u16_to_str({})", code),
            Type::U32 => format!("ve_u32_to_str({})", code),
            Type::U64 => format!("ve_u64_to_str({})", code),
            Type::Array(_) => "[array]".to_string(),
            Type::SizedArray(_, _) => "[sized array]".to_string(),
            Type::Any => "[any]".to_string(),
            Type::Unknown => {
                eprintln!("Warning: Unknown type in conversion to string. Check type inference.");
                "[unknown]".to_string()
            }
            Type::Generic(name) => format!("[generic: {}]", name),
            Type::GenericInstance(name, _) => format!("[generic instance: {}]", name),
            _ => {
                eprintln!("Warning: Cannot convert type {:?} to string", ty);
                "[unsupported type]".to_string()
            }
        }
    }

    fn emit_struct(&mut self, struct_def: &ast::StructDef) -> Result<(), CompileError> {
        let struct_name = format!("ve_{}", struct_def.name);
        let mut struct_code = format!("typedef struct {} {{\n", struct_name);

        for field in &struct_def.fields {
            let field_type = self.type_to_c(&field.ty);
            struct_code.push_str(&format!("    {} {};\n", field_type, field.name));
        }

        struct_code.push_str(&format!("}} {};\n\n", struct_name));
        self.header.push_str(&struct_code);

        Ok(())
    }

    fn emit_enum(&mut self, enum_def: &ast::EnumDef) -> Result<(), CompileError> {
        let enum_name = format!("ve_{}", enum_def.name);

        if !enum_def.generic_params.is_empty() {
            return Ok(());
        }

        self.header.push_str(&format!("typedef enum {{\n"));
        for (i, variant) in enum_def.variants.iter().enumerate() {
            self.header
                .push_str(&format!("    {}_{} = {},\n", enum_name, variant.name, i));
        }
        self.header.push_str(&format!("}} {}_Tag;\n\n", enum_name));

        self.header
            .push_str(&format!("typedef struct {} {{\n", enum_name));
        self.header
            .push_str(&format!("    {}_Tag tag;\n", enum_name));
        self.header.push_str("    union {\n");

        for variant in &enum_def.variants {
            if let Some(data_types) = &variant.data {
                if !data_types.is_empty() {
                    self.header.push_str(&format!("        struct {{\n"));
                    for (i, ty) in data_types.iter().enumerate() {
                        let c_type = self.type_to_c(ty);
                        self.header
                            .push_str(&format!("            {} field{};\n", c_type, i));
                    }
                    self.header
                        .push_str(&format!("        }} {};\n", variant.name.to_lowercase()));
                }
            }
        }

        self.header.push_str("    } data;\n");
        self.header.push_str(&format!("}} {};\n\n", enum_name));

        for variant in &enum_def.variants {
            if let Some(data_types) = &variant.data {
                if !data_types.is_empty() {
                    let mut params = Vec::new();
                    for (i, ty) in data_types.iter().enumerate() {
                        params.push(format!("{} field{}", self.type_to_c(ty), i));
                    }

                    self.header.push_str(&format!(
                        "static {} {}_{}_new({}) {{\n",
                        enum_name,
                        enum_name,
                        variant.name,
                        params.join(", ")
                    ));
                    self.header
                        .push_str(&format!("    {} result;\n", enum_name));
                    self.header.push_str(&format!(
                        "    result.tag = {}_{};\n",
                        enum_name, variant.name
                    ));

                    for (i, _) in data_types.iter().enumerate() {
                        self.header.push_str(&format!(
                            "    result.data.{}.field{} = field{};\n",
                            variant.name.to_lowercase(),
                            i,
                            i
                        ));
                    }

                    self.header.push_str("    return result;\n");
                    self.header.push_str("}\n\n");
                } else {
                    self.header.push_str(&format!(
                        "static {} {}_{}_new() {{\n",
                        enum_name, enum_name, variant.name
                    ));
                    self.header
                        .push_str(&format!("    {} result;\n", enum_name));
                    self.header.push_str(&format!(
                        "    result.tag = {}_{};\n",
                        enum_name, variant.name
                    ));
                    self.header.push_str("    return result;\n");
                    self.header.push_str("}\n\n");
                }
            } else {
                self.header.push_str(&format!(
                    "static {} {}_{}_new() {{\n",
                    enum_name, enum_name, variant.name
                ));
                self.header
                    .push_str(&format!("    {} result;\n", enum_name));
                self.header.push_str(&format!(
                    "    result.tag = {}_{};\n",
                    enum_name, variant.name
                ));
                self.header.push_str("    return result;\n");
                self.header.push_str("}\n\n");
            }
        }

        Ok(())
    }

    fn emit_generic_enum_instance(
        &mut self,
        enum_def: &ast::EnumDef,
        args: &[Type],
    ) -> Result<(), CompileError> {
        let mut name = format!("ve_{}", enum_def.name);
        for arg in args {
            name.push('_');
            name.push_str(&self.type_to_c_name(arg));
        }

        self.header.push_str(&format!("typedef enum {{\n"));
        for (i, variant) in enum_def.variants.iter().enumerate() {
            self.header
                .push_str(&format!("    {}_{} = {},\n", name, variant.name, i));
        }
        self.header.push_str(&format!("}} {}_Tag;\n\n", name));

        self.header.push_str(&format!("typedef struct {} {{\n", name));
        self.header.push_str(&format!("    {}_Tag tag;\n", name));
        self.header.push_str("    union {\n");
        for variant in &enum_def.variants {
            if let Some(data_types) = &variant.data {
                if !data_types.is_empty() {
                    self.header.push_str("        struct {\n");
                    for (i, ty) in data_types.iter().enumerate() {
                        let concrete_ty = if let Some(idx) = enum_def.generic_params.iter().position(|gp| gp == &ty.to_string()) {
                            &args[idx]
                        } else {
                            ty
                        };
                        let c_type = self.type_to_c(concrete_ty);
                        self.header.push_str(&format!("            {} field{};\n", c_type, i));
                    }
                    self.header.push_str(&format!("        }} {};\n", variant.name.to_lowercase()));
                }
            }
        }
        self.header.push_str("    } data;\n");
        self.header.push_str(&format!("}} {};\n\n", name));

        for variant in &enum_def.variants {
            if let Some(data_types) = &variant.data {
                if !data_types.is_empty() {
                    let mut params = Vec::new();
                    for (i, ty) in data_types.iter().enumerate() {
                        let concrete_ty = if let Some(idx) = enum_def.generic_params.iter().position(|gp| gp == &ty.to_string()) {
                            &args[idx]
                        } else {
                            ty
                        };
                        params.push(format!("{} field{}", self.type_to_c(concrete_ty), i));
                    }
                    self.header.push_str(&format!(
                        "static {} {}_{}_new({}) {{\n",
                        name,
                        name,
                        variant.name,
                        params.join(", ")
                    ));
                    self.header.push_str(&format!("    {} result;\n", name));
                    self.header.push_str(&format!("    result.tag = {}_{};\n", name, variant.name));
                    for (i, _) in data_types.iter().enumerate() {
                        self.header.push_str(&format!(
                            "    result.data.{}.field{} = field{};\n",
                            variant.name.to_lowercase(),
                                                       i,
                            i
                        ));
                    }
                    self.header.push_str("    return result;\n");
                    self.header.push_str("}\n\n");
                } else {
                    self.header.push_str(&format!(
                        "static {} {}_{}_new() {{\n",
                        name, name, variant.name
                    ));
                    self.header.push_str(&format!("    {} result;\n", name));
                    self.header.push_str(&format!("    result.tag = {}_{};\n", name, variant.name));
                    self.header.push_str("    return result;\n");
                    self.header.push_str("}\n\n");
                }
            } else {
                self.header.push_str(&format!(
                    "static {} {}_{}_new() {{\n",
                    name, name, variant.name
                ));
                self.header.push_str(&format!("    {} result;\n", name));
                self.header.push_str(&format!("    result.tag = {}_{};\n", name, variant.name));
                self.header.push_str("    return result;\n");
                self.header.push_str("}\n\n");
            }
        }
        Ok(())
    }

    fn emit_match_switch_with_result(
        &mut self,
        matched_var: &str,
        result_var: &str,
        arms: &[ast::MatchArm],
        code: &mut String,
    ) -> Result<(), CompileError> {
        let matched_type = self.variables.borrow().get(matched_var).cloned();
        let (enum_name, is_generic, tag_prefix) = if let Some(Type::GenericInstance(name, args)) = &matched_type {
            (name.clone(), true, format!("ve_{}", self.type_to_c_name(&matched_type.as_ref().unwrap())))
        } else {
            ("".to_string(), false, "".to_string())
        };

        code.push_str(&format!("switch ({}.tag) {{\n", matched_var));

        for arm in arms {
            match &arm.pattern {
                ast::Pattern::EnumVariant(enum_name_arm, variant_name, patterns, _) => {
                    let case_value = if is_generic {
                        format!("{}_{}", tag_prefix, variant_name)
                    } else {
                        format!("ve_{}_{}", enum_name_arm, variant_name)
                    };
                    code.push_str(&format!("    case {}: {{\n", case_value));

                    for (i, pattern) in patterns.iter().enumerate() {
                        if let ast::Pattern::Variable(var_name, _) = pattern {
                            let mut field_type = "int".to_string();
                            if let Some(Type::GenericInstance(enum_name, args)) = &matched_type {
                                if let Some(enum_def) = self.enum_defs.get(enum_name) {
                                    enum_def.variants.iter().find(|v| v.name == *variant_name).map(|variant| {
                                        if let Some(data_types) = &variant.data {
                                            if let Some(ty) = data_types.get(i) {
                                                field_type = self.type_to_c(if let Some(idx) = enum_def.generic_params.iter().position(|gp| gp == &ty.to_string()) {
                                                    &args[idx]
                                                } else {
                                                    ty
                                                });
                                            }
                                        }
                                    });
                                }
                            }
                            code.push_str(&format!(
                                "    {} {} = {}.data.{}.field{};\n",
                                field_type,
                                var_name,
                                matched_var,
                                variant_name.to_lowercase(),
                                i
                            ));
                        }
                    }

                    let body_code = match &arm.body {
                        ast::MatchArmBody::Expr(expr) => self.emit_expr(expr)?,
                        ast::MatchArmBody::Block(stmts) => {
                            let mut block_code = String::new();
                            for stmt in stmts {
                                block_code.push_str(&self.emit_stmt_to_string(stmt)?);
                            }
                            block_code
                        }
                    };
                    code.push_str(&format!("    {} = {};\n", result_var, body_code));
                    code.push_str("    break;\n");
                    code.push_str("}\n");
                }
                ast::Pattern::Wildcard(_) => {
                    code.push_str("    default: {\n");
                    let body_code = match &arm.body {
                        ast::MatchArmBody::Expr(expr) => self.emit_expr(expr)?,
                        ast::MatchArmBody::Block(stmts) => {
                            let mut block_code = String::new();
                            for stmt in stmts {
                                block_code.push_str(&self.emit_stmt_to_string(stmt)?);
                            }
                            block_code
                        }
                    };
                    code.push_str(&format!("        {} = {};\n", result_var, body_code));
                    code.push_str("        break;\n");
                    code.push_str("    }\n");
                }
                ast::Pattern::Variable(var_name, _) => {
                    code.push_str("    default: {\n");
                    let expr_type = Type::Unknown;
                    let c_type = self.type_to_c(&expr_type);
                    code.push_str(&format!(
                        "        {} {} = {};\n",
                        c_type, var_name, matched_var
                    ));
                    let body_code = match &arm.body {
                        ast::MatchArmBody::Expr(expr) => self.emit_expr(expr)?,
                        ast::MatchArmBody::Block(stmts) => {
                            let mut block_code = String::new();
                            for stmt in stmts {
                                block_code.push_str(&self.emit_stmt_to_string(stmt)?);
                            }
                            block_code
                        }
                    };
                    code.push_str(&format!("        {} = {};\n", result_var, body_code));
                    code.push_str("        break;\n");
                    code.push_str("    }\n");
                }
                ast::Pattern::Literal(_expr, _) => {
                    return Err(CompileError::CodegenError {
                        message: "Literal patterns in enum match not yet supported".to_string(),
                        span: Some(arm.span),
                        file_id: self.file_id,
                    });
                }
            }
        }

        code.push_str("}\n");
        Ok(())
    }

    fn analyze_memory_requirements(&mut self, program: &ast::Program) {
            self.memory_analysis.total_functions = program.functions.len();
            for func in &program.functions {
                let depth = self.analyze_function_memory(&func.body);
                self.memory_analysis.max_function_depth =
                    self.memory_analysis.max_function_depth.max(depth);
            }
            for stmt in &program.stmts {
                self.analyze_stmt_memory(stmt);
            }
            self.calculate_arena_size();
        }

    fn analyze_function_memory(&mut self, stmts: &[ast::Stmt]) -> usize {
        let mut depth = 0;
        for stmt in stmts {
            depth = depth.max(self.analyze_stmt_memory(stmt));
        }
        depth
    }

    fn analyze_stmt_memory(&mut self, stmt: &ast::Stmt) -> usize {
        match stmt {
            ast::Stmt::Let(_, ty, expr, _, _) => {
                if let Some(ty) = ty {
                    self.estimate_type_size(ty);
                }
                self.analyze_expr_memory(expr)
            }
            ast::Stmt::Expr(expr, _) => self.analyze_expr_memory(expr),
            ast::Stmt::Block(stmts, _) => {
                let mut max_depth = 0;
                for stmt in stmts {
                    max_depth = max_depth.max(self.analyze_stmt_memory(stmt));
                }
                max_depth + 1
           
            }
            ast::Stmt::While(_, body, _) | ast::Stmt::For(_, _, body, _) => {
                let mut max_depth = 0;
                for stmt in body {
                    max_depth = max_depth.max(self.analyze_stmt_memory(stmt));
                }
                max_depth + 1
            }
            ast::Stmt::If(_, then_branch, else_branch, _) => {
                let then_depth = then_branch
                    .iter()
                    .map(|s| self.analyze_stmt_memory(s))
                    .max()
                    .unwrap_or(0);
                let else_depth = else_branch
                    .as_ref()
                    .map(|stmts| {
                        stmts
                            .iter()
                            .map(|s| self.analyze_stmt_memory(s))
                            .max()
                            .unwrap_or(0)
                    })
                    .unwrap_or(0);
                then_depth.max(else_depth) + 1
            }
            _ => 0,
        }
    }

    fn analyze_expr_memory(&mut self, expr: &ast::Expr) -> usize {
        match expr {
            ast::Expr::Str(s, _) => {
                self.memory_analysis.string_allocations += s.len() + 1;
                1
            }
            ast::Expr::ArrayInit(elements, _) => {
                self.memory_analysis.array_allocations += elements.len() * 8; 
                for elem in elements {
                    self.analyze_expr_memory(elem);
                }
                                1
            }
            ast::Expr::StructInit(name, fields, _) => {
                if let Some(struct_fields) = self.struct_defs.get(name) {
                    let size = struct_fields
                        .iter()
                        .map(|(_, ty)| self.get_type_size(ty))
                        .sum::<usize>();
                    self.memory_analysis.struct_allocations += size;
                }
                for (_, expr) in fields {
                    self.analyze_expr_memory(expr);
                }
                1
            }
            ast::Expr::TemplateStr(parts, _) => {
                let estimated_size = parts
                    .iter()
                    .map(|part| match part {
                        ast::TemplateStrPart::Literal(s) => s.len(),
                        ast::TemplateStrPart::Expression(_) => 32, 
                    })
                    .sum::<usize>();
                self.memory_analysis.string_allocations += estimated_size;
                1
            }
            ast::Expr::BinOp(left, _, right, _) => self
                .analyze_expr_memory(left)
                .max(self.analyze_expr_memory(right)),
            ast::Expr::Call(_, args, _) => {
                args.iter()
                    .map(|arg| self.analyze_expr_memory(arg))
                    .max()
                    .unwrap_or(0)
                    + 1
            }
            _ => 0,
        }
    }

    fn estimate_type_size(&mut self, ty: &Type) {
        let size = self.get_type_size(ty);
        self.memory_analysis.estimated_arena_size += size;
    }

    fn get_type_size(&self, ty: &Type) -> usize {
        match ty {
            Type::I32 | Type::F32 | Type::Bool => 4,
            Type::I64 | Type::F64 => 8,
            Type::I8 | Type::U8 => 1,
            Type::I16 | Type::U16 => 2,
            Type::U32 => 4,
            Type::U64 => 8,
            Type::String => 256,
            Type::Pointer(_) | Type::RawPtr => 8,
            Type::Array(_) => 1024,
            Type::SizedArray(inner, size) => size * 8,
            Type::Struct(name) => {
                if let Some(fields) = self.struct_defs.get(name) {
                    fields.iter().map(|(_, ty)| self.get_type_size(ty)).sum()
                } else {
                    64
                }
            }
            Type::Generic(_) => 8,
            Type::GenericInstance(_, _) => 8,
            _ => 8,
        }
    }

    fn calculate_arena_size(&mut self) {
        let base_size = 4 * 1024;
        let function_overhead = self.memory_analysis.total_functions * 1024;
        let string_overhead = self.memory_analysis.string_allocations;
        let array_overhead = self.memory_analysis.array_allocations;
        let struct_overhead = self.memory_analysis.struct_allocations;
        let depth_multiplier = (self.memory_analysis.max_function_depth + 1) * 2;

        let calculated_size = base_size
            + (function_overhead + string_overhead + array_overhead + struct_overhead)
                * depth_multiplier;

        self.memory_analysis.estimated_arena_size = calculated_size;

        self.memory_analysis.estimated_arena_size = self
            .memory_analysis
            .estimated_arena_size
            .next_power_of_two();

        if self.memory_analysis.estimated_arena_size > 64 * 1024 * 1024 {
            self.memory_analysis.estimated_arena_size = 64 * 1024 * 1024;
        }
    }
}
