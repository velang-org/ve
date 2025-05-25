use crate::ast::Type;
use crate::{ast, codegen::{CodegenConfig, CompileError}};
use codespan::{FileId, Span};
use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap};
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
    struct_defs: HashMap<String, Vec<(String, Type)>>,
    imported_structs: Vec<ast::StructDef>,
}

impl CBackend {
    pub fn new(config: CodegenConfig, file_id: FileId, imported_functions: HashMap<String, (Vec<Type>, Type)>, imported_structs: Vec<ast::StructDef>, imported_ffi_vars: Vec<ast::FfiVariable>) -> Self {
        let backend = Self {
            config,
            header: String::new(),
            body: String::new(),
            file_id,
            includes: RefCell::new(BTreeSet::new()),
            variables: RefCell::new(HashMap::new()),
            functions_map: HashMap::new(),
            imported_functions,
            struct_defs: HashMap::new(),
            imported_structs,
        };

        for ffi_var in imported_ffi_vars {
            backend.variables.borrow_mut().insert(ffi_var.name, ffi_var.ty);
        }

        backend
    }

    fn generate_struct_to_str_functions<'a, I>(&mut self, structs: I)
    where
        I: IntoIterator<Item = &'a ast::StructDef>,
    {
        for struct_def in structs {
            let struct_name = &struct_def.name;
            self.header.push_str(&format!("static char* {}_to_str(const {} *obj) {{\n", struct_name, struct_name));
            self.header.push_str("    char *buffer = malloc(256);\n");
            self.header.push_str(&format!("    if (!buffer) return \"<failed to allocate memory for {}>\";\n", struct_name));
            let mut format_parts = Vec::new();
            let mut args = Vec::new();
            format_parts.push(format!("{}{{", struct_name));
            for (i, field) in struct_def.fields.iter().enumerate() {
                let (fmt, arg) = match field.ty {
                    Type::I32 => ("%d", format!("obj->{}", field.name)),
                    Type::Bool => ("%s", format!("(obj->{} ? \"true\" : \"false\")", field.name)),
                    Type::String => ("%s", format!("(obj->{} ? obj->{} : \"null\")", field.name, field.name)),
                    Type::Pointer(_) | Type::RawPtr => ("%p", format!("(void*)obj->{}", field.name)),
                    Type::Struct(ref s) => ("%s", format!("{}_to_str(&obj->{})", s, field.name)),
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
            self.header.push_str(&format!("    sprintf(buffer, \"{}\"", format_str));
            for arg in args {
                self.header.push_str(&format!(", {}", arg));
            }
            self.header.push_str(");\n");
            self.header.push_str("    return buffer;\n");
            self.header.push_str("}\n\n");
        }
    }

    pub fn compile(&mut self, program: &ast::Program, output_path: &Path) -> Result<(), CompileError> {
        self.emit_header();

        self.generate_ffi_declarations(program)?;

        let imported_structs = self.imported_structs.clone();
        for struct_def in &imported_structs {
            self.emit_struct(struct_def)?;
        }

        for struct_def in &program.structs {
            let fields: Vec<(String, Type)> = struct_def.fields
                .iter()
                .map(|f| (f.name.clone(), f.ty.clone()))
                .collect();
            self.struct_defs.insert(struct_def.name.clone(), fields);
        }

        for struct_def in &program.structs {
            self.emit_struct(struct_def)?;
        }

        self.functions_map = program.functions.iter()
            .map(|f| (f.name.clone(), f.return_type.clone()))
            .chain(self.imported_functions.iter().map(|(k, v)| (k.clone(), v.1.clone())))
            .collect();

        for ffi in &program.ffi_functions {
            self.functions_map.insert(
                ffi.name.clone(),
                ffi.return_type.clone(),
            );
        }

        self.emit_globals(program)?;
        self.emit_functions(program)?;

        let imported_structs = self.imported_structs.clone();
        let all_structs: Vec<&ast::StructDef> = program.structs.iter().chain(imported_structs.iter()).collect();
        self.generate_struct_to_str_functions(all_structs);

        self.emit_main_if_missing(program)?;
        self.write_output(output_path)?;
        Ok(())
    }

    fn generate_ffi_declarations(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        let mut ffi_decls = String::new();

        for ffi in &program.ffi_functions {
            let ret = self.type_to_c(&ffi.return_type);
            let params = ffi.params.iter()
                .map(|ty| self.type_to_c(ty))
                .collect::<Vec<String>>()
                .join(", ");

            let param_str = if params.is_empty() { "void" } else { &params };

            if let Some(header) = ffi.metadata.as_ref().and_then(|m| m.get("header")) {
                self.includes.borrow_mut().insert(format!("<{}>", header));
            }

            if let Some(link) = ffi.metadata.as_ref().and_then(|m| m.get("link")) {
                self.header.push_str(&format!("#pragma comment(lib, \"{}\")\n", link));
            }

            ffi_decls.push_str(&format!("extern {} {}({});\n", ret, ffi.name, param_str));
        }

        if !ffi_decls.is_empty() {
            self.header.push_str("// FFI function declarations\n");
            self.header.push_str(&ffi_decls);
            self.header.push_str("\n");
        }

        Ok(())
    }

    fn emit_header(&mut self) {
        self.header.push_str(&format!(
            "// Generated by Verve Compiler (target: {})\n",
            self.config.target_triple
        ));
        self.header.push_str("#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n");
        self.header.push_str("#include <stdbool.h>\n");
        self.header.push_str("#include <math.h>\n");

        self.header.push_str("typedef unsigned char u8;\n");
        self.header.push_str("typedef unsigned short u16;\n");
        self.header.push_str("typedef unsigned int u32;\n");
        self.header.push_str("typedef unsigned long long u64;\n");
        self.header.push_str("typedef signed char i8;\n");
        self.header.push_str("typedef signed short i16;\n");
        self.header.push_str("typedef signed int i32;\n");
        self.header.push_str("typedef signed long long i64;\n\n");

        for include in self.includes.borrow().iter() {
            self.header.push_str(&format!("#include {}\n", include));
        }

        self.header.push('\n');

        self.header.push_str("static char* int_to_str(int num) {\n");
        self.header.push_str("    char* buffer = malloc(12);\n");
        self.header.push_str("    sprintf(buffer, \"%d\", num);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header.push_str("static char* float_to_str(float num) {\n");
        self.header.push_str("    char* buffer = malloc(32);\n");
        self.header.push_str("    sprintf(buffer, \"%g\", num);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header.push_str("static char* bool_to_str(bool b) {\n");
        self.header.push_str("    const char* val = b ? \"true\" : \"false\";\n");
        self.header.push_str("    size_t len = strlen(val) + 1;\n");
        self.header.push_str("    char* buffer = malloc(len);\n");
        self.header.push_str("    if (buffer) memcpy(buffer, val, len);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header.push_str("static char* ptr_to_str(void* ptr) {\n");
        self.header.push_str("    char* buffer = malloc(20);\n");
        self.header.push_str("    sprintf(buffer, \"%p\", ptr);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header.push_str("static char* concat(const char* s1, const char* s2) {\n");
        self.header.push_str("    size_t len1 = strlen(s1);\n");
        self.header.push_str("    size_t len2 = strlen(s2);\n");
        self.header.push_str("    char* result = malloc(len1 + len2 + 1);\n");
        self.header.push_str("    if (result) {\n");
        #[cfg(target_os = "windows")]
        {
            self.header.push_str("        strcpy_s(result, len1 + len2 + 1, s1);\n");
            self.header.push_str("        strcat_s(result, len1 + len2 + 1, s2);\n");
        }
        #[cfg(not(target_os = "windows"))]
        {
            self.header.push_str("        strcpy(result, s1);\n");
            self.header.push_str("        strcat(result, s2);\n");
        }
        self.header.push_str("    }\n");
        self.header.push_str("    return result;\n");
        self.header.push_str("}\n\n");
    }

    fn emit_globals(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        for stmt in &program.stmts {
            if let ast::Stmt::Let(name, ty, expr, _) = stmt {
                if self.is_constant_expr(expr) {
                    let c_ty = self.type_to_c(ty.as_ref().unwrap_or(&Type::I32));
                    let value = self.emit_expr(expr)?;
                    self.body.push_str(&format!("{} {} = {};\n", c_ty, name, value));
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
        matches!(expr,
            ast::Expr::Int(..) |
            ast::Expr::Str(..) |
            ast::Expr::Bool(..) |
            ast::Expr::F32(..)
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
            let params = func.params.iter()
                .map(|(name, ty)| format!("{} {}", self.type_to_c(ty), name))
                .collect::<Vec<_>>()
                .join(", ");
            self.body.push_str(&format!("{} {}({});\n", return_type, func.name, params));
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

        let mut param_strings = Vec::new();
        for (name, ty) in &func.params {
            let c_ty = self.type_to_c(ty);
            param_strings.push(format!("{} {}", c_ty, name));
            self.variables.borrow_mut().insert(name.clone(), ty.clone());
        }
        let params = param_strings.join(", ");

        self.body.push_str(&format!("{} {}({}) {{\n", return_type, func.name, params));

        for stmt in &func.body {
            self.emit_stmt(stmt)?;
        }

        if func.name == "main" {
            let last_is_return = func.body.last().is_some_and(|s| matches!(s, ast::Stmt::Return(..)));

            if !last_is_return {
                self.body.push_str("    return 0;\n");
            }
        } else if func.return_type == Type::Void {
            self.body.push_str("    return;\n");
        }

        self.body.push_str("}\n\n");
        Ok(())
    }

    fn emit_stmt(&mut self, stmt: &ast::Stmt) -> Result<(), CompileError> {
        match stmt {            ast::Stmt::Let(name, ty, expr, _) => {
                let var_type = if let Some(ty) = ty {
                    ty.clone()
                } else {
                    match expr {
                        ast::Expr::Int(_, _) => Type::I32,
                        ast::Expr::F32(_, _) => Type::F32,
                        ast::Expr::Bool(_, _) => Type::Bool,
                        ast::Expr::Str(_, _) => Type::String,
                        ast::Expr::Call(func_name, _, _) => {
                            self.functions_map.get(func_name)
                                .cloned()
                                .ok_or_else(|| CompileError::CodegenError {
                                    message: format!("Undefined function '{}'", func_name),
                                    span: Some(expr.span()),
                                    file_id: self.file_id,
                                })?
                        }
                        _ => expr.get_type()
                    }
                };
                let c_ty = self.type_to_c(&var_type);
                let expr_code = self.emit_expr(expr)?;
                self.body.push_str(&format!("{} {} = {};\n", c_ty, name, expr_code));
                self.variables.borrow_mut().insert(name.clone(), var_type);
            }
            ast::Stmt::Return(expr, _) => {
                if let ast::Expr::Void(_) = expr {
                    let current_func = self.body.rsplit_once("(").and_then(|(before, _)| before.rsplit_once(' ').map(|(_, name)| name.trim()));
                    let func_name = current_func.unwrap_or("");
                    let ret_type = self.functions_map.get(func_name);
                    if func_name == "main" || ret_type == Some(&Type::I32) {
                        self.body.push_str("return 0;\n");
                        return Ok(());
                    } else if ret_type == Some(&Type::Void) {
                        self.body.push_str("return;\n");
                        return Ok(());
                    } else {
                        self.body.push_str("// ERROR: Return statement in function with non-void return type\n");
                        return Ok(());
                    }
                }
                let expr_code = self.emit_expr(expr)?;
                self.body.push_str(&format!("return {};\n", expr_code));
            },
            ast::Stmt::Expr(expr, _) => {
                let expr_code = self.emit_expr(expr)?;
                if expr_code.starts_with('{') {
                    self.body.push_str(&expr_code);
                } else if !expr_code.ends_with(';') {
                    self.body.push_str(&format!("{};\n", expr_code));
                } else {
                    self.body.push_str(&format!("{}\n", expr_code));
                }
            },
            ast::Stmt::Block(stmts, _) => {
                for s in stmts {
                    self.emit_stmt(s)?;
                }
            },
            ast::Stmt::While(cond, body, _) => {
                let cond_code = self.emit_expr(cond)?;
                self.body.push_str(&format!("while ({}) {{\n", cond_code)); 
                for stmt in body {
                    self.emit_stmt(stmt)?;
                }
                self.body.push_str("}\n");
            },
            ast::Stmt::For(var_name, range, body, _) => {
                let range_code = self.emit_expr(range)?;
                let parts: Vec<&str> = range_code.split("..").collect();
                let start = parts[0].trim();
                let end = parts[1].trim();
                self.body.push_str(&format!("for (int {var} = {start}; {var} < {end}; {var}++) {{\n", var=var_name));

                for stmt in body {
                    self.emit_stmt(stmt)?;
                }
                self.body.push_str("}\n");
            },
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
            ast::Expr::F32(f, _) => Ok(f.to_string()),
            ast::Expr::Bool(b, _) => {
                self.includes.borrow_mut().insert("<stdbool.h>".to_string());
                Ok(if *b { "true" } else { "false" }.to_string())
            },
            ast::Expr::BinOp(left, op, right, _) => {
                let left_code = self.emit_expr(left)?;
                let right_code = self.emit_expr(right)?;

                let left_type = left.get_type();
                let right_type = right.get_type();
                let is_string_cmp = matches!(left_type, Type::String) || matches!(right_type, Type::String);

                if is_string_cmp {
                    match op {
                        ast::BinOp::Eq => {
                            self.includes.borrow_mut().insert("<string.h>".to_string());
                            let left_conv = self.convert_to_c_str(&left_code, &left_type);
                            let right_conv = self.convert_to_c_str(&right_code, &right_type);
                            return Ok(format!("(strcmp({}, {}) == 0)", left_conv, right_conv));
                        },
                        ast::BinOp::NotEq => {
                            self.includes.borrow_mut().insert("<string.h>".to_string());
                            let left_conv = self.convert_to_c_str(&left_code, &left_type);
                            let right_conv = self.convert_to_c_str(&right_code, &right_type);
                            return Ok(format!("(strcmp({}, {}) != 0)", left_conv, right_conv));
                        },
                        _ => {
                            let left_conv = self.convert_to_c_str(&left_code, &left_type);
                            let right_conv = self.convert_to_c_str(&right_code, &right_type);
                            return Ok(format!("concat({}, {})", left_conv, right_conv));
                        }
                    }
                }

                let result_type = expr.get_type();
                match result_type {
                    Type::String => {
                        match op {
                            ast::BinOp::Eq => {
                                self.includes.borrow_mut().insert("<string.h>".to_string());
                                let left_conv = self.convert_to_c_str(&left_code, &left.get_type());
                                let right_conv = self.convert_to_c_str(&right_code, &right.get_type());
                                Ok(format!("(strcmp({}, {}) == 0)", left_conv, right_conv))
                            },
                            ast::BinOp::NotEq => {
                                self.includes.borrow_mut().insert("<string.h>".to_string());
                                let left_conv = self.convert_to_c_str(&left_code, &left.get_type());
                                let right_conv = self.convert_to_c_str(&right_code, &right.get_type());
                                Ok(format!("(strcmp({}, {}) != 0)", left_conv, right_conv))
                            },
                            _ => {
                                let left_conv = self.convert_to_c_str(&left_code, &left.get_type());
                                let right_conv = self.convert_to_c_str(&right_code, &right.get_type());
                                Ok(format!("concat({}, {})", left_conv, right_conv))
                            }
                        }
                    }
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
            },
            ast::Expr::Assign(target, value, _) => {
                let target_code = self.emit_expr(target)?;
                let value_code = self.emit_expr(value)?;
                Ok(format!("({} = {})", target_code, value_code))
            },
            ast::Expr::Str(s, _) => Ok(format!("\"{}\"", s.replace("\n", "\\n").replace("\"", "\\\""))),
            ast::Expr::Var(name, info) => {
                if name == "true" || name == "false" {
                    self.includes.borrow_mut().insert("<stdbool.h>".to_string());
                    return Ok(name.clone());
                }

                let var_type = self.variables.borrow().get(name).cloned().unwrap_or(Type::Unknown);

                match var_type {
                    Type::I32 => Ok(name.clone()),
                    Type::Bool => {
                        self.includes.borrow_mut().insert("<stdbool.h>".to_string());
                        Ok(name.clone())
                    },
                    Type::String => Ok(name.clone()),
                    Type::Pointer(_) | Type::RawPtr => Ok(name.clone()),
                    Type::Struct(_) => Ok(name.clone()),
                    Type::Array(_) => Ok(name.clone()),
                    Type::SizedArray(_, _) => Ok(name.clone()),
                    Type::F32 => Ok(name.clone()),
                    Type::Unknown => Ok(name.clone()),
                    _ => Err(CompileError::CodegenError {
                        message: format!("Cannot print type (in var) {:?}", var_type),
                        span: Some(info.span),
                        file_id: self.file_id,
                    }),
                }
            },
            ast::Expr::Call(name, args, _expr_info) => {
                let mut args_code = Vec::new();

                let param_types = self.imported_functions
                    .get(name)
                    .map(|(params, _)| params.clone());

                for (i, arg) in args.iter().enumerate() {
                    let value = self.emit_expr(arg)?;
                    let arg_type = arg.get_type();

                    let expected_type = param_types
                        .as_ref()
                        .and_then(|types| types.get(i))
                        .cloned();

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

                Ok(format!("{}({})", name, args_code.join(", ")))
            },
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
                        },
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
            },
            ast::Expr::Deref(expr, _) => {
                let inner = self.emit_expr(expr)?;
                Ok(format!("(*{})", inner))
            }
            ast::Expr::Cast(expr, target_ty, _) => {
                let expr_code = self.emit_expr(expr)?;
                let expr_type = expr.get_type();

                match (&expr_type, target_ty) {
                    (Type::I32, Type::String) => Ok(format!("int_to_str({})", expr_code)),
                    (Type::Bool, Type::String) => Ok(format!("bool_to_str({})", expr_code)),
                    (Type::Pointer(_), Type::String) | (Type::RawPtr, Type::String) => Ok(format!("ptr_to_str({})", expr_code)),
                    (Type::String, Type::I32) => {
                        self.includes.borrow_mut().insert("<stdlib.h>".to_string());
                        Ok(format!("atoi({})", expr_code))
                    },
                    (Type::RawPtr, Type::Pointer(inner_ty)) => {
                        Ok(format!("({}*)({})", self.type_to_c(inner_ty), expr_code))
                    },
                    (_, Type::Pointer(inner_ty)) => {
                        Ok(format!("({}*)({})", self.type_to_c(inner_ty), expr_code))
                    },
                    (_, Type::RawPtr) => {
                        Ok(format!("(void*)({})", expr_code))
                    },
                    _ => Ok(format!("({})({})", self.type_to_c(target_ty), expr_code)),
                }
            },
            ast::Expr::Range(start, end, _) => {
                let start_code = self.emit_expr(start)?;
                let end_code = self.emit_expr(end)?;
                Ok(format!("{} .. {}", start_code, end_code))
            },
            ast::Expr::StructInit(name, fields, _) => {
                let mut field_inits = Vec::new();
                for (field_name, field_expr) in fields {
                    let field_code = self.emit_expr(field_expr)?;
                    field_inits.push(format!(".{} = {}", field_name, field_code));
                }

                Ok(format!("({}){{ {} }}", name, field_inits.join(", ")))
            },
            ast::Expr::FieldAccess(obj, field_name, _info) => {
                let obj_code = self.emit_expr(obj)?;
                Ok(format!("{}.{}", obj_code, field_name))
            },
            ast::Expr::ArrayInit(elements, info) => {

                let element_type = match &info.ty {
                    Type::Array(inner) => inner.clone(),
                    _ => return Err(CompileError::CodegenError {
                        message: "Array initialization with non-array type".to_string(),
                        span: Some(info.span),
                        file_id: self.file_id,
                    }),
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

                Ok(format!("({{ \
                    {} {}[] = {{ {} }}; \
                    {}* {} = malloc({} * sizeof({})); \
                    if ({}) {{ \
                        for (int _i = 0; _i < {}; _i++) {{{}[_i] = {}[_i];}} \
                    }} \
                    {}; \
                }})",
                    element_type_str, temp_var, elements_str,
                    element_type_str, temp_var_ptr, size, element_type_str,
                    temp_var_ptr, size, temp_var_ptr, temp_var,
                    temp_var_ptr
                ))
            },
            ast::Expr::ArrayAccess(array, index, _info) => {
                let array_expr = self.emit_expr(array)?;
                let index_expr = self.emit_expr(index)?;
                let array_type = array.get_type();

                match array_type {
                    Type::Pointer(inner_ty) => {
                        Ok(format!("(({}*){})[{}]", self.type_to_c(&inner_ty), array_expr, index_expr))
                    }
                    Type::RawPtr => {
                        Ok(format!("((unsigned char*){})[{}]", array_expr, index_expr))
                    }
                    _ => {
                        Ok(format!("{}[{}]", array_expr, index_expr))
                    }
                }
            },
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
                        },
                        ast::TemplateStrPart::Expression(expr) => {
                            let expr_code = self.emit_expr(expr)?;
                            let expr_type = expr.get_type();

                            match &**expr {
                                ast::Expr::ArrayAccess(array, _, _) => {
                                    let array_type = array.get_type();
                                    match array_type {
                                        Type::Array(element_type) | Type::SizedArray(element_type, _) => {
                                            match *element_type {
                                                Type::I32 => format!("int_to_str({})", expr_code),
                                                Type::Bool => format!("bool_to_str({})", expr_code),
                                                Type::String => expr_code,
                                                _ => format!("\"[unsupported array element type]\"")
                                            }
                                        },
                                        _ => format!("\"[not an array]\"")
                                    }
                                },
                                _ => self.convert_to_c_str(&expr_code, &expr_type)
                            }
                        }
                    };

                    if is_first {
                        result = part_code;
                        is_first = false;
                    } else {
                        result = format!("concat({}, {})", result, part_code);
                    }
                }

                Ok(result)
            },
            ast::Expr::FfiCall(name, args, _) => {
                let mut args_code = Vec::new();
                for arg in args {
                    let arg_code = self.emit_expr(arg)?;
                    args_code.push(arg_code);
                }
                Ok(format!("{}({})", name, args_code.join(", ")))
            },
            ast::Expr::UnaryOp(op, expr, _) => {
                let inner_code = self.emit_expr(expr)?;
                match op {
                    ast::UnOp::Neg => Ok(format!("-{}", inner_code)),
                    ast::UnOp::Plus => Ok(format!("{}", inner_code)),
                }
            }


            _ => {
                Err(CompileError::CodegenError {
                    message: format!("Unsupported expression type: {:?}", expr),
                    span: Some(expr.span()),
                    file_id: self.file_id,
                })
            }

        }
    }

    #[allow(dead_code)]
    fn unify_types(&self, t1: &Type, t2: &Type, span: Span) -> Result<Type, CompileError> {
        match (t1, t2) {
            (Type::I32, Type::I32) => Ok(Type::I32),
            (Type::Bool, Type::Bool) => Ok(Type::Bool),
            (Type::Unknown, t) | (t, Type::Unknown) => Ok(t.clone()),
            (Type::Any, t) | (t, Type::Any) => Ok(t.clone()),
            _ => Err(CompileError::TypeError {
                message: format!("Type mismatch: {:?} vs {:?}", t1, t2),
                span: Some(span),
                file_id: self.file_id,
            }),
        }
    }
    
    fn emit_stmt_to_string(&mut self, stmt: &ast::Stmt) -> Result<String, CompileError> {
        let original_body = std::mem::take(&mut self.body);
        self.emit_stmt(stmt)?;
        let result = std::mem::replace(&mut self.body, original_body);
        Ok(result)
    }

    fn type_to_c(&self, ty: &Type) -> String {
        match ty {
            Type::I32 => "int".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "const char*".to_string(),
            Type::Void => "void".to_string(),
            Type::Ellipsis => "...".to_string(),
            Type::Unknown => "void*".to_string(),
            Type::Function(args, ret) => {
                let args_str = args.iter()
                    .map(|t| self.type_to_c(t))
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret_str = self.type_to_c(ret);
                format!("{}(*)({})", ret_str, args_str)
            },
            Type::Arena => "struct ArenaAllocator*".to_string(),
            Type::Pointer(inner) => format!("{}*", self.type_to_c(inner)),
            Type::RawPtr => "void*".to_string(),
            Type::Struct(name) => name.to_string(),
            Type::Array(inner) => format!("{}*", self.type_to_c(inner)),
            Type::SizedArray(inner, _) => format!("{}*", self.type_to_c(inner)),
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
            Type::I32 => format!("int_to_str({})", code),
            Type::Bool => format!("bool_to_str({})", code),
            Type::Pointer(_) | Type::RawPtr => format!("ptr_to_str({})", code),
            Type::String => code.to_string(),
            Type::Struct(name) => format!("{}_to_str(&{})", name, code),
            Type::F32 => format!("float_to_str({})", code),
            Type::Array(_) => "[array]".to_string(),
            Type::SizedArray(_, _) => "[sized array]".to_string(),
            Type::Any => "[any]".to_string(),
            Type::Unknown => {
                eprintln!("Warning: Unknown type in conversion to string. Check type inference.");
                "[unknown]".to_string()
            },
            _ => {
                eprintln!("Warning: Cannot convert type {:?} to string", ty);
                "[unsupported type]".to_string()
            }
        }
    }

    fn emit_struct(&mut self, struct_def: &ast::StructDef) -> Result<(), CompileError> {
        let mut struct_code = format!("typedef struct {} {{\n", struct_def.name);
        
        for field in &struct_def.fields {
            let field_type = self.type_to_c(&field.ty);
            struct_code.push_str(&format!("    {} {};\n", field_type, field.name));
        }
        
        struct_code.push_str(&format!("}} {};\n\n", struct_def.name));
        self.header.push_str(&struct_code);
        
        Ok(())
    }
}

#[allow(dead_code)]
pub const PRELUDE: &str = r#"#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

char* concat(const char* str1, const char* str2) {
    size_t len1 = strlen(str1);
    size_t len2 = strlen(str2);
    char* result = (char*)malloc(len1 + len2 + 1);
    if (result == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }
    strcpy(result, str1);
    strcat(result, str2);
    return result;
}
"#;
