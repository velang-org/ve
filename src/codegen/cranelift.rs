use crate::ast::{self, Type};
use crate::codegen::{CodegenConfig, CompileError};
use codespan::FileId;
use cranelift::prelude::*;
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::collections::HashMap;
use std::path::Path;

pub struct CraneliftBackend {
    config: CodegenConfig,
    file_id: FileId,
    builder_ctx: FunctionBuilderContext,
    module: ObjectModule,
    ctx: codegen::Context,
    functions: HashMap<String, FuncId>,
    imported_functions: HashMap<String, (Vec<Type>, Type)>,
    struct_defs: HashMap<String, Vec<(String, Type)>>,
}

impl CraneliftBackend {
    pub fn new(config: CodegenConfig, file_id: FileId, imported_functions: HashMap<String, (Vec<Type>, Type)>) -> Self {
        // Konfiguracja flagów Cranelift
        let mut flag_builder = settings::builder();
        flag_builder.set("opt_level", "speed").unwrap();
        flag_builder.set("is_pic", "true").unwrap();
        
        // Ustawienia dla natywnej architektury
        let isa_builder = cranelift_native::builder().unwrap();
        let isa = isa_builder.finish(settings::Flags::new(flag_builder)).unwrap();
        
        // Utworzenie modułu obiektowego
        let builder = ObjectBuilder::new(isa, "main".to_string(), cranelift_module::default_libcall_names()).unwrap();
        let module = ObjectModule::new(builder);
        
        Self {
            config,
            file_id,
            builder_ctx: FunctionBuilderContext::new(),
            module,
            ctx: codegen::Context::new(),
            functions: HashMap::new(),
            imported_functions,
            struct_defs: HashMap::new(),
        }
    }

    pub fn compile(&mut self, program: &ast::Program, output_path: &Path) -> Result<(), CompileError> {
        // 1. Rejestracja struktur
        for struct_def in &program.structs {
            let fields: Vec<(String, Type)> = struct_def.fields
                .iter()
                .map(|f| (f.name.clone(), f.ty.clone()))
                .collect();
            self.struct_defs.insert(struct_def.name.clone(), fields);
        }
        
        // 2. Deklaracja funkcji importowanych
        self.declare_imported_functions()?;
        
        // 3. Deklaracja funkcji użytkownika
        for func in &program.functions {
            self.declare_function(func)?;
        }
        
        // 4. Definicja funkcji użytkownika
        for func in &program.functions {
            self.define_function(func)?;
        }
        
        // 5. Zapis skompilowanego kodu do pliku
        self.write_output(output_path)?;
        
        Ok(())
    }
    
    fn declare_imported_functions(&mut self) -> Result<(), CompileError> {
        // Deklaracja standardowych funkcji runtime
        self.declare_runtime_functions()?;
        
        // Deklaracja funkcji importowanych z innych modułów
        for (name, (param_types, return_type)) in &self.imported_functions {
            let mut sig = Signature::new(isa::CallConv::SystemV);
            
            // Dodanie typów parametrów
            for param_type in param_types {
                sig.params.push(AbiParam::new(self.type_to_cranelift(param_type)?));
            }
            
            // Dodanie typu zwracanego
            sig.returns.push(AbiParam::new(self.type_to_cranelift(return_type)?));
            
            // Deklaracja funkcji w module
            let func_id = self.module.declare_function(
                name, 
                Linkage::Import, 
                &sig
            ).map_err(|e| CompileError::CodegenError {
                message: format!("Error declaring imported function '{}': {}", name, e),
                span: None,
                file_id: self.file_id,
            })?;
            
            self.functions.insert(name.clone(), func_id);
        }
        
        Ok(())
    }
    
    fn declare_runtime_functions(&mut self) -> Result<(), CompileError> {
        // Deklaracja funkcji print_int
        let mut sig = Signature::new(isa::CallConv::SystemV);
        sig.params.push(AbiParam::new(types::I32));
        let func_id = self.module.declare_function(
            "print_int", 
            Linkage::Import, 
            &sig
        ).map_err(|e| CompileError::CodegenError {
            message: format!("Error declaring runtime function: {}", e),
            span: None,
            file_id: self.file_id,
        })?;
        self.functions.insert("print_int".to_string(), func_id);
        
        // Podobnie deklarujemy inne funkcje runtime
        // print_str, print_bool, itp.
        
        Ok(())
    }
    
    fn declare_function(&mut self, func: &ast::Function) -> Result<(), CompileError> {
        let mut sig = Signature::new(isa::CallConv::SystemV);
        
        // Dodanie typów parametrów
        for param in &func.params {
            sig.params.push(AbiParam::new(self.type_to_cranelift(&param.ty)?));
        }
        
        // Dodanie typu zwracanego
        sig.returns.push(AbiParam::new(self.type_to_cranelift(&func.return_type)?));
        
        // Deklaracja funkcji w module
        let func_id = self.module.declare_function(
            &func.name, 
            Linkage::Local, 
            &sig
        ).map_err(|e| CompileError::CodegenError {
            message: format!("Error declaring function '{}': {}", func.name, e),
            span: Some(func.span),
            file_id: self.file_id,
        })?;
        
        self.functions.insert(func.name.clone(), func_id);
        
        Ok(())
    }
    
    fn define_function(&mut self, func: &ast::Function) -> Result<(), CompileError> {
        // Pobieranie ID funkcji
        let func_id = self.functions.get(&func.name).ok_or_else(|| {
            CompileError::CodegenError {
                message: format!("Function '{}' not declared", func.name),
                span: Some(func.span),
                file_id: self.file_id,
            }
        })?.clone();
        
        // Tworzenie kontekstu funkcji
        self.ctx.func.signature = self.module.declare_signature().map_err(|e| CompileError::CodegenError {
            message: format!("Error creating function signature: {}", e),
            span: Some(func.span),
            file_id: self.file_id,
        })?;
        
        // Tworzenie budowniczego funkcji
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);
        
        // Tworzenie podstawowego bloku
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        
        // Tutaj będziemy kompilować ciało funkcji
        // ...
        
        // Na razie, dla uproszczenia, po prostu zwracamy wartość stałą
        if !matches!(func.return_type, Type::Void) {
            let value = match func.return_type {
                Type::I32 => builder.ins().iconst(types::I32, 42),
                Type::Bool => builder.ins().iconst(types::I32, 1), // true
                _ => return Err(CompileError::CodegenError {
                    message: format!("Unsupported return type: {:?}", func.return_type),
                    span: Some(func.span),
                    file_id: self.file_id,
                }),
            };
            builder.ins().return_(&[value]);
        } else {
            builder.ins().return_(&[]);
        }
        
        // Finalizacja funkcji
        builder.seal_all_blocks();
        builder.finalize();
        
        // Definicja funkcji w module
        self.module.define_function(
            func_id,
            &mut self.ctx,
            &mut DataDescription::new()
        ).map_err(|e| CompileError::CodegenError {
            message: format!("Error defining function '{}': {}", func.name, e),
            span: Some(func.span),
            file_id: self.file_id,
        })?;
        
        // Czyszczenie kontekstu
        self.module.clear_context(&mut self.ctx);
        
        Ok(())
    }
    
    fn type_to_cranelift(&self, ty: &Type) -> Result<types::Type, CompileError> {
        match ty {
            Type::I32 => Ok(types::I32),
            Type::Bool => Ok(types::I32), // bool reprezentujemy jako i32
            Type::String => Ok(types::I64), // Wskaźniki jako i64
            Type::Pointer(_) | Type::RawPtr => Ok(types::I64),
            Type::Void => Ok(types::I32), // void reprezentujemy jako i32(0)
            Type::Struct(_) => Ok(types::I64), // Struktury przekazujemy przez wskaźnik
            Type::Array(_) => Ok(types::I64), // Tablice przekazujemy przez wskaźnik
            _ => Err(CompileError::CodegenError {
                message: format!("Unsupported type for Cranelift: {:?}", ty),
                span: None,
                file_id: self.file_id,
            }),
        }
    }
    
    fn write_output(&mut self, output_path: &Path) -> Result<(), CompileError> {
        // Finalizacja modułu i zapis do pliku
        let product = self.module.finish();
        let bytes = product.emit().map_err(|e| CompileError::CodegenError {
            message: format!("Error emitting object file: {}", e),
            span: None,
            file_id: self.file_id,
        })?;
        
        std::fs::write(output_path, bytes).map_err(CompileError::IOError)?;
        
        Ok(())
    }
} 