use crate::ast::Type;
use crate::{ast, codegen::{CodegenConfig, CompileError}};
use codespan::{FileId, Span};
use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap};

pub struct CBackend {
    config: CodegenConfig,
    header: String,
    body: String,
    file_id: FileId,
    includes: RefCell<BTreeSet<&'static str>>,
    variables: RefCell<HashMap<String, Type>>,
    functions_map: HashMap<String, Type>,
    imported_functions: HashMap<String, (Vec<Type>, Type)>,
}

impl CBackend {
    pub fn new(config: CodegenConfig, file_id: FileId, imported_functions: HashMap<String, (Vec<Type>, Type)>) -> Self {
        Self {
            config,
            header: String::new(),
            body: String::new(),
            file_id,
            includes: RefCell::new(BTreeSet::new()),
            variables: RefCell::new(HashMap::new()),
            functions_map: HashMap::new(),
            imported_functions,
        }
    }

    pub fn compile(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        self.functions_map = program.functions.iter()
            .map(|f| (f.name.clone(), f.return_type.clone()))
            .chain(self.imported_functions.iter().map(|(k, v)| (k.clone(), v.1.clone())))
            .collect();
        self.emit_globals(program)?;
        self.emit_functions(program)?;
        self.emit_main_if_missing(program)?;
        self.emit_header();
        self.write_output()?;
        Ok(())
    }

    fn emit_header(&mut self) {
        self.header.push_str(&format!(
            "// Generated by Verve Compiler (target: {})\n",
            self.config.target_triple
        ));
        self.header.push_str("#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n");
        self.header.push_str("#include <stdbool.h>\n");

        for include in self.includes.borrow().iter() {
            self.header.push_str(&format!("#include {}\n", include));
        }

        self.header.push('\n');

        self.header.push_str("static char* int_to_str(int num) {\n");
        self.header.push_str("    char* buffer = malloc(12);\n");
        self.header.push_str("    sprintf(buffer, \"%d\", num);\n");
        self.header.push_str("    return buffer;\n");
        self.header.push_str("}\n\n");

        self.header.push_str("static char* bool_to_str(bool b) {\n");
        self.header.push_str("    const char* val = b ? \"true\" : \"false\";\n");
        self.header.push_str("    size_t len = strlen(val) + 1;\n");
        self.header.push_str("    char* buffer = malloc(len);\n");
        self.header.push_str("    if (buffer) strcpy_s(buffer, len, val);\n");
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
            ast::Expr::Bool(..)
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

            #[cfg(target_os = "windows")]
            self.body.push_str("    system(\"pause\");\n");
            #[cfg(not(target_os = "windows"))]
            self.body.push_str("    getchar();\n");

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

        for (name, (param_types, return_type)) in &self.imported_functions {
            let return_type_c = self.type_to_c(return_type);
            let params_c = param_types.iter()
                .map(|ty| self.type_to_c(ty))
                .collect::<Vec<_>>()
                .join(", ");

            self.body.push_str(&format!("{} {}({});\n", return_type_c, name, params_c));
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
            #[cfg(target_os = "windows")]
            self.body.push_str("    system(\"pause\");\n");
            #[cfg(not(target_os = "windows"))]
            self.body.push_str("    getchar();\n");

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
        match stmt {
            ast::Stmt::Let(name, ty, expr, _) => {
                let var_type = if let Some(ty) = ty {
                    ty.clone()
                } else {
                    match expr {
                        ast::Expr::Call(func_name, _, _, _) => {
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
            ast::Expr::Int(n, _, _) => Ok(n.to_string()),
            ast::Expr::BinOp(left, op, right, _span, _) => {
                let left_code = self.emit_expr(left)?;
                let right_code = self.emit_expr(right)?;

                let result_type = expr.get_type();

                match result_type {
                    Type::String => {
                        let left_conv = self.convert_to_c_str(&left_code, &left.get_type());
                        let right_conv = self.convert_to_c_str(&right_code, &right.get_type());
                        Ok(format!("concat({}, {})", left_conv, right_conv))
                    }
                    _ => {
                        let c_op = match op {
                            ast::BinOp::Add => "+",
                            ast::BinOp::Sub => "-",
                            ast::BinOp::Mul => "*",
                            ast::BinOp::Div => "/",
                            ast::BinOp::Eq => "==",
                            ast::BinOp::Gt => ">",
                            ast::BinOp::Lt => "<",
                            ast::BinOp::And => "&&",
                            ast::BinOp::Or => "||",
                        };
                        Ok(format!("({} {} {})", left_code, c_op, right_code))
                    }
                }
            },
            ast::Expr::Assign(target, value, _, _) => {
                let target_code = self.emit_expr(target)?;
                let value_code = self.emit_expr(value)?;
                Ok(format!("({} = {})", target_code, value_code)) 
            },
            ast::Expr::Str(s, _, _) => Ok(format!("\"{}\"", s)),
            ast::Expr::Var(name, _, _) => {
                if name == "true" || name == "false" {
                    self.includes.borrow_mut().insert("<stdbool.h>");
                    Ok(name.clone())
                } else {
                    let var_type = self.variables.borrow().get(name).cloned().unwrap_or(Type::Unknown);
                    match var_type {
                        Type::I32 => Ok(name.clone()),
                        Type::Bool => Ok(name.clone()),
                        Type::String => Ok(name.clone()),
                        _ => Err(CompileError::CodegenError {
                            message: format!("Cannot print type {:?}", var_type),
                            span: Some(expr.span()),
                            file_id: self.file_id,
                        }),
                    }
                }
            },
            ast::Expr::Print(expr, _span, _) => {
                let value = self.emit_expr(expr)?;
                let expr_ty = match &**expr {
                    ast::Expr::Var(name, _, _) => {
                        self.variables.borrow()
                            .get(name)
                            .cloned()
                            .unwrap_or(Type::Unknown)
                    }
                    _ => expr.get_type(),
                };

                let (format_spec, arg) = match expr_ty {
                    Type::I32 => ("%d", value),
                    Type::Bool => ("%s", format!("({} ? \"true\" : \"false\")", value)),
                    Type::String => ("%s", value),
                    Type::Pointer(_) | Type::RawPtr => {
                        ("%p", format!("(void*){}", value))
                    },
                    _ => return Err(CompileError::CodegenError {
                        message: format!("Cannot print type {:?}", expr_ty),
                        span: Some(expr.span()),
                        file_id: self.file_id,
                    }),
                };
                Ok(format!("printf(\"{}\\n\", {});", format_spec, arg))
            },
            ast::Expr::Call(name, args, _, _) => {
                let mut args_code = Vec::new();
                for arg in args {
                    args_code.push(self.emit_expr(arg)?);
                }
                Ok(format!("{}({})", name, args_code.join(", ")))
            },
            ast::Expr::IntrinsicCall(name, args, span, _) => match name.as_str() {
                "__alloc" => {
                    if args.len() != 1 {
                        return Err(CompileError::CodegenError {
                            message: "__alloc expects 1 argument".to_string(),
                            span: Some(*span),
                            file_id: self.file_id,
                        });
                    }
                    let size = self.emit_expr(&args[0])?;
                    Ok(format!("malloc({})", size))
                },
                "__dealloc" => {
                    if args.len() != 1 {
                        return Err(CompileError::CodegenError {
                            message: "__dealloc expects 1 argument".to_string(),
                            span: Some(*span),
                            file_id: self.file_id,
                        });
                    }
                    let ptr = self.emit_expr(&args[0])?;
                    Ok(format!("free({})", ptr))
                }
                _ => Err(CompileError::CodegenError {
                    message: format!("Unknown intrinsic function: {}", name),
                    span: Some(*span),
                    file_id: self.file_id,
                }),
            },
            ast::Expr::SafeBlock(stmts, _span, _) => {
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
            ast::Expr::Deref(expr, _, _) => {
                let inner = self.emit_expr(expr)?;
                Ok(format!("(*{})", inner))
            }
            ast::Expr::Cast(expr, target_ty, _, _) => {
                let expr_code = self.emit_expr(expr)?;
                let expr_type = expr.get_type();

                let target_c_ty = if expr_type.is_pointer() && *target_ty == Type::I32 {
                    self.includes.borrow_mut().insert("<stdint.h>");
                    "uintptr_t".to_string()
                } else {
                    self.type_to_c(target_ty)
                };

                Ok(format!("({})({})", target_c_ty, expr_code))
            },
            ast::Expr::Range(start, end, _, _) => {
                let start_code = self.emit_expr(start)?;
                let end_code = self.emit_expr(end)?;
                Ok(format!("{} .. {}", start_code, end_code))
            },
            _ => Err(CompileError::CodegenError {
                message: "Unsupported expression".to_string(),
                span: Some(expr.span()),
                file_id: self.file_id,
            }),
        }
    }

    fn unify_types(&self, t1: &Type, t2: &Type, span: Span) -> Result<Type, CompileError> {
        match (t1, t2) {
            (Type::I32, Type::I32) => Ok(Type::I32),
            (Type::Bool, Type::Bool) => Ok(Type::Bool),
            (Type::Unknown, t) | (t, Type::Unknown) => Ok(t.clone()),
            _ => Err(CompileError::TypeError {
                message: format!("Type mismatch: {:?} vs {:?}", t1, t2),
                span: Some(span),
                file_id: self.file_id,
            }),
        }
    }
    
    fn emit_stmt_to_string(&mut self, stmt: &ast::Stmt) -> Result<String, CompileError> {
        let mut buffer = String::new();
        let original_body = std::mem::take(&mut self.body);
        self.emit_stmt(stmt)?;
        buffer = std::mem::replace(&mut self.body, original_body);
        Ok(buffer)
    }

    fn type_to_c(&self, ty: &Type) -> String {
        match ty {
            Type::I32 => "int".to_string(),
            Type::Bool => {
                self.includes.borrow_mut().insert("<stdbool.h>");
                "bool".to_string()
            },
            Type::String => "const char*".to_string(),
            Type::Void => "void".to_string(),
            Type::Pointer(inner) => {
                let inner_type = self.type_to_c(inner);
                format!("{}*", inner_type)
            },
            Type::RawPtr => "void*".to_string(),
            _ => "/* UNSUPPORTED TYPE */".to_string(),
        }
    }

    fn write_output(&self) -> Result<(), CompileError> {
        let full_output = format!("{}{}", self.header, self.body);
        std::fs::write("output.c", &full_output)?;
        Ok(())
    }

    fn convert_to_c_str(&self, code: &str, ty: &Type) -> String {
        match ty {
            Type::I32 => format!("int_to_str({})", code),
            Type::Bool => format!("bool_to_str({})", code),
            Type::Pointer(_) | Type::RawPtr => format!("ptr_to_str({})", code),
            Type::String => code.to_string(),
            Type::Unknown => {
                panic!("Cannot convert unknown type to string. Check type inference.")
            },
            _ => {
                panic!("Cannot convert type {:?} to string", ty)
            }
        }
    }
}