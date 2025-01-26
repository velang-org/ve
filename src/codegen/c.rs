use std::cell::RefCell;
use std::collections::HashSet;
use codespan::FileId;
use crate::{ast, codegen::{CodegenConfig, CompileError}};
use crate::ast::Type;

pub struct CBackend {
    config: CodegenConfig,
    output: String,
    file_id: FileId,
    includes: RefCell<HashSet<&'static str>>
}

impl CBackend {
    pub fn new(config: CodegenConfig, file_id: FileId) -> Self {
        Self {
            config,
            output: String::new(),
            file_id,
            includes: RefCell::new(HashSet::new()),
        }
    }

    pub fn compile(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        self.emit_header();
        self.emit_globals(program)?;
        self.emit_functions(program)?;
        self.emit_main_if_missing(program)?;
        self.write_output()?;
        Ok(())
    }

    fn emit_header(&mut self) {
        self.output.push_str(&format!("// Generated by Verve Compiler (target: {})\n", self.config.target_triple));
        self.output.push_str("#include <stdio.h>\n#include <stdlib.h>\n");


        for include in self.includes.borrow().iter() {
            self.output.push_str(&format!("#include {}\n", include));
        }

        self.output.push_str("\n");
    }

    fn emit_globals(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        for stmt in &program.stmts {
            if let ast::Stmt::Let(name, ty, expr, _) = stmt {
                let c_ty = match ty {
                    Some(t) => self.type_to_c(t),
                    None => "int",
                };
                let value = self.emit_expr(expr)?;
                self.output.push_str(&format!("{} {} = {};\n", c_ty, name, value));
            }
        }
        Ok(())
    }

    fn emit_main_if_missing(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        if !program.functions.iter().any(|f| f.name == "main") {
            self.output.push_str("\nint main() {\n");

            for stmt in &program.stmts {
                if !matches!(stmt, ast::Stmt::Let(..)) {
                    self.emit_stmt(stmt)?;
                }
            }

            self.output.push_str("    getchar();\n");
            self.output.push_str("    return 0;\n}\n");
        }
        Ok(())
    }

    fn emit_functions(&mut self, program: &ast::Program) -> Result<(), CompileError> {
        for func in &program.functions {
            self.emit_function(func)?;
        }
        Ok(())
    }

    fn emit_function(&mut self, func: &ast::Function) -> Result<(), CompileError> {
        let return_type = self.type_to_c(&func.return_type);

        let mut param_strings = Vec::new();
        for (name, ty) in &func.params {
            let c_ty = self.type_to_c(ty);
            param_strings.push(format!("{} {}", c_ty, name));
        }
        let params = param_strings.join(", ");

        self.output.push_str(&format!("{} {}({}) {{\n", return_type, func.name, params));

        for stmt in &func.body {
            self.emit_stmt(stmt)?;
        }

        if func.return_type == Type::Void {
            self.output.push_str("return;\n");
        }

        self.output.push_str("}\n\n");
        Ok(())
    }

    fn emit_stmt(&mut self, stmt: &ast::Stmt) -> Result<(), CompileError> {
        match stmt {
            ast::Stmt::Let(name, ty, expr, _) => {
                let c_ty = match ty {
                    Some(t) => self.type_to_c(t),
                    None => "int",
                };
                let expr_code = self.emit_expr(expr)?;
                self.output.push_str(&format!("{} {} = {};\n", c_ty, name, expr_code));
            }
            ast::Stmt::Return(expr, _) => {
                let expr_code = self.emit_expr(expr)?;
                self.output.push_str(&format!("return {};\n", expr_code));
            },
            ast::Stmt::Expr(expr, _) => {
                let expr_code = self.emit_expr(expr)?;
                self.output.push_str(&format!("{};\n", expr_code));
            }
            _ => unimplemented!(),
        }
        Ok(())
    }

    fn emit_expr(&self, expr: &ast::Expr) -> Result<String, CompileError> {
        match expr {
            ast::Expr::Int(n, _, _) => Ok(n.to_string()),
            ast::Expr::BinOp(left, op, right, span, _) => {
                let left_code = self.emit_expr(left)?;
                let right_code = self.emit_expr(right)?;
                let op_str = match op {
                    ast::BinOp::Add => "+",
                    ast::BinOp::Sub => "-",
                    ast::BinOp::Mul => "*",
                    ast::BinOp::Div => "/",
                    _ => return Err(CompileError::CodegenError {
                        message: "Unsupported operator".to_string(),
                        span: Some(*span),
                        file_id: self.file_id,
                    }),
                };
                Ok(format!("({} {} {})", left_code, op_str, right_code))
            },
            ast::Expr::Str(s, _, _) => Ok(format!("\"{}\"", s)),
            ast::Expr::Var(name, _, _) => Ok(name.clone()),
            ast::Expr::Print(expr, _span, _) => {
                let value = self.emit_expr(expr)?;
                let expr_ty = expr.get_type(); 
                let (format_spec, arg) = match expr_ty {
                    Type::I32 => ("%d", value),
                    Type::Bool => ("%s", format!("({} ? \"true\" : \"false\")", value)),
                    Type::String => ("%s", value),
                    _ => return Err(CompileError::CodegenError {
                        message: format!("Cannot print type {}", expr_ty),
                        span: Some(expr.span()),
                        file_id: self.file_id,
                    }),
                };
                Ok(format!("printf(\"{}\\n\", {});", format_spec, arg))
            }
            _ => Err(CompileError::CodegenError {
                message: "Unsupported expression".to_string(),
                span: Some(expr.span()),
                file_id: self.file_id,
            }),
        }
    }

    fn type_to_c(&self, ty: &ast::Type) -> &str {
        match ty {
            Type::I32 => "int",
            Type::Bool => {
                self.includes.borrow_mut().insert("<stdbool.h>");
                "bool"
            },
            Type::String => "const char*",
            Type::Void => "void",
            Type::Pointer(t) => match &**t {
                Type::Void => "void*",
                _ => "/* UNSUPPORTED POINTER TYPE */",
            },
            Type::RawPtr => "void*",
            _ => "/* UNSUPPORTED TYPE */",
        }
    }

    fn write_output(&self) -> Result<(), CompileError> {
        std::fs::write("output.c", &self.output)?;
        Ok(())
    }
}