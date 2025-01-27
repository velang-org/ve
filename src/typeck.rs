use super::ast::{self, BinOp, Expr, Stmt, Type};
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::collections::HashMap;

#[derive(Debug)]
struct Context {
    variables: HashMap<String, Type>,
    functions: HashMap<String, (Vec<Type>, Type)>,
    current_return_type: Type,
    in_safe: bool,
}

impl Context {
    fn new() -> Self {
        Context {
            variables: HashMap::new(),
            functions: HashMap::new(),
            current_return_type: Type::Void,
            in_safe: false,
        }
    }
}

#[derive(Debug)]
pub struct TypeChecker {
    errors: Vec<Diagnostic<FileId>>,
    context: Context,
    file_id: FileId,
}

impl TypeChecker {
    pub fn new(file_id: FileId) -> Self {
        TypeChecker {
            file_id,
            errors: Vec::new(),
            context: Context::new(),
        }
    }

    pub fn check(&mut self, program: &ast::Program) -> Result<(), Vec<Diagnostic<FileId>>> {
        for func in &program.functions {
            let params: Vec<Type> = func.params.iter().map(|(_, t)| t.clone()).collect();
            self.context
                .functions
                .insert(func.name.clone(), (params, func.return_type.clone()));
        }

        for func in &program.functions {
            self.context.current_return_type = func.return_type.clone();
            self.check_function(func)?;
        }

        for stmt in &program.stmts {
            self.check_stmt(stmt)?;
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn check_function(&mut self, func: &ast::Function) -> Result<(), Vec<Diagnostic<FileId>>> {
        let mut local_ctx = Context::new();
        local_ctx.current_return_type = func.return_type.clone();

        for (name, ty) in &func.params {
            local_ctx.variables.insert(name.clone(), ty.clone());
        }

        let old_ctx = std::mem::replace(&mut self.context, local_ctx);
        for stmt in &func.body {
            if let Err(errors) = self.check_stmt(stmt) {
                self.errors.extend(errors);
            }
        }
        self.context = old_ctx;

        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<(), Vec<Diagnostic<FileId>>> {
        match stmt {
            Stmt::Let(name, decl_ty, expr, _) => {
                let expr_ty = self.check_expr(expr).unwrap_or(Type::Unknown);

                if let Some(decl_ty) = decl_ty {
                    if !Self::is_convertible(&expr_ty, decl_ty) {
                        self.report_error(
                            &format!("Cannot convert {} to {}", expr_ty, decl_ty),
                            expr.span(),
                        );
                    }
                }

                let ty = decl_ty.clone().unwrap_or(expr_ty);
                self.context.variables.insert(name.clone(), ty);
            }
            Stmt::Expr(expr, _) => {
                self.check_expr(expr)?;
            }
            Stmt::If(cond, then_branch, else_branch, _) => {
                let cond_ty = self.check_expr(cond).unwrap_or(Type::Unknown);
                self.expect_type(&cond_ty, &Type::Bool, cond.span())?;

                self.check_block(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.check_block(else_branch)?;
                }
            }
            Stmt::Return(expr, _) => {
                let expr_ty = self.check_expr(expr).unwrap_or(Type::Unknown);
                let expected_type = self.context.current_return_type.clone();
                self.expect_type(&expr_ty, &expected_type, expr.span())?;
            },
            Stmt::Defer(expr, span) => {
                let expr_ty = self.check_expr(expr)?;

                if expr_ty != Type::Void {
                    self.report_error(
                        "Defer expects void-returning expression",
                        *span
                    );
                }

                if let Expr::IntrinsicCall(name, _, _, _) = expr {
                    if !self.context.in_safe && (name == "__dealloc" || name == "__free") {
                        self.report_error(
                            "Memory operations require safe context",
                            *span
                        );
                    }
                }
            },
            Stmt::While(cond, body, _) => {
                let cond_ty = self.check_expr(cond)?;
                self.expect_type(&cond_ty, &Type::Bool, cond.span())?;
                self.check_block(body)?;
            },
            Stmt::For(init, cond, incr, body, _) => {
                if let Some(init) = init {
                    self.check_stmt(init)?;
                }
                
                if let Some(cond) = cond {
                    let cond_ty = self.check_expr(cond)?;
                    self.expect_type(&cond_ty, &Type::Bool, cond.span())?;
                }
                
                if let Some(incr) = incr {
                    self.check_expr(incr)?;
                }
                
                self.check_block(body)?;
            }
        }
        Ok(())
    }

    fn check_expr(&mut self, expr: &Expr) -> Result<Type, Vec<Diagnostic<FileId>>> {
        match expr {
            Expr::Int(_, _, _) => Ok(Type::I32),
            Expr::Bool(_, _, _) => Ok(Type::Bool),
            Expr::Str(_, _, _) => Ok(Type::String),
            Expr::Var(name, span, _) => self.context
                .variables
                .get(name)
                .cloned()
                .ok_or_else(|| {
                    self.report_error(&format!("Undefined variable '{}'", name), *span);
                    vec![]
                }),
            Expr::BinOp(left, op, right, span, _) => {
                let left_ty = self.check_expr(left).unwrap_or(Type::Unknown);
                let right_ty = self.check_expr(right).unwrap_or(Type::Unknown);

                let result_ty = match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        if left_ty == Type::I32 && right_ty == Type::I32 {
                            Type::I32
                        } else {
                            self.report_error(
                                &format!("Cannot apply {:?} to {} and {}", op, left_ty, right_ty),
                                *span,
                            );
                            Type::Unknown
                        }
                    }
                    BinOp::Gt | BinOp::Eq => {
                        if Self::is_convertible(&left_ty, &right_ty) {
                            Type::Bool
                        } else {
                            self.report_error(
                                &format!("Cannot compare {} and {}", left_ty, right_ty),
                                *span,
                            );
                            Type::Unknown
                        }
                    },
                };
                Ok(result_ty)
            },
            Expr::Deref(expr, span, _) => {
                let ty = self.check_expr(expr)?;
                match ty {
                    Type::Pointer(inner) => Ok(*inner),
                    Type::RawPtr => Ok(Type::Unknown),
                    _ => {
                        self.report_error(
                            &format!("Cannot dereference value of type {}", ty),
                            *span
                        );
                        Ok(Type::Unknown)
                    }
                }
            }
            Expr::Assign(target, value, span, _) => {
                let target_ty = self.check_expr(target)?;
                let value_ty = self.check_expr(value)?;

                if !Self::is_convertible(&value_ty, &target_ty) {
                    self.report_error(
                        &format!("Cannot assign {} to {}", value_ty, target_ty),
                        *span
                    );
                }

                Ok(Type::Void)
            },
            Expr::Call(name, args, span, _) => {
                let Some((param_types, return_type)) = self.context.functions.get(name).cloned() else {
                    self.report_error(&format!("Undefined function '{}'", name), *span);
                    return Ok(Type::Unknown);
                };

                if args.len() != param_types.len() {
                    self.report_error(
                        &format!("Expected {} arguments, got {}", param_types.len(), args.len()),
                        *span,
                    );
                }

                for (i, (arg, param_ty)) in args.iter().zip(param_types.iter()).enumerate() {
                    let arg_ty = self.check_expr(arg).unwrap_or(Type::Unknown);
                    if !Self::is_convertible(&arg_ty, param_ty) {
                        self.report_error(
                            &format!("Argument {}: expected {}, got {}", i + 1, param_ty, arg_ty),
                            arg.span(),
                        );
                    }
                }

                Ok(return_type)
            },
            Expr::IntrinsicCall(name, args, span, _) => match name.as_str() {
                "__alloc" => {
                    if args.len() != 1 {
                        self.report_error("__alloc expects 1 argument", *span);
                    }
                    Ok(Type::RawPtr)
                }
                "__dealloc" => {
                    if args.len() != 1 {
                        self.report_error("__dealloc expects 1 argument", *span);
                    }
                    Ok(Type::Void)
                }
                _ => {
                    self.report_error(&format!("Undefined intrinsic '{}'", name), *span);
                    Ok(Type::Unknown)
                }
            },
            Expr::SafeBlock(stmts, _, _) => {
                let old_in_safe = self.context.in_safe;
                self.context.in_safe = true;
                let result = self.check_block(stmts);

                self.context.in_safe = old_in_safe;
                result?;
                Ok(Type::Void)
            },
            Expr::Cast(expr, target_ty, span, _) => {
                let source_ty = self.check_expr(expr)?;

                match (&source_ty, target_ty) {
                    (Type::RawPtr, Type::Pointer(_)) => Ok(target_ty.clone()),
                    (Type::Pointer(_), Type::RawPtr) => Ok(target_ty.clone()),
                    (Type::Pointer(_), Type::I32) => Ok(target_ty.clone()),
                    (Type::I32, Type::Pointer(_)) => Ok(target_ty.clone()),
                    (Type::I32, Type::I32) => Ok(source_ty),
                    (Type::I32, Type::Bool) => Ok(target_ty.clone()),

                    _ => {
                        if !Self::is_convertible(&source_ty, target_ty) {
                            self.report_error(
                                &format!("Invalid cast from {} to {}", source_ty, target_ty),
                                *span
                            );
                            Ok(Type::Unknown)
                        } else {
                            Ok(target_ty.clone())
                        }
                    }
                }
            },
            Expr::Print(expr, span, _) => {
                let expr_ty = self.check_expr(expr)?;

                if !matches!(expr_ty, Type::I32 | Type::Bool | Type::String) {
                    self.report_error(
                        &format!("Cannot print value of type {}", expr_ty),
                        *span
                    );
                }

                Ok(Type::Void)
            }
        }
    }

    fn expect_type(
        &mut self,
        actual: &Type,
        expected: &Type,
        span: Span,
    ) -> Result<(), Vec<Diagnostic<FileId>>> {
        if !Self::is_convertible(actual, expected) {
            self.report_error(&format!("Expected {}, got {}", expected, actual), span);
            Err(vec![])
        } else {
            Ok(())
        }
    }

    fn is_convertible(from: &Type, to: &Type) -> bool {
        match (from, to) {
            (Type::I32, Type::Bool) => true,
            (Type::RawPtr, Type::Pointer(_)) => true,
            (Type::Pointer(_), Type::RawPtr) => true,
            (Type::Pointer(_), Type::I32) => true,
            (Type::I32, Type::Pointer(_)) => true,
            (Type::I32, Type::I32) => true,
            (Type::Pointer(a), Type::Pointer(b)) => a == b,
            _ => from == to
        }
    }

    fn check_block(&mut self, stmts: &[Stmt]) -> Result<(), Vec<Diagnostic<FileId>>> {
        let old_vars = self.context.variables.clone();
        for stmt in stmts {
            self.check_stmt(stmt)?;
        }
        self.context.variables = old_vars;
        Ok(())
    }

    fn report_error(&mut self, message: &str, span: Span) {
        self.errors.push(
            Diagnostic::error()
                .with_message(message)
                .with_labels(vec![Label::primary(self.file_id, span)]),
        );
    }
}