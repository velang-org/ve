use super::ast::{self, BinOp, Expr, Stmt, Type};
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::collections::HashMap;

#[derive(Debug)]
struct Context {
    variables: HashMap<String, Type>,
    current_return_type: Type,
    in_safe: bool,
    struct_defs: HashMap<String, Vec<(String, Type)>>,
    enum_defs: HashMap<String, Vec<String>>,
    inferring_return_type: bool,
    inferred_return_type: Option<Type>,
}

impl Context {
    fn new() -> Self {
        Context {
            variables: HashMap::new(),
            current_return_type: Type::Void,
            in_safe: false,
            struct_defs: HashMap::new(),
            enum_defs: HashMap::new(),
            inferring_return_type: false,
            inferred_return_type: None,
        }
    }
}

#[derive(Debug)]
pub struct TypeChecker {
    errors: Vec<Diagnostic<FileId>>,
    context: Context,
    functions: HashMap<String, (Vec<Type>, Type)>,
    file_id: FileId,
}

impl TypeChecker {
    pub fn new(file_id: FileId, imported_functions: HashMap<String, (Vec<Type>, Type)>, imported_structs: Vec<ast::StructDef>, imported_ffi_vars: Vec<ast::FfiVariable>) -> Self {
        let mut checker = TypeChecker {
            file_id,
            errors: Vec::new(),
            context: Context::new(),
            functions: imported_functions,
        };

        for struct_def in imported_structs {
            let fields: Vec<(String, Type)> = struct_def.fields
                .iter()
                .map(|f| (f.name.clone(), f.ty.clone()))
                .collect();
            checker.context.struct_defs.insert(struct_def.name.clone(), fields);
        }

        for ffi_var in imported_ffi_vars {
            checker.context.variables.insert(ffi_var.name, ffi_var.ty);
        }

        checker
    }

    pub fn check(&mut self, program: &mut ast::Program) -> Result<(), Vec<Diagnostic<FileId>>> {
        for ffi in &program.ffi_functions {
            self.functions.insert(
                ffi.name.clone(),
                (ffi.params.clone(), ffi.return_type.clone())
            );
        }

        for ffi_var in &program.ffi_variables {
            self.context.variables.insert(ffi_var.name.clone(), ffi_var.ty.clone());
        }

        for struct_def in &program.structs {
            let fields: Vec<(String, Type)> = struct_def.fields
                .iter()
                .map(|f| (f.name.clone(), f.ty.clone()))
                .collect();
            self.context.struct_defs.insert(struct_def.name.clone(), fields);
        }

        for enum_def in &program.enums {
            let variants: Vec<String> = enum_def.variants
                .iter()
                .map(|v| v.name.clone())
                .collect();
            self.context.enum_defs.insert(enum_def.name.clone(), variants);
        }        for func in &program.functions {
            let params: Vec<Type> = func.params.iter().map(|(_, t)| t.clone()).collect();
            self.functions.insert(
                func.name.clone(),
                (params, func.return_type.clone())
            );
        }

        for stmt in &mut program.stmts {
            self.check_stmt(stmt)?;
        }

        for func in &mut program.functions {
            self.context.current_return_type = func.return_type.clone();
            self.check_function(func)?;
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }    fn check_function(&mut self, func: &mut ast::Function) -> Result<(), Vec<Diagnostic<FileId>>> {
        let mut local_ctx = Context::new();
        local_ctx.current_return_type = func.return_type.clone();
        local_ctx.struct_defs = self.context.struct_defs.clone();
        local_ctx.enum_defs = self.context.enum_defs.clone();
        
        local_ctx.variables = self.context.variables.clone();

        for (name, ty) in &func.params {
            local_ctx.variables.insert(name.clone(), ty.clone());
        }

        let original_ctx = std::mem::replace(&mut self.context, local_ctx);

        let mut inferred_return_type: Option<Type> = None;
        let explicit_return_type = func.return_type != Type::Void;
        
        if !explicit_return_type {
            self.context.inferring_return_type = true;
        }

        for stmt in &mut func.body {
            self.check_stmt(stmt)?;
            
            if !explicit_return_type && self.context.inferred_return_type.is_some() {
                if inferred_return_type.is_none() {
                    inferred_return_type = self.context.inferred_return_type.clone();
                    self.context.current_return_type = inferred_return_type.clone().unwrap();
                    func.return_type = inferred_return_type.clone().unwrap();
                    
                    if let Some((params, _)) = self.functions.get(&func.name).cloned() {
                        self.functions.insert(
                            func.name.clone(),
                            (params, inferred_return_type.clone().unwrap())
                        );
                    }
                }
            }
        }

        self.context = original_ctx;

        Ok(())
    }

    fn check_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Vec<Diagnostic<FileId>>> {
        match stmt {
            Stmt::Let(name, decl_ty, expr, _, _) => {
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
            },
            Stmt::Var(name, decl_ty, _) => {
                let ty = decl_ty.clone().unwrap_or(Type::Unknown);
                self.context.variables.insert(name.clone(), ty);
            },
            Stmt::Expr(expr, _) => {
                self.check_expr(expr)?;
            },
            Stmt::Block(stmts, _) => self.check_block(stmts)?,
            Stmt::If(cond, then_branch, else_branch, _) => {
                let cond_ty = self.check_expr(cond).unwrap_or(Type::Unknown);
                self.expect_type(&cond_ty, &Type::Bool, cond.span())?;

                self.check_block(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.check_block(else_branch)?;
                }
            },
            Stmt::Return(expr, _) => {
                let expr_ty = self.check_expr(expr).unwrap_or(Type::Unknown);
                
                if self.context.inferring_return_type && self.context.inferred_return_type.is_none() {
                    self.context.inferred_return_type = Some(expr_ty.clone());
                    self.context.current_return_type = expr_ty;
                } else {
                    let expected_type = self.context.current_return_type.clone();
                    self.expect_type(&expr_ty, &expected_type, expr.span())?;
                }
            },
            Stmt::Defer(expr, span) => {
                let expr_ty = self.check_expr(expr)?;

                if expr_ty != Type::Void {
                    self.report_error(
                        "Defer expects void-returning expression",
                        *span
                    );
                }
            },
            Stmt::While(cond, body, _) => {
                let cond_ty = self.check_expr(cond)?;
                self.expect_type(&cond_ty, &Type::Bool, cond.span())?;
                self.check_block(body)?;
            },
            Stmt::For(name, range, body, _) => {
                self.check_expr(range)?;

                self.context.variables.insert(name.clone(), Type::I32);
                self.check_block(body)?;
            }
            Stmt::Match(pattern, arms, _) => {
                let _matched_expr_ty = match pattern.as_ref() {
                    ast::Pattern::Variable(var_name, _) => {
                        self.context.variables.get(var_name).cloned().unwrap_or(Type::Unknown)
                    }
                    _ => Type::Unknown
                };

                for arm in arms {
                    match &arm.pattern {
                        ast::Pattern::EnumVariant(enum_name, variant_name, _, _) => {
                            if let Some(variants) = self.context.enum_defs.get(enum_name) {
                                if !variants.contains(variant_name) {
                                    self.report_error(
                                        &format!("Variant '{}' not found in enum '{}'", variant_name, enum_name),
                                        arm.span,
                                    );
                                }
                            } else {
                                self.report_error(
                                    &format!("Enum '{}' not found", enum_name),
                                    arm.span,
                                );
                            }
                        }
                        _ => {}
                    }

                    self.check_expr(&mut arm.body.clone())?;
                }
            }
            _ => {
                self.report_error(
                    &format!("Unsupported statement: {:?}", stmt),
                    stmt.span(),
                );
            }
        }
        Ok(())
    }

    fn check_expr(&mut self, expr: &mut Expr) -> Result<Type, Vec<Diagnostic<FileId>>> {
        if let Expr::FieldAccess(obj, field_name, span_info) = expr {
            if let Expr::Var(name, _) = obj.as_ref() {
                if self.context.enum_defs.contains_key(name) {
                    let enum_type = Type::Enum(name.clone());
                    *expr = Expr::EnumConstruct(
                        name.clone(),
                        field_name.clone(),
                        vec![],
                        ast::ExprInfo { span: span_info.span, ty: enum_type.clone() }
                    );
                    return Ok(enum_type);
                }
            }
        }

        match expr {
            Expr::Int(_, _) => Ok(Type::I32),
            Expr::Bool(_, _) => Ok(Type::Bool),
            Expr::Str(_, _) => Ok(Type::String),
            Expr::F32(_, _) => Ok(Type::F32),
            Expr::Void(_) => Ok(Type::Void),
            Expr::Var(name, ast::ExprInfo {span, ty: expr_type } ) => {
                let ty = match name.as_str() {
                    "true" | "false" => Type::Bool,
                    _ => match self.context.variables.get(name).cloned() {
                        Some(var_type) => var_type,
                        None => {
                            self.report_error(&format!("Undefined variable '{}'", name), *span);
                            Type::Unknown
                        }
                    }
                };
                *expr_type = ty.clone();
                Ok(ty)
            }
            Expr::BinOp(left, op, right, ast::ExprInfo { span, ty: expr_type } ) => {
                let left_ty = self.check_expr(left)?;
                let right_ty = self.check_expr(right)?;

                let result_ty = match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Pow | BinOp::Pow2 | BinOp::Mod => {
                        if (left_ty == Type::I32 && right_ty == Type::I32)
                            || (left_ty == Type::F32 && right_ty == Type::F32)
                            || (left_ty == Type::F64 && right_ty == Type::F64)
                        {
                            if left_ty == Type::F32 || right_ty == Type::F32 {
                                Type::F32
                            } else if left_ty == Type::F64 || right_ty == Type::F64 {
                                Type::F64
                            } else {
                                Type::I32
                            }
                        } else if left_ty == Type::String && right_ty == Type::String && matches!(op, BinOp::Add) {
                            Type::String
                        } else {
                            self.report_error(
                                &format!("Cannot apply {:?} to {} and {}", op, left_ty, right_ty),
                                *span,
                            );
                            Type::Unknown
                        }
                    },
                    BinOp::Gt | BinOp::Eq | BinOp::Lt | BinOp::NotEq | BinOp::GtEq | BinOp::LtEq => {
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
                    BinOp::And | BinOp::Or => {
                        if left_ty == Type::Bool && right_ty == Type::Bool {
                            Type::Bool
                        } else {
                            self.report_error(
                                &format!("Logical {} requires bool operands, got {} and {}", op, left_ty, right_ty),
                                *span,
                            );
                            Type::Unknown
                        }
                    }
                };

                *expr_type = result_ty.clone();

                Ok(result_ty)
            },
            Expr::Deref(inner_expr, info) => {
                let inner_ty = self.check_expr(inner_expr)?;
                let result_ty = match inner_ty {
                    Type::Pointer(t) => *t,
                    Type::RawPtr => Type::Unknown,
                    _ => {
                        self.report_error(
                            &format!("Cannot dereference type {}", inner_ty),
                            info.span
                        );
                        Type::Unknown
                    }
                };
                info.ty = result_ty.clone();
                Ok(result_ty)
            }
            Expr::UnaryOp(op, operand, ast::ExprInfo { span, ty: expr_type }) => {
                let operand_ty = self.check_expr(operand)?;
                let result_ty = match op {
                    ast::UnOp::Neg => {
                        if operand_ty == Type::I32 || operand_ty == Type::F32 {
                            operand_ty
                        } else {
                            self.report_error(
                                &format!("Cannot negate type {}", operand_ty),
                                *span,
                            );
                            Type::Unknown
                        }
                    }
                    ast::UnOp::Plus => operand_ty,
                };
                *expr_type = result_ty.clone();
                Ok(result_ty)
            },
            Expr::Assign(target, value, ast::ExprInfo { span, ty: _expr_type }) => {
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
            Expr::Call(name, args, ast::ExprInfo { span, ty: expr_type}) => {
                let mut found = self.functions.get(name).cloned();
                if found.is_none() {
                    found = self.functions.iter().find(|(k, _)| k.as_str() == name).map(|(_, v)| v.clone());
                }
                let (param_types, return_type) = match found {
                    Some(types) => types,
                    None => {
                        self.report_error(&format!("Undefined function '{}'", name), *span);
                        *expr_type = Type::Unknown;
                        return Ok(Type::Unknown);
                    }
                };

                let has_ellipsis = param_types.last() == Some(&Type::Ellipsis);
                let min_args = if has_ellipsis { param_types.len() - 1 } else { param_types.len() };

                if args.len() < min_args || (!has_ellipsis && args.len() > min_args) {
                    self.report_error(
                        &format!(
                            "Expected {}{} arguments, got {}",
                            min_args,
                            if has_ellipsis { "+" } else { "" },
                            args.len()
                        ),
                        *span,
                    );
                }

                for (arg, param_ty) in args.iter_mut().zip(param_types.iter()).take(min_args) {
                    let arg_ty = self.check_expr(arg).unwrap_or(Type::Unknown);
                    if !Self::is_convertible(&arg_ty, param_ty) {
                        self.report_error(
                            &format!("Expected {}, got {}", param_ty, arg_ty),
                            arg.span(),
                        );
                    }
                }
                *expr_type = return_type.clone();

                Ok(return_type.clone())
            },
            Expr::SafeBlock(stmts, _) => {
                let old_in_safe = self.context.in_safe;
                self.context.in_safe = true;
                let result = self.check_block(stmts);

                self.context.in_safe = old_in_safe;
                result?;
                Ok(Type::Void)
            },
            Expr::Cast(expr, target_ty, ast::ExprInfo { span, ty: _expr_type }) => {
                let source_ty = self.check_expr(expr)?;

                match (&source_ty, &target_ty) {
                    (Type::RawPtr, Type::Pointer(_)) => Ok(target_ty.clone()),
                    (Type::Pointer(_), Type::RawPtr) => Ok(target_ty.clone()),
                    (Type::Pointer(_), Type::I32) => Ok(target_ty.clone()),
                    (Type::I32, Type::Pointer(_)) => Ok(target_ty.clone()),
                    (Type::I32, Type::I32) => Ok(source_ty),
                    (Type::I32, Type::Bool) => Ok(target_ty.clone()),
                    (Type::F32, Type::F32) => Ok(source_ty),
                    (Type::F32, Type::I32) => Ok(target_ty.clone()),
                    (Type::I32, Type::F32) => Ok(target_ty.clone()),
                    (Type::I32, Type::U32) => Ok(target_ty.clone()),
                    (Type::U32, Type::I32) => Ok(target_ty.clone()),
                    (Type::String, Type::I32) => Ok(target_ty.clone()),
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
            Expr::Range(start, end, _) => {
                let start_ty = self.check_expr(start)?;
                let end_ty = self.check_expr(end)?;

                if start_ty != Type::I32 {
                    self.report_error("Range start must be an integer", start.span());
                }

                if end_ty != Type::I32 {
                    self.report_error("Range end must be an integer", end.span());
                }

                Ok(Type::Unknown)
            },
            Expr::StructInit(name, fields, ast::ExprInfo { span, ty: expr_type }) => {
                let struct_name = name.clone();
                let struct_fields = match self.context.struct_defs.get(&struct_name) {
                    Some(fields) => fields.clone(),
                    None => {
                        self.report_error(&format!("Undefined struct '{}'", name), *span);
                        *expr_type = Type::Unknown;
                        return Ok(Type::Unknown);
                    }
                };

                let mut seen_fields = HashMap::new();
                for (field_name, field_expr) in fields {
                    let field_ty = self.check_expr(field_expr)?;
                    let field_name = field_name.clone();

                    match struct_fields.iter().find(|(name, _)| name == &field_name) {
                        Some((_, expected_ty)) => {
                            if !Self::is_convertible(&field_ty, expected_ty) {
                                self.report_error(
                                    &format!("Type mismatch for field '{}': expected {}, got {}",
                                             field_name, expected_ty, field_ty),
                                    field_expr.span()
                                );
                            }
                        }
                        None => {
                            self.report_error(
                                &format!("Unknown field '{}' in struct '{}'", field_name, name),
                                field_expr.span()
                            );
                        }
                    }

                    seen_fields.insert(field_name, ());
                }

                for (field_name, _) in struct_fields {
                    if !seen_fields.contains_key(&field_name) {
                        self.report_error(
                            &format!("Missing field '{}' in struct initialization", field_name),
                            *span
                        );
                    }
                }

                let ty = Type::Struct(name.clone());
                *expr_type = ty.clone();
                Ok(ty)
            },
            Expr::FieldAccess(obj, field_name, ast::ExprInfo { span, ty: expr_type }) => {
                let obj_ty = self.check_expr(obj)?;
                match obj_ty {
                    Type::Struct(struct_name) => {
                        match self.context.struct_defs.get(&struct_name) {
                            Some(fields) => {
                                match fields.iter().find(|(name, _)| name == field_name) {
                                    Some((_, field_ty)) => {
                                        *expr_type = field_ty.clone();
                                        Ok(field_ty.clone())
                                    }
                                    None => {
                                        self.report_error(
                                            &format!("No field '{}' in struct '{}'", field_name, struct_name),
                                            *span
                                        );
                                        Ok(Type::Unknown)
                                    }
                                }
                            }
                            None => {
                                self.report_error(
                                    &format!("Unknown struct '{}'", struct_name),
                                    *span
                                );
                                Ok(Type::Unknown)
                            }
                        }
                    }
                    _ => {
                        self.report_error(
                            &format!("Cannot access field '{}' on type {}", field_name, obj_ty),
                            *span
                        );
                        Ok(Type::Unknown)
                    }
                }
            },
            Expr::ArrayInit(elements, ast::ExprInfo { span, ty: expr_type }) => {
                if elements.is_empty() {
                    self.report_error("Cannot infer type of empty array", *span);
                    *expr_type = Type::Unknown;
                    return Ok(Type::Unknown);
                }

                let first_type = self.check_expr(&mut elements[0])?;

                for (i, element) in elements.iter_mut().enumerate().skip(1) {
                    let el_type = self.check_expr(element)?;
                    if !Self::is_convertible(&el_type, &first_type) {
                        self.report_error(
                            &format!("Array element {} has type {}, but expected {}", i, el_type, first_type),
                            element.span()
                        );
                    }
                }

                let array_type = Type::Array(Box::new(first_type));
                *expr_type = array_type.clone();
                Ok(array_type)
            },
            Expr::ArrayAccess(array, index, ast::ExprInfo { span: _, ty: expr_type }) => {
                let array_type = self.check_expr(array)?;
                let index_type = self.check_expr(index)?;
                if !Self::is_convertible(&index_type, &Type::I32) {
                    self.report_error(
                        &format!("Array index must be i32, got {}", index_type),
                        index.span()
                    );
                }

                match array_type {
                    Type::Array(element_type) => {
                        let element_type = *element_type;
                        *expr_type = element_type.clone();
                        Ok(element_type)
                    },
                    Type::SizedArray(element_type, _) => {
                        let element_type = *element_type;
                        *expr_type = element_type.clone();
                        Ok(element_type)
                    },
                    _ => {
                        self.report_error(
                            &format!("Cannot index non-array type: {}", array_type),
                            array.span()
                        );
                        Ok(Type::Unknown)
                    }
                }
            },
            Expr::TemplateStr(parts, info) => {
                for part in parts {
                    if let ast::TemplateStrPart::Expression(expr) = part {
                        let ty = self.check_expr(expr)?;

                        match expr.as_mut() {
                            ast::Expr::ArrayAccess(array, index, array_info) => {
                                let array_ty = self.check_expr(array)?;
                                let index_ty = self.check_expr(index)?;

                                if !Self::is_convertible(&index_ty, &Type::I32) {
                                    self.report_error(
                                        &format!("Array index must be i32, got {}", index_ty),
                                        index.span()
                                    );
                                }

                                match array_ty {
                                    Type::Array(element_type) => {
                                        array_info.ty = *element_type.clone();
                                        if !Self::is_convertible(&element_type, &Type::String)
                                            && !Self::is_convertible(&element_type, &Type::I32)
                                            && !Self::is_convertible(&element_type, &Type::Bool) {
                                            self.report_error(
                                                &format!("Cannot convert array element type {} to string", element_type),
                                                array.span()
                                            );
                                        }
                                    },
                                    Type::SizedArray(element_type, _) => {
                                        array_info.ty = *element_type.clone();
                                        if !Self::is_convertible(&element_type, &Type::String)
                                            && !Self::is_convertible(&element_type, &Type::I32)
                                            && !Self::is_convertible(&element_type, &Type::Bool) {
                                            self.report_error(
                                                &format!("Cannot convert array element type {} to string", element_type),
                                                array.span()
                                            );
                                        }
                                    },
                                    _ => {
                                        self.report_error(
                                            &format!("Cannot index non-array type: {}", array_ty),
                                            array.span()
                                        );
                                    }
                                }
                            },
                            ast::Expr::Var(name, var_info) => {
                                let ty = match name.as_str() {
                                    "true" | "false" => Type::Bool,
                                    _ => self.context
                                        .variables
                                        .get(name)
                                        .cloned()
                                        .ok_or_else(|| {
                                            self.report_error(&format!("Undefined variable '{}'", name), var_info.span);
                                            vec![]
                                        })?,
                                };
                                var_info.ty = ty.clone();
                                if !matches!(ty, Type::String | Type::I32 | Type::Bool | Type::F32) {
                                    let msg = format!("Cannot convert type {:?} to string", ty);
                                    let span = expr.span();
                                    self.errors.push(Diagnostic::error().with_message(msg).with_labels(vec![
                                        Label::primary(self.file_id, span)
                                    ]));
                                }
                            },
                            _ => {
                                if !matches!(ty, Type::String | Type::I32 | Type::Bool | Type::F32) {
                                    let msg = format!("Cannot convert type {:?} to string", ty);
                                    let span = expr.span();
                                    self.errors.push(Diagnostic::error().with_message(msg).with_labels(vec![
                                        Label::primary(self.file_id, span)
                                    ]));
                                }
                            }
                        }
                    }
                }

                info.ty = Type::String;
                Ok(Type::String)
            }
            Expr::FfiCall(_name, _args, _info) => {
                // TODO: Implement FFI call type checking
                Ok(Type::Unknown)
            }
            Expr::EnumConstruct(enum_name, _variant_name, args, info) => {
                // TODO: Validate enum and variant exist
                for arg in args {
                    self.check_expr(arg)?;
                }
                let enum_type = Type::Enum(enum_name.clone());
                info.ty = enum_type.clone();
                Ok(enum_type)
            }
            Expr::MatchExpr(pattern, arms, info) => {
                let matched_ty = match pattern.as_ref() {
                    ast::Pattern::Variable(var_name, _) => {
                        self.context.variables.get(var_name).cloned().unwrap_or(Type::Unknown)
                    }
                    ast::Pattern::EnumVariant(enum_name, _, _, _) => {
                        Type::Enum(enum_name.clone())
                    }
                    _ => Type::Unknown
                };

                let mut arm_types = Vec::new();
                for arm in arms.iter_mut() {
                    if let ast::Pattern::EnumVariant(enum_name, variant_name, subpatterns, span) = &arm.pattern {
                        if let Some(variants) = self.context.enum_defs.get(enum_name) {
                            if !variants.contains(variant_name) {
                                self.report_error(
                                    &format!("Variant '{}' not found in enum '{}'", variant_name, enum_name),
                                    *span,
                                );
                            }
                        } else {
                            self.report_error(
                                &format!("Enum '{}' not found", enum_name),
                                *span,
                            );
                        }
                        for subpat in subpatterns {
                            self.check_pattern(subpat, &Type::Unknown)?;
                        }
                    } else {
                        self.check_pattern(&arm.pattern, &matched_ty)?;
                    }
                    let arm_ty = self.check_expr(&mut arm.body.clone())?;
                    arm_types.push(arm_ty);
                }
                let result_ty = arm_types.get(0).cloned().unwrap_or(Type::Unknown);
                if !arm_types.iter().all(|t| Self::is_convertible(t, &result_ty)) {
                    self.report_error("All match arms must return the same type", info.span);
                }
                info.ty = result_ty.clone();
                Ok(result_ty)
            }
        }
    }

    fn check_pattern(&mut self, pattern: &ast::Pattern, expected_ty: &Type) -> Result<(), Vec<Diagnostic<FileId>>> {
        match pattern {
            ast::Pattern::Wildcard(_) => {
                Ok(())
            }
            ast::Pattern::Variable(name, _) => {
                self.context.variables.insert(name.clone(), expected_ty.clone());
                Ok(())
            }
            ast::Pattern::EnumVariant(enum_name, _variant_name, patterns, span) => {
                match expected_ty {
                    Type::Enum(expected_enum) if expected_enum == enum_name => {
                        for pattern in patterns {
                            self.check_pattern(pattern, &Type::Unknown)?;
                        }
                        Ok(())
                    }
                    _ => {
                        Err(self.report_error_vec(
                            &format!("Pattern expects enum {}, but got {}", enum_name, expected_ty),
                            *span,
                        ))
                    }
                }
            }
            ast::Pattern::Literal(expr, span) => {
                let literal_ty = expr.get_type();
                if Self::is_convertible(&literal_ty, expected_ty) {
                    Ok(())
                } else {
                    Err(self.report_error_vec(
                        &format!("Literal pattern type {} doesn't match expected type {}", literal_ty, expected_ty),
                        *span,
                    ))
                }
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
        if from == to {
            return true;
        }

        match (from, to) {
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (_, Type::Any) => true,
            (Type::Any, _) => true,
            (Type::I32, Type::Bool) | (Type::Bool, Type::I32) => true,
            (Type::Bool, Type::String) => true,
            (Type::Pointer(_), Type::String) => true,
            (Type::RawPtr, Type::String) => true,
            (Type::I32, Type::String) => true,
            (Type::Struct(_), Type::String) => true,
            (Type::Array(_), Type::String) => true,
            (Type::SizedArray(_, _), Type::String) => true,
            (Type::Pointer(_), Type::RawPtr) => true,
            (Type::RawPtr, Type::Pointer(_)) => true,
            (Type::Pointer(_), Type::I32) => true,
            (Type::I32, Type::Pointer(_)) => true,
            (Type::RawPtr, Type::I32) => true,
            (Type::I32, Type::RawPtr) => true,
            (Type::Pointer(a), Type::Pointer(b)) => Self::is_convertible(a, b),
            (Type::Struct(a), Type::Struct(b)) => a == b,
            (Type::Array(from_elem), Type::Array(to_elem)) => Self::is_convertible(from_elem, to_elem),
            (Type::SizedArray(from_elem, _), Type::Array(to_elem)) => Self::is_convertible(from_elem, to_elem),
            (Type::Array(from_elem), Type::SizedArray(to_elem, _)) => Self::is_convertible(from_elem, to_elem),
            (Type::SizedArray(from_elem, from_size), Type::SizedArray(to_elem, to_size)) => {
                from_size == to_size && Self::is_convertible(from_elem, to_elem)
            },
            (Type::F32, Type::F32) => true,
            (Type::F32, Type::I32) => true,
            (Type::I32, Type::F32) => true,
            (Type::F32, Type::F64) => true,
            (Type::F64, Type::F32) => true,
            (Type::I32, Type::F64) => true,
            (Type::F64, Type::I32) => true,
            (Type::F64, Type::String) => true,
            _ => false,
        }
    }

    fn check_block(&mut self, stmts: &mut [Stmt]) -> Result<(), Vec<Diagnostic<FileId>>> {
        for stmt in stmts {
            self.check_stmt(stmt)?;
        }
        Ok(())
    }
    fn report_error_vec(&mut self, message: &str, span: Span) -> Vec<Diagnostic<FileId>> {
        let diag = Diagnostic::error()
            .with_message(message)
            .with_labels(vec![Label::primary(self.file_id, span)]);
        vec![diag]
    }

    fn report_error(&mut self, message: &str, span: Span) {
        self.errors.push(
            Diagnostic::error()
                .with_message(message)
                .with_labels(vec![Label::primary(self.file_id, span)]),
        );
    }
}

