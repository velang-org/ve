use super::ast::{self, BinOp, Expr, Stmt, Type};
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::collections::HashMap;

#[derive(Debug)]
struct Context {
    variables: HashMap<String, Type>,
    current_return_type: Type,
    in_safe: bool,
    in_loop: bool,
    break_types: Vec<Type>,
    struct_defs: HashMap<String, Vec<(String, Type)>>,
    enum_defs: HashMap<String, Vec<String>>,
    enum_def_map: HashMap<String, ast::EnumDef>,
    inferring_return_type: bool,
    inferred_return_type: Option<Type>,
}

impl Context {
    fn new() -> Self {
        Context {
            variables: HashMap::new(),
            current_return_type: Type::Void,
            in_safe: false,
            in_loop: false,
            break_types: Vec::new(),
            struct_defs: HashMap::new(),
            enum_defs: HashMap::new(),
            enum_def_map: HashMap::new(),
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
    enums: Vec<ast::EnumDef>,
    impls: Vec<ast::ImplBlock>,
}

impl TypeChecker {
    pub fn new(
        file_id: FileId,
        imported_functions: HashMap<String, (Vec<Type>, Type)>,
        imported_structs: Vec<ast::StructDef>,
        imported_ffi_vars: Vec<ast::FfiVariable>,
    ) -> Self {
        let mut checker = TypeChecker {
            file_id,
            errors: Vec::new(),
            context: Context::new(),
            functions: imported_functions,
            enums: Vec::new(),
            impls: Vec::new(),
        };

        for struct_def in imported_structs {
            let fields: Vec<(String, Type)> = struct_def
                .fields
                .iter()
                .map(|f| (f.name.clone(), f.ty.clone()))
                .collect();
            checker
                .context
                .struct_defs
                .insert(struct_def.name.clone(), fields);
        }

        for ffi_var in imported_ffi_vars {
            checker.context.variables.insert(ffi_var.name, ffi_var.ty);
        }

        checker
    }

    pub fn check(&mut self, program: &mut ast::Program) -> Result<(), Vec<Diagnostic<FileId>>> {
        self.enums = program.enums.clone();
        self.impls = program.impls.clone(); 

        for ffi in &program.ffi_functions {
            self.functions.insert(
                ffi.name.clone(),
                (ffi.params.clone(), ffi.return_type.clone()),
            );
        }

        for ffi_var in &program.ffi_variables {
            self.context
                .variables
                .insert(ffi_var.name.clone(), ffi_var.ty.clone());
        }

        for struct_def in &program.structs {
            let fields: Vec<(String, Type)> = struct_def
                .fields
                .iter()
                .map(|f| (f.name.clone(), f.ty.clone()))
                .collect();
            self.context
                .struct_defs
                .insert(struct_def.name.clone(), fields);
        }

        for enum_def in &program.enums {
            let variants: Vec<String> = enum_def.variants.iter().map(|v| v.name.clone()).collect();
            self.context
                .enum_defs
                .insert(enum_def.name.clone(), variants);
            self.context
                .enum_def_map
                .insert(enum_def.name.clone(), enum_def.clone());
        }
        for func in &program.functions {
            let params: Vec<Type> = func.params.iter().map(|(_, t)| t.clone()).collect();
            self.functions
                .insert(func.name.clone(), (params, func.return_type.clone()));
        }

        for stmt in &mut program.stmts {
            self.check_stmt(stmt)?;
        }

        for func in &mut program.functions {
            self.context.current_return_type = func.return_type.clone();
            if let Err(e) = self.check_function(func) {
                return Err(e);
            }
        }

        for impl_block in &mut program.impls {
            self.check_impl_block(impl_block)?;
        }

        for test in &mut program.tests {
            self.check_test(test)?;
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }
    fn check_function(&mut self, func: &mut ast::Function) -> Result<(), Vec<Diagnostic<FileId>>> {
        let mut local_ctx = Context::new();
        local_ctx.current_return_type = func.return_type.clone();
        local_ctx.struct_defs = self.context.struct_defs.clone();
        local_ctx.enum_defs = self.context.enum_defs.clone();
        local_ctx.enum_def_map = self.context.enum_def_map.clone();

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
                            (params, inferred_return_type.clone().unwrap()),
                        );
                    }
                }
            }
        }

        self.context = original_ctx;

        Ok(())
    }

    fn check_test(&mut self, test: &mut ast::Test) -> Result<(), Vec<Diagnostic<FileId>>> {
        let mut local_ctx = Context::new();
        local_ctx.current_return_type = Type::Void;
        local_ctx.struct_defs = self.context.struct_defs.clone();
        local_ctx.enum_defs = self.context.enum_defs.clone();
        local_ctx.enum_def_map = self.context.enum_def_map.clone();
        local_ctx.variables = self.context.variables.clone();

        let original_ctx = std::mem::replace(&mut self.context, local_ctx);

        for stmt in &mut test.stmts {
            self.check_stmt(stmt)?;
        }

        self.context = original_ctx;

        Ok(())
    }

    fn check_stmt(&mut self, stmt: &mut Stmt) -> Result<(), Vec<Diagnostic<FileId>>> {
        match stmt {
            Stmt::Let(name, decl_ty, expr, var_span, _) => {
                let expr_ty = self.check_expr(expr).unwrap_or(Type::Unknown);

                if expr_ty == Type::Void {
                    self.report_error("Cannot assign void expression to variable",
                    *var_span);
                    return Ok(());
                }

                let final_ty = if let Some(decl_ty) = decl_ty {
                    if expr_ty == Type::NoneType {
                        match decl_ty {
                            Type::Optional(_) => decl_ty.clone(),
                            _ => {
                                self.report_error(
                                    &format!("Cannot assign None to non-optional type {}", decl_ty),
                                    expr.span(),
                                );
                                decl_ty.clone()
                            }
                        }
                    } else if !Self::is_convertible(&expr_ty, decl_ty) {
                        self.report_error(
                            &format!("Cannot convert {} to {}", expr_ty, decl_ty),
                            expr.span(),
                        );
                        decl_ty.clone()
                    } else {
                        decl_ty.clone()
                    }
                } else {
                    if expr_ty == Type::NoneType {
                        self.report_error(
                            "Cannot infer type from None literal - specify type annotation",
                            expr.span(),
                        );
                        Type::Unknown
                    } else {
                        expr_ty
                    }
                };

                self.context.variables.insert(name.clone(), final_ty);
            }
            Stmt::Var(name, decl_ty, _) => {
                let ty = decl_ty.clone().unwrap_or(Type::Unknown);
                self.context.variables.insert(name.clone(), ty);
            }
            Stmt::Expr(expr, _) => {
                self.check_expr(expr)?;
            }
            Stmt::Block(stmts, _) => self.check_block(stmts)?,
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

                if self.context.inferring_return_type && self.context.inferred_return_type.is_none()
                {
                    self.context.inferred_return_type = Some(expr_ty.clone());
                    self.context.current_return_type = expr_ty;
                } else {
                    let expected_type = self.context.current_return_type.clone();
                    self.expect_type(&expr_ty, &expected_type, expr.span())?;
                }
            }
            Stmt::Defer(expr, span) => {
                let expr_ty = self.check_expr(expr)?;

                if expr_ty != Type::Void {
                    self.report_error("Defer expects void-returning expression", *span);
                }
            }
            Stmt::While(cond, body, _) => {
                let cond_ty = self.check_expr(cond)?;
                self.expect_type(&cond_ty, &Type::Bool, cond.span())?;
                self.context.in_loop = true;
                self.check_block(body)?;
                self.context.in_loop = false;
            }
            Stmt::Loop(body, _) => {
                self.context.in_loop = true;
                self.check_block(body)?;
                self.context.in_loop = false;
            }
            Stmt::For(name, index_var, range, step, body, _) => {
                self.check_expr(range)?;
                
                if let Some(step_expr) = step {
                    let step_type = self.check_expr(step_expr)?;
                    if !matches!(step_type, Type::I32 | Type::I64 | Type::U32 | Type::U64) {
                        self.report_error("Step value must be an integer", step_expr.span());
                    }
                }

                let old_variables = self.context.variables.clone();
                self.context.variables.insert(name.clone(), Type::I32);
                if let Some(idx_var) = index_var {
                    self.context.variables.insert(idx_var.clone(), Type::I32);
                }
                self.context.in_loop = true;
                self.check_block(body)?;
                self.context.in_loop = false;
                self.context.variables = old_variables;
            }
            Stmt::Break(expr, span) => {
                if !self.context.in_loop {
                    self.report_error("Break statement outside of loop", *span);
                }
                if let Some(expr) = expr {
                    let break_type = self.check_expr(expr)?;
                    self.context.break_types.push(break_type);
                } else {
                    self.context.break_types.push(Type::Void);
                }
            }
            Stmt::Continue(span) => {
                if !self.context.in_loop {
                    self.report_error("Continue statement outside of loop", *span);
                }
            }
        }
        Ok(())
    }
    fn check_expr(&mut self, expr: &mut Expr) -> Result<Type, Vec<Diagnostic<FileId>>> {
        if let Expr::FieldAccess(obj, field_name, span_info) = expr {
            if let Expr::Var(name, _) = obj.as_ref() {
                if let Some(var_type) = self.context.variables.get(name) {
                    if let Type::Enum(enum_name) = var_type {
                        let enum_type = Type::Enum(enum_name.clone());
                        *expr = Expr::EnumConstruct(
                            enum_name.clone(),
                            field_name.clone(),
                            vec![],
                            ast::ExprInfo {
                                span: span_info.span,
                                ty: enum_type.clone(),
                                is_tail: span_info.is_tail,
                            },
                        );
                        return Ok(enum_type);
                    }
                } else if self.context.enum_defs.contains_key(name) {
                    let enum_type = Type::Enum(name.clone());
                    *expr = Expr::EnumConstruct(
                        name.clone(),
                        field_name.clone(),
                        vec![],
                        ast::ExprInfo {
                            span: span_info.span,
                            ty: enum_type.clone(),
                            is_tail: span_info.is_tail,
                        },
                    );
                    return Ok(enum_type);
                }
            }
        }

        match expr {
            Expr::Int(value, info) => {
                let expected_type = &self.context.current_return_type;
                if *value >= 0 {
                    match expected_type {
                        Type::U8 => {
                            if *value > u8::MAX as i32 {
                                self.report_error(
                                    &format!("Value {} exceeds maximum for u8 ({})", value, u8::MAX),
                                    info.span,
                                );
                                info.ty = Type::Unknown;
                                return Ok(Type::Unknown);
                            }
                            info.ty = Type::U8;
                            Ok(Type::U8)
                        }
                        Type::U16 => {
                            if *value > u16::MAX as i32 {
                                self.report_error(
                                    &format!("Value {} exceeds maximum for u16 ({})", value, u16::MAX),
                                    info.span,
                                );
                                info.ty = Type::Unknown;
                                return Ok(Type::Unknown);
                            }
                            info.ty = Type::U16;
                            Ok(Type::U16)
                        }
                        Type::U32 => {
                            if *value > u32::MAX as i32 {
                                self.report_error(
                                    &format!("Value {} exceeds maximum for u32 ({})", value, u32::MAX),
                                    info.span,
                                );
                                info.ty = Type::Unknown;
                                return Ok(Type::Unknown);
                            }
                            info.ty = Type::U32;
                            Ok(Type::U32)
                        }
                        Type::U64 => {
                            info.ty = Type::U64;
                            Ok(Type::U64)
                        }
                        _ => {
                            info.ty = Type::I32;
                            Ok(Type::I32)
                        }
                    }
                } else {
                    match expected_type {
                        Type::I8 => {
                            if *value < i8::MIN as i32 || *value > i8::MAX as i32 {
                                self.report_error(
                                    &format!("Value {} is out of range for i8 ({}..{})", value, i8::MIN, i8::MAX),
                                    info.span,
                                );
                                info.ty = Type::Unknown;
                                return Ok(Type::Unknown);
                            }
                            info.ty = Type::I8;
                            Ok(Type::I8)
                        }
                        Type::I16 => {
                            if *value < i16::MIN as i32 || *value > i16::MAX as i32 {
                                self.report_error(
                                    &format!("Value {} is out of range for i16 ({}..{})", value, i16::MIN, i16::MAX),
                                    info.span,
                                );
                                info.ty = Type::Unknown;
                                return Ok(Type::Unknown);
                            }
                            info.ty = Type::I16;
                            Ok(Type::I16)
                        }
                        Type::I32 => {
                            info.ty = Type::I32;
                            Ok(Type::I32)
                        }
                        Type::I64 => {
                            info.ty = Type::I64;
                            Ok(Type::I64)
                        }
                        Type::U8 | Type::U16 | Type::U32 | Type::U64 => {
                            self.report_error(
                                &format!("Cannot assign negative value {} to unsigned type {}", value, expected_type),
                                info.span,
                            );
                            info.ty = Type::Unknown;
                            Ok(Type::Unknown)
                        }
                        _ => {
                            info.ty = Type::I32;
                            Ok(Type::I32)
                        }
                    }
                }
            }
            Expr::Bool(_, _) => Ok(Type::Bool),
            Expr::Str(_, _) => Ok(Type::String),
            Expr::Int64(_, _) => Ok(Type::I64),
            Expr::F32(_, _) => Ok(Type::F32),
            Expr::Void(_) => Ok(Type::Void),
            Expr::Var(
                name,
                ast::ExprInfo {
                    span,
                    ty: expr_type,
                    is_tail: _,
                },
            ) => {
                let ty = match name.as_str() {
                    "true" | "false" => Type::Bool,
                    _ => {
                        match self.context.variables.get(name).cloned() {
                            Some(ty) => ty,
                            None => {
                                self.report_error(
                                    &format!("Undefined variable '{}'", name),
                                    *span,
                                );
                                Type::Unknown
                            }
                        }
                    },
                };
                *expr_type = ty.clone();
                Ok(ty)
            }
            Expr::BinOp(
                left,
                op,
                right,
                ast::ExprInfo {
                    span,
                    ty: expr_type,
                    is_tail: _,
                },
            ) => {
                let left_ty = self.check_expr(left)?;
                let right_ty = self.check_expr(right)?;
                let result_ty = match op {
                    BinOp::Add
                    | BinOp::Sub
                    | BinOp::Mul
                    | BinOp::Div
                    | BinOp::Pow
                    | BinOp::Pow2
                    | BinOp::Mod => {
                        if (left_ty == Type::I8 && right_ty == Type::I8)
                            || (left_ty == Type::I16 && right_ty == Type::I16)
                            || (left_ty == Type::I32 && right_ty == Type::I32)
                            || (left_ty == Type::I64 && right_ty == Type::I64)
                            || (left_ty == Type::U8 && right_ty == Type::U8)
                            || (left_ty == Type::U16 && right_ty == Type::U16)
                            || (left_ty == Type::U32 && right_ty == Type::U32)
                            || (left_ty == Type::U64 && right_ty == Type::U64)
                            || (left_ty == Type::F32 && right_ty == Type::F32)
                            || (left_ty == Type::F64 && right_ty == Type::F64)
                            || (matches!(left_ty, Type::Struct(ref name) if name == "size_t")
                                && right_ty == Type::I32)
                            || (left_ty == Type::I32
                                && matches!(right_ty, Type::Struct(ref name) if name == "size_t"))
                            || (matches!(left_ty, Type::Struct(ref name) if name == "size_t")
                                && matches!(right_ty, Type::Struct(ref name2) if name2 == "size_t"))
                            || (left_ty == Type::CSize && right_ty == Type::I32)
                            || (left_ty == Type::I32 && right_ty == Type::CSize)
                            || (left_ty == Type::CSize && right_ty == Type::CSize)
                        {
                            if left_ty == Type::F32 || right_ty == Type::F32 {
                                Type::F32
                            } else if left_ty == Type::F64 || right_ty == Type::F64 {
                                Type::F64
                            } else if left_ty == Type::I64 || right_ty == Type::I64 {
                                Type::I64
                            } else if left_ty == Type::U64 || right_ty == Type::U64 {
                                Type::U64
                            } else if left_ty == Type::I32 || right_ty == Type::I32 {
                                Type::I32
                            } else if left_ty == Type::U32 || right_ty == Type::U32 {
                                Type::U32
                            } else if left_ty == Type::I16 || right_ty == Type::I16 {
                                Type::I16
                            } else if left_ty == Type::U16 || right_ty == Type::U16 {
                                Type::U16
                            } else if left_ty == Type::I8 || right_ty == Type::I8 {
                                Type::I8
                            } else if left_ty == Type::U8 || right_ty == Type::U8 {
                                Type::U8
                            } else if matches!(left_ty, Type::Struct(ref name) if name == "size_t")
                                || matches!(right_ty, Type::Struct(ref name) if name == "size_t")
                            {
                                Type::Struct("size_t".to_string())
                            } else if left_ty == Type::CSize || right_ty == Type::CSize {
                                Type::CSize
                            } else {
                                left_ty
                            }
                        } else if left_ty == Type::String
                            && right_ty == Type::String
                            && matches!(op, BinOp::Add)
                        {
                            Type::String
                        } else {
                            self.report_error(
                                &format!("Cannot apply {:?} to {} and {}", op, left_ty, right_ty),
                                *span,
                            );
                            Type::Unknown
                        }
                    }
                    BinOp::Gt
                    | BinOp::Eq
                    | BinOp::Lt
                    | BinOp::NotEq
                    | BinOp::GtEq
                    | BinOp::LtEq => {
                        if Self::is_convertible(&left_ty, &right_ty) {
                            Type::Bool
                        } else {
                            self.report_error(
                                &format!("Cannot compare {} and {}", left_ty, right_ty),
                                *span,
                            );
                            Type::Unknown
                        }
                    }
                    BinOp::And | BinOp::Or => {
                        if left_ty == Type::Bool && right_ty == Type::Bool {
                            Type::Bool
                        } else {
                            self.report_error(
                                &format!(
                                    "Logical {} requires bool operands, got {} and {}",
                                    op, left_ty, right_ty
                                ),
                                *span,
                            );
                            Type::Unknown
                        }
                    }
                };

                *expr_type = result_ty.clone();

                Ok(result_ty)
            }
            Expr::Deref(inner_expr, info) => {
                let inner_ty = self.check_expr(inner_expr)?;
                let result_ty = match inner_ty {
                    Type::Pointer(t) => *t,
                    Type::RawPtr => Type::Unknown,
                    _ => {
                        self.report_error(
                            &format!("Cannot dereference type {}", inner_ty),
                            info.span,
                        );
                        Type::Unknown
                    }
                };
                info.ty = result_ty.clone();
                Ok(result_ty)
            }
            Expr::UnaryOp(
                op,
                operand,
                ast::ExprInfo {
                    span,
                    ty: expr_type,
                    is_tail: _,
                },
            ) => {
                let operand_ty = self.check_expr(operand)?;
                let result_ty = match op {
                    ast::UnOp::Neg => {
                        if operand_ty == Type::I8
                            || operand_ty == Type::I16
                            || operand_ty == Type::I32
                            || operand_ty == Type::I64
                            || operand_ty == Type::F32
                            || operand_ty == Type::F64
                        {
                            operand_ty
                        } else {
                            self.report_error(&format!("Cannot negate type {}", operand_ty), *span);
                            Type::Unknown
                        }
                    }
                    ast::UnOp::Plus => {
                        if operand_ty == Type::I8
                            || operand_ty == Type::I16
                            || operand_ty == Type::I32
                            || operand_ty == Type::I64
                            || operand_ty == Type::U8
                            || operand_ty == Type::U16
                            || operand_ty == Type::U32
                            || operand_ty == Type::U64
                            || operand_ty == Type::F32
                            || operand_ty == Type::F64
                        {
                            operand_ty
                        } else {
                            self.report_error(
                                &format!("Cannot apply unary plus to type {}", operand_ty),
                                *span,
                            );
                            Type::Unknown
                        }
                    }
                    ast::UnOp::Not => {
                        if operand_ty == Type::Bool {
                            Type::Bool
                        } else {
                            self.report_error(
                                &format!("Cannot apply logical NOT to type {}", operand_ty),
                                *span,
                            );
                            Type::Unknown
                        }
                    }
                };
                *expr_type = result_ty.clone();
                Ok(result_ty)
            }
            Expr::Assign(
                target,
                value,
                ast::ExprInfo {
                    span,
                    ty: _expr_type,
                    is_tail: _,
                },
            ) => {
                let target_ty = self.check_expr(target)?;
                let value_ty = self.check_expr(value)?;

                if !Self::is_convertible(&value_ty, &target_ty) {
                    self.report_error(
                        &format!("Cannot assign {} to {}", value_ty, target_ty),
                        *span,
                    );
                }

                Ok(Type::Void)
            }
            Expr::Call(
                name,
                args,
                ast::ExprInfo {
                    span,
                    ty: expr_type,
                    is_tail: _,
                },
            ) => {
                if name.starts_with("<method>.") {
                    let method_name = &name[9..]; 
                    if let Some(obj_expr) = args.first_mut() {
                        let obj_type = self.check_expr(obj_expr)?;
                        
                        let type_name = obj_type.to_string();
                        
                        let impls = self.impls.clone();
                        let mut method_found = false;
                        let mut method_return_type = Type::Unknown;
                        let mut expected_args = 0;
                        
                        for impl_block in &impls {
                            if impl_block.target_type == type_name {
                                for method in &impl_block.methods {
                                    if method.name == method_name {
                                        method_found = true;
                                        method_return_type = method.return_type.clone();
                                        expected_args = method.params.len();
                                        break;
                                    }
                                }
                                if method_found { break; }
                            }
                        }
                        
                        if !method_found {
                            self.report_error(
                                &format!("Method '{}' not found for type '{}'", method_name, type_name),
                                *span,
                            );
                            *expr_type = Type::Unknown;
                            return Ok(Type::Unknown);
                        }
                        
                        if args.len() != expected_args {
                            self.report_error(
                                &format!("Method '{}' expects {} arguments, got {}", 
                                    method_name, expected_args, args.len()),
                                *span,
                            );
                        }
                        
                        *expr_type = method_return_type.clone();
                        return Ok(method_return_type);
                    }
                }
                
                let mut found = self.functions.get(name).cloned();
                if found.is_none() {
                    found = self
                        .functions
                        .iter()
                        .find(|(k, _)| k.as_str() == name)
                        .map(|(_, v)| v.clone());
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
                let min_args = if has_ellipsis {
                    param_types.len() - 1
                } else {
                    param_types.len()
                };

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
            }
            Expr::SafeBlock(stmts, _) => {
                let old_in_safe = self.context.in_safe;
                self.context.in_safe = true;
                let result = self.check_block(stmts);

                self.context.in_safe = old_in_safe;
                result?;
                Ok(Type::Void)
            }
            Expr::Cast(
                expr,
                target_ty,
                ast::ExprInfo {
                    span,
                    ty: _expr_type,
                    is_tail: _,
                },
            ) => {
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
                    (Type::String, Type::RawPtr) => Ok(target_ty.clone()),
                    (Type::F32, Type::String) => Ok(target_ty.clone()),
                    _ => {
                        if !self.is_cast_allowed(&source_ty, target_ty) {
                            self.report_error(
                                &format!("Invalid cast from {} to {}", source_ty, target_ty),
                                *span,
                            );
                            Ok(Type::Unknown)
                        } else {
                            Ok(target_ty.clone())
                        }
                    }
                }
            }
            Expr::Range(start, end, _, _) => {
                let start_ty = self.check_expr(start)?;
                let end_ty = self.check_expr(end)?;

                if start_ty != Type::I32 {
                    self.report_error("Range start must be an integer", start.span());
                }

                if end_ty != Type::I32 {
                    self.report_error("Range end must be an integer", end.span());
                }

                Ok(Type::Unknown)
            }
            Expr::InfiniteRange(_, info) => {
                info.ty = Type::I32;
                Ok(Type::I32)
            }
            Expr::StructInit(
                name,
                fields,
                ast::ExprInfo {
                    span,
                    ty: expr_type,
                    is_tail: _,
                },
            ) => {
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
                    let field_name = field_name.clone();

                    let expected_ty = match struct_fields.iter().find(|(name, _)| name == &field_name) {
                        Some((_, expected_ty)) => expected_ty.clone(),
                        None => Type::Unknown,
                    };

                    let field_ty = self.check_expr_with_expected(field_expr, &expected_ty)?;

                    match struct_fields.iter().find(|(name, _)| name == &field_name) {
                        Some((_, expected_ty)) => {
                            if !Self::is_convertible(&field_ty, expected_ty) {
                                self.report_error(
                                    &format!(
                                        "Type mismatch for field '{}': expected {}, got {}",
                                        field_name, expected_ty, field_ty
                                    ),
                                    field_expr.span(),
                                );
                            }
                        }
                        None => {
                            self.report_error(
                                &format!("Unknown field '{}' in struct '{}'", field_name, name),
                                field_expr.span(),
                            );
                        }
                    }

                    seen_fields.insert(field_name, ());
                }

                for (field_name, _) in struct_fields {
                    if !seen_fields.contains_key(&field_name) {
                        self.report_error(
                            &format!("Missing field '{}' in struct initialization", field_name),
                            *span,
                        );
                    }
                }

                let ty = Type::Struct(name.clone());
                *expr_type = ty.clone();
                Ok(ty)
            }
            Expr::FieldAccess(
                obj,
                field_name,
                ast::ExprInfo {
                    span,
                    ty: expr_type,
                    is_tail
                },
            ) => {
                let obj_ty = self.check_expr(obj)?;
                match obj_ty {
                    Type::Struct(struct_name) => match self.context.struct_defs.get(&struct_name) {
                        Some(fields) => match fields.iter().find(|(name, _)| name == field_name) {
                            Some((_, field_ty)) => {
                                *expr_type = field_ty.clone();
                                Ok(field_ty.clone())
                            }
                            None => {
                                self.report_error(
                                    &format!(
                                        "No field '{}' in struct '{}'",
                                        field_name, struct_name
                                    ),
                                    *span,
                                );
                                Ok(Type::Unknown)
                            }
                        },
                        None => {
                            self.report_error(&format!("Unknown struct '{}'", struct_name), *span);
                            Ok(Type::Unknown)
                        }
                    },
                    _ => {
                        self.report_error(
                            &format!("Cannot access field '{}' on type {}", field_name, obj_ty),
                            *span,
                        );
                        Ok(Type::Unknown)
                    }
                }
            }
            Expr::ArrayInit(
                elements,
                ast::ExprInfo {
                    span,
                    ty: expr_type,
                    is_tail
                },
            ) => {
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
                            &format!(
                                "Array element {} has type {}, but expected {}",
                                i, el_type, first_type
                            ),
                            element.span(),
                        );
                    }
                }

                let array_type = Type::Array(Box::new(first_type));
                *expr_type = array_type.clone();
                Ok(array_type)
            }
            Expr::ArrayAccess(
                array,
                index,
                ast::ExprInfo {
                    span: _,
                    ty: expr_type,
                    is_tail: _,
                },
            ) => {
                let array_type = self.check_expr(array)?;
                let index_type = self.check_expr(index)?;
                if !Self::is_convertible(&index_type, &Type::I32)
                    && !matches!(index_type, Type::Struct(ref name) if name == "size_t")
                    && index_type != Type::CSize
                {
                    self.report_error(
                        &format!("Array index must be i32 or size_t, got {}", index_type),
                        index.span(),
                    );
                }

                match array_type {
                    Type::Array(element_type) => {
                        let element_type = *element_type;
                        *expr_type = element_type.clone();
                        Ok(element_type)
                    }
                    Type::SizedArray(element_type, _) => {
                        let element_type = *element_type;
                        *expr_type = element_type.clone();
                        Ok(element_type)
                    }
                    Type::Pointer(element_type) => {
                        let element_type = *element_type;
                        *expr_type = element_type.clone();
                        Ok(element_type)
                    }
                    Type::RawPtr => {
                        let element_type = Type::U8;
                        *expr_type = element_type.clone();
                        Ok(element_type)
                    }
                    _ => {
                        self.report_error(
                            &format!("Cannot index non-array type: {}", array_type),
                            array.span(),
                        );
                        Ok(Type::Unknown)
                    }
                }
            }
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
                                        index.span(),
                                    );
                                }

                                match array_ty {
                                    Type::Array(element_type) => {
                                        array_info.ty = *element_type.clone();
                                        if !Self::is_convertible(&element_type, &Type::String)
                                            && !matches!(
                                                *element_type,
                                                Type::I8
                                                    | Type::I16
                                                    | Type::I32
                                                    | Type::I64
                                                    | Type::U8
                                                    | Type::U16
                                                    | Type::U32
                                                    | Type::U64
                                                    | Type::F32
                                                    | Type::F64
                                                    | Type::Bool
                                                    | Type::Generic(_)
                                                    | Type::Optional(_)  
                                            )
                                        {
                                            self.report_error(
                                                &format!("Cannot convert array element type {} to string", element_type),
                                                array.span()
                                            );
                                        }
                                    }
                                    Type::SizedArray(element_type, _) => {
                                        array_info.ty = *element_type.clone();
                                        if !Self::is_convertible(&element_type, &Type::String)
                                            && !matches!(
                                                *element_type,
                                                Type::I8
                                                    | Type::I16
                                                    | Type::I32
                                                    | Type::I64
                                                    | Type::U8
                                                    | Type::U16
                                                    | Type::U32
                                                    | Type::U64
                                                    | Type::F32
                                                    | Type::F64
                                                    | Type::Bool
                                                    | Type::Generic(_)
                                                    | Type::Optional(_) 
                                            )
                                        {
                                            self.report_error(
                                                &format!("Cannot convert array element type {} to string", element_type),
                                                array.span()
                                            );
                                        }
                                    }
                                    _ => {
                                        self.report_error(
                                            &format!("Cannot index non-array type: {}", array_ty),
                                            array.span(),
                                        );
                                    }
                                }
                            }
                            ast::Expr::Var(name, var_info) => {
                                let ty = if name == "true" || name == "false" {
                                    Type::Bool
                                } else {
                                    match self.context.variables.get(name).cloned() {
                                        Some(ty) => ty,
                                        None => {
                                            self.report_error(
                                                &format!("Undefined variable '{}'", name),
                                                var_info.span,
                                            );
                                            Type::Unknown
                                        }
                                    }
                                };
                                var_info.ty = ty.clone();
                                if !matches!(
                                    ty,
                                    Type::String
                                        | Type::I8
                                        | Type::I16
                                        | Type::I32
                                        | Type::I64
                                        | Type::U8
                                        | Type::U16
                                        | Type::U32
                                        | Type::U64
                                        | Type::F32
                                        | Type::F64
                                        | Type::Bool
                                        | Type::Generic(_)
                                        | Type::Optional(_)  
                                ) {
                                    let msg = format!("Cannot convert type {:?} to string", ty);
                                    let span = expr.span();
                                    self.errors.push(
                                        Diagnostic::error()
                                            .with_message(msg)
                                            .with_labels(vec![Label::primary(self.file_id, span)]),
                                    );
                                }
                            }
                            _ => {
                                if !matches!(
                                    ty,
                                    Type::String
                                        | Type::I8
                                        | Type::I16
                                        | Type::I32
                                        | Type::I64
                                        | Type::U8
                                        | Type::U16
                                        | Type::U32
                                        | Type::U64
                                        | Type::F32
                                        | Type::F64
                                        | Type::Bool
                                        | Type::Generic(_)
                                        | Type::Optional(_) 
                                ) {
                                    let msg = format!("Cannot convert type {:?} to string", ty);
                                    let span = expr.span();
                                    self.errors.push(
                                        Diagnostic::error()
                                            .with_message(msg)
                                            .with_labels(vec![Label::primary(self.file_id, span)]),
                                    );
                                }
                            }
                        }
                    }
                }

                info.ty = Type::String;
                Ok(Type::String)
            }
            Expr::FfiCall(_name, _args, _info) => Ok(Type::Unknown), 
            Expr::EnumConstruct(enum_name, _, args, info) => {
                for arg in args.iter_mut() {
                    self.check_expr(arg)?;
                }
                let ty = if let Some(enum_def) = self.context.enum_def_map.get(enum_name) {
                    if !enum_def.generic_params.is_empty() {
                        let mut context_ty = None;
                        if let Type::GenericInstance(n, params) = &self.context.current_return_type {
                            if n == enum_name && params.len() == enum_def.generic_params.len() {
                                context_ty = Some(Type::GenericInstance(n.clone(), params.clone()));
                            }
                        }
                        if context_ty.is_none() {
                            if let Some(last_var_ty) = self.context.variables.values().last() {
                                if let Type::GenericInstance(n, params) = last_var_ty {
                                    if n == enum_name && params.len() == enum_def.generic_params.len() {
                                        context_ty = Some(Type::GenericInstance(n.clone(), params.clone()));
                                    }
                                }
                            }
                        }
                        if let Some(t) = context_ty {
                            t
                        } else {
                            let arg_types: Vec<Type> = args.iter().map(|e| e.get_type()).collect();
                            Type::GenericInstance(enum_name.clone(), arg_types)
                        }
                    } else {
                        Type::Enum(enum_name.clone())
                    }
                } else {
                    Type::Enum(enum_name.clone())
                };
                info.ty = ty.clone();
                Ok(ty)
            }
            Expr::Match(pattern, arms, info) => {
                let matched_ty = match pattern.as_ref() {
                    ast::Pattern::Variable(var_name, _) => self
                        .context
                        .variables
                        .get(var_name)
                        .cloned()
                        .unwrap_or(Type::Unknown),
                    ast::Pattern::EnumVariant(enum_name, _, _, _) => Type::Enum(enum_name.clone()),
                    _ => Type::Unknown,
                };
                let mut arm_types = Vec::new();
                for arm in arms.iter_mut() {
                    let original_variables = self.context.variables.clone();
                    match &arm.pattern {
                        ast::Pattern::EnumVariant(enum_name, variant_name, subpatterns, _) => {
                            let expected = if let Type::GenericInstance(name, args) = &matched_ty {
                                if name == enum_name {
                                    Type::GenericInstance(name.clone(), args.clone())
                                } else {
                                    Type::Enum(enum_name.clone())
                                }
                            } else {
                                Type::Enum(enum_name.clone())
                            };
                            self.check_pattern(&arm.pattern, &expected)?;
                        }
                        _ => {
                            self.check_pattern(&arm.pattern, &matched_ty)?;
                        }
                    }
                    
                    if let Some(guard) = &mut arm.guard {
                        let guard_ty = self.check_expr(guard)?;
                        if !matches!(guard_ty, Type::Bool) {
                            self.report_error("Guard condition must be a boolean expression", guard.span());
                        }
                    }
                    
                    let arm_ty = match &mut arm.body {
                        ast::MatchArmBody::Expr(expr) => self.check_expr(expr)?,
                        ast::MatchArmBody::Block(stmts) => {
                            let last_ty = Type::Void;
                            for stmt in stmts.iter_mut() {
                                self.check_stmt(stmt)?;
                            }
                            last_ty
                        }
                    };
                    arm_types.push(arm_ty);
                    self.context.variables = original_variables;
                }
                let result_ty = arm_types.get(0).cloned().unwrap_or(Type::Unknown);
                if !arm_types.iter().all(|t| Self::is_convertible(t, &result_ty)) {
                    self.report_error("All match arms must return the same type", info.span);
                }
                info.ty = result_ty.clone();
                Ok(result_ty)
            }
            Expr::If(condition, then_branch, else_branch, info) => {
                let condition_ty = self.check_expr(condition)?;
                if !Self::is_convertible(&condition_ty, &Type::Bool) {
                    self.report_error("If condition must be boolean", info.span);
                }

                let original_variables = self.context.variables.clone();
                
                for stmt in then_branch.iter_mut() {
                    self.check_stmt(stmt)?;
                }
                let then_ty = if let Some(last_stmt) = then_branch.last() {
                    match last_stmt {
                        ast::Stmt::Expr(expr, _) => expr.get_type(),
                        _ => Type::Void,
                    }
                } else {
                    Type::Void
                };

                let else_ty = if let Some(else_stmts) = else_branch {
                    self.context.variables = original_variables.clone();
                    for stmt in else_stmts.iter_mut() {
                        self.check_stmt(stmt)?;
                    }
                    if let Some(last_stmt) = else_stmts.last() {
                        match last_stmt {
                            ast::Stmt::Expr(expr, _) => expr.get_type(),
                            _ => Type::Void,
                        }
                    } else {
                        Type::Void
                    }
                } else {
                    Type::Void
                };

                self.context.variables = original_variables;

                let result_ty = if Self::is_convertible(&then_ty, &else_ty) {
                    then_ty
                } else if Self::is_convertible(&else_ty, &then_ty) {
                    else_ty
                } else if then_ty == Type::Void && else_ty == Type::Void {
                    Type::Void
                } else {
                    then_ty
                };

                info.ty = result_ty.clone();
                Ok(result_ty)
            }
            Expr::Loop(body, info) => {
                self.context.in_loop = true;
                
                let previous_break_types = std::mem::take(&mut self.context.break_types);
                
                for stmt in body.iter_mut() {
                    self.check_stmt(stmt)?;
                }
                
                let break_types = std::mem::take(&mut self.context.break_types);
                self.context.break_types = previous_break_types;
                self.context.in_loop = false;
                
                let loop_type = if break_types.is_empty() {
                    Type::Void
                } else {
                    let mut common_type = break_types[0].clone();
                    for break_type in &break_types[1..] {
                        if !Self::is_convertible(&common_type, break_type) && 
                           !Self::is_convertible(break_type, &common_type) {
                            if common_type != *break_type {
                                self.report_error(&format!(
                                    "Inconsistent types in break statements: {:?} vs {:?}", 
                                    common_type, break_type
                                ), info.span);
                                common_type = Type::Unknown;
                                break;
                            }
                        }
                    }
                    common_type
                };
                
                info.ty = loop_type.clone();
                Ok(loop_type)
            }
            Expr::None(info) => {
                info.ty = Type::NoneType;
                Ok(Type::NoneType)
            }
        }
    }

    fn check_expr_with_expected(&mut self, expr: &mut Expr, expected_ty: &Type) -> Result<Type, Vec<Diagnostic<FileId>>> {
        if let Expr::ArrayInit(elements, ast::ExprInfo { ty: expr_type, .. }) = expr {
            if elements.is_empty() {
                if let Type::Array(_element_ty) = expected_ty {
                    *expr_type = expected_ty.clone();
                    return Ok(expected_ty.clone());
                }
            }
        }
        
        self.check_expr(expr)
    }

    fn is_cast_allowed(&self, from: &Type, to: &Type) -> bool {
    match (from, to) {
        (Type::Bool, Type::String) => true,
        (Type::I32, Type::String) => true,
        (Type::I64, Type::String) => true,
        (Type::F32, Type::String) => true,
        (Type::F64, Type::String) => true,
        (Type::Generic(_), Type::String) => true,
        (Type::RawPtr, Type::Pointer(_)) => true,
        (Type::Pointer(_), Type::RawPtr) => true,
        (Type::Pointer(_), Type::I32) => true,
        (Type::I32, Type::Pointer(_)) => true,
        (Type::I32, Type::Bool) => true,
        (Type::F32, Type::I32) => true,
        (Type::I32, Type::F32) => true,
        (Type::I32, Type::U32) => true,
        (Type::U32, Type::I32) => true,
        (Type::U8, Type::I32) => true,
        (Type::String, Type::I32) => true,
        (Type::String, Type::RawPtr) => true,
        (Type::RawPtr, Type::String) => true,
        (Type::Enum(enum_name), Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::I8 | Type::I16 | Type::I32 | Type::I64) => {
            if let Some(enum_def) = self.context.enum_def_map.get(enum_name) {
                enum_def.variants.iter().any(|v| v.value.is_some())
            } else {
                false
            }
        },
        _ => Self::is_convertible(from, to),
    }
}

    fn check_pattern(
        &mut self,
        pattern: &ast::Pattern,
        expected_ty: &Type,
    ) -> Result<(), Vec<Diagnostic<FileId>>> {
        match pattern {
            ast::Pattern::Wildcard(_) => Ok(()),
            ast::Pattern::Variable(name, _) => {
                self.context
                    .variables
                    .insert(name.clone(), expected_ty.clone());
                Ok(())
            }
            ast::Pattern::EnumVariant(enum_name, variant_name, patterns, _) => {
                match expected_ty {
                    Type::GenericInstance(expected_enum, generic_args) if expected_enum == enum_name => {
                        if let Some(enum_def) = self.enums.iter().find(|e| &e.name == enum_name) {
                            if let Some(variant) = enum_def.variants.iter().find(|v| &v.name == variant_name) {
                                let mut subst = std::collections::HashMap::new();
                                for (gp, arg) in enum_def.generic_params.iter().zip(generic_args.iter()) {
                                    subst.insert(gp, arg);
                                }
                                let data_types = if let Some(data) = &variant.data {
                                    data.iter().map(|t| substitute_generics(t, &subst)).collect::<Vec<_>>()
                                } else {
                                    vec![]
                                };
                                for (i, subpat) in patterns.iter().enumerate() {
                                    let ty = data_types.get(i).cloned().unwrap_or(Type::Unknown);
                                    self.check_pattern(subpat, &ty)?;
                                }
                                return Ok(());
                            }
                        }
                        for (i, subpat) in patterns.iter().enumerate() {
                            let ty = generic_args.get(i).cloned().unwrap_or(Type::Unknown);
                            self.check_pattern(subpat, &ty)?;
                        }
                        Ok(())
                    }
                    Type::Enum(expected_enum) if expected_enum == enum_name => {
                        for pattern in patterns {
                            self.check_pattern(pattern, &Type::Unknown)?;
                        }
                        Ok(())
                    }
                    _ => {
                        self.report_error(
                            &format!(
                                "Pattern expects enum {}, but got {}",
                                enum_name, expected_ty
                            ),
                            pattern.span(),
                        );
                        Ok(())
                    }
                }
            }
            ast::Pattern::Literal(expr, span) => {
                let literal_ty = expr.get_type();
                let mut expected = expected_ty;
                if let Type::Unknown = expected_ty {
                    if let Some(var_ty) = self.context.variables.values().last() {
                        expected = var_ty;
                    }
                }
                if Self::is_convertible(&literal_ty, expected) {
                    Ok(())
                } else {
                    self.report_error(
                        &format!(
                            "Literal pattern type {} doesn't match expected type {}",
                            literal_ty, expected
                        ),
                        *span,
                    );
                    Ok(())
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
        }
        Ok(())
    }
    fn is_convertible(from: &Type, to: &Type) -> bool {
        if from == to { return true; }
        match (from, to) {
            (Type::GenericInstance(n1, a1), Type::GenericInstance(n2, a2)) =>
                n1 == n2 && a1.len() == a2.len() && a1.iter().zip(a2).all(|(a, b)| Self::is_convertible(a, b)),
            (Type::Generic(_), _) | (_, Type::Generic(_)) => true,
            (Type::I8, Type::I16 | Type::I32 | Type::I64)
            | (Type::I16, Type::I32 | Type::I64)
            | (Type::I32, Type::I64 | Type::F32 | Type::F64 | Type::U32 | Type::Bool | Type::Pointer(_) | Type::RawPtr | Type::CSize | Type::U8 | Type::U16 | Type::I8 | Type::I16)
            | (Type::I64, Type::U32 | Type::U64 | Type::U8 | Type::U16 | Type::I8 | Type::I16 | Type::I32)
            | (Type::U8, Type::U16 | Type::U32 | Type::U64 | Type::I32)
            | (Type::U16, Type::U32 | Type::U64)
            | (Type::U32, Type::U64 | Type::I32)
            | (Type::Bool, Type::I32)
            | (Type::F32, Type::F64 | Type::I32 | Type::I64 | Type::String)
            | (Type::F64, Type::I32 | Type::I64 | Type::F32)
            | (Type::Pointer(_), Type::RawPtr | Type::I32)
            | (Type::RawPtr, Type::Pointer(_) | Type::I32)
            | (Type::CSize, Type::I32)
            | (Type::Void, Type::Void)
            => true,
            (Type::Struct(n1), Type::Struct(n2)) => n1 == "size_t" && n2 == "size_t",
            (Type::Struct(n1), Type::I32) => n1 == "size_t",
            (Type::I32, Type::Struct(n2)) => n2 == "size_t",
            (Type::Pointer(inner), Type::String) => matches!(&**inner, Type::U8),
            (Type::Array(a), Type::Array(b)) | (Type::Pointer(a), Type::Pointer(b)) =>
                Self::is_convertible(a, b),
            (Type::SizedArray(a, n1), Type::SizedArray(b, n2)) =>
                n1 == n2 && Self::is_convertible(a, b),
            (Type::Function(a1, r1), Type::Function(a2, r2)) =>
                a1.len() == a2.len() && a1.iter().zip(a2).all(|(a, b)| Self::is_convertible(a, b)) && Self::is_convertible(r1, r2),
            (Type::Enum(n1), Type::Enum(n2)) => n1 == n2,
            (Type::Optional(inner1), Type::Optional(inner2)) => Self::is_convertible(inner1, inner2),
            (Type::Optional(inner), to) => Self::is_convertible(inner, to),
            (from, Type::Optional(inner)) => {
                if from == &Type::NoneType {
                    true
                } else {
                    Self::is_convertible(from, inner)
                }
            }
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            _ => false,
        }
    }
    fn report_error(&mut self, message: &str, span: Span) {
        self.errors.push(
            Diagnostic::error()
                .with_message(message)
                .with_labels(vec![Label::primary(self.file_id, span)]),
        );
    }
    
    fn check_block(&mut self, stmts: &mut [Stmt]) -> Result<(), Vec<Diagnostic<FileId>>> {
        for stmt in stmts {
            self.check_stmt(stmt)?;
        }
        Ok(())
    }

    fn check_impl_block(&mut self, impl_block: &mut ast::ImplBlock) -> Result<(), Vec<Diagnostic<FileId>>> {
        let target_type = self.parse_type_name(&impl_block.target_type);
        
        for method in &mut impl_block.methods {
            for (param_name, param_type) in &mut method.params {
                if param_name == "self" {
                    *param_type = target_type.clone();
                }
            }
            
            self.context.current_return_type = method.return_type.clone();
            self.check_function(method)?;
        }
        
        Ok(())
    }
    
    fn parse_type_name(&self, type_name: &str) -> Type {
        match type_name {
            "string" => Type::String,
            "i32" => Type::I32,
            "i64" => Type::I64,
            "i8" => Type::I8,
            "i16" => Type::I16,
            "u8" => Type::U8,
            "u16" => Type::U16,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            "bool" => Type::Bool,
            "void" => Type::Void,
            _ => {
                if self.context.struct_defs.contains_key(type_name) {
                    Type::Struct(type_name.to_string())
                } else if self.context.enum_defs.contains_key(type_name) {
                    Type::Enum(type_name.to_string())
                } else {
                    Type::Unknown
                }
            }
        }
    }
}


fn substitute_generics(ty: &Type, subst: &std::collections::HashMap<&String, &Type>) -> Type {
    match ty {
        Type::Generic(name) => subst.get(name).cloned().cloned().unwrap_or(Type::Unknown),
        Type::Pointer(inner) => Type::Pointer(Box::new(substitute_generics(inner, subst))),
        Type::Array(inner) => Type::Array(Box::new(substitute_generics(inner, subst))),
        Type::SizedArray(inner, n) => Type::SizedArray(Box::new(substitute_generics(inner, subst)), *n),
        Type::Function(args, ret) => Type::Function(args.iter().map(|a| substitute_generics(a, subst)).collect(), Box::new(substitute_generics(ret, subst))),
        Type::GenericInstance(name, args) => Type::GenericInstance(name.clone(), args.iter().map(|a| substitute_generics(a, subst)).collect()),
        _ => ty.clone(),
    }
}
