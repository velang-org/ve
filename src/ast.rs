use codespan::Span;
use std::collections::{HashMap, HashSet};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum Type {
    I32,
    Bool,
    String,
    Void,
    Function(Vec<Type>, Box<Type>),
    Unknown,
    Pointer(Box<Type>),
    RawPtr,
    Struct(String),
    Enum(String),
    Array(Box<Type>),
    SizedArray(Box<Type>, usize),
    Any,
    F32,
    I8,
    I16,
    I64,
    U8,
    U16,
    U32,
    U64,
    F64,
    CChar,
    CInt,
    CSize,
    Ellipsis,
    Generic(String),
    GenericInstance(String, Vec<Type>),
    Optional(Box<Type>),
    NoneType,
}

impl Type {
    #[allow(dead_code)]
    pub(crate) fn is_pointer(&self) -> bool {
        matches!(self, Type::Pointer(_) | Type::RawPtr)
    }

    #[allow(dead_code)]
    pub(crate) fn is_array(&self) -> bool {
        matches!(self, Type::Array(_))
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub generic_params: Vec<String>,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: Vec<Stmt>,
    #[allow(dead_code)]
    pub span: Span,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct Test {
    pub name: String,
    pub stmts: Vec<Stmt>,
    #[allow(dead_code)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
    #[allow(dead_code)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub generic_params: Vec<String>,
    pub fields: Vec<StructField>,
    #[allow(dead_code)]
    pub span: Span,
    pub visibility: Visibility,
    #[allow(dead_code)]
    pub repr: Option<String>,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub data: Option<Vec<Type>>,
    pub value: Option<i32>, 
    #[allow(dead_code)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: String,
    pub generic_params: Vec<String>,
    pub variants: Vec<EnumVariant>,
    #[allow(dead_code)]
    pub span: Span,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: MatchArmBody,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum MatchArmBody {
    Expr(Expr),
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard(Span),                                  // _
    Variable(String, Span),                          // x
    EnumVariant(String, String, Vec<Pattern>, Span), // Color.Red, Color.Rgb(r, g, b)
    Literal(Expr, Span),                             // 42, "hello"
}

#[derive(Debug, Clone)]
pub struct Program {
    pub imports: Vec<ImportDeclaration>,
    pub stmts: Vec<Stmt>,
    pub functions: Vec<Function>,
    pub structs: Vec<StructDef>,
    pub enums: Vec<EnumDef>,
    pub ffi_functions: Vec<FfiFunction>,
    pub ffi_variables: Vec<FfiVariable>,
    pub tests: Vec<Test>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Private,
    Internal,
    Public,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Stmt {
    Let(String, Option<Type>, Expr, Span, Visibility),
    Var(String, Option<Type>, Span),
    Expr(Expr, Span),
    If(Expr, Vec<Stmt>, Option<Vec<Stmt>>, Span),
    Return(Expr, Span),
    Defer(Expr, Span),
    While(Expr, Vec<Stmt>, Span),
    Loop(Vec<Stmt>, Span),
    For(String, Expr, Vec<Stmt>, Span),
    Break(Option<Expr>, Span),
    Continue(Span),
    Block(Vec<Stmt>, Span),
}

#[derive(Debug, Clone)]
pub struct ExprInfo {
    pub span: Span,
    pub ty: Type,
    pub is_tail: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FfiFunction {
    pub name: String,
    pub params: Vec<Type>,
    pub return_type: Type,
    pub metadata: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FfiVariable {
    pub name: String,
    pub ty: Type,
    pub metadata: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Expr {
    Int(i32, ExprInfo),
    Int64(i64, ExprInfo),
    Bool(bool, ExprInfo),
    Str(String, ExprInfo),
    Var(String, ExprInfo),
    BinOp(Box<Expr>, BinOp, Box<Expr>, ExprInfo),
    UnaryOp(UnOp, Box<Expr>, ExprInfo),
    Call(String, Vec<Expr>, ExprInfo),
    Cast(Box<Expr>, Type, ExprInfo),
    SafeBlock(Vec<Stmt>, ExprInfo),
    Deref(Box<Expr>, ExprInfo),
    Assign(Box<Expr>, Box<Expr>, ExprInfo),
    Range(Box<Expr>, Box<Expr>, ExprInfo),
    StructInit(String, Vec<(String, Expr)>, ExprInfo),
    FieldAccess(Box<Expr>, String, ExprInfo),
    ArrayInit(Vec<Expr>, ExprInfo),
    ArrayAccess(Box<Expr>, Box<Expr>, ExprInfo),
    TemplateStr(Vec<TemplateStrPart>, ExprInfo),
    F32(f32, ExprInfo),
    FfiCall(String, Vec<Expr>, ExprInfo),
    EnumConstruct(String, String, Vec<Expr>, ExprInfo),
    Match(Box<Pattern>, Vec<MatchArm>, ExprInfo),
    If(Box<Expr>, Vec<Stmt>, Option<Vec<Stmt>>, ExprInfo),
    Loop(Vec<Stmt>, ExprInfo),
    Void(ExprInfo),
    None(ExprInfo),
}

#[derive(Debug, Clone)]
pub enum TemplateStrPart {
    Literal(String),
    Expression(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleType {
    Standard,
    Local,
    External,
}

#[derive(Debug, Clone)]
pub enum ImportDeclaration {
    ImportAll {
        module_path: String,
        module_type: ModuleType,
        alias: Option<String>,
    },
    ImportSpecifiers {
        module_path: String,
        module_type: ModuleType,
        specifiers: Vec<ImportSpecifier>,
    },
}

#[derive(Debug, Clone)]
pub struct ImportSpecifier {
    pub name: String,
    pub alias: Option<String>,
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int(_, info) => info.span,
            Expr::Int64(_, info) => info.span,
            Expr::Bool(_, info) => info.span,
            Expr::Str(_, info) => info.span,
            Expr::Var(_, info) => info.span,
            Expr::BinOp(_, _, _, info) => info.span,
            Expr::UnaryOp(_, _, info) => info.span,
            Expr::Call(_, _, info) => info.span,
            Expr::Cast(_, _, info) => info.span,
            Expr::SafeBlock(_, info) => info.span,
            Expr::Deref(_, info) => info.span,
            Expr::Assign(_, _, info) => info.span,
            Expr::Range(_, _, info) => info.span,
            Expr::StructInit(_, _, info) => info.span,
            Expr::FieldAccess(_, _, info) => info.span,
            Expr::ArrayInit(_, info) => info.span,
            Expr::ArrayAccess(_, _, info) => info.span,
            Expr::TemplateStr(_, info) => info.span,
            Expr::F32(_, info) => info.span,
            Expr::FfiCall(_, _, info) => info.span,
            Expr::EnumConstruct(_, _, _, info) => info.span,
            Expr::Match(_, _, info) => info.span,
            Expr::If(_, _, _, info) => info.span,
            Expr::Loop(_, info) => info.span,
            Expr::Void(info) => info.span,
            Expr::None(info) => info.span,
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Expr::Int(_, info) => info.ty.clone(),
            Expr::Int64(_, info) => info.ty.clone(),
            Expr::Bool(_, info) => info.ty.clone(),
            Expr::Str(_, info) => info.ty.clone(),
            Expr::Var(_, info) => info.ty.clone(),
            Expr::BinOp(_, _, _, info) => info.ty.clone(),
            Expr::UnaryOp(_, _, info) => info.ty.clone(),
            Expr::Call(_, _, info) => info.ty.clone(),
            Expr::Cast(_, _, info) => info.ty.clone(),
            Expr::SafeBlock(_, info) => info.ty.clone(),
            Expr::Deref(_, info) => info.ty.clone(),
            Expr::Assign(_, _, info) => info.ty.clone(),
            Expr::Range(_, _, info) => info.ty.clone(),
            Expr::StructInit(_, _, info) => info.ty.clone(),
            Expr::FieldAccess(_, _, info) => info.ty.clone(),
            Expr::ArrayInit(_, info) => info.ty.clone(),
            Expr::ArrayAccess(_, _, info) => info.ty.clone(),
            Expr::TemplateStr(_, info) => info.ty.clone(),
            Expr::F32(_, info) => info.ty.clone(),
            Expr::FfiCall(_, _, info) => info.ty.clone(),
            Expr::EnumConstruct(_, _, _, info) => info.ty.clone(),
            Expr::Match(_, _, info) => info.ty.clone(),
            Expr::If(_, _, _, info) => info.ty.clone(),
            Expr::Loop(_, info) => info.ty.clone(),
            Expr::Void(info) => info.ty.clone(),
            Expr::None(info) => info.ty.clone(),
        }
    }

    pub fn get_info(&self) -> &ExprInfo {
        match self {
            Expr::Int(_, info) => info,
            Expr::Int64(_, info) => info,
            Expr::Bool(_, info) => info,
            Expr::Str(_, info) => info,
            Expr::Var(_, info) => info,
            Expr::BinOp(_, _, _, info) => info,
            Expr::UnaryOp(_, _, info) => info,
            Expr::Call(_, _, info) => info,
            Expr::Cast(_, _, info) => info,
            Expr::SafeBlock(_, info) => info,
            Expr::Deref(_, info) => info,
            Expr::Assign(_, _, info) => info,
            Expr::Range(_, _, info) => info,
            Expr::StructInit(_, _, info) => info,
            Expr::FieldAccess(_, _, info) => info,
            Expr::ArrayInit(_, info) => info,
            Expr::ArrayAccess(_, _, info) => info,
            Expr::TemplateStr(_, info) => info,
            Expr::F32(_, info) => info,
            Expr::FfiCall(_, _, info) => info,
            Expr::EnumConstruct(_, _, _, info) => info,
            Expr::Match(_, _, info) => info,
            Expr::If(_, _, _, info) => info,
            Expr::Loop(_, info) => info,
            Expr::Void(info) => info,
            Expr::None(info) => info,
        }
    }

    #[allow(dead_code)]
    pub fn is_constant(&self) -> bool {
        matches!(
            self,
            Expr::Int(_, _) | Expr::Str(_, _) | Expr::Bool(_, _) | Expr::F32(_, _) | Expr::None(_)
        )
    }

    #[allow(dead_code)]
    pub(crate) fn is_pointer_cast(&self) -> bool {
        if let Expr::Cast(inner, target_ty, _) = self {
            inner.get_type().is_pointer() && *target_ty == Type::I32
        } else {
            false
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Neg,
    Plus,
    Not,
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Pow2,
    Mod,
    Gt,
    Eq,
    Lt,
    NotEq,
    GtEq,
    LtEq,
    And,
    Or,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Pow => "**",
            BinOp::Pow2 => "^",
            BinOp::Mod => "%",
            BinOp::Gt => ">",
            BinOp::Eq => "==",
            BinOp::Lt => "<",
            BinOp::NotEq => "!=",
            BinOp::GtEq => ">=",
            BinOp::LtEq => "<=",
            BinOp::And => "&&",
            BinOp::Or => "||",
        })
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "string"),
            Type::Void => write!(f, "void"),
            Type::Generic(name) => write!(f, "{}", name),
            Type::GenericInstance(name, args) => {
                write!(f, "{}<", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", arg)?;
                }
                write!(f, ">")
            }
            Type::Function(args, ret) => {
                write!(f, "fn(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ") -> {}", ret)
            }
            Type::Unknown => write!(f, "<?>"),
            Type::Pointer(ty) => write!(f, "*{}", ty),
            Type::RawPtr => write!(f, "*void"),
            Type::Struct(name) => write!(f, "{}", name),
            Type::Enum(name) => write!(f, "{}", name),
            Type::Array(ty) => write!(f, "[]{}", ty),
            Type::SizedArray(ty, size) => write!(f, "[{}; {}]", ty, size),
            Type::Any => write!(f, "any"),
            Type::F32 => write!(f, "f32"),
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I64 => write!(f, "i64"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::F64 => write!(f, "f64"),
            Type::CChar => write!(f, "cchar"),
            Type::CInt => write!(f, "int"),
            Type::CSize => write!(f, "size_t"),
            Type::Ellipsis => write!(f, "..."),
            Type::Optional(ty) => write!(f, "{}?", ty),
            Type::NoneType => write!(f, "None"),
        }
    }
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Wildcard(span) => *span,
            Pattern::Variable(_, span) => *span,
            Pattern::EnumVariant(_, _, _, span) => *span,
            Pattern::Literal(_, span) => *span,
        }
    }
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Let(_, _, _, span, _) => *span,
            Stmt::Var(_, _, span) => *span,
            Stmt::Expr(_, span) => *span,
            Stmt::If(_, _, _, span) => *span,
            Stmt::Return(_, span) => *span,
            Stmt::Defer(_, span) => *span,
            Stmt::While(_, _, span) => *span,
            Stmt::Loop(_, span) => *span,
            Stmt::For(_, _, _, span) => *span,
            Stmt::Break(_, span) => *span,
            Stmt::Continue(span) => *span,
            Stmt::Block(_, span) => *span,
        }
    }
}


pub trait AstVisitor {
    fn visit_program(&mut self, program: &Program) {
        for import in &program.imports {
            self.visit_import(import);
        }
        for stmt in &program.stmts {
            self.visit_stmt(stmt);
        }
        for func in &program.functions {
            self.visit_function(func);
        }
        for struct_def in &program.structs {
            self.visit_struct_def(struct_def);
        }
        for enum_def in &program.enums {
            self.visit_enum_def(enum_def);
        }
        for ffi_func in &program.ffi_functions {
            self.visit_ffi_function(ffi_func);
        }
        for ffi_var in &program.ffi_variables {
            self.visit_ffi_variable(ffi_var);
        }
        for test in &program.tests {
            self.visit_test(test);
        }
    }

    fn visit_function(&mut self, function: &Function) {
        for param in &function.params {
            self.visit_param(&param.0, &param.1);
        }
        self.visit_type(&function.return_type);
        for stmt in &function.body {
            self.visit_stmt(stmt);
        }
    }

    fn visit_struct_def(&mut self, struct_def: &StructDef) {
        for field in &struct_def.fields {
            self.visit_field(field);
        }
    }

    fn visit_field(&mut self, field: &StructField) {
        self.visit_type(&field.ty);
    }

    fn visit_enum_def(&mut self, enum_def: &EnumDef) {
        for variant in &enum_def.variants {
            self.visit_enum_variant(variant);
        }
    }

    fn visit_enum_variant(&mut self, variant: &EnumVariant) {
        if let Some(types) = &variant.data {
            for ty in types {
                self.visit_type(ty);
            }
        }
    }

    fn visit_test(&mut self, test: &Test) {
        for stmt in &test.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_import(&mut self, import: &ImportDeclaration) {
        match import {
            ImportDeclaration::ImportAll { .. } => {}
            ImportDeclaration::ImportSpecifiers { specifiers, .. } => {
                for specifier in specifiers {
                    self.visit_import_specifier(specifier);
                }
            }
        }
    }

    fn visit_import_specifier(&mut self, _specifier: &ImportSpecifier) {
    }

    fn visit_ffi_function(&mut self, ffi_func: &FfiFunction) {
        for ty in &ffi_func.params {
            self.visit_type(ty);
        }
        self.visit_type(&ffi_func.return_type);
    }

    fn visit_ffi_variable(&mut self, ffi_var: &FfiVariable) {
        self.visit_type(&ffi_var.ty);
    }

    fn visit_param(&mut self, _name: &str, ty: &Type) {
        self.visit_type(ty);
    }

    fn visit_type(&mut self, ty: &Type) {
        match ty {
            Type::Function(args, ret) => {
                for arg in args {
                    self.visit_type(arg);
                }
                self.visit_type(ret);
            }
            Type::Pointer(inner) | Type::Array(inner) | Type::SizedArray(inner, _) | Type::Optional(inner) => {
                self.visit_type(inner);
            }
            Type::GenericInstance(_, args) => {
                for arg in args {
                    self.visit_type(arg);
                }
            }
            _ => {}
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(_, ty, expr, _, _) => {
                if let Some(ty) = ty {
                    self.visit_type(ty);
                }
                self.visit_expr(expr);
            }
            Stmt::Var(_, ty, _) => {
                if let Some(ty) = ty {
                    self.visit_type(ty);
                }
            }
            Stmt::Expr(expr, _) => {
                self.visit_expr(expr);
            }
            Stmt::If(cond, then_branch, else_branch, _) => {
                self.visit_expr(cond);
                for stmt in then_branch {
                    self.visit_stmt(stmt);
                }
                if let Some(else_branch) = else_branch {
                    for stmt in else_branch {
                        self.visit_stmt(stmt);
                    }
                }
            }
            Stmt::Return(expr, _) => {
                self.visit_expr(expr);
            }
            Stmt::Defer(expr, _) => {
                self.visit_expr(expr);
            }
            Stmt::While(cond, body, _) => {
                self.visit_expr(cond);
                for stmt in body {
                    self.visit_stmt(stmt);
                }
            }
            Stmt::Loop(body, _) => {
                for stmt in body {
                    self.visit_stmt(stmt);
                }
            }
            Stmt::For(_, iter, body, _) => {
                self.visit_expr(iter);
                for stmt in body {
                    self.visit_stmt(stmt);
                }
            }
            Stmt::Break(expr, _) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr);
                }
            }
            Stmt::Continue(_) => {}
            Stmt::Block(stmts, _) => {
                for stmt in stmts {
                    self.visit_stmt(stmt);
                }
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Int(_, _)
            | Expr::Int64(_, _)
            | Expr::Bool(_, _)
            | Expr::Str(_, _)
            | Expr::Var(_, _)
            | Expr::F32(_, _)
            | Expr::Void(_)
            | Expr::None(_) => {}

            Expr::BinOp(left, _, right, _) => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            Expr::UnaryOp(_, expr, _) => {
                self.visit_expr(expr);
            }
            Expr::Call(_, args, _) => {
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Expr::Cast(expr, ty, _) => {
                self.visit_expr(expr);
                self.visit_type(ty);
            }
            Expr::SafeBlock(stmts, _) => {
                for stmt in stmts {
                    self.visit_stmt(stmt);
                }
            }
            Expr::Deref(expr, _) => {
                self.visit_expr(expr);
            }
            Expr::Assign(left, right, _) => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            Expr::Range(start, end, _) => {
                self.visit_expr(start);
                self.visit_expr(end);
            }
            Expr::StructInit(_, fields, _) => {
                for (_, expr) in fields {
                    self.visit_expr(expr);
                }
            }
            Expr::FieldAccess(expr, _, _) => {
                self.visit_expr(expr);
            }
            Expr::ArrayInit(elements, _) => {
                for element in elements {
                    self.visit_expr(element);
                }
            }
            Expr::ArrayAccess(array, index, _) => {
                self.visit_expr(array);
                self.visit_expr(index);
            }
            Expr::TemplateStr(parts, _) => {
                for part in parts {
                    match part {
                        TemplateStrPart::Literal(_) => {}
                        TemplateStrPart::Expression(expr) => {
                            self.visit_expr(expr);
                        }
                    }
                }
            }
            Expr::FfiCall(_, args, _) => {
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Expr::EnumConstruct(_, _, args, _) => {
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Expr::Match(pattern, arms, _) => {
                self.visit_pattern(pattern);
                for arm in arms {
                    self.visit_match_arm(arm);
                }
            }
            Expr::If(condition, then_branch, else_branch, _) => {
                self.visit_expr(condition);
                for stmt in then_branch {
                    self.visit_stmt(stmt);
                }
                if let Some(else_stmts) = else_branch {
                    for stmt in else_stmts {
                        self.visit_stmt(stmt);
                    }
                }
            }
            Expr::Loop(body, _) => {
                for stmt in body {
                    self.visit_stmt(stmt);
                }
            }
        }
    }

    fn visit_match_arm(&mut self, arm: &MatchArm) {
        self.visit_pattern(&arm.pattern);
        match &arm.body {
            MatchArmBody::Expr(expr) => {
                self.visit_expr(expr);
            }
            MatchArmBody::Block(stmts) => {
                for stmt in stmts {
                    self.visit_stmt(stmt);
                }
            }
        }
    }

    fn visit_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard(_) | Pattern::Variable(_, _) => {}
            Pattern::EnumVariant(_, _, patterns, _) => {
                for pattern in patterns {
                    self.visit_pattern(pattern);
                }
            }
            Pattern::Literal(expr, _) => {
                self.visit_expr(expr);
            }
        }
    }
}

pub struct GenericCallCollector {
    pub generic_calls: HashSet<(String, Vec<Type>)>,
    pub generic_functions: HashMap<String, Vec<String>>,
    pub generic_instances: HashSet<Type>,
}

impl GenericCallCollector {
    pub fn new() -> Self {
        Self {
            generic_calls: HashSet::new(),
            generic_functions: HashMap::new(),
            generic_instances: HashSet::new(),
        }
    }
    
    pub fn with_functions(functions: &[Function]) -> Self {
        let mut collector = Self::new();
        for func in functions {
            if !func.generic_params.is_empty() {
                collector.generic_functions.insert(func.name.clone(), func.generic_params.clone());
            }
        }
        collector
    }
}

impl AstVisitor for GenericCallCollector {
    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Call(func_name, args, info) => {
                if self.generic_functions.contains_key(func_name) {
                    let arg_types: Vec<Type> = args.iter().map(|arg| arg.get_type()).collect();
                    self.generic_calls.insert((func_name.clone(), arg_types));
                }
                
                if let Type::GenericInstance(_, type_args) = &info.ty {
                    self.generic_calls.insert((func_name.clone(), type_args.clone()));
                }
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Expr::EnumConstruct(_, _, _, info) => {
                if let Type::GenericInstance(_, _) = &info.ty {
                    self.generic_instances.insert(info.ty.clone());
                }
            }
            _ => {
                match &expr.get_type() {
                    Type::GenericInstance(_, _) => {
                        self.generic_instances.insert(expr.get_type());
                    }
                    _ => {}
                }
            }
        }
        
        match expr {
            Expr::Int(_, _)
            | Expr::Int64(_, _)
            | Expr::Bool(_, _)
            | Expr::Str(_, _)
            | Expr::Var(_, _)
            | Expr::F32(_, _)
            | Expr::Void(_)
            | Expr::None(_) => {}
            Expr::BinOp(left, _, right, _) => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            Expr::UnaryOp(_, expr, _) => {
                self.visit_expr(expr);
            }
            Expr::Call(_, args, _) => {
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Expr::Cast(expr, ty, _) => {
                self.visit_expr(expr);
                self.visit_type(ty);
            }
            Expr::SafeBlock(stmts, _) => {
                for stmt in stmts {
                    self.visit_stmt(stmt);
                }
            }
            Expr::Deref(expr, _) => {
                self.visit_expr(expr);
            }
            Expr::Assign(left, right, _) => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            Expr::Range(start, end, _) => {
                self.visit_expr(start);
                self.visit_expr(end);
            }
            Expr::StructInit(_, fields, _) => {
                for (_, expr) in fields {
                    self.visit_expr(expr);
                }
            }
            Expr::FieldAccess(expr, _, _) => {
                self.visit_expr(expr);
            }
            Expr::ArrayInit(elements, _) => {
                for element in elements {
                    self.visit_expr(element);
                }
            }
            Expr::ArrayAccess(array, index, _) => {
                self.visit_expr(array);
                self.visit_expr(index);
            }
            Expr::TemplateStr(parts, _) => {
                for part in parts {
                    match part {
                        TemplateStrPart::Literal(_) => {}
                        TemplateStrPart::Expression(expr) => {
                            self.visit_expr(expr);
                        }
                    }
                }
            }
            Expr::FfiCall(_, args, _) => {
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Expr::EnumConstruct(_, _, args, _) => {
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Expr::Match(pattern, arms, _) => {
                self.visit_pattern(pattern);
                for arm in arms {
                    self.visit_match_arm(arm);
                }
            }
            Expr::If(condition, then_branch, else_branch, _) => {
                self.visit_expr(condition);
                for stmt in then_branch {
                    self.visit_stmt(stmt);
                }
                if let Some(else_stmts) = else_branch {
                    for stmt in else_stmts {
                        self.visit_stmt(stmt);
                    }
                }
            }
            Expr::Loop(body, _) => {
                for stmt in body {
                    self.visit_stmt(stmt);
                }
            }
        }
    }
}


pub struct GenericCallTransformer {
    call_mappings: HashMap<(String, Vec<Type>), String>,
}

impl GenericCallTransformer {
    pub fn new() -> Self {
        Self {
            call_mappings: HashMap::new(),
        }
    }
    
    pub fn add_mapping(&mut self, original_name: String, type_args: Vec<Type>, new_name: String) {
        self.call_mappings.insert((original_name, type_args), new_name);
    }

}

impl AstTransformer for GenericCallTransformer {
    fn transform_call_name(&mut self, name: String, args: &[Expr], _info: &ExprInfo) -> String {
        let arg_types: Vec<Type> = args.iter().map(|arg| arg.get_type()).collect();
        
        if let Some(new_name) = self.call_mappings.get(&(name.clone(), arg_types)) {
            new_name.clone()
        } else {
            name
        }
    }
}


pub trait AstTransformer {
    fn transform_program(&mut self, program: Program) -> Program {
        Program {
            imports: program.imports.into_iter().map(|i| self.transform_import(i)).collect(),
            stmts: program.stmts.into_iter().map(|s| self.transform_stmt(s)).collect(),
            functions: program.functions.into_iter().map(|f| self.transform_function(f)).collect(),
            structs: program.structs.into_iter().map(|s| self.transform_struct_def(s)).collect(),
            enums: program.enums.into_iter().map(|e| self.transform_enum_def(e)).collect(),
            ffi_functions: program.ffi_functions,
            ffi_variables: program.ffi_variables,
            tests: program.tests.into_iter().map(|t| self.transform_test(t)).collect(),
        }
    }

    fn transform_function(&mut self, function: Function) -> Function {
        Function {
            name: function.name,
            generic_params: function.generic_params,
            params: function.params,
            return_type: function.return_type,
            body: function.body.into_iter().map(|s| self.transform_stmt(s)).collect(),
            span: function.span,
            visibility: function.visibility,
        }
    }

    fn transform_struct_def(&mut self, struct_def: StructDef) -> StructDef {
        struct_def
    }

    fn transform_enum_def(&mut self, enum_def: EnumDef) -> EnumDef {
        enum_def
    }

    fn transform_test(&mut self, test: Test) -> Test {
        Test {
            name: test.name,
            stmts: test.stmts.into_iter().map(|s| self.transform_stmt(s)).collect(),
            span: test.span,
        }
    }

    fn transform_import(&mut self, import: ImportDeclaration) -> ImportDeclaration {
        import
    }

    fn transform_stmt(&mut self, stmt: Stmt) -> Stmt {
        match stmt {
            Stmt::Let(name, ty, expr, span, vis) => {
                Stmt::Let(name, ty, self.transform_expr(expr), span, vis)
            }
            Stmt::Var(name, ty, span) => Stmt::Var(name, ty, span),
            Stmt::Expr(expr, span) => Stmt::Expr(self.transform_expr(expr), span),
            Stmt::If(cond, then_branch, else_branch, span) => {
                Stmt::If(
                    self.transform_expr(cond),
                    then_branch.into_iter().map(|s| self.transform_stmt(s)).collect(),
                    else_branch.map(|e| e.into_iter().map(|s| self.transform_stmt(s)).collect()),
                    span,
                )
            }
            Stmt::Return(expr, span) => Stmt::Return(self.transform_expr(expr), span),
            Stmt::Defer(expr, span) => Stmt::Defer(self.transform_expr(expr), span),
            Stmt::While(cond, body, span) => {
                Stmt::While(
                    self.transform_expr(cond),
                    body.into_iter().map(|s| self.transform_stmt(s)).collect(),
                    span,
                )
            }
            Stmt::Loop(body, span) => {
                Stmt::Loop(
                    body.into_iter().map(|s| self.transform_stmt(s)).collect(),
                    span,
                )
            }
            Stmt::For(var, iter, body, span) => {
                Stmt::For(
                    var,
                    self.transform_expr(iter),
                    body.into_iter().map(|s| self.transform_stmt(s)).collect(),
                    span,
                )
            }
            Stmt::Break(expr, span) => Stmt::Break(
                expr.map(|e| self.transform_expr(e)),
                span,
            ),
            Stmt::Continue(span) => Stmt::Continue(span),
            Stmt::Block(stmts, span) => {
                Stmt::Block(stmts.into_iter().map(|s| self.transform_stmt(s)).collect(), span)
            }
        }
    }

    fn transform_expr(&mut self, expr: Expr) -> Expr {
        match expr {
            Expr::Int(n, info) => Expr::Int(n, info),
            Expr::Int64(n, info) => Expr::Int64(n, info),
            Expr::Bool(b, info) => Expr::Bool(b, info),
            Expr::Str(s, info) => Expr::Str(s, info),
            Expr::Var(name, info) => Expr::Var(name, info),
            Expr::F32(f, info) => Expr::F32(f, info),
            Expr::Void(info) => Expr::Void(info),
            Expr::None(info) => Expr::None(info),
            
            Expr::BinOp(left, op, right, info) => {
                Expr::BinOp(
                    Box::new(self.transform_expr(*left)),
                    op,
                    Box::new(self.transform_expr(*right)),
                    info,
                )
            }
            Expr::UnaryOp(op, expr, info) => {
                Expr::UnaryOp(op, Box::new(self.transform_expr(*expr)), info)
            }
            Expr::Call(name, args, info) => {
                Expr::Call(
                    self.transform_call_name(name, &args, &info),
                    args.into_iter().map(|a| self.transform_expr(a)).collect(),
                    info,
                )
            }
            Expr::Cast(expr, ty, info) => {
                Expr::Cast(Box::new(self.transform_expr(*expr)), ty, info)
            }
            Expr::SafeBlock(stmts, info) => {
                Expr::SafeBlock(stmts.into_iter().map(|s| self.transform_stmt(s)).collect(), info)
            }
            Expr::Deref(expr, info) => {
                Expr::Deref(Box::new(self.transform_expr(*expr)), info)
            }
            Expr::Assign(left, right, info) => {
                Expr::Assign(
                    Box::new(self.transform_expr(*left)),
                    Box::new(self.transform_expr(*right)),
                    info,
                )
            }
            Expr::Range(start, end, info) => {
                Expr::Range(
                    Box::new(self.transform_expr(*start)),
                    Box::new(self.transform_expr(*end)),
                    info,
                )
            }
            Expr::StructInit(name, fields, info) => {
                Expr::StructInit(
                    name,
                    fields.into_iter().map(|(n, e)| (n, self.transform_expr(e))).collect(),
                    info,
                )
            }
            Expr::FieldAccess(expr, field, info) => {
                Expr::FieldAccess(Box::new(self.transform_expr(*expr)), field, info)
            }
            Expr::ArrayInit(elements, info) => {
                Expr::ArrayInit(elements.into_iter().map(|e| self.transform_expr(e)).collect(), info)
            }
            Expr::ArrayAccess(array, index, info) => {
                Expr::ArrayAccess(
                    Box::new(self.transform_expr(*array)),
                    Box::new(self.transform_expr(*index)),
                    info,
                )
            }
            Expr::TemplateStr(parts, info) => {
                Expr::TemplateStr(
                    parts.into_iter().map(|p| match p {
                        TemplateStrPart::Literal(s) => TemplateStrPart::Literal(s),
                        TemplateStrPart::Expression(e) => TemplateStrPart::Expression(Box::new(self.transform_expr(*e))),
                    }).collect(),
                    info,
                )
            }
            Expr::FfiCall(name, args, info) => {
                Expr::FfiCall(name, args.into_iter().map(|a| self.transform_expr(a)).collect(), info)
            }
            Expr::EnumConstruct(enum_name, variant, args, info) => {
                Expr::EnumConstruct(
                    enum_name,
                    variant,
                    args.into_iter().map(|a| self.transform_expr(a)).collect(),
                    info,
                )
            }
            Expr::Match(pattern, arms, info) => {
                Expr::Match(
                    pattern,
                    arms.into_iter().map(|arm| self.transform_match_arm(arm)).collect(),
                    info,
                )
            }
            Expr::If(condition, then_branch, else_branch, info) => {
                Expr::If(
                    Box::new(self.transform_expr(*condition)),
                    then_branch.into_iter().map(|s| self.transform_stmt(s)).collect(),
                    else_branch.map(|stmts| stmts.into_iter().map(|s| self.transform_stmt(s)).collect()),
                    info,
                )
            }
            Expr::Loop(body, info) => {
                Expr::Loop(
                    body.into_iter().map(|s| self.transform_stmt(s)).collect(),
                    info,
                )
            }
        }
    }

    fn transform_match_arm(&mut self, arm: MatchArm) -> MatchArm {
        MatchArm {
            pattern: arm.pattern,
            body: match arm.body {
                MatchArmBody::Expr(expr) => MatchArmBody::Expr(self.transform_expr(expr)),
                MatchArmBody::Block(stmts) => MatchArmBody::Block(stmts.into_iter().map(|s| self.transform_stmt(s)).collect()),
            },
            span: arm.span,
        }
    }

    fn transform_call_name(&mut self, name: String, _args: &[Expr], _info: &ExprInfo) -> String {
        name
    }
}