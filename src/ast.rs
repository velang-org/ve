use codespan::Span;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
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
    For(String, Expr, Vec<Stmt>, Span),
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
    Void(ExprInfo),
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
            Expr::Void(info) => info.span,
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
            Expr::Void(info) => info.ty.clone(),
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
            Expr::Void(info) => info,
        }
    }

    #[allow(dead_code)]
    pub fn is_constant(&self) -> bool {
        matches!(
            self,
            Expr::Int(_, _) | Expr::Str(_, _) | Expr::Bool(_, _) | Expr::F32(_, _)
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
            Stmt::For(_, _, _, span) => *span,
            Stmt::Block(_, span) => *span,
        }
    }
}
