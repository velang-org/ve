use std::fmt;
use codespan::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I32,
    Bool,
    String,
    Void,
    Function(Vec<Type>, Box<Type>),
    Unknown,
    Arena,
    Pointer(Box<Type>),
    RawPtr,
    Struct(String),
    Array(Box<Type>),
    SizedArray(Box<Type>, usize),
    Any,
    F32,
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
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: Vec<Stmt>,
    #[allow(dead_code)]
    pub span: Span,
    pub exported: bool,
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
    pub fields: Vec<StructField>,
    #[allow(dead_code)]
    pub span: Span,
}

#[derive(Debug)]
pub struct Program {
    pub imports: Vec<ImportDeclaration>,
    pub stmts: Vec<Stmt>,
    pub functions: Vec<Function>,
    pub structs: Vec<StructDef>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let(String, Option<Type>, Expr, Span),
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
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i32, ExprInfo),
    Bool(bool, ExprInfo),
    Str(String, ExprInfo),
    Var(String, ExprInfo),
    BinOp(Box<Expr>, BinOp, Box<Expr>, ExprInfo),
    UnaryOp(UnOp, Box<Expr>, ExprInfo),
    Call(String, Vec<Expr>, ExprInfo),
    IntrinsicCall(String, Vec<Expr>, ExprInfo),
    Print(Box<Expr>, ExprInfo),
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
}

#[derive(Debug, Clone)]
pub enum TemplateStrPart {
    Literal(String),
    Expression(Box<Expr>),
}

#[derive(Debug)]
pub enum ImportDeclaration {
    ImportAll { module_path: String, alias: Option<String>},
    ImportSpecifiers { module_path: String, specifiers: Vec<ImportSpecifier> },
}

#[derive(Debug)]
pub struct ImportSpecifier {
    pub name: String,
    pub alias: Option<String>
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int(_, info) => info.span,
            Expr::Bool(_, info) => info.span,
            Expr::Str(_, info) => info.span,
            Expr::Var(_, info) => info.span,
            Expr::BinOp(_, _, _, info) => info.span,
            Expr::UnaryOp(_, _, info) => info.span,
            Expr::Call(_, _, info) => info.span,
            Expr::IntrinsicCall(_, _, info) => info.span,
            Expr::Print(_, info) => info.span,
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
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Expr::Int(_, info) => info.ty.clone(),
            Expr::Bool(_, info) => info.ty.clone(),
            Expr::Str(_, info) => info.ty.clone(),
            Expr::Var(_, info) => info.ty.clone(),
            Expr::BinOp(_, _, _, info) => info.ty.clone(),
            Expr::UnaryOp(_, _, info) => info.ty.clone(),
            Expr::Call(_, _, info) => info.ty.clone(),
            Expr::IntrinsicCall(_, _, info) => info.ty.clone(),
            Expr::Print(_, info) => info.ty.clone(),
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
        }
    }

    #[allow(dead_code)]
    pub fn is_constant(&self) -> bool {
        matches!(self, Expr::Int(_, _) | Expr::Str(_, _) | Expr::Bool(_, _) | Expr::F32(_, _))
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
        write!(
            f,
            "{}",
            match self {
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
            }
        )
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "string"),
            Type::Void => write!(f, "void"),
            Type::Function(args, ret) => {
                write!(f, "fn(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ") -> {}", ret)
            },
            Type::Unknown => write!(f, "<?>"),
            Type::Arena => write!(f, "arena"),
            Type::Pointer(ty) => write!(f, "*{}", ty),
            Type::RawPtr => write!(f, "*void"),
            Type::Struct(name) => write!(f, "{}", name),
            Type::Array(ty) => write!(f, "[]{}", ty),
            Type::SizedArray(ty, size) => write!(f, "[{}; {}]", ty, size),
            Type::Any => write!(f, "any"),
            Type::F32 => write!(f, "f32"),
        }
    }
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Let(_, _, _, span) => *span,
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

