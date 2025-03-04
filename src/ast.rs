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
}

impl Type {
    pub(crate) fn is_pointer(&self) -> bool {
        matches!(self, Type::Pointer(_))
    }
}


#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>,
    pub functions: Vec<Function>,
}


#[derive(Debug)]
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

#[derive(Debug)]
pub enum Expr {
    Int(i64, Span, Type),
    Bool(bool, Span, Type),
    Str(String, Span, Type),
    BinOp(Box<Expr>, BinOp, Box<Expr>, Span, Type),
    Var(String, Span, Type),
    Call(String, Vec<Expr>, Span, Type),
    SafeBlock(Vec<Stmt>, Span, Type),
    IntrinsicCall(String, Vec<Expr>, Span, Type),
    Cast(Box<Expr>, Type, Span, Type),
    Deref(Box<Expr>, Span, Type),
    Assign(Box<Expr>, Box<Expr>, Span, Type),
    Print(Box<Expr>, Span, Type),
    Range(Box<Expr>, Box<Expr>, Span, Type),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int(_, span, _) => *span,
            Expr::Bool(_, span, _) => *span,
            Expr::Str(_, span, _) => *span,
            Expr::BinOp(_, _, _, span, _) => *span,
            Expr::Var(_, span, _) => *span,
            Expr::Call(_, _, span, _) => *span,
            Expr::SafeBlock(_, span, _) => *span,
            Expr::IntrinsicCall(_, _, span, _) => *span,
            Expr::Cast(_, _, span, _) => *span,
            Expr::Deref(_, span, _) => *span,
            Expr::Assign(_, _, span, _) => *span,
            Expr::Print(_, span, _) => *span,
            Expr::Range(_, _, span, _) => *span,
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Expr::Int(_, _, ty) => ty.clone(),
            Expr::Bool(_, _, ty) => ty.clone(),
            Expr::Str(_, _, ty) => ty.clone(),
            Expr::BinOp(_, _, _, _, ty) => ty.clone(),
            Expr::Var(_, _, ty) => ty.clone(),
            Expr::Call(_, _, _, ty) => ty.clone(),
            Expr::SafeBlock(_, _, ty) => ty.clone(),
            Expr::IntrinsicCall(_, _, _, ty) => ty.clone(),
            Expr::Cast(_, target_ty, _, _) => target_ty.clone(),
            Expr::Deref(_, _, ty) => ty.clone(),
            Expr::Assign(_, _, _, ty) => ty.clone(),
            Expr::Print(_, _, ty) => ty.clone(),
            Expr::Range(_, _, _, ty) => ty.clone(),
        }
    }

    pub(crate) fn is_pointer_cast(&self) -> bool {
        if let Expr::Cast(inner, target_ty, _, _) = self {
            inner.get_type().is_pointer() && *target_ty == Type::I32
        } else {
            false
        }
    }
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Eq,
    Lt,
    And,
    Or,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Gt => write!(f, ">"),
            BinOp::Eq => write!(f, "=="),
            BinOp::Lt => write!(f, "<"),
            BinOp::And => write!(f, "&&"),
            BinOp::Or => write!(f, "||"),
        }
    }
}


impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "string"),
            Type::Void => write!(f, "void"),
            Type::Function(params, ret) => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", ret)
            }
            Type::Unknown => write!(f, "<?>"),
            Type::Arena => write!(f, "arena"),
            Type::Pointer(ty) => write!(f, "*{}", ty),
            Type::RawPtr => write!(f, "rawptr"),
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