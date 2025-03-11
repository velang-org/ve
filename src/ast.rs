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
    pub exported: bool,
}

#[derive(Debug)]
pub struct Import {
    pub path: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct Program {
    pub imports: Vec<Import>,
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
pub struct ExprInfo {
    pub span: Span,
    pub ty: Type,
}

#[derive(Debug)]
pub enum Expr {
    Int(i64, ExprInfo),
    Bool(bool, ExprInfo),
    Str(String, ExprInfo),
    BinOp(Box<Expr>, BinOp, Box<Expr>, ExprInfo),
    Var(String, ExprInfo),
    Call(String, Vec<Expr>, ExprInfo),
    SafeBlock(Vec<Stmt>, ExprInfo),
    IntrinsicCall(String, Vec<Expr>, ExprInfo),
    Cast(Box<Expr>, Type, ExprInfo),
    Deref(Box<Expr>, ExprInfo),
    Assign(Box<Expr>, Box<Expr>, ExprInfo),
    Print(Box<Expr>, ExprInfo),
    Range(Box<Expr>, Box<Expr>, ExprInfo),
    UnaryOp(UnOp, Box<Expr>, ExprInfo),
}

impl Expr {
    pub fn span(&self) -> Span {
        self.info().span
    }

    pub fn get_type(&self) -> Type {
        self.info().ty.clone()
    }

    fn info(&self) -> &ExprInfo {
        match self {
            Expr::Int(_, info) => info,
            Expr::Bool(_, info) => info,
            Expr::Str(_, info) => info,
            Expr::BinOp(_, _, _, info) => info,
            Expr::Var(_, info) => info,
            Expr::Call(_, _, info) => info,
            Expr::SafeBlock(_, info) => info,
            Expr::IntrinsicCall(_, _, info) => info,
            Expr::Cast(_, _, info) => info,
            Expr::Deref(_, info) => info,
            Expr::Assign(_, _, info) => info,
            Expr::Print(_, info) => info,
            Expr::Range(_, _, info) => info,
            Expr::UnaryOp(_, _, info) => info,
        }
    }

    pub(crate) fn is_pointer_cast(&self) -> bool {
        if let Expr::Cast(inner, target_ty, _) = self {
            inner.get_type().is_pointer() && *target_ty == Type::I32
        } else {
            false
        }
    }
}

#[derive(Debug)]
pub enum UnOp {
    Neg,
    Plus,
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
        write!(
            f,
            "{}",
            match self {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::Gt => ">",
                BinOp::Eq => "==",
                BinOp::Lt => "<",
                BinOp::And => "&&",
                BinOp::Or => "||",
            }
        )
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
                let params_str = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "fn({}) -> {}", params_str, ret)
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