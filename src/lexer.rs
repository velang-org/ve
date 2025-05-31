use codespan::{FileId, Files, Span};
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[token("fn")]
    KwFn,
    #[token("let")]
    KwLet,
    #[token("var")]
    KwVar,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("return")]
    KwReturn,
    #[token("safe")]
    KwSafe,
    #[token("rawptr")]
    KwRawPtr,
    #[token("defer")]
    KwDefer,
    #[token("as")]
    KwAs,
    #[token("while")]
    KwWhile,
    #[token("for")]
    KwFor,
    #[token("import")]
    KwImport,
    #[token("from")]
    KwFrom,
    #[token("export")]
    KwExport,
    #[token("struct")]
    KwStruct,
    #[token("enum")]
    KwEnum,
    #[token("match")]
    KwMatch,
    #[token("true")]
    KwTrue,
    #[token("false")]
    KwFalse,
    #[token(".")]
    Dot,
    #[token("...")]
    Ellipsis,
    #[token("foreign")]
    Foreign,
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    Str(String),
    #[regex(r#"`([^`\\]|\\.)*`"#, |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    TemplateStr(String),

    #[token("bool")]
    TyBool,
    #[token("string")]
    TyString,
    #[token("void")]
    TyVoid,
    #[token("any")]
    TyAny,


    #[token("byte")]
    #[token("u8")]
    TyU8,

    #[token("ushort")]
    #[token("u16")]
    TyU16,

    #[token("uint")]
    #[token("u32")]
    TyU32,

    #[token("ulong")]
    #[token("u64")]
    TyU64,

    #[token("sbyte")]
    #[token("i8")]
    TyI8,

    #[token("short")]
    #[token("i16")]
    TyI16,

    #[token("int")]
    #[token("i32")]
    TyI32,

    #[token("long")]
    #[token("i64")]
    TyI64,

    #[token("float")]
    #[token("f32")]
    TyF32,

    #[token("double")]
    #[token("f64")]
    TyF64,

    #[token("->")]
    Arrow,
    #[token(":")]
    Colon,
    #[token("==")]
    EqEq,
    #[token("!=")]
    NotEq,
    #[token(">=")]
    GtEq,
    #[token("<=")]
    LtEq,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("[]")]
    EmptyArray,
    #[token(",")]
    Comma,
    #[token("=")]
    Eq,
    #[token(";")]
    Semi,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("**")]
    DoubleStar,
    #[token("^")]
    Caret,
    #[token("%")]
    Percent,
    #[token(">")]
    Gt,
    #[token("<")]
    Lt,
    #[token("in")]
    KwIn,
    #[token("..")]
    DotDot,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("|")]
    Pipe,
    #[token("=>")]
    Arrow2,
    #[token("#")]
    Hash,

    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse().ok())]
    F32(f32),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    Int(i64),

    #[regex(r"[ \t\n]+", logos::skip)]
    Whitespace,

    #[regex(r"//[^\n]*", logos::skip)]
    SingleLineComment,

    #[regex(r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/", logos::skip)]
    MultiLineComment,

    Error,
}

pub struct Lexer<'a> {
    pub(crate) files: &'a Files<String>,
    pub(crate) file_id: FileId,
}

impl<'a> Lexer<'a> {
    pub fn new(files: &'a Files<String>, file_id: FileId) -> Self {
        Self { files, file_id }
    }

    pub fn tokens(&self) -> Vec<(Token, Span)> {
        let source = self.files.source(self.file_id);
        Token::lexer(source)
            .spanned()
            .filter_map(|(token, span)| match token {
                Ok(token) if token != Token::Error => Some((token, Span::new(span.start as u32, span.end as u32))),
                _ => None,
            })
            .collect()
    }
}

