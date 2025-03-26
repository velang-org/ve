use super::{ast, lexer::{Lexer, Token}};
use codespan::{FileId, Files, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};

type Precedence = u8;

pub struct Parser<'a> {
    tokens: Vec<(Token, Span)>,
    current: usize,
    #[allow(dead_code)]
    files: &'a Files<String>,
    file_id: FileId,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            tokens: lexer.tokens(),
            current: 0,
            files: lexer.files,
            file_id: lexer.file_id,
        }
    }

    pub fn parse(&mut self) -> Result<ast::Program, Diagnostic<FileId>> {
        let mut program = ast::Program {
            imports: Vec::new(),
            stmts: Vec::new(),
            functions: Vec::new(),
            structs: Vec::new(),
        };

        while !self.is_at_end() {
            if self.check(Token::KwImport) {
                let import = self.parse_import()?;
                program.imports.push(import);
            } else if self.check(Token::KwExport) {
                self.advance(); 
                let mut func = self.parse_function()?;
                func.exported = true;
                program.functions.push(func);
            } else if self.check(Token::KwFn) {
                program.functions.push(self.parse_function()?);
            } else if self.check(Token::KwStruct) {
                program.structs.push(self.parse_struct()?);
            } else {
                program.stmts.push(self.parse_stmt()?);
            }
        }

        Ok(program)
    }
    
    fn parse_import(&mut self) -> Result<ast::ImportDeclaration, Diagnostic<FileId>> {
        self.consume(Token::KwImport, "Expected 'import'")?;
        let import_decl = match self.peek_token() {
            Token::Str(_) => self.parse_import_all()?,
            Token::LBrace => self.parse_import_specifiers()?,
            Token::Ident(_) => self.parse_import_specifier()?,
            _ => return self.error("Invalid import statement", self.peek_span()),
        };

        self.expect(Token::Semi)?;
        Ok(import_decl)
    }

    fn parse_import_all(&mut self) -> Result<ast::ImportDeclaration, Diagnostic<FileId>> {
        let module_path = match self.advance().cloned() {
            Some((Token::Str(path), _)) => path.clone(),
            _ => return self.error("Expected module path", self.peek_span()),
        };

        let alias = if self.check(Token::KwAs) {
            self.advance();
            match self.consume_ident()? {
                (name, _) => Some(name),
            }
        } else {
            None
        };

        Ok(ast::ImportDeclaration::ImportAll { module_path, alias })
    }

    fn parse_import_specifiers(&mut self) -> Result<ast::ImportDeclaration, Diagnostic<FileId>> {
        self.expect(Token::LBrace)?;
        let mut specifiers = Vec::new();

        loop {
            let (name, alias) = self.parse_import_specifier_item()?;
            specifiers.push(ast::ImportSpecifier { name, alias });

            if !self.check(Token::Comma) {
                break;
            }
            self.advance();
        }

        self.expect(Token::RBrace)?;
        self.expect(Token::KwFrom)?;
        let module_path = match self.advance().cloned() {
            Some((Token::Str(path), _)) => path.clone(),
            _ => return self.error("Expected module path", self.peek_span()),
        };

        Ok(ast::ImportDeclaration::ImportSpecifiers { module_path, specifiers })
    }

    fn parse_import_specifier(&mut self) -> Result<ast::ImportDeclaration, Diagnostic<FileId>> {
        let (name, alias) = self.parse_import_specifier_item()?;
        self.expect(Token::KwFrom)?;
        let module_path = match self.advance().cloned() {
            Some((Token::Str(path), _)) => path.clone(),
            _ => return self.error("Expected module path", self.peek_span()),
        };

        Ok(ast::ImportDeclaration::ImportSpecifiers {
            module_path,
            specifiers: vec![ast::ImportSpecifier { name, alias }],
        })
    }

    fn parse_import_specifier_item(&mut self) -> Result<(String, Option<String>), Diagnostic<FileId>> {
        let name = match self.consume_ident()? {
            (name, _) => name,
        };

        let alias = if self.check(Token::KwAs) {
            self.advance();
            match self.consume_ident()? {
                (alias, _) => Some(alias),
            }
        } else {
            None
        };

        Ok((name, alias))
    }

    fn parse_expr(&mut self) -> Result<ast::Expr,   Diagnostic<FileId>> {
        self.parse_expr_bp(0)
    }

    fn parse_expr_bp(&mut self, min_bp: Precedence) -> Result<ast::Expr, Diagnostic<FileId>> {
        let mut lhs = self.parse_prefix()?;

        while let Some((op, _)) = self.peek() {
            let op = op.clone();
            self.peek_span();
            let Some((lbp, rbp)) = self.get_infix_bp(&op) else {
                break;
            };

            if lbp < min_bp {
                break;
            }
            self.advance();
            lhs = self.parse_infix(&op, lhs, lbp, rbp)?;
        }

        Ok(lhs)
    }

    fn peek_token(&self) -> Token {
        self.peek().map(|(t, _)| t.clone()).unwrap_or(Token::Error)
    }
    fn peek_span(&self) -> Span {
        self.peek().map(|(_, s)| *s).unwrap_or(Span::new(0, 0))
    }

    fn parse_prefix(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        let token = self.peek_token();
        match token {
            Token::Star => {
                let op_span = self.peek_span();
                self.advance();
                let prefix_bp = self.get_prefix_bp(&token);
                let expr = self.parse_expr_bp(prefix_bp)?;
                Ok(ast::Expr::Deref(Box::new(expr), ast::ExprInfo {
                    span: op_span,
                    ty: ast::Type::Unknown
                }))
            }
            Token::Minus | Token::Plus => {
                let (op_token, _) = self.advance().unwrap();
                let op_token = op_token.clone(); 
                let prefix_bp = self.get_prefix_bp(&op_token);
                let expr = self.parse_expr_bp(prefix_bp)?;
                let span = expr.span();
                Ok(ast::Expr::UnaryOp(
                    match op_token {
                        Token::Minus => ast::UnOp::Neg,
                        Token::Plus => ast::UnOp::Plus,
                        _ => unreachable!(),
                    },
                    Box::new(expr),
                    ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown
                    }
                ))
            }
            _ => {
                self.parse_atom()
            }
        }
    }

    fn parse_infix(
        &mut self,
        op: &Token,
        lhs: ast::Expr,
        _lbp: Precedence,
        rbp: Precedence,
    ) -> Result<ast::Expr, Diagnostic<FileId>> {
        match op {
            Token::Eq => {
                let rhs = self.parse_expr_bp(rbp)?;
                let span = Span::new(lhs.span().start(), rhs.span().end());
                Ok(ast::Expr::Assign(Box::new(lhs), Box::new(rhs), ast::ExprInfo {
                    span,
                    ty: ast::Type::Unknown
                }))
            }
            Token::Plus | Token::Minus | Token::Star | Token::Slash | Token::DoubleStar | Token::Caret | Token::Percent
            | Token::EqEq | Token::NotEq | Token::Gt | Token::Lt | Token::GtEq | Token::LtEq 
            | Token::AndAnd | Token::OrOr => {
                let bin_op = match op {
                    Token::Plus => ast::BinOp::Add,
                    Token::Minus => ast::BinOp::Sub,
                    Token::Star => ast::BinOp::Mul,
                    Token::Slash => ast::BinOp::Div,
                    Token::DoubleStar => ast::BinOp::Pow,
                    Token::Caret => ast::BinOp::Pow2,
                    Token::Percent => ast::BinOp::Mod,
                    Token::EqEq => ast::BinOp::Eq,
                    Token::NotEq => ast::BinOp::NotEq,
                    Token::Gt => ast::BinOp::Gt,
                    Token::Lt => ast::BinOp::Lt,
                    Token::GtEq => ast::BinOp::GtEq,
                    Token::LtEq => ast::BinOp::LtEq,
                    Token::AndAnd => ast::BinOp::And,
                    Token::OrOr => ast::BinOp::Or,
                    _ => unreachable!(),
                };
                let rhs = self.parse_expr_bp(rbp)?;
                let span = Span::new(lhs.span().start(), rhs.span().end());
                Ok(ast::Expr::BinOp(
                    Box::new(lhs),
                    bin_op,
                    Box::new(rhs),
                    ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown
                    }
                ))
            }
            Token::LBracket => {
                let index = self.parse_expr()?;
                let end_span = self.expect(Token::RBracket)?;
                let span = Span::new(lhs.span().start(), end_span.end());
                
                Ok(ast::Expr::ArrayAccess(Box::new(lhs), Box::new(index), ast::ExprInfo {
                    span,
                    ty: ast::Type::Unknown,
                }))
            }
            Token::Dot => {
                let (field, field_span) = self.consume_ident()?;
                let span = Span::new(lhs.span().start(), field_span.end());
                Ok(ast::Expr::FieldAccess(
                    Box::new(lhs),
                    field,
                    ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                    },
                ))
            }
            Token::DotDot => {
                let rhs = self.parse_expr_bp(rbp)?;
                let span = Span::new(lhs.span().start(), rhs.span().end());
                Ok(ast::Expr::Range(
                    Box::new(lhs),
                    Box::new(rhs),
                    ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                    },
                ))
            }
            Token::KwAs => {
                let cast_type = self.parse_type()?;
                let end_span = self.previous().map(|(_, s)| *s).unwrap();
                let span = Span::new(lhs.span().start(), end_span.end());
                
                Ok(ast::Expr::Cast(
                    Box::new(lhs),
                    cast_type,
                    ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                    },
                ))
            }
            _ => {
                let token = self.peek().unwrap();
                self.error(
                    &format!("Unexpected token in infix position: {:?}", token.0),
                    token.1,
                )
            }
        }
    }

    fn get_prefix_bp(&self, token: &Token) -> Precedence {
        match token {
            Token::Star | Token::Plus | Token::Minus => 8,
            _ => 0,
        }
    }

    fn get_infix_bp(&self, token: &Token) -> Option<(Precedence, Precedence)> {
        match token {
            Token::OrOr => Some((1, 2)),
            Token::AndAnd => Some((3, 4)),
            Token::EqEq | Token::NotEq => Some((5, 6)),
            Token::Gt | Token::Lt | Token::GtEq | Token::LtEq => Some((7, 8)),
            Token::Plus | Token::Minus => Some((9, 10)),
            Token::Star | Token::Slash | Token::Percent => Some((11, 12)),
            Token::DoubleStar | Token::Caret => Some((13, 14)),
            Token::DotDot => Some((15, 16)),
            Token::KwAs => Some((17, 18)),
            Token::Dot => Some((19, 20)),
            Token::LBracket => Some((21, 22)),
            Token::Eq => Some((1, 2)),
            _ => None,
        }
    }
    
    fn parse_block(&mut self) -> Result<Vec<ast::Stmt>, Diagnostic<FileId>> {
        self.expect(Token::LBrace)?;
        let mut stmts = Vec::new();
        while !self.check(Token::RBrace) {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(Token::RBrace)?;
        Ok(stmts)
    }

    fn consume(&mut self, expected: Token, err_msg: &str) -> Result<Span, Diagnostic<FileId>> {
        if self.check(expected.clone()) {
            let span = self.peek().map(|(_, s)| *s).unwrap();
            self.advance();
            Ok(span)
        } else {
            let span = self.peek().map(|(_, s)| *s).unwrap_or(Span::new(0, 0));
            self.error(err_msg, span)
        }
    }

    fn consume_ident(&mut self) -> Result<(String, Span), Diagnostic<FileId>> {
        let token = self.advance().cloned();
        match token.as_ref() {
            Some((Token::Ident(name), span)) => Ok((name.clone(), *span)),
            Some((_, span)) => self.error("Expected identifier", *span),
            None => self.error("Expected identifier", Span::new(0, 0)),
        }
    }

    fn parse_type(&mut self) -> Result<ast::Type, Diagnostic<FileId>> {
        let next = self.advance().map(|(t, s)| (t.clone(), *s));

        match next {
            Some((Token::TyI32, _)) => Ok(ast::Type::I32),
            Some((Token::TyBool, _)) => Ok(ast::Type::Bool),
            Some((Token::TyString, _)) => Ok(ast::Type::String),
            Some((Token::TyVoid, _)) => Ok(ast::Type::Void),
            Some((Token::TyAny, _)) => Ok(ast::Type::Any),
            Some((Token::KwRawPtr, _)) => Ok(ast::Type::RawPtr),
            Some((Token::Star, _)) => {
                let target_type = self.parse_type()?;
                Ok(ast::Type::Pointer(Box::new(target_type)))
            },
            Some((Token::EmptyArray, _)) => {
                let element_type = self.parse_type()?;
                Ok(ast::Type::Array(Box::new(element_type)))
            },
            Some((Token::LBracket, _)) => {
                let element_type = self.parse_type()?;
                
                if self.check(Token::Semi) {
                    self.advance();

                    let size_token = self.advance().cloned();
                    
                    match size_token {
                        Some((Token::Int(size), _)) => {
                            self.expect(Token::RBracket)?;
                            Ok(ast::Type::SizedArray(Box::new(element_type), size as usize))
                        },
                        _ => self.error("Expected array size (integer)", self.peek_span())
                    }
                } else {
                    self.expect(Token::RBracket)?;
                    Ok(ast::Type::Array(Box::new(element_type)))
                }
            },
            Some((Token::Ident(name), _)) => Ok(ast::Type::Struct(name)),
            Some((_, span)) => self.error("Expected type annotation", span),
            None => self.error("Expected type annotation", Span::new(0, 0)),
        }
    }
    
    fn parse_return(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        self.expect(Token::KwReturn)?;
        let ret_span = self.previous().map(|(_, s)| *s).unwrap();
        let expr = self.parse_expr()?;
        self.expect(Token::Semi)?;
        let end_span = self.previous().map(|(_, s)| *s).unwrap();
        Ok(ast::Stmt::Return(expr, Span::new(ret_span.start(), end_span.end())))
    }

    fn parse_function(&mut self) -> Result<ast::Function, Diagnostic<FileId>> {
        self.consume(Token::KwFn, "Expected 'fn'")?;
        let start_span = self.previous().map(|(_, s)| *s).unwrap();

        let (name, _name_span) = self.consume_ident()?;
        let params = self.parse_parameters()?;

        let return_type = if self.check(Token::Arrow) {
            self.advance();
            self.parse_type()?
        } else {
            ast::Type::Void
        };

        let body = self.parse_block()?;
        let end_span = match body.last() {
            Some(last_stmt) => last_stmt.span(),
            None => start_span,
        };

        Ok(ast::Function {
            name,
            params,
            return_type,
            body,
            span: Span::new(start_span.start(), end_span.end()),
            exported: false,
        })
    }

    fn parse_parameters(&mut self) -> Result<Vec<(String, ast::Type)>, Diagnostic<FileId>> {
        self.consume(Token::LParen, "Expected '(' after function name")?;
        let mut params = Vec::new();

        while !self.check(Token::RParen) {
            let (name, _) = self.consume_ident()?;
            self.consume(Token::Colon, "Expected ':' after parameter name")?;
            let param_type = self.parse_type()?;
            params.push((name, param_type));

            if !self.check(Token::Comma) {
                break;
            }
            self.advance();
        }

        self.consume(Token::RParen, "Expected ')' after parameters")?;
        Ok(params)
    }

    fn parse_stmt(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        if self.check(Token::KwLet) {
            self.advance();
            self.parse_let(true)
        } else if self.check(Token::KwIf) {
            self.parse_if()
        } else if self.check(Token::KwReturn) {
            self.parse_return()
        } else if self.check(Token::KwWhile) {
            self.parse_while()
        } else if self.check(Token::KwFor) {
            self.parse_for()
        } else {
            let expr = self.parse_expr()?;
            let span = expr.span();
            
            if self.check(Token::Semi) { 
                self.advance(); 
            }
            
            Ok(ast::Stmt::Expr(expr, span))
        }
    }

    fn parse_defer(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        self.expect(Token::KwDefer)?;
        let start_span = self.previous().map(|(_, s)| *s).unwrap();
        let expr = self.parse_expr()?;
        self.expect(Token::Semi)?;
        let end_span = self.previous().map(|(_, s)| *s).unwrap();
        Ok(ast::Stmt::Defer(expr, Span::new(start_span.start(), end_span.end())))
    }


    fn parse_while(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        self.expect(Token::KwWhile)?;
        let while_span = self.previous().map(|(_, s)| *s).unwrap();
        
        let condition = self.parse_expr()?;

        let body = self.parse_block()?;

        Ok(ast::Stmt::While(
            condition,
            body,
            Span::new(while_span.start(), self.previous().unwrap().1.end()),
        ))
    }

    fn parse_for(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        self.consume(Token::KwFor, "Expected 'for'")?;
        let for_span = self.previous().map(|(_, s)| *s).unwrap();

        let (ident, _) = self.consume_ident()?;
        self.consume(Token::KwIn, "Expected 'in' after loop variable")?;

        let range_expr = self.parse_expr()?;
        let body = self.parse_block()?;

        Ok(ast::Stmt::For(
            ident,
            range_expr,
            body,
            Span::new(for_span.start(), self.previous().unwrap().1.end()),
        ))
    }

    fn parse_if(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        self.consume(Token::KwIf, "Expected 'if'")?;
        let if_span = self.previous().map(|(_, s)| *s).unwrap();
        let condition = self.parse_expr()?;
        let then_branch = self.parse_block()?;
        let mut else_branch = None;

        if self.check(Token::KwElse) {
            self.advance();
            else_branch = Some(if self.check(Token::KwIf) {
                vec![self.parse_if()?]
            } else {
                self.parse_block()?
            });
        }

        let end_span = else_branch.as_ref()
            .and_then(|b| b.last())
            .map(|s| s.span().end())
            .unwrap_or_else(|| then_branch.last().unwrap().span().end());

        Ok(ast::Stmt::If(
            condition,
            then_branch,
            else_branch,
            Span::new(if_span.start(), end_span),
        ))
    }
    fn parse_let(&mut self, expect_semi: bool) -> Result<ast::Stmt, Diagnostic<FileId>> {
        let let_span = self.previous().map(|(_, s)| *s).unwrap();
        
        let mut idents = vec![];
        loop {
            let token = self.advance().cloned();
            let (ident, span) = match token.as_ref() {
                Some((Token::Ident(name), sp)) => (name.clone(), *sp),
                Some((_, sp)) => return self.error("Expected identifier", *sp),
                None => return self.error("Expected identifier", Span::new(0, 0)),
            };
            idents.push((ident, span));

            if !self.check(Token::Comma) {
                break;
            }
            self.advance();
        }

        let type_annot = if self.check(Token::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(Token::Eq)?;

        let mut exprs = vec![];
        loop {
            exprs.push(self.parse_expr()?);
            if !self.check(Token::Comma) {
                break;
            }
            self.advance();
        }

        if idents.len() != exprs.len() {
            return self.error(
                &format!("Expected {} expressions, got {}", idents.len(), exprs.len()),
                Span::new(idents[0].1.start(), exprs.last().unwrap().span().end())
            );
        }

        let mut stmts = vec![];
        for ((ident, span), expr) in idents.into_iter().zip(exprs) {
            match &expr {
                ast::Expr::Int(..) => ast::Type::I32,
                ast::Expr::Str(..) => ast::Type::String,
                ast::Expr::Bool(..) => ast::Type::Bool,
                _ => ast::Type::Unknown,
            };

            stmts.push(ast::Stmt::Let(
                ident,
                type_annot.clone(),
                expr,
                span,
            ));
        }

        if expect_semi {
            self.expect(Token::Semi)?;
        }

        let_span.start();
        let end = if expect_semi {
            self.previous().map(|(_, s)| s.end()).unwrap_or_else(|| let_span.end())
        } else {
            stmts.last().map(|s| s.span().end()).unwrap_or_else(|| let_span.end())
        };

        Ok(ast::Stmt::Block(
            stmts,
            Span::new(let_span.start(), end)
        ))
    }
    
    fn parse_atom(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        let current = self.advance().cloned();
        match current {
            Some((Token::Int(n), span)) => {
                Ok(ast::Expr::Int(n.try_into().unwrap(), ast::ExprInfo {
                    span,
                    ty: ast::Type::Unknown,
                }))
            }
            Some((Token::Str(s), span)) => {
                Ok(ast::Expr::Str(s, ast::ExprInfo {
                    span,
                    ty: ast::Type::String,
                }))
            }
            Some((Token::TemplateStr(s), span)) => {
                self.parse_template_string(s, span)
            }
            Some((Token::KwTrue, span)) => {
                Ok(ast::Expr::Bool(true, ast::ExprInfo {
                    span,
                    ty: ast::Type::Bool,
                }))
            }
            Some((Token::KwFalse, span)) => {
                Ok(ast::Expr::Bool(false, ast::ExprInfo {
                    span,
                    ty: ast::Type::Bool,
                }))
            }
            Some((Token::LParen, span_start)) => {
                let expr = self.parse_expr()?;
                let span_end = self.expect(Token::RParen)?;
                let _span = Span::new(span_start.start(), span_end.end());
                
                Ok(expr)
            }
            Some((Token::Ident(name), span)) => {
                if self.check(Token::LBrace) && !self.is_in_comparison_context() {
                    self.advance();

                    let mut fields = Vec::new();
                    let struct_name = name.clone();
                    
                    while !self.check(Token::RBrace) {
                        let (field_name, _) = self.consume_ident()?;
                        self.expect(Token::Colon)?;
                        let expr = self.parse_expr()?;
                        
                        fields.push((field_name, expr));
                        
                        if !self.check(Token::RBrace) {
                            self.expect(Token::Comma)?;
                        }
                    }
                    
                    let end_span = self.expect(Token::RBrace)?;
                    
                    Ok(ast::Expr::StructInit(
                        struct_name.clone(),
                        fields,
                        ast::ExprInfo {
                            span: Span::new(span.start(), end_span.end()),
                            ty: ast::Type::Struct(struct_name),
                        },
                    ))
                } else if self.check(Token::LParen) {
                    self.parse_function_call(name, span)
                } else {
                    Ok(ast::Expr::Var(name, ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                    }))
                }
            }
            Some((Token::LBracket, span)) => {
                let mut elements = Vec::new();
                
                if !self.check(Token::RBracket) {
                    loop {
                        elements.push(self.parse_expr()?);
                        
                        if !self.check(Token::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                
                let end_span = self.expect(Token::RBracket)?;
                
                Ok(ast::Expr::ArrayInit(elements, ast::ExprInfo {
                    span: Span::new(span.start(), end_span.end()),
                    ty: ast::Type::Unknown,
                }))
            }
            Some((Token::KwIntrinsic, span)) => {
                self.parse_intrinsic_call(span)
            }
            Some((Token::KwSafe, span)) => {
                self.parse_safe_block(span)
            },
            _ => {
                let token = self.previous().map(|(t, _)| t.clone()).unwrap();
                let span = self.previous().map(|(_, s)| *s).unwrap();
                self.error(&format!("Unexpected token in expression: {:?}", token), span)
            }
        }
    }

    fn parse_function_call(&mut self, name: String, span: Span) -> Result<ast::Expr, Diagnostic<FileId>> {
        self.expect(Token::LParen)?;
        let mut args = Vec::new();
        while !self.check(Token::RParen) {
            args.push(self.parse_expr()?);
            if !self.check(Token::Comma) {
                break;
            }
            self.advance();
        }
        let rparen_span = self.expect(Token::RParen)?;
        
        Ok(ast::Expr::Call(name, args, ast::ExprInfo {
            span: Span::new(span.start(), rparen_span.end()),
            ty: ast::Type::Unknown
        }))
    }

    fn parse_safe_block(&mut self, start_span: Span) -> Result<ast::Expr, Diagnostic<FileId>> {
        self.expect(Token::LBrace)?;
        let mut stmts = Vec::new();
        while !self.check(Token::RBrace) {
            if self.check(Token::KwDefer) {
                stmts.push(self.parse_defer()?);
            } else {
                stmts.push(self.parse_stmt()?);
            }
        }
        self.expect(Token::RBrace)?;
        let end_span = self.previous().map(|(_, s)| *s).unwrap();
        Ok(ast::Expr::SafeBlock(stmts, ast::ExprInfo {
            span: Span::new(start_span.start(), end_span.end()),
            ty: ast::Type::Unknown
        }))
    }

    fn parse_intrinsic_call(&mut self, span: Span) -> Result<ast::Expr, Diagnostic<FileId>> {
        let (name, name_span) = self.consume_ident()?;
        self.expect(Token::LParen)?;
        let mut args = Vec::new();
        while !self.check(Token::RParen) {
            args.push(self.parse_expr()?);
            if !self.check(Token::Comma) { break; }
            self.advance();
        }
        self.expect(Token::RParen)?;
        Ok(ast::Expr::IntrinsicCall(name, args, ast::ExprInfo {
            span: Span::new(span.start(), name_span.end()),
            ty: ast::Type::Unknown
        }))
    }

    fn parse_struct(&mut self) -> Result<ast::StructDef, Diagnostic<FileId>> {
        let start_span = self.consume(Token::KwStruct, "Expected 'struct'")?;
        let (name, _) = self.consume_ident()?;
        
        self.expect(Token::LBrace)?;
        
        let mut fields = Vec::new();
        while !self.check(Token::RBrace) {
            let (field_name, field_span) = self.consume_ident()?;
            self.expect(Token::Colon)?;
            let field_type = self.parse_type()?;
            
            fields.push(ast::StructField {
                name: field_name,
                ty: field_type,
                span: field_span,
            });
            
            if !self.check(Token::RBrace) {
                self.expect(Token::Comma)?;
            }
        }
        
        let end_span = self.expect(Token::RBrace)?;
        
        Ok(ast::StructDef {
            name,
            fields,
            span: Span::new(start_span.start(), end_span.end()),
        })
    }

    fn check(&self, expected: Token) -> bool {
        self.peek().map(|(t, _)| t == &expected).unwrap_or(false)
    }

    fn advance(&mut self) -> Option<&(Token, Span)> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn previous(&self) -> Option<&(Token, Span)> {
        self.tokens.get(self.current.saturating_sub(1))
    }

    fn expect(&mut self, expected: Token) -> Result<Span, Diagnostic<FileId>> {
        if self.check(expected.clone()) {
            let span = self.peek().map(|(_, s)| *s).unwrap_or(Span::new(0, 0));
            self.advance();
            Ok(span)
        } else {
            let span = self.peek().map(|(_, s)| *s).unwrap_or(Span::new(0, 0));
            self.error(&format!("Expected '{:?}'", expected), span)
        }
    }

    fn error<T>(&self, msg: &str, span: Span) -> Result<T, Diagnostic<FileId>> {
        Err(Diagnostic::error()
            .with_message(msg)
            .with_labels(vec![
                Label::primary(self.file_id, span)
                    .with_message("error occurred here")
            ]))
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> Option<&(Token, Span)> {
        self.tokens.get(self.current)
    }

    fn is_in_comparison_context(&self) -> bool {
        if let Some((token, _)) = self.previous() {
            match token {
                Token::Gt | Token::Lt | Token::EqEq | Token::AndAnd | Token::OrOr => {
                    return true;
                }
                _ => {}
            }
        }
        
        let mut i = self.current.saturating_sub(2);
        while i > 0 && i < self.tokens.len() {
            if let Some((token, _)) = self.tokens.get(i) {
                match token {
                    Token::KwIf | Token::KwWhile => {
                        return true;
                    }
                    Token::Semi | Token::LBrace | Token::RBrace => break,
                    _ => {}
                }
            }
            i = i.saturating_sub(1);
        }
        
        false
    }

    fn parse_template_string(&mut self, content: String, span: Span) -> Result<ast::Expr, Diagnostic<FileId>> {
        let mut parts = Vec::new();
        let mut current = String::new();
        let mut i = 0;
        let chars: Vec<char> = content.chars().collect();
        
        while i < chars.len() {
            let c = chars[i];
            i += 1;
            
            if c == '$' && i < chars.len() && chars[i] == '{' {
                i += 1;

                if !current.is_empty() {
                    parts.push(ast::TemplateStrPart::Literal(std::mem::take(&mut current)));
                }

                let expr_start = i;
                let mut brace_count = 1;
                
                while i < chars.len() {
                    let c = chars[i];
                    i += 1;
                    
                    if c == '{' {
                        brace_count += 1;
                    } else if c == '}' {
                        brace_count -= 1;
                        if brace_count == 0 {
                            break;
                        }
                    }
                }

                let expr_end = i - 1;
                let expr_text: String = chars[expr_start..expr_end].iter().collect();

                if expr_text.trim().chars().all(|c| c.is_alphanumeric() || c == '_') {
                    parts.push(ast::TemplateStrPart::Expression(Box::new(
                        ast::Expr::Var(expr_text.trim().to_string(), ast::ExprInfo {
                            span,
                            ty: ast::Type::Unknown,
                        })
                    )));
                } else if let Some(index_pos) = expr_text.find('[') {
                    if expr_text.ends_with(']') {
                        let array_name = expr_text[..index_pos].trim().to_string();
                        let index_str = expr_text[index_pos+1..expr_text.len()-1].trim();

                        if let Ok(index) = index_str.parse::<i32>() {
                            parts.push(ast::TemplateStrPart::Expression(Box::new(
                                ast::Expr::ArrayAccess(
                                    Box::new(ast::Expr::Var(array_name, ast::ExprInfo {
                                        span,
                                        ty: ast::Type::Unknown,
                                    })),
                                    Box::new(ast::Expr::Int(index, ast::ExprInfo {
                                        span,
                                        ty: ast::Type::I32,
                                    })),
                                    ast::ExprInfo {
                                        span,
                                        ty: ast::Type::Unknown,
                                    }
                                )
                            )));
                        } else {
                            parts.push(ast::TemplateStrPart::Expression(Box::new(
                                ast::Expr::ArrayAccess(
                                    Box::new(ast::Expr::Var(array_name, ast::ExprInfo {
                                        span,
                                        ty: ast::Type::Unknown,
                                    })),
                                    Box::new(ast::Expr::Var(index_str.to_string(), ast::ExprInfo {
                                        span,
                                        ty: ast::Type::Unknown,
                                    })),
                                    ast::ExprInfo {
                                        span,
                                        ty: ast::Type::Unknown,
                                    }
                                )
                            )));
                        }
                    } else {
                        parts.push(ast::TemplateStrPart::Expression(Box::new(
                            ast::Expr::Var(expr_text.trim().to_string(), ast::ExprInfo {
                                span,
                                ty: ast::Type::Unknown,
                            })
                        )));
                    }
                } else {
                    if let Some(op_pos) = find_operator(&expr_text) {
                        let (left, right) = split_at_operator(&expr_text, op_pos);
                        let op = determine_operator(&expr_text, op_pos);
                        
                        let left_expr = if left.trim().chars().all(|c| c.is_alphanumeric() || c == '_') {
                            ast::Expr::Var(left.trim().to_string(), ast::ExprInfo {
                                span,
                                ty: ast::Type::Unknown,
                            })
                        } else {
                            parse_nested_expr(left.trim(), span, self.file_id)?
                        };
                        
                        let right_expr = if right.trim().chars().all(|c| c.is_alphanumeric() || c == '_') {
                            ast::Expr::Var(right.trim().to_string(), ast::ExprInfo {
                                span,
                                ty: ast::Type::Unknown,
                            })
                        } else {
                            parse_nested_expr(right.trim(), span, self.file_id)?
                        };
                        
                        parts.push(ast::TemplateStrPart::Expression(Box::new(
                            ast::Expr::BinOp(
                                Box::new(left_expr),
                                op,
                                Box::new(right_expr),
                                ast::ExprInfo {
                                    span,
                                    ty: ast::Type::Unknown,
                                }
                            )
                        )));
                    } else {
                        parts.push(ast::TemplateStrPart::Expression(Box::new(
                            ast::Expr::Var(expr_text.trim().to_string(), ast::ExprInfo {
                                span,
                                ty: ast::Type::Unknown,
                            })
                        )));
                    }
                }
            } else {
                current.push(c);
            }
        }

        if !current.is_empty() {
            parts.push(ast::TemplateStrPart::Literal(current));
        }
        
        Ok(ast::Expr::TemplateStr(parts, ast::ExprInfo {
            span,
            ty: ast::Type::String,
        }))
    }
}

fn find_operator(expr: &str) -> Option<usize> {
    let chars: Vec<char> = expr.chars().collect();
    let mut paren_level = 0;
    let mut bracket_level = 0;
    
    for i in 0..chars.len() {
        match chars[i] {
            '(' => paren_level += 1,
            ')' => paren_level -= 1,
            '[' => bracket_level += 1,
            ']' => bracket_level -= 1,
            '+' | '-' | '*' | '/' | '%' | '=' | '>' | '<' | '&' | '|' | '^' => {
                if paren_level == 0 && bracket_level == 0 {
                    if i+1 < chars.len() {
                        let next = chars[i+1];
                        if (chars[i] == '*' && next == '*') || // **
                           (chars[i] == '=' && next == '=') || // ==
                           (chars[i] == '!' && next == '=') || // !=
                           (chars[i] == '>' && next == '=') || // >=
                           (chars[i] == '<' && next == '=') || // <=
                           (chars[i] == '&' && next == '&') || // &&
                           (chars[i] == '|' && next == '|') {  // ||
                            return Some(i);
                        }
                    }

                    return Some(i);
                }
            },
            _ => {}
        }
    }
    
    None
}

fn split_at_operator(expr: &str, pos: usize) -> (String, String) {
    let mut end_pos = pos + 1;
    let chars: Vec<char> = expr.chars().collect();

    if pos+1 < chars.len() {
        let curr = chars[pos];
        let next = chars[pos+1];
        if (curr == '*' && next == '*') || // **
           (curr == '=' && next == '=') || // ==
           (curr == '!' && next == '=') || // !=
           (curr == '>' && next == '=') || // >=
           (curr == '<' && next == '=') || // <=
           (curr == '&' && next == '&') || // &&
           (curr == '|' && next == '|') {  // ||
            end_pos = pos + 2;
        }
    }
    
    let left = expr[..pos].to_string();
    let right = expr[end_pos..].to_string();
    
    (left, right)
}

fn determine_operator(expr: &str, pos: usize) -> ast::BinOp {
    let chars: Vec<char> = expr.chars().collect();
    let c = chars[pos];
    
    if pos+1 < chars.len() {
        let next = chars[pos+1];
        match (c, next) {
            ('*', '*') => return ast::BinOp::Pow,
            ('=', '=') => return ast::BinOp::Eq,
            ('!', '=') => return ast::BinOp::NotEq,
            ('>', '=') => return ast::BinOp::GtEq,
            ('<', '=') => return ast::BinOp::LtEq,
            ('&', '&') => return ast::BinOp::And,
            ('|', '|') => return ast::BinOp::Or,
            _ => {}
        }
    }
    
    match c {
        '+' => ast::BinOp::Add,
        '-' => ast::BinOp::Sub,
        '*' => ast::BinOp::Mul,
        '/' => ast::BinOp::Div,
        '%' => ast::BinOp::Mod,
        '^' => ast::BinOp::Pow2,
        '>' => ast::BinOp::Gt,
        '<' => ast::BinOp::Lt,
        _ => ast::BinOp::Add
    }
}

fn parse_nested_expr(expr: &str, span: codespan::Span, file_id: codespan::FileId) -> Result<ast::Expr, codespan_reporting::diagnostic::Diagnostic<codespan::FileId>> {
    let expr = expr.trim();

    if expr.starts_with('(') && expr.ends_with(')') {
        let inner = &expr[1..expr.len()-1];
        return parse_nested_expr(inner, span, file_id);
    } 

    if let Ok(num) = expr.parse::<i32>() {
        return Ok(ast::Expr::Int(num, ast::ExprInfo {
            span,
            ty: ast::Type::I32,
        }));
    } 

    if expr == "true" || expr == "false" {
        return Ok(ast::Expr::Bool(expr == "true", ast::ExprInfo {
            span,
            ty: ast::Type::Bool,
        }));
    }

    if let Some(pos) = find_lowest_priority_operator(expr) {
        let (left, right) = split_at_operator(expr, pos);
        let op = determine_operator(expr, pos);
        
        let left_expr = parse_nested_expr(left.trim(), span, file_id)?;
        let right_expr = parse_nested_expr(right.trim(), span, file_id)?;
        
        return Ok(ast::Expr::BinOp(
            Box::new(left_expr),
            op,
            Box::new(right_expr),
            ast::ExprInfo {
                span,
                ty: ast::Type::Unknown,
            }
        ));
    }

    if let Some(index_pos) = expr.find('[') {
        if expr.ends_with(']') {
            let array_name = expr[..index_pos].trim().to_string();
            let index_str = expr[index_pos+1..expr.len()-1].trim();
            

            if let Ok(index) = index_str.parse::<i32>() {
                return Ok(ast::Expr::ArrayAccess(
                    Box::new(ast::Expr::Var(array_name, ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                    })),
                    Box::new(ast::Expr::Int(index, ast::ExprInfo {
                        span,
                        ty: ast::Type::I32,
                    })),
                    ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                    }
                ));
            } else {
                return Ok(ast::Expr::ArrayAccess(
                    Box::new(ast::Expr::Var(array_name, ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                    })),
                    Box::new(ast::Expr::Var(index_str.to_string(), ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                    })),
                    ast::ExprInfo {
                        span,
                        ty: ast::Type::Unknown,
                    }
                ));
            }
        }
    }

    Ok(ast::Expr::Var(expr.to_string(), ast::ExprInfo {
        span,
        ty: ast::Type::Unknown,
    }))
}

fn find_lowest_priority_operator(expr: &str) -> Option<usize> {
    let chars: Vec<char> = expr.chars().collect();
    let mut paren_level = 0;
    let mut bracket_level = 0;

    let mut in_array_access = false;
    let mut array_access_positions = Vec::new();
    
    for i in 0..chars.len() {
        match chars[i] {
            '[' => {
                bracket_level += 1;
                if bracket_level == 1 && paren_level == 0 {
                    in_array_access = true;
                    array_access_positions.push(i);
                }
            },
            ']' => {
                if bracket_level > 0 {
                    bracket_level -= 1;
                    if bracket_level == 0 && paren_level == 0 {
                        in_array_access = false;
                        array_access_positions.push(i);
                    }
                }
            },
            '(' => paren_level += 1,
            ')' => if paren_level > 0 { paren_level -= 1 },
            _ => {}
        }
    }

    let mut add_sub_pos = None;
    for i in 0..chars.len() {
        if is_inside_array_access(i, &array_access_positions) {
            continue;
        }
        
        match chars[i] {
            '+' | '-' => {
                if i > 0 && !is_operator_char(chars[i-1]) && paren_level == 0 && bracket_level == 0 {
                    add_sub_pos = Some(i);
                    return add_sub_pos;
                }
            },
            _ => {}
        }
    }

    let mut mul_div_pos = None;
    for i in 0..chars.len() {
        if is_inside_array_access(i, &array_access_positions) {
            continue;
        }
        
        match chars[i] {
            '*' | '/' | '%' => {
                if chars[i] == '*' && i+1 < chars.len() && chars[i+1] == '*' {
                    continue;
                }
                
                if paren_level == 0 && bracket_level == 0 {
                    mul_div_pos = Some(i);
                    return mul_div_pos;
                }
            },
            _ => {}
        }
    }

    let mut pow_pos = None;
    for i in 0..chars.len() {
        if is_inside_array_access(i, &array_access_positions) {
            continue;
        }
        
        match chars[i] {
            '*' => {
                if i+1 < chars.len() && chars[i+1] == '*' && paren_level == 0 && bracket_level == 0 {
                    pow_pos = Some(i);
                    return pow_pos;
                }
            },
            '^' => {
                if paren_level == 0 && bracket_level == 0 {
                    pow_pos = Some(i);
                    return pow_pos;
                }
            },
            _ => {}
        }
    }

    for i in 0..chars.len() {
        if is_inside_array_access(i, &array_access_positions) {
            continue;
        }
        
        match chars[i] {
            '>' | '<' | '=' | '!' => {
                if paren_level == 0 && bracket_level == 0 {
                    if i+1 < chars.len() {
                        let next = chars[i+1];
                        if (chars[i] == '=' && next == '=') || // ==
                           (chars[i] == '!' && next == '=') || // !=
                           (chars[i] == '>' && next == '=') || // >=
                           (chars[i] == '<' && next == '=') {  // <=
                            return Some(i);
                        }
                    }

                    if chars[i] == '>' || chars[i] == '<' {
                        return Some(i);
                    }
                }
            },
            _ => {}
        }
    }

    None
}

fn is_inside_array_access(pos: usize, array_access_positions: &[usize]) -> bool {
    if array_access_positions.len() < 2 {
        return false;
    }
    
    for i in 0..array_access_positions.len() / 2 {
        let start = array_access_positions[i * 2];
        let end = array_access_positions[i * 2 + 1];
        
        if pos > start && pos < end {
            return true;
        }
    }
    
    false
}

fn is_operator_char(c: char) -> bool {
    matches!(c, '+' | '-' | '*' | '/' | '%' | '=' | '>' | '<' | '(' | '[' | ' ' | '!')
}
