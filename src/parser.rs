use super::{ast, lexer::{Lexer, Token}};
use codespan::{FileId, Files, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};

type Precedence = u8;

pub struct Parser<'a> {
    tokens: Vec<(Token, Span)>,
    current: usize,
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
            } else {
                program.stmts.push(self.parse_stmt()?);
            }
        }

        Ok(program)
    }


    fn parse_import(&mut self) -> Result<ast::Import, Diagnostic<FileId>> {
        self.consume(Token::KwImport, "Expected 'import'")?;
        let start_span = self.previous().map(|(_, s)| *s).unwrap();

        let next_token = self.advance();
        let path = match next_token {
            Some((Token::Str(path), _)) => path.clone(),
            Some((_, span)) => {
                let span = *span;
                return self.error("Expected string literal", span);
            },
            None => return self.error("Expected string literal", Span::new(0, 0)),
        };

        self.expect(Token::Semi)?;
        let end_span = self.previous().map(|(_, s)| *s).unwrap();

        Ok(ast::Import {
            path,
            span: Span::new(start_span.start(), end_span.end()),
        })
    }
    
    fn parse_expr(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
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
                Ok(ast::Expr::Deref(Box::new(expr), op_span, ast::Type::Unknown))
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
                    span,
                    ast::Type::Unknown
                ))
            }
            _ => self.parse_atom(),
        }
    }


    fn parse_infix(
        &mut self,
        op: &Token,
        lhs: ast::Expr,
        lbp: Precedence,
        rbp: Precedence,
    ) -> Result<ast::Expr, Diagnostic<FileId>> {
        match op {
            Token::Eq => {
                let rhs = self.parse_expr_bp(rbp)?;
                let span = Span::new(lhs.span().start(), rhs.span().end());
                Ok(ast::Expr::Assign(Box::new(lhs), Box::new(rhs), span, ast::Type::Void))
            }
            Token::Plus | Token::Minus | Token::Star | Token::Slash
            | Token::EqEq | Token::Gt | Token::Lt | Token::AndAnd | Token::OrOr => {
                let bin_op = match op {
                    Token::Plus => ast::BinOp::Add,
                    Token::Minus => ast::BinOp::Sub,
                    Token::Star => ast::BinOp::Mul,
                    Token::Slash => ast::BinOp::Div,
                    Token::EqEq => ast::BinOp::Eq,
                    Token::Gt => ast::BinOp::Gt,
                    Token::Lt => ast::BinOp::Lt,
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
                    span,
                    ast::Type::Unknown
                ))
            }
            Token::KwAs => {
                let target_type = self.parse_type()?;
                let span = Span::new(lhs.span().start(), self.previous().unwrap().1.end());
                Ok(ast::Expr::Cast(
                    Box::new(lhs),
                    target_type.clone(),
                    span,
                    target_type
                ))
            }
            Token::DotDot => {
                let end = self.parse_expr_bp(rbp)?;
                let span = Span::new(lhs.span().start(), end.span().end());
                Ok(ast::Expr::Range(Box::new(lhs), Box::new(end), span, ast::Type::Unknown))
            },
            
            
            _ => {
                let token = self.peek().unwrap();
                println!("{:?}", token);
                self.error("Unexpected infix operator", self.peek().unwrap().1)
                
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
            Token::EqEq => Some((5, 6)),
            Token::Gt | Token::Lt => Some((7, 8)),
            Token::Plus | Token::Minus => Some((9, 10)),
            Token::Star | Token::Slash => Some((11, 12)),
            Token::DotDot => Some((13, 14)),
            Token::KwAs => Some((15, 16)),
            Token::Eq => Some((0, 0)),
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
            Some((Token::KwRawPtr, _)) => Ok(ast::Type::RawPtr),
            Some((Token::Star, _)) => {
                let target_type = self.parse_type()?;
                Ok(ast::Type::Pointer(Box::new(target_type)))
            },
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

        let end_span = self.previous().map(|(_, s)| *s).unwrap();
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
        } else if self.check(Token::KwPrint) {
            self.parse_print()
        } else if self.check(Token::KwWhile) {
          self.parse_while()
        } else if self.check(Token::KwFor) {
            self.parse_for()
        } else {
            let expr = self.parse_expr()?;
            let span = expr.span();
            if self.check(Token::Semi) { self.advance(); }
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

    fn parse_print(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        self.expect(Token::KwPrint)?;
        let start_span = self.previous().map(|(_, s)| *s).unwrap();
        self.expect(Token::LParen)?;
        let expr = self.parse_expr()?;
        self.expect(Token::RParen)?;
        self.expect(Token::Semi)?;
        let end_span = self.previous().map(|(_, s)| *s).unwrap();
        Ok(ast::Stmt::Expr(
            ast::Expr::Print(Box::new(expr), Span::new(start_span.start(), end_span.end()), ast::Type::Void),
            Span::new(start_span.start(), end_span.end())
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
            Some((Token::Int(n), span)) => Ok(ast::Expr::Int(n, span, ast::Type::I32)),
            Some((Token::Ident(name), span)) if name.starts_with("__") => {
                self.parse_intrinsic_call(name, span)
            },
            Some((Token::Str(value), span)) => Ok(ast::Expr::Str(value, span, ast::Type::String)),
            Some((Token::Ident(name), span)) => {
                let has_paren = self.check(Token::LParen);
                if has_paren {
                    self.parse_function_call(name, span)
                } else {
                    Ok(ast::Expr::Var(name, span, ast::Type::Unknown))
                }
            },
            Some((Token::LParen, _)) => {
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            },
            Some((Token::KwSafe, span)) => {
                self.parse_safe_block(span)
            },
            Some((t, span)) => self.error(&format!("Unexpected token: {:?}", t), span),
            None => self.error("Unexpected end of input", Span::new(0, 0)),
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
        self.expect(Token::RParen)?;
        Ok(ast::Expr::Call(name, args, span, ast::Type::Unknown))
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
        Ok(ast::Expr::SafeBlock(stmts, Span::new(start_span.start(), end_span.end()), ast::Type::Void))
    }

    fn parse_intrinsic_call(&mut self, name: String, span: Span) -> Result<ast::Expr, Diagnostic<FileId>> {
        self.expect(Token::LParen)?;
        let mut args = Vec::new();
        while !self.check(Token::RParen) {
            args.push(self.parse_expr()?);
            if !self.check(Token::Comma) { break; }
            self.advance();
        }
        self.expect(Token::RParen)?;
        Ok(ast::Expr::IntrinsicCall(name, args, span, ast::Type::Unknown))
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

    fn expect(&mut self, expected: Token) -> Result<(), Diagnostic<FileId>> {
        if self.check(expected.clone()) {
            self.advance();
            Ok(())
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
}