use super::{ast, lexer::{Token, Lexer}};
use codespan::{FileId, Files, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};

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
            stmts: Vec::new(),
            functions: Vec::new(),
        };

        while !self.is_at_end() {
            if self.check(Token::KwFn) {
                program.functions.push(self.parse_function()?);
            } else {
                program.stmts.push(self.parse_stmt()?);
            }
        }

        Ok(program)
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

    fn parse_unary(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        if self.check(Token::Star) {
            let op_span = self.peek().map(|(_, s)| *s).unwrap();
            self.advance();
            let expr = self.parse_unary()?;
            Ok(ast::Expr::Deref(Box::new(expr), op_span, ast::Type::Unknown))
        } else {
            self.parse_primary()
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
        self.expect(Token::KwFn)?;
        let start_span = self.previous().map(|(_, s)| *s).unwrap();

        let token = self.advance().cloned();
        let (name, _name_span) = match token.as_ref() {
            Some((Token::Ident(name), span)) => (name.clone(), *span),
            Some((_, span)) => {
                return self.error("Expected function name", *span);
            }
            None => {
                return self.error("Expected function name", Span::new(0, 0));
            }
        };

        self.expect(Token::LParen)?;
        let mut params = Vec::new();
        while !self.check(Token::RParen) {
            let token = self.advance().cloned();

            let (param_name, _param_span) = match token.as_ref() {
                Some((Token::Ident(name), span)) => (name.clone(), *span),
                Some((_, span)) => return self.error("Expected parameter name", *span),
                None => return self.error("Expected parameter name", Span::new(0, 0)),
            };

            self.expect(Token::Colon)?;
            let param_type = self.parse_type()?;
            params.push((param_name, param_type));

            if !self.check(Token::Comma) {
                break;
            }
            self.advance();
        }
        self.expect(Token::RParen)?;

        let return_type = if self.check(Token::Arrow) {
            self.advance();
            self.parse_type()?
        } else {
            ast::Type::Void
        };

        self.expect(Token::LBrace)?;
        let mut body = Vec::new();
        while !self.check(Token::RBrace) {
            body.push(self.parse_stmt()?);
        }
        self.expect(Token::RBrace)?;

        let end_span = self.previous().map(|(_, s)| *s).unwrap();
        Ok(ast::Function {
            name,
            params,
            return_type,
            body,
            span: Span::new(start_span.start(), end_span.end()),
        })
    }

    fn parse_stmt(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        if self.check(Token::KwLet) {
            self.advance();
            self.parse_let(true)
        } else if self.check(Token::KwIf) {
            self.parse_if()
        } else if self.check(Token::KwReturn) {
            self.parse_return()
        } else if self.check(Token::KwDefer) {
            self.parse_defer()
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

        self.expect(Token::LParen)?;
        let condition = self.parse_expr()?;
        self.expect(Token::RParen)?;

        self.expect(Token::LBrace)?;
        let mut body = Vec::new();
        while !self.check(Token::RBrace) {
            body.push(self.parse_stmt()?);
        }
        self.expect(Token::RBrace)?;

        Ok(ast::Stmt::While(
            condition,
            body,
            Span::new(while_span.start(), self.previous().unwrap().1.end()),
        ))
    }

    fn parse_for(&mut self) -> Result<ast::Stmt, Diagnostic<FileId>> {
        self.expect(Token::KwFor)?;
        let for_span = self.previous().map(|(_, s)| *s).unwrap();

        self.expect(Token::LParen)?;

        let initializer = if self.check(Token::Semi) {
            None
        } else if self.check(Token::KwLet) {
            // Handle let initializer without semicolon
            self.advance(); // consume 'let'
            let let_stmt = self.parse_let(false)?;
            Some(Box::new(let_stmt))
        } else {
            // Parse as an expression statement (without semicolon)
            let expr = self.parse_expr()?;
            let expr_span = expr.span();
            Some(Box::new(ast::Stmt::Expr(expr, expr_span)))
        };
        self.expect(Token::Semi)?; // consume the semicolon after initializer

        let condition = if self.check(Token::Semi) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        self.expect(Token::Semi)?;

        let increment = if self.check(Token::RParen) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        self.expect(Token::RParen)?;

        self.expect(Token::LBrace)?;
        let mut body = Vec::new();
        while !self.check(Token::RBrace) {
            body.push(self.parse_stmt()?);
        }
        self.expect(Token::RBrace)?;

        Ok(ast::Stmt::For(
            initializer,
            condition,
            increment,
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
        self.expect(Token::KwIf)?;
        let if_span = self.previous().map(|(_, s)| *s).unwrap();
        self.expect(Token::LParen)?;
        let condition = self.parse_expr()?;
        self.expect(Token::RParen)?;
        self.expect(Token::LBrace)?;
        let mut then_branch = Vec::new();
        while !self.check(Token::RBrace) {
            then_branch.push(self.parse_stmt()?);
        }
        self.expect(Token::RBrace)?;
        let then_end = self.previous().map(|(_, s)| *s).unwrap();

        let mut else_branch = None;
        let else_span = if self.check(Token::KwElse) {
            self.advance();
            self.expect(Token::LBrace)?;
            let mut else_body = Vec::new();
            while !self.check(Token::RBrace) {
                else_body.push(self.parse_stmt()?);
            }
            self.expect(Token::RBrace)?;
            else_branch = Some(else_body);
            self.previous().map(|(_, s)| *s).unwrap()
        } else {
            then_end
        };

        Ok(ast::Stmt::If(
            condition,
            then_branch,
            else_branch,
            Span::new(if_span.start(), else_span.end()),
        ))
    }

    fn parse_let(&mut self, expect_semi: bool) -> Result<ast::Stmt, Diagnostic<FileId>> {
        let let_span = self.previous().map(|(_, s)| *s).unwrap();
        let token = self.advance().cloned();
        let (ident, _) = match token.as_ref() {
            Some((Token::Ident(name), span)) => (name.clone(), *span),
            Some((_, span)) => return self.error("Expected identifier", *span),
            None => return self.error("Expected identifier", Span::new(0, 0)),
        };

        let type_annot = if self.check(Token::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(Token::Eq)?;
        let expr = self.parse_expr()?;
        if expect_semi {
            self.expect(Token::Semi)?;
        }
        let end_span = if expect_semi {
            self.previous().map(|(_, s)| *s).unwrap()
        } else {
            expr.span()
        };
        Ok(ast::Stmt::Let(
            ident,
            type_annot,
            expr,
            Span::new(let_span.start(), end_span.end()),
        ))
    }

    fn parse_expr(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        let expr = self.parse_equality()?;
        if self.check(Token::Eq) {
            self.advance();
            let value = self.parse_assignment()?;
            let span = Span::new(expr.span().start(), value.span().end());
            Ok(ast::Expr::Assign(Box::new(expr), Box::new(value), span, ast::Type::Void))
        } else {
            Ok(expr)
        }
    }

    fn parse_equality(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        let mut expr = self.parse_comparison()?;
        while self.check(Token::EqEq) {
            let _op_span = self.peek().map(|(_, s)| *s).unwrap();
            self.advance();
            let right = self.parse_comparison()?;
            let span = Span::new(expr.span().start(), right.span().end());
            expr = ast::Expr::BinOp(Box::new(expr), ast::BinOp::Eq, Box::new(right), span, ast::Type::Unknown);
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        let mut expr = self.parse_additive()?;
        while self.check(Token::Gt) {
            let _op_span = self.peek().map(|(_, s)| *s).unwrap();
            self.advance();
            let right = self.parse_additive()?;
            let span = Span::new(expr.span().start(), right.span().end());
            expr = ast::Expr::BinOp(Box::new(expr), ast::BinOp::Gt, Box::new(right), span, ast::Type::Unknown);
        }
        Ok(expr)
    }

    fn parse_additive(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        let mut expr = self.parse_unary()?;
        while self.check(Token::Plus) || self.check(Token::Minus) {
            let op = match self.advance().unwrap().0 {
                Token::Plus => ast::BinOp::Add,
                Token::Minus => ast::BinOp::Sub,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            let span = Span::new(expr.span().start(), right.span().end());
            expr = ast::Expr::BinOp(Box::new(expr), op, Box::new(right), span, ast::Type::Unknown);
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        let mut expr = self.parse_atom()?;
        while self.check(Token::KwAs) {
            let start = expr.span().start();
            self.advance();
            let target_type = self.parse_type()?;
            let end_span = self.previous().map(|(_, s)| *s).unwrap();
            expr = ast::Expr::Cast(Box::new(expr), target_type.clone(), Span::new(start, end_span.end()), target_type);
        }
        Ok(expr)
    }

    fn parse_atom(&mut self) -> Result<ast::Expr, Diagnostic<FileId>> {
        let token = self.advance().cloned();
        match token {
            Some((Token::Int(n), span)) => Ok(ast::Expr::Int(n, span, ast::Type::I32)),
            Some((Token::Ident(name), span)) if name.starts_with("__") => {
                self.parse_intrinsic_call(name, span)
            },
            Some((Token::Str(value), span)) => Ok(ast::Expr::Str(value, span, ast::Type::String)),
            Some((Token::Ident(name), span)) => {
                if self.check(Token::LParen) {
                    self.parse_function_call(name, span)
                } else {
                    Ok(ast::Expr::Var(name, span, ast::Type::Unknown))
                }
            },
            Some((Token::LParen, _)) => {
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            Some((Token::KwSafe, span)) => {
                self.parse_safe_block(span)
            },
            Some((_, span)) => self.error("Expected primary expression", span),
            None => self.error("Expected primary expression", Span::new(0, 0)),
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
            stmts.push(self.parse_stmt()?);
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
        if !self.is_at_end() { self.current += 1; }
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