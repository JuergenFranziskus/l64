use num::{BigInt, Num};

use crate::{lexer::{Token, TokenKind}, ast::{Program, Line, ExprKind, LineKind, Expr, OpKind, Arg, Operation, ArgKind, Register, MemorySize, MemoryArg}, span::Span};





pub struct Parser<'a, 'b> {
    tokens: &'b [Token<'a>],
    index: usize,
}
impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(tokens: &'b [Token<'a>]) -> Self {
        Self {
            tokens,
            index: 0,
        }
    }

    pub fn parse(mut self) -> Program<'a> {
        let mut lines = Vec::new();
        
        while !self.at_end() {
            if let Some(line) = self.parse_line() {
                lines.push(line);
            }
        }

        Program {
            lines,
        }
    }


    fn parse_line(&mut self) -> Option<Line<'a>> {
        if self.is_newline() {
            self.next();
            return None;
        }

        let start = self.curr_span();
        let label = self.parse_optional_label();
        if label.is_some() {
            self.maybe_consume(TokenKind::Colon);
        }

        let kind = if self.is_newline() || self.at_end() {
            None
        }
        else if self.is_equ() {
            Some(self.parse_equ())
        }
        else {
            Some(self.parse_operation())
        };

        if !self.at_end() {
            self.consume_token(TokenKind::Newline);
        }

        let span = Span::merge(start, kind.as_ref().map(LineKind::span).unwrap_or(start));
        Some(Line {
            span,
            label,
            kind,
        })
    }


    fn parse_equ(&mut self) -> LineKind<'a> {
        self.consume_token(TokenKind::Equ);
        let value = self.parse_expr();
        LineKind::Equ(value)
    }

    fn parse_operation(&mut self) -> LineKind<'a> {
        let start = self.curr_span();
        let kind = self.parse_op_kind();
        let mut args = Vec::new();

        while !self.at_end() && !self.is_newline() {
            let arg = self.parse_arg();
            args.push(arg);

            if self.is_comma() {
                self.next();
            }
            else {
                break;
            }
        }

        let end = args.last().map(|a| a.span).unwrap_or(start);
        let span = Span::merge(start, end);

        LineKind::Op(Operation {
            span,
            op: kind,
            args
        })
    }

    fn parse_arg(&mut self) -> Arg<'a> {
        if let TokenKind::Register(_) = self.curr().kind {
            self.parse_reg_arg()
        }
        else if self.is_memory_arg() {
            self.parse_memory_arg()
        }
        else {
            let e = self.parse_expr();
            Arg {
                span: e.span,
                kind: ArgKind::Expression(e)
            }
        }
    }
    fn parse_reg_arg(&mut self) -> Arg<'a> {
        let span = self.curr_span();
        let TokenKind::Register(name) = self.curr().kind else { panic!() };
        self.next();

        let num = match name {
            "r0" => 0,
            "r1" => 1,
            "r2" => 2,
            "r3" => 3,
            "r4" => 4,
            "r5" => 5,
            "r6" => 6,
            "r7" => 7,
            "r8" => 8,
            "r9" => 9,
            "r10" => 10,
            "r11" => 11,
            "r12" => 12,
            "r13" => 13,
            "r14" => 14,
            "r15" | "rip" => 15,
            _ => panic!(),
        };

        Arg {
            span,
            kind: ArgKind::Register(Register(num))
        }
    }
    fn parse_memory_arg(&mut self) -> Arg<'a> {
        let start = self.curr_span();
        let size = self.parse_optional_memory_size();
        self.consume_token(TokenKind::OpenBracket);
        let offset = self.parse_expr();
        let end = self.curr_span();
        self.consume_token(TokenKind::CloseBracket);

        Arg {
            span: Span::merge(start, end),
            kind: ArgKind::Memory(MemoryArg {
                size,
                base: None,
                index: None,
                scale: None,
                offset: Some(offset),
            })
        }
    }
    fn parse_optional_memory_size(&mut self) -> Option<MemorySize> {
        let kind = match self.curr().kind {
            TokenKind::Byte => MemorySize::Byte,
            TokenKind::Short => MemorySize::Short,
            TokenKind::Long => MemorySize::Long,
            TokenKind::Word => MemorySize::Word,
            _ => return None,
        };
        self.next();
        Some(kind)
    }

    fn parse_op_kind(&mut self) -> OpKind {
        let kind = match self.curr().kind {
            TokenKind::Add => OpKind::Add,
            TokenKind::Mov => OpKind::Mov,
            TokenKind::Load => OpKind::Load,
            TokenKind::Store => OpKind::Store,
            TokenKind::Jump => OpKind::Jump,
            _ => panic!(),
        };
        self.next();
        kind
    }


    fn parse_expr(&mut self) -> Expr<'a> {
        match self.curr().kind {
            TokenKind::Identifier(_) => self.parse_label_expr(),
            TokenKind::Decimal(_) => self.parse_decimal(),
            TokenKind::Hex(_) => self.parse_hex(),
            _ => panic!(),
        }
    }

    fn parse_label_expr(&mut self) -> Expr<'a> {
        let span = self.curr_span();
        let TokenKind::Identifier(label) = self.curr().kind else { panic!() };
        self.next();

        Expr {
            span,
            kind: ExprKind::Label(label)
        }
    }
    fn parse_decimal(&mut self) -> Expr<'a> {
        let span = self.curr_span();
        let TokenKind::Decimal(literal) = self.curr().kind else { panic!() };
        self.next();
        let value = literal.parse().unwrap();

        Expr {
            span,
            kind: ExprKind::Integer(value)
        }
    }
    fn parse_hex(&mut self) -> Expr<'a> {
        let span = self.curr_span();
        let TokenKind::Hex(mut literal) = self.curr().kind else { panic!() };
        self.next();

        let is_negative = literal.starts_with('-');
        if is_negative {
            literal = &literal[1..];
        }
        literal = &literal[2..];

        let mut value: BigInt = BigInt::from_str_radix(literal, 16).unwrap();
        if is_negative {
            value = -value;
        }

        Expr {
            span,
            kind: ExprKind::Integer(value)
        }
    }

    fn is_memory_arg(&self) -> bool {
        self.is_token(TokenKind::OpenBracket)
            || self.is_token(TokenKind::Byte)
            || self.is_token(TokenKind::Short)
            || self.is_token(TokenKind::Long)
            || self.is_token(TokenKind::Word)
    }
    fn is_comma(&self) -> bool {
        self.is_token(TokenKind::Comma)
    }
    fn is_newline(&self) -> bool {
        self.is_token(TokenKind::Newline)
    }
    fn is_equ(&self) -> bool {
        self.is_token(TokenKind::Equ)
    }

    fn parse_optional_label(&mut self) -> Option<&'a str> {
        if let TokenKind::Identifier(label) = self.curr().kind {
            self.next();
            Some(label)
        }
        else {
            None
        }
    }

    fn maybe_consume(&mut self, token: TokenKind) {
        if self.is_token(token) {
            self.next();
        }
    }
    fn consume_token(&mut self, token: TokenKind) {
        assert!(self.is_token(token));
        self.next();
    }
    fn is_token(&self, token: TokenKind) -> bool {
        self.try_curr().map(|t| t.kind) == Some(token)
    }

    fn next(&mut self) {
        self.advance(1);
    }
    fn advance(&mut self, by: usize) {
        self.index += by;
    }
    fn at_end(&self) -> bool {
        self.index >= self.tokens.len()
    }
    fn curr(&self) -> Token<'a> {
        self.peek(0)
    }
    fn peek(&self, offset: usize) -> Token<'a> {
        self.try_peek(offset).unwrap()
    }
    fn try_curr(&self) -> Option<Token<'a>> {
        self.try_peek(0)
    }
    fn try_peek(&self, offset: usize) -> Option<Token<'a>> {
        self.tokens.get(self.index + offset).copied()
    }

    fn curr_span(&self) -> Span {
        self.curr().span
    }
}
