use logos::{Logos, Lexer};
use crate::span::Span;


pub fn lex(src: &str) -> Vec<Token> {
    let lexer: Lexer<TokenKind> = Lexer::new(src);

    lexer.spanned()
        .map(|(t, s)| Token { span: Span::from_logos(s), kind: t })
        .collect()
}



#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Token<'a> {
    pub span: Span,
    pub kind: TokenKind<'a>,
}


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[derive(Logos)]
pub enum TokenKind<'a> {
    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("[")]
    OpenBracket,

    #[token("]")]
    CloseBracket,

    #[token("jmp")]
    Jump,

    #[token("load")]
    Load,

    #[token("store")]
    Store,

    #[token("add")]
    Add,
    #[token("mov")]
    Mov,


    #[token("byte")]
    Byte,

    #[token("short")]
    Short,

    #[token("long")]
    Long,

    #[token("word")]
    Word,

    #[token("equ")]
    Equ,


    #[regex(r"r[0-9]?[0-9]|rip")]
    Register(&'a str),

    #[regex(r"[_a-zA-Z][0-9a-zA-Z_]*")]
    Identifier(&'a str),

    #[regex(r"-?[0-9][_0-9]*")]
    Decimal(&'a str),

    #[regex(r"-?0[xX][0-9a-fA-F]+")]
    Hex(&'a str),


    #[regex(r"\n")]
    Newline,

    #[error]
    #[regex(r"[ \t\r\f]+", logos::skip)]
    Error,
}
