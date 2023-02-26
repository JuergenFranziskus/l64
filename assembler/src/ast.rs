use num::BigInt;
use crate::span::Span;


#[derive(Clone, Debug)]
pub struct Program<'a> {
    pub lines: Vec<Line<'a>>,
}


#[derive(Clone, Debug)]
pub struct Line<'a> {
    pub span: Span,
    pub label: Option<&'a str>,
    pub kind: Option<LineKind<'a>>,
}


#[derive(Clone, Debug)]
pub enum LineKind<'a> {
    Equ(Expr<'a>),
    Op(Operation<'a>)
}
impl LineKind<'_> {
    pub fn span(&self) -> Span {
        match self {
            Self::Equ(e) => e.span,
            Self::Op(op) => op.span,
        }
    }
}


#[derive(Clone, Debug)]
pub struct Operation<'a> {
    pub span: Span,
    pub op: OpKind,
    pub args: Vec<Arg<'a>>,
}


#[derive(Clone, Debug)]
pub enum OpKind {
    Mov,
    Store,
    Load,
    Add,
    Jump,
}


#[derive(Clone, Debug)]
pub struct Arg<'a> {
    pub span: Span,
    pub kind: ArgKind<'a>,
}


#[derive(Clone, Debug)]
pub enum ArgKind<'a> {
    Expression(Expr<'a>),
    Register(Register),
    Memory(MemoryArg<'a>),
}

#[derive(Clone, Debug)]
pub struct MemoryArg<'a> {
    pub size: Option<MemorySize>,
    pub base: Option<Register>,
    pub index: Option<Register>,
    pub scale: Option<Expr<'a>>,
    pub offset: Option<Expr<'a>>,
}

#[derive(Copy, Clone, Debug)]
pub enum MemorySize {
    Byte,
    Short,
    Long,
    Word,
}


#[derive(Clone, Debug)]
pub struct Expr<'a> {
    pub span: Span,
    pub kind: ExprKind<'a>,
}

#[derive(Clone, Debug)]
pub enum ExprKind<'a> {
    Label(&'a str),
    Integer(BigInt),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Register(pub u8);
