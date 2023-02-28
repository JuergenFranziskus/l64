use crate::span::Span;
use num::BigInt;

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
    DefineBytes(Vec<Expr<'a>>),
    Op(Operation<'a>),
}
impl LineKind<'_> {
    pub fn span(&self) -> Span {
        match self {
            Self::Equ(e) => e.span,
            Self::DefineBytes(values) => values.iter().map(|v| v.span).reduce(Span::merge).unwrap(),
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

#[derive(Copy, Clone, Debug)]
pub enum OpKind {
    Nop,
    Mov,
    Lea,
    Store,
    Load,
    Add,
    Jump,
    JumpIf,
    CompareNotBelow,
    Call,
}

#[derive(Clone, Debug)]
pub struct Arg<'a> {
    pub span: Span,
    pub kind: ArgKind<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArgKind<'a> {
    Expression(Expr<'a>),
    Register(Register),
    Memory(MemoryArg<'a>),
    Flags,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemoryArg<'a> {
    pub size: Option<MemorySize>,
    pub base: Option<Register>,
    pub index: Option<Register>,
    pub scale: Option<Expr<'a>>,
    pub offset: Option<Expr<'a>>,
}
impl MemoryArg<'_> {
    pub fn size(&self) -> MemorySize {
        self.size.unwrap_or(MemorySize::Word)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum MemorySize {
    Byte,
    Short,
    Long,
    Word,
}
impl MemorySize {
    pub fn bit_pattern(self) -> u64 {
        match self {
            Self::Byte => 0b00,
            Self::Short => 0b01,
            Self::Long => 0b10,
            Self::Word => 0b11,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr<'a> {
    pub span: Span,
    pub kind: ExprKind<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind<'a> {
    Label(&'a str),
    Integer(BigInt),
    String(&'a str),
    Here,
    Binary(Box<Expr<'a>>, BinaryOp, Box<Expr<'a>>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Register(pub u8);
