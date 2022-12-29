use crate::lexer::{Ident, Literal, Operator};

#[derive(Debug)]
pub struct File {
    pub items: Vec<Item>,
}

#[derive(Debug)]
pub enum Item {
    Fn(Fn),
}

#[derive(Debug)]
pub struct Fn {
    pub name: Ident,
    pub body: Vec<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Let(LetExpr),
    While(WhileExpr),
    BinOp(BinOpExpr),
    If(IfExpr),
    Var(VarExpr),
    Lit(LitExpr),
    FnCall(FnCallExpr),
}

#[derive(Debug)]
pub struct LetExpr {
    pub name: Ident,
    pub value: Box<Expr>,
}

#[derive(Debug)]
pub struct WhileExpr {
    pub cond: Box<Expr>,
    pub body: Vec<Expr>,
}

#[derive(Debug)]
pub struct BinOpExpr {
    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond: Box<Expr>,
    pub then: Vec<Expr>,
}

#[derive(Debug)]
pub struct VarExpr {
    pub name: Ident,
}

#[derive(Debug)]
pub struct LitExpr {
    pub value: Literal,
}

#[derive(Debug)]
pub struct FnCallExpr {
    pub name: Ident,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Plus,
    Minus,
    PlusEq,
    MinusEq,
    Eq,
    EqEq,
    Lt,
    Gt,
    Lte,
    Gte,
}

impl BinOp {
    pub fn from_operator(op: Operator) -> Self {
        match op {
            Operator::Plus => BinOp::Plus,
            Operator::Minus => BinOp::Minus,
            Operator::PlusEq => BinOp::PlusEq,
            Operator::MinusEq => BinOp::MinusEq,
            Operator::Eq => BinOp::Eq,
            Operator::EqEq => BinOp::EqEq,
            Operator::Lt => BinOp::Lt,
            Operator::Gt => BinOp::Gt,
            Operator::Lte => BinOp::Lte,
            Operator::Gte => BinOp::Gte,
        }
    }
}
