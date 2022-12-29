use std::fmt;
use super::*;

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Fn(fun) => fun.fmt(f),
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Let(e) => e.fmt(f),
            Self::While(e) => e.fmt(f),
            Self::BinOp(e) => e.fmt(f),
            Self::If(e) => e.fmt(f),
            Self::Var(e) => e.fmt(f),
            Self::Lit(e) => e.fmt(f),
            Self::FnCall(e) => e.fmt(f),
        }
    }
}

impl fmt::Debug for VarExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "VarExpr(\"{}\")", self.name)
    }
}
