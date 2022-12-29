use std::cmp::Ordering;
use crate::{
    ast::{self, FnCallExpr, Expr, BinOp},
    lexer::{
        Lexer, Token, Keyword, Operator, Punct, TokenKind, Ident,
    },
};
use crate::ast::{LitExpr, VarExpr};

const PEEK_LEN: usize = 3;

struct PeekableLexer<'a> {
    pub peek: [Option<Token>; PEEK_LEN],
    lexer: Lexer<'a>,
}

impl<'a> PeekableLexer<'a> {
    fn new(input: &'a str) -> Self {
        let mut lexer = crate::lexer::lex(input);
        let peek = std::array::from_fn(|_| lexer.next());
        Self { lexer, peek }
    }

    fn bump(&mut self) -> Option<Token> {
        self.peek.rotate_left(1);
        std::mem::replace(self.peek.last_mut().unwrap(), self.lexer.next())
    }

    fn expect_punct(&mut self, punct: Punct) {
        if self.bump().map(|t| t.kind) != Some(TokenKind::Punct(punct)) {
            panic!("Expected {punct:?}");
        }
    }

    fn expect_op(&mut self, op: Operator) {
        if self.bump().map(|t| t.kind) != Some(TokenKind::Operator(op)) {
            panic!("Expected {op:?}");
        }
    }

    fn peek_precedence(&self) -> Option<Precedence> {
        let Some(Token { kind: TokenKind::Operator(op), .. }) = self.peek[0] else {
            return None;
        };
        Some(Precedence::of(BinOp::from_operator(op)))
    }

    fn expect_ident(&mut self) -> Ident {
        match self.bump() {
            Some(Token { kind: TokenKind::Ident(ident), .. }) => ident,
            _ => panic!("Expected identifier"),
        }
    }
}

pub fn parse(input: &str) -> ast::File {
    let mut tokens = PeekableLexer::new(input);

    let funs = vec![
        |i| parse_fn(i).map(ast::Item::Fn),
    ];
    let mut items = vec![];

    while tokens.peek[0].is_some() {
        /*
        for fun in &funs {
            if let Some(item) = fun(&mut tokens) {
                items.push(item);
            }
        }
        */
        if let Some(item) = parse_fn(&mut tokens) {
            items.push(ast::Item::Fn(item));
        }
    }

    ast::File { items }
}

fn parse_fn(input: &mut PeekableLexer) -> Option<ast::Fn> {
    if input.peek[0].as_ref()?.kind != TokenKind::Keyword(Keyword::Fn) {
        return None;
    }
    input.bump(); // eat fn
    let Some(Token { kind: TokenKind::Ident(name), .. }) = input.bump() else {
        panic!("expected identifier after fn keyword");
    };

    input.expect_punct(Punct::LParen);
    input.expect_punct(Punct::RParen);

    Some(ast::Fn { name, body: parse_block(input) })
}

fn parse_unary_expr(tokens: &mut PeekableLexer) -> Option<Expr> {
    let out = match tokens.bump()? {
        Token { kind: TokenKind::Literal(value), .. } => {
            Expr::Lit(LitExpr { value })
        },
        Token { kind: TokenKind::Ident(name), .. } => {
            if tokens.peek[0].as_ref().map(|t| &t.kind) == Some(&TokenKind::Punct(Punct::LParen)) {
                tokens.expect_punct(Punct::LParen);
                tokens.expect_punct(Punct::RParen);
                Expr::FnCall(FnCallExpr { name })
            } else {
                Expr::Var(VarExpr { name })
            }
        },
        _ => return None,
    };

    Some(out)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Precedence {
    // Order is important!
    Any, // Dummy, lower than any other
    Assign, // = or +=
    Or,
    And,
    Compare, // > < >= <= == !=
    Shift, // << >>
    Arithmetic, // + or -
    Term, // * or /
}

impl PartialOrd for Precedence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some((*self as u8).cmp(&(*other as u8)))
    }
}

impl Precedence {
    fn of(op: BinOp) -> Self {
        match op {
            BinOp::Plus | BinOp::Minus => Self::Arithmetic,
            BinOp::PlusEq | BinOp::MinusEq | BinOp::Eq => Self::Assign,
            BinOp::EqEq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => Self::Compare,
        }
    }
}

fn parse_block(input: &mut PeekableLexer) -> Vec<Expr> {
    input.expect_punct(Punct::LBrace);
    let mut body = vec![];
    while input.peek[0].as_ref().map_or(false, |t| t.kind != TokenKind::Punct(Punct::RBrace)) {
        body.push(parse_expr(input));
        input.expect_punct(Punct::Semicolon);
    }
    input.expect_punct(Punct::RBrace);
    body
}

fn parse_expr(input: &mut PeekableLexer) -> Expr {
    if input.peek[0].as_ref().map(|t| &t.kind) == Some(&TokenKind::Keyword(Keyword::Let)) {
        input.bump(); // eat let
        let name = input.expect_ident();
        input.expect_op(Operator::Eq);
        let value = parse_expr(input);
        Expr::Let(ast::LetExpr { name, value: Box::new(value) })
    } else if input.peek[0].as_ref().map(|t| &t.kind) == Some(&TokenKind::Keyword(Keyword::While)) {
        input.bump(); // eat while
        input.expect_punct(Punct::LParen);
        let cond = parse_expr(input);
        input.expect_punct(Punct::RParen);
        Expr::While(ast::WhileExpr { cond: Box::new(cond), body: parse_block(input) })
    } else {
        let lhs = parse_unary_expr(input).expect("expected expr");
        parse_expr_impl(input, lhs, Precedence::Any)
    }
}

fn parse_bin_op(input: &mut PeekableLexer) -> BinOp {
    match input.bump().expect("unexpected EOF").kind {
        TokenKind::Operator(op) => BinOp::from_operator(op),
        _ => panic!("expected sth else"),
    }
}

fn parse_expr_impl(
    input: &mut PeekableLexer,
    mut lhs: Expr,
    base: Precedence,
) -> Expr {
    loop {
        if input.peek_precedence().map_or(false, |p| p >= base) {
            let op = parse_bin_op(input);
            let prec = Precedence::of(op);
            let mut rhs = parse_unary_expr(input).expect("require unary expr");
            while let Some(next_prec) = input.peek_precedence() {
                if next_prec > prec {
                    rhs = parse_expr_impl(input, rhs, next_prec);
                } else {
                    break;
                }
            }

            lhs = Expr::BinOp(ast::BinOpExpr {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            });
        } else {
            break;
        }
    }

    lhs
}
