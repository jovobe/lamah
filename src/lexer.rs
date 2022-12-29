use std::iter::Peekable;
use std::ops::Range;
use std::str::Chars;

pub fn lex<'a>(input: &'a str) -> impl Iterator<Item = Token> + 'a {
    Lexer::new(input)
}

struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    iter: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            iter: input.chars().peekable(),
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.iter.peek().copied()
    }

    fn bump(&mut self) -> Option<char> {
        let out = self.iter.next();
        self.pos += out.map_or(0, |c| c.len_utf8());
        out
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let old_pos = self.pos;
            let c = self.bump()?;

            let punct = TokenKind::Punct;
            let op = TokenKind::Operator;
            let key = TokenKind::Keyword;
            let lit = TokenKind::Literal;

            let kind = match c {
                // Whitespaces and comments
                // TODO: comments
                c if c.is_whitespace() => continue,

                // Punctiation
                '(' => punct(Punct::LParen),
                ')' => punct(Punct::RParen),
                '{' => punct(Punct::LBrace),
                '}' => punct(Punct::RBrace),
                '[' => punct(Punct::LBracket),
                ']' => punct(Punct::RBracket),
                ',' => punct(Punct::Comma),
                '.' => punct(Punct::Dot),
                ':' => punct(Punct::Colon),
                ';' => punct(Punct::Semicolon),

                // Operators
                '+' => match self.peek() {
                    Some('=') => {
                        self.bump();
                        op(Operator::PlusEq)
                    }
                    _ => op(Operator::Plus),
                },
                '-' => match self.peek() {
                    Some('=') => {
                        self.bump();
                        op(Operator::MinusEq)
                    }
                    _ => op(Operator::Minus),
                },
                '=' => match self.peek() {
                    Some('=') => {
                        self.bump();
                        op(Operator::EqEq)
                    }
                    _ => op(Operator::Eq),
                },
                '<' => match self.peek() {
                    Some('=') => {
                        self.bump();
                        op(Operator::Lte)
                    }
                    _ => op(Operator::Lt),
                },
                '>' => match self.peek() {
                    Some('=') => {
                        self.bump();
                        op(Operator::Gte)
                    }
                    _ => op(Operator::Gt),
                },

                // Idents and keywords
                c if is_ident_start(c) => {
                    let mut s = c.to_string();
                    while self.peek().map_or(false, is_ident_continuation) {
                        s.push(self.bump().unwrap());
                    }

                    match s.as_str() {
                        "let" => key(Keyword::Let),
                        "if" => key(Keyword::If),
                        "while" => key(Keyword::While),
                        "fn" => key(Keyword::Fn),
                        "static" => key(Keyword::Static),
                        "struct" => key(Keyword::Struct),
                        _ => TokenKind::Ident(s),
                    }
                },

                // Literals
                '0'..='9' => {
                    let mut s = c.to_string();
                    while self.peek().map_or(false, |c| c.is_digit(10)) {
                        s.push(self.bump().unwrap());
                    }

                    lit(Literal::Int {
                        value: s.parse().unwrap(),
                        ty: None, // TODO
                    })
                }

                _ => panic!("Unexpected character: {}", c),
            };

            return Some(Token {
                kind,
                span: old_pos..self.pos,
            })
        }
    }
}

fn is_ident_start(c: char) -> bool {
    matches!(c, '_' | '@' | 'a'..='z' | 'A'..='Z')
}

fn is_ident_continuation(c: char) -> bool {
    is_ident_start(c) || matches!(c, '0'..='9')
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    span: Range<usize>,
}

#[derive(Debug)]
enum TokenKind {
    Keyword(Keyword),
    Ident(String),
    Punct(Punct),
    Literal(Literal),
    Operator(Operator),
}

#[derive(Debug)]
enum Keyword {
    Let,
    If,
    While,
    Fn,
    Static,
    Struct,
}

#[derive(Debug)]
enum Punct {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Dot,
    Colon,
    Semicolon,
}

#[derive(Debug)]
enum Literal {
    Int {
        value: i32,
        ty: Option<IntType>,
        // TODO: base
    },
}

#[derive(Debug)]
enum IntType {
    U8,
    U16,
    I8,
    I16,
}

#[derive(Debug)]
enum Operator {
    Plus,
    Minus,
    Eq,
    EqEq,
    PlusEq,
    MinusEq,
    Lt,
    Gt,
    Lte,
    Gte,
}
