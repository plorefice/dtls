use std::str;

use chumsky::{prelude::*, Stream};
use derive_more::From;

use crate::lexer::{lexer, Spanned, Token};

mod lexer;
mod parser;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dts(Vec<Spanned<Statement>>);

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum Statement {
    Node(Spanned<Node>),
    Property(Spanned<Property>),
    Directive(Spanned<Directive>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    Version(Version),
    Include(Include),
    DeleteProperty(String),
    DeleteNode(NodeId),
    OmitNode(NodeId),
    MemReserve(u64, u64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    pub id: NodeId,
    pub labels: Vec<String>,
    pub contents: Vec<Statement>,
    pub ommittable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum NodeId {
    Name(NodeName),
    Phandle(Phandle),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeName {
    pub name: String,
    pub address: Option<String>,
}

impl<U, V> From<(U, Option<V>)> for NodeName
where
    U: ToString,
    V: ToString,
{
    fn from((name, address): (U, Option<V>)) -> Self {
        Self {
            name: name.to_string(),
            address: address.map(|v| v.to_string()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum Phandle {
    Label(String),
    Path(Vec<NodeName>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Property {
    pub name: String,
    pub values: Option<Vec<PropertyValue>>,
    pub storage_modifier: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum PropertyValue {
    Str(String),
    Phandle(Phandle),
    Bytestring(Vec<u8>),
    CellArray(Vec<PropertyCell>),
}

impl From<&str> for PropertyValue {
    fn from(s: &str) -> Self {
        Self::from(s.to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum PropertyCell {
    Phandle(Phandle),
    Expr(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Lit(IntLiteral),
    Unary(UnaryOp, Box<Expression>),
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    Ternary {
        cond: Box<Expression>,
        then: Box<Expression>,
        else_: Box<Expression>,
    },
}

impl Expression {
    pub fn eval(&self) -> i64 {
        match self {
            Expression::Lit(l) => l.eval(),
            Expression::Unary(op, e) => op.eval(e),
            Expression::Binary(lhs, op, rhs) => op.eval(lhs, rhs),
            Expression::Ternary { cond, then, else_ } => {
                if cond.eval() != 0 {
                    then.eval()
                } else {
                    else_.eval()
                }
            }
        }
    }
}

impl From<i64> for Expression {
    fn from(i: i64) -> Self {
        Expression::Lit(i.into())
    }
}

impl From<char> for Expression {
    fn from(c: char) -> Self {
        Expression::Lit(c.into())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntLiteral {
    Num(i64),
    Char(char),
}

impl IntLiteral {
    pub fn eval(&self) -> i64 {
        match self {
            IntLiteral::Num(n) => *n,
            IntLiteral::Char(c) => *c as i64,
        }
    }
}

impl From<i64> for IntLiteral {
    fn from(i: i64) -> Self {
        IntLiteral::Num(i)
    }
}

impl From<char> for IntLiteral {
    fn from(c: char) -> Self {
        IntLiteral::Char(c)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    BitNot,
    LogicalNot,
}

impl UnaryOp {
    pub fn eval<E>(&self, e: E) -> i64
    where
        E: AsRef<Expression>,
    {
        let e = e.as_ref();

        match self {
            UnaryOp::Neg => -e.eval(),
            UnaryOp::BitNot => !e.eval(),
            UnaryOp::LogicalNot => (e.eval() == 0) as i64,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    LShift,
    RShift,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
}

impl BinaryOp {
    pub fn eval<L, R>(&self, l: L, r: R) -> i64
    where
        L: AsRef<Expression>,
        R: AsRef<Expression>,
    {
        let l = l.as_ref();
        let r = r.as_ref();

        match self {
            BinaryOp::Add => l.eval() + r.eval(),
            BinaryOp::Sub => l.eval() - r.eval(),
            BinaryOp::Mul => l.eval() * r.eval(),
            BinaryOp::Div => l.eval() / r.eval(),
            BinaryOp::Mod => l.eval() % r.eval(),
            BinaryOp::BitAnd => l.eval() & r.eval(),
            BinaryOp::BitOr => l.eval() | r.eval(),
            BinaryOp::BitXor => l.eval() ^ r.eval(),
            BinaryOp::LShift => l.eval() << r.eval(),
            BinaryOp::RShift => l.eval() >> r.eval(),
            BinaryOp::And => (l.eval() != 0 && r.eval() != 0) as i64,
            BinaryOp::Or => (l.eval() != 0 || r.eval() != 0) as i64,
            BinaryOp::Eq => (l.eval() == r.eval()) as i64,
            BinaryOp::Neq => (l.eval() != r.eval()) as i64,
            BinaryOp::Lt => (l.eval() < r.eval()) as i64,
            BinaryOp::Gt => (l.eval() > r.eval()) as i64,
            BinaryOp::Le => (l.eval() <= r.eval()) as i64,
            BinaryOp::Ge => (l.eval() >= r.eval()) as i64,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Include {
    C(String),
    Dts(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Version {
    V1,
}

/// Parse the contents of a string as a Devicetree file.
pub fn from_str(src: &str) -> (Option<Dts>, Vec<Simple<Token>>) {
    let (tokens, errs) = lexer().parse_recovery(src);

    match tokens {
        Some(tokens) => {
            let len = src.chars().count();
            parser::parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()))
        }
        None => panic!("{errs:?}"),
    }
}
