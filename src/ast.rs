pub use parser::*;

mod parser;

#[derive(Debug, PartialEq)]
pub struct Dts {
    version: DtsVersion,
    includes: Vec<Include>,
    nodes: Vec<Node>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum DtsVersion {
    V0,
    V1,
}

#[derive(Debug, PartialEq)]
pub enum Include {
    Local(String),
    Global(String),
}

#[derive(Debug, PartialEq)]
pub struct Node {
    name: String,
    label: Option<String>,
    address: Option<String>,
    props: Vec<Property>,
    children: Vec<Node>,
}

#[derive(Debug, PartialEq)]
pub struct Property {
    name: String,
    value: Option<Vec<PropertyValue>>,
}

#[derive(Debug, PartialEq)]
pub enum PropertyValue {
    Str(String),
    Alias(String),
    CellArray(Vec<PropertyCell>),
}

#[derive(Debug, PartialEq)]
pub enum PropertyCell {
    Ref(String),
    Expr(IntegerExpression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntegerExpression {
    Lit(u32),
    Binary(
        Box<IntegerExpression>,
        BinaryOperator,
        Box<IntegerExpression>,
    ),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    LShift,
    RShift,
    BitAnd,
    BitOr,
    BitXor,
}
