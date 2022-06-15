pub use parser::*;

mod parser;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dts {
    version: DtsVersion,
    includes: Vec<String>,
    nodes: Vec<Node>,
    deleted_nodes: Vec<NodeId>,
    memreserves: Vec<(u64, u64)>,
}

impl Default for Dts {
    fn default() -> Self {
        Dts {
            version: DtsVersion::V0,
            includes: Vec::new(),
            nodes: Vec::new(),
            deleted_nodes: Vec::new(),
            memreserves: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DtsVersion {
    V0,
    V1,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    id: NodeId,
    labels: Vec<String>,
    contents: NodeContents,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeId {
    Ref(Reference),
    Name(String, Option<String>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reference(String);

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NodeContents {
    props: Vec<Property>,
    children: Vec<Node>,
    includes: Vec<String>,
    deleted_props: Vec<String>,
    deleted_nodes: Vec<NodeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Property {
    name: String,
    value: Option<Vec<PropertyValue>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropertyValue {
    Str(String),
    Ref(Reference),
    Bytestring(Vec<u8>),
    CellArray(Vec<PropertyCell>),
    Bits(u32, Vec<IntegerExpression>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropertyCell {
    Ref(Reference),
    Expr(IntegerExpression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntegerExpression {
    Lit(IntegerLiteral),
    Unary(UnaryOperator, Box<IntegerExpression>),
    Binary(
        Box<IntegerExpression>,
        BinaryOperator,
        Box<IntegerExpression>,
    ),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegerLiteral {
    Num(i64),
    Char(char),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    BitNot,
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
