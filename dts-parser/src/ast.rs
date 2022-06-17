pub use parser::*;

mod parser;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Root(Vec<RootItem>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RootItem {
    Include(Include),
    Version(DtsVersion),
    Node(Node),
    OmitNode(NodeId),
    DeleteNode(NodeId),
    MemReserve((u64, u64)),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Include {
    C(String),
    Dts(String),
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
    contents: Vec<NodeItem>,
    ommittable: bool,
}

impl Default for Node {
    fn default() -> Self {
        Self {
            id: NodeId::Name(Default::default(), None),
            labels: Default::default(),
            contents: Default::default(),
            ommittable: Default::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeId {
    Ref(Reference),
    Name(String, Option<String>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reference(String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeItem {
    Include(Include),
    Property(Property),
    ChildNode(Node),
    DeletedProp(String),
    DeletedNode(NodeId),
    Invalid,
}

impl From<Include> for NodeItem {
    fn from(i: Include) -> Self {
        Self::Include(i)
    }
}

impl From<Property> for NodeItem {
    fn from(p: Property) -> Self {
        Self::Property(p)
    }
}

impl From<Node> for NodeItem {
    fn from(n: Node) -> Self {
        Self::ChildNode(n)
    }
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
    Bits(u32, Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropertyCell {
    Ref(Reference),
    Expr(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Lit(IntegerLiteral),
    Unary(UnaryOperator, Box<Expression>),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Ternary {
        cond: Box<Expression>,
        left: Box<Expression>,
        right: Box<Expression>,
    },
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
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
}
