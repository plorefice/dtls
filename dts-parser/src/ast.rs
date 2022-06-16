#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dts {
    pub version: DtsVersion,
    pub includes: Vec<String>,
    pub nodes: Vec<Node>,
    pub deleted_nodes: Vec<NodeId>,
    pub omitted_nodes: Vec<NodeId>,
    pub memreserves: Vec<(u64, u64)>,
}

impl Default for Dts {
    fn default() -> Self {
        Dts {
            version: DtsVersion::V0,
            includes: Vec::new(),
            nodes: Vec::new(),
            deleted_nodes: Vec::new(),
            omitted_nodes: Vec::new(),
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
    pub id: NodeId,
    pub labels: Vec<String>,
    pub contents: NodeContents,
    pub omit_if_no_ref: bool,
}

impl Default for Node {
    fn default() -> Self {
        Self {
            id: NodeId::Name(String::new(), None),
            labels: Default::default(),
            contents: Default::default(),
            omit_if_no_ref: Default::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeId {
    Ref(Reference),
    Name(String, Option<String>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reference(pub String);

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NodeContents {
    pub props: Vec<Property>,
    pub children: Vec<Node>,
    pub includes: Vec<String>,
    pub deleted_props: Vec<String>,
    pub deleted_nodes: Vec<NodeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Property {
    pub name: String,
    pub value: Option<Vec<PropertyValue>>,
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
