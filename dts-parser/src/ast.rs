#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dts<'s> {
    pub includes: Vec<Include<'s>>,
    pub version: DtsVersion,
    pub nodes: Vec<Node<'s>>,
    pub deleted_nodes: Vec<NodeId<'s>>,
    pub omitted_nodes: Vec<NodeId<'s>>,
    pub memreserves: Vec<(u64, u64)>,
}

impl<'s> Default for Dts<'s> {
    fn default() -> Self {
        Dts {
            includes: Vec::new(),
            version: DtsVersion::V0,
            nodes: Vec::new(),
            deleted_nodes: Vec::new(),
            omitted_nodes: Vec::new(),
            memreserves: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Include<'s> {
    C(&'s str),
    Dts(&'s str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DtsVersion {
    V0,
    V1,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node<'s> {
    pub id: NodeId<'s>,
    pub labels: Vec<&'s str>,
    pub contents: NodeContents<'s>,
    pub omit_if_no_ref: bool,
}

impl<'s> Default for Node<'s> {
    fn default() -> Self {
        Self {
            id: NodeId::Name("", None),
            labels: Default::default(),
            contents: Default::default(),
            omit_if_no_ref: Default::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeId<'s> {
    Ref(Reference<'s>),
    Name(&'s str, Option<&'s str>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reference<'s>(pub &'s str);

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NodeContents<'s> {
    pub includes: Vec<Include<'s>>,
    pub props: Vec<Property<'s>>,
    pub children: Vec<Node<'s>>,
    pub deleted_props: Vec<&'s str>,
    pub deleted_nodes: Vec<NodeId<'s>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Property<'s> {
    pub name: &'s str,
    pub value: Option<Vec<PropertyValue<'s>>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropertyValue<'s> {
    Str(&'s str),
    Ref(Reference<'s>),
    Bytestring(Vec<u8>),
    CellArray(Vec<PropertyCell<'s>>),
    Bits(u32, Vec<IntegerExpression>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropertyCell<'s> {
    Ref(Reference<'s>),
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
