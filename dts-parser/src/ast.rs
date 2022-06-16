#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Root<'s>(pub(crate) Vec<RootItem<'s>>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RootItem<'s> {
    Include(Include<'s>),
    Version(DtsVersion),
    Node(Node<'s>),
    OmitNode(NodeId<'s>),
    DeleteNode(NodeId<'s>),
    MemReserve((u64, u64)),
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
    pub contents: Vec<NodeItem<'s>>,
    pub ommittable: bool,
}

impl<'s> Default for Node<'s> {
    fn default() -> Self {
        Self {
            id: NodeId::Name("", None),
            labels: Default::default(),
            contents: Default::default(),
            ommittable: Default::default(),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeItem<'s> {
    Include(Include<'s>),
    Property(Property<'s>),
    ChildNode(Node<'s>),
    DeletedProp(&'s str),
    DeletedNode(NodeId<'s>),
}

impl<'s> From<Include<'s>> for NodeItem<'s> {
    fn from(i: Include<'s>) -> Self {
        Self::Include(i)
    }
}

impl<'s> From<Property<'s>> for NodeItem<'s> {
    fn from(p: Property<'s>) -> Self {
        Self::Property(p)
    }
}

impl<'s> From<Node<'s>> for NodeItem<'s> {
    fn from(n: Node<'s>) -> Self {
        Self::ChildNode(n)
    }
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
    Bits(u32, Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropertyCell<'s> {
    Ref(Reference<'s>),
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
