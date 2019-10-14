mod parser;

pub use parser::*;

#[derive(Debug)]
pub struct Dts {
    includes: Vec<Include>,
    nodes: Vec<Node>,
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
    address: Option<u32>,
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
    CellArray(Vec<PropertyCell>),
}

#[derive(Debug, PartialEq)]
pub enum PropertyCell {
    U32(u32),
    Ref(String),
}
