mod parser;

use parser::*;

struct Dts {
    includes: Vec<Include>,
    nodes: Vec<Node>,
}

#[derive(Debug)]
enum Include {
    Local(String),
    Global(String),
}

#[derive(Debug, PartialEq)]
struct Node {
    name: String,
    label: Option<String>,
    address: Option<u32>,
    props: Vec<Property>,
    children: Vec<Node>,
}

#[derive(Debug, PartialEq)]
struct Property {
    name: String,
    value: Option<Vec<PropertyValue>>,
}

#[derive(Debug, PartialEq)]
enum PropertyValue {
    Str(String),
    CellArray(Vec<PropertyCell>),
}

#[derive(Debug, PartialEq)]
enum PropertyCell {
    U32(u32),
    Ref(String),
}
