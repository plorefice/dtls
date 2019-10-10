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

struct Node {
    name: String,
    label: Option<String>,
    props: Vec<Property>,
    children: Vec<Node>,
}

#[derive(Debug)]
struct Property {
    name: String,
    value: PropertyValue,
}

#[derive(Debug)]
enum PropertyValue {
    Empty,
    U32(u32),
    U64(u64),
    Str(String),
    Phandle(String),
    StringList(Vec<PropertyValue>),
    PropEncodedArray(Vec<PropertyValue>),
}
