use std::str;

use nom::{
    branch::alt,
    bytes::complete::{escaped, is_a, is_not, tag, take_while_m_n},
    character::complete::{
        alphanumeric1, anychar, char, hex_digit1, i64, line_ending, multispace1, one_of, space1,
        u64,
    },
    combinator::{all_consuming, cut, map, opt, recognize},
    error::{convert_error, ParseError, VerboseError},
    multi::{many0, many1, many_m_n, many_till, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    Finish,
};

use crate::ast::*;

type Input<'a> = &'a str;

type IResult<'a, T, E> = nom::IResult<Input<'a>, T, E>;

/// Parse a Device Tree from a string.
pub fn from_str(s: &str) -> Result<Dts, String> {
    match all_consuming(dts_file::<VerboseError<&str>>)(s).finish() {
        Ok((_, dts)) => Ok(dts),
        Err(e) => Err(convert_error(s, e)),
    }
}

/// Parse a Device Tree source file.
fn dts_file<'a, E>(input: Input<'a>) -> IResult<'a, Dts, E>
where
    E: ParseError<Input<'a>>,
{
    enum TopLevelContents<'s> {
        Node(Node<'s>),
        Include(&'s str),
        VersionDirective,
        DeletedNodeDirective(NodeId<'s>),
        OmitIfNoRefDirective(NodeId<'s>),
        MemreserveDirective((u64, u64)),
    }

    let root_node = map(root_node, TopLevelContents::Node);
    let node_override = map(node_override, TopLevelContents::Node);
    let version_directive = map(version_directive, |_| TopLevelContents::VersionDirective);
    let include_directive = map(include_directive, TopLevelContents::Include);
    let deleted_node_directive = map(deleted_node, TopLevelContents::DeletedNodeDirective);
    let omit_if_no_ref_directive = map(omit_if_no_ref, TopLevelContents::OmitIfNoRefDirective);
    let memreserve_directive = map(memreserve, TopLevelContents::MemreserveDirective);

    map(
        many0(alt((
            root_node,
            node_override,
            version_directive,
            include_directive,
            deleted_node_directive,
            omit_if_no_ref_directive,
            memreserve_directive,
        ))),
        move |contents| {
            let mut dts = Dts::default();

            for content in contents {
                match content {
                    TopLevelContents::Node(node) => dts.nodes.push(node),
                    TopLevelContents::Include(inc) => dts.includes.push(inc),
                    TopLevelContents::VersionDirective => dts.version = DtsVersion::V1,
                    TopLevelContents::DeletedNodeDirective(n) => dts.deleted_nodes.push(n),
                    TopLevelContents::OmitIfNoRefDirective(n) => dts.omitted_nodes.push(n),
                    TopLevelContents::MemreserveDirective(m) => dts.memreserves.push(m),
                }
            }

            dts
        },
    )(input)
}

/// Parse a version directive.
fn version_directive<'a, E>(input: Input<'a>) -> IResult<'a, DtsVersion, E>
where
    E: ParseError<Input<'a>>,
{
    map(terminated(dts_v1_keyword, cut(terminator)), |_| {
        DtsVersion::V1
    })(input)
}

/// Parse a valid root node.
///
/// The root node is a top-level named node in the file and its name should always be '/'.
fn root_node<'a, E>(input: Input<'a>) -> IResult<'a, Node, E>
where
    E: ParseError<Input<'a>>,
{
    map(
        tuple((root_node_name, node_body, cut(terminator))),
        |(name, contents, _)| Node {
            id: NodeId::Name(name, None),
            contents,
            ..Default::default()
        },
    )(input)
}

/// Parse a valid node override.
///
/// Node overrides are only valid in the top-level of the file and their name should be
/// a valid node reference.
fn node_override<'a, E>(input: Input<'a>) -> IResult<'a, Node, E>
where
    E: ParseError<Input<'a>>,
{
    map(
        tuple((
            opt(omit_if_no_ref_keyword),
            node_labels,
            node_reference,
            node_body,
            cut(terminator),
        )),
        |(omit, labels, reference, contents, _)| Node {
            id: NodeId::Ref(reference),
            labels,
            contents,
            omit_if_no_ref: omit.is_some(),
        },
    )(input)
}

/// Parse a valid inner node.
///
/// Inner nodes are only valid within the body of a node. Their name should be a valid node name.
fn inner_node<'a, E>(input: Input<'a>) -> IResult<'a, Node, E>
where
    E: ParseError<Input<'a>>,
{
    map(
        tuple((
            opt(omit_if_no_ref_keyword),
            node_labels,
            node_name,
            node_body,
            cut(terminator),
        )),
        |(omit, labels, name, contents, _)| Node {
            id: name,
            labels,
            contents,
            omit_if_no_ref: omit.is_some(),
        },
    )(input)
}

/// Recognize the name of a root node.
fn root_node_name<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(tag("/"))(input)
}

/// Parse the body of a device tree node.
fn node_body<'a, E>(input: Input<'a>) -> IResult<'a, NodeContents, E>
where
    E: ParseError<Input<'a>>,
{
    preceded(braces_start, cut(terminated(node_contents, braces_end)))(input)
}

/// Parse a list of node labels.
fn node_labels<'a, E>(input: Input<'a>) -> IResult<'a, Vec<&'a str>, E>
where
    E: ParseError<Input<'a>>,
{
    many0(terminated(node_label, label_separator))(input)
}

/// Parse a node label.
fn node_label<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(node_label_str)(input)
}

/// Parse the contents of a node.
fn node_contents<'a, E>(input: Input<'a>) -> IResult<'a, NodeContents, E>
where
    E: ParseError<Input<'a>>,
{
    enum NodeContent<'s> {
        Node(Node<'s>),
        Prop(Property<'s>),
        Include(&'s str),
        DeletedProp(&'s str),
        DeletedNode(NodeId<'s>),
    }

    // A node can contain 0+ properties and 0+ child nodes.
    // To make the parsing easier, wrap them both in a `NodeContent` enum
    // and split them later in the closure.
    map(
        many0(alt((
            map(inner_node, NodeContent::Node),
            map(property, NodeContent::Prop),
            map(include_directive, NodeContent::Include),
            map(deleted_property, NodeContent::DeletedProp),
            map(deleted_node, NodeContent::DeletedNode),
        ))),
        |contents| {
            contents
                .into_iter()
                .fold(NodeContents::default(), |mut contents, elem| {
                    match elem {
                        NodeContent::Prop(p) => contents.props.push(p),
                        NodeContent::Node(c) => contents.children.push(c),
                        NodeContent::Include(c) => contents.includes.push(c),
                        NodeContent::DeletedProp(c) => contents.deleted_props.push(c),
                        NodeContent::DeletedNode(c) => contents.deleted_nodes.push(c),
                    };
                    contents
                })
        },
    )(input)
}

/// Parse a node property.
fn property<'a, E>(input: Input<'a>) -> IResult<'a, Property, E>
where
    E: ParseError<Input<'a>>,
{
    map(
        tuple((
            prop_name,
            opt(preceded(assignment, cut(prop_values))),
            cut(terminator),
        )),
        |(name, value, _)| Property { name, value },
    )(input)
}

/// Parse a propery name.
fn prop_name<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(prop_name_str)(input)
}

/// Parse a property value list.
fn prop_values<'a, E>(input: Input<'a>) -> IResult<'a, Vec<PropertyValue>, E>
where
    E: ParseError<Input<'a>>,
{
    separated_list1(
        list_separator,
        alt((
            prop_value_bits,
            prop_value_cell_array,
            prop_value_bytestring,
            prop_value_alias,
            prop_value_str,
        )),
    )(input)
}

/// Parse a property value corresponding to the `/bits/` keyword followed by its arguments.
fn prop_value_bits<'a, E>(input: Input<'a>) -> IResult<'a, PropertyValue, E>
where
    E: ParseError<Input<'a>>,
{
    map(
        tuple((
            bits_keyword,
            cut(dec),
            cut(delimited(
                cell_array_start,
                many1(integer_expr),
                cell_array_end,
            )),
        )),
        |(_, n, bits)| PropertyValue::Bits(n as u32, bits),
    )(input)
}

/// Parse a property value corresponding to a reference to another node.
fn prop_value_alias<'a, E>(input: Input<'a>) -> IResult<'a, PropertyValue, E>
where
    E: ParseError<Input<'a>>,
{
    map(node_reference, PropertyValue::Ref)(input)
}

/// Parse a property value corresponding to a string.
fn prop_value_str<'a, E>(input: Input<'a>) -> IResult<'a, PropertyValue, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(map(string_literal, PropertyValue::Str))(input)
}

/// Parse a property value corresponding to a cell array.
///
/// A cell array can be empty.
fn prop_value_cell_array<'a, E>(input: Input<'a>) -> IResult<'a, PropertyValue, E>
where
    E: ParseError<Input<'a>>,
{
    map(
        preceded(
            cell_array_start,
            cut(terminated(
                many0(alt((prop_cell_expr, prop_cell_ref))),
                cell_array_end,
            )),
        ),
        PropertyValue::CellArray,
    )(input)
}

/// Parse a property value corresponding to a byte string.
fn prop_value_bytestring<'a, E>(input: Input<'a>) -> IResult<'a, PropertyValue, E>
where
    E: ParseError<Input<'a>>,
{
    map(
        preceded(
            bracket_start,
            cut(terminated(many1(lexeme(hex_byte)), bracket_end)),
        ),
        PropertyValue::Bytestring,
    )(input)
}

/// Parse a property cell containing a reference to another node.
fn prop_cell_ref<'a, E>(input: Input<'a>) -> IResult<'a, PropertyCell, E>
where
    E: ParseError<Input<'a>>,
{
    map(node_reference, PropertyCell::Ref)(input)
}

/// Parse a property cell containing an integer expression.
fn prop_cell_expr<'a, E>(input: Input<'a>) -> IResult<'a, PropertyCell, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(map(integer_expr, PropertyCell::Expr))(input)
}

/// Parse a deleted node.
fn deleted_node<'a, E>(input: Input<'a>) -> IResult<'a, NodeId, E>
where
    E: ParseError<Input<'a>>,
{
    delimited(
        delete_node_keyword,
        cut(alt((node_name, map(node_reference, NodeId::Ref)))),
        cut(terminator),
    )(input)
}

/// Parse a deleted property.
fn deleted_property<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    delimited(delete_property_keyword, cut(prop_name), cut(terminator))(input)
}

/// Parse a valid memreserve directive.
fn memreserve<'a, E>(input: Input<'a>) -> IResult<'a, (u64, u64), E>
where
    E: ParseError<Input<'a>>,
{
    delimited(
        memreserve_keyword,
        cut(pair(unsigned_literal, unsigned_literal)),
        cut(terminator),
    )(input)
}

/// Parse a valid omitted node directive.
fn omit_if_no_ref<'a, E>(input: Input<'a>) -> IResult<'a, NodeId, E>
where
    E: ParseError<Input<'a>>,
{
    delimited(
        omit_if_no_ref_keyword,
        cut(alt((node_name, map(node_reference, NodeId::Ref)))),
        cut(terminator),
    )(input)
}

/// Parse a valid node reference.
fn node_reference<'a, E>(input: Input<'a>) -> IResult<'a, Reference, E>
where
    E: ParseError<Input<'a>>,
{
    let node_label = map(node_label_str, Reference);
    let node_path = map(delimited(braces_start, node_path, braces_end), Reference);

    preceded(reference_operator, cut(alt((node_label, node_path))))(input)
}

/// Parse a valid node name.
///
/// A node name is composed of node-name part and an optional unit-address in hex format.
fn node_name<'a, E>(input: Input<'a>) -> IResult<'a, NodeId, E>
where
    E: ParseError<Input<'a>>,
{
    map(
        tuple((node_name_identifier, opt(node_address_identifier))),
        |(name, address)| NodeId::Name(name, address),
    )(input)
}

/// Parse a valid node name identifier, i.e. the part of the node name before the unit-address.
fn node_name_identifier<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(node_name_str)(input)
}

/// Parse a valid node unit-address identifier.
fn node_address_identifier<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    preceded(char('@'), cut(node_name_str))(input)
}

/// Parse a valid top-level integer expression in a property cell.
///
/// Valid expressions include a single integer literal (e.g. `<0>`) or a parenthesized expression
/// (e.g. `<(1 << 1)>`).
fn integer_expr<'a, E>(input: Input<'a>) -> IResult<'a, IntegerExpression, E>
where
    E: ParseError<Input<'a>>,
{
    alt((integer_expr_lit, integer_expr_parens))(input)
}

/// Parse a valid integer literal expression in a property cell.
fn integer_expr_lit<'a, E>(input: Input<'a>) -> IResult<'a, IntegerExpression, E>
where
    E: ParseError<Input<'a>>,
{
    map(integer_literal, IntegerExpression::Lit)(input)
}

/// Parse a valid parenthesized integer expression in a property cell.
///
/// Parenthesized expressions may contain a single term or an inner parenthesized expression.
fn integer_expr_parens<'a, E>(input: Input<'a>) -> IResult<'a, IntegerExpression, E>
where
    E: ParseError<Input<'a>>,
{
    preceded(
        paren_start,
        cut(terminated(
            alt((integer_expr_term, integer_expr_parens)),
            paren_end,
        )),
    )(input)
}

/// Parse a valid integer expression term in a property cell.
///
/// A term may be a single integer literal, a binary operator applied to two terms
/// or a unary operator applied to a single term.
fn integer_expr_term<'a, E>(input: Input<'a>) -> IResult<'a, IntegerExpression, E>
where
    E: ParseError<Input<'a>>,
{
    alt((integer_expr_binary, integer_expr_unary, integer_expr_lit))(input)
}

/// Parse a valid binary integer expression term in a property cell.
fn integer_expr_binary<'a, E>(input: Input<'a>) -> IResult<'a, IntegerExpression, E>
where
    E: ParseError<Input<'a>>,
{
    map(
        tuple((
            integer_expr,
            arith_operator_binary,
            cut(alt((integer_expr_term, integer_expr_parens))),
        )),
        |(left, op, right)| IntegerExpression::Binary(Box::new(left), op, Box::new(right)),
    )(input)
}

/// Parse a valid unary integer expression term in a property cell.
fn integer_expr_unary<'a, E>(input: Input<'a>) -> IResult<'a, IntegerExpression, E>
where
    E: ParseError<Input<'a>>,
{
    map(
        tuple((arith_operator_unary, cut(integer_expr))),
        |(op, right)| IntegerExpression::Unary(op, Box::new(right)),
    )(input)
}

fn integer_literal<'a, E>(input: Input<'a>) -> IResult<'a, IntegerLiteral, E>
where
    E: ParseError<Input<'a>>,
{
    alt((
        map(numeric_literal, IntegerLiteral::Num),
        map(char_literal, IntegerLiteral::Char),
    ))(input)
}

/// Parse a valid unsigned integer number in any base.
fn unsigned_literal<'a, E>(input: Input<'a>) -> IResult<'a, u64, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(alt((unsigned_hex, unsigned_dec)))(input)
}

/// Parse a valid signed integer number in any base.
fn numeric_literal<'a, E>(input: Input<'a>) -> IResult<'a, i64, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(alt((hex, dec)))(input)
}

/// Parse a valid character literal.
fn char_literal<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    delimited(char('\''), cut(anychar), cut(char('\'')))(input)
}

/// Parse a valid include directive.
fn include_directive<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    preceded(
        include_keyword,
        cut(delimited(double_quote, include_path_str, double_quote)),
    )(input)
}

/// Parse a valid string literal.
fn string_literal<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    preceded(double_quote, cut(terminated(printable_ascii, double_quote)))(input)
}

/* === Low-level syntax parsers === */

/// Recognize a double-quote character.
fn double_quote<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(char('"'))(input)
}

/// Recognize an assigment operator.
fn assignment<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(char('='))(input)
}

/// Recognize a statement terminator.
fn terminator<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(char(';'))(input)
}

/// Recognize a list separator.
fn list_separator<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(char(','))(input)
}

/// Recognize a label separator.
fn label_separator<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(char(':'))(input)
}

/// Recognize the start of a brace.
fn braces_start<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(char('{'))(input)
}

/// Recognize the end of a brace.
fn braces_end<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(char('}'))(input)
}

/// Recognize the start of a cell array.
fn cell_array_start<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(char('<'))(input)
}

/// Recognize the end of a cell array.
fn cell_array_end<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(char('>'))(input)
}

/// Recognize the start of a parenthesis.
fn paren_start<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(char('('))(input)
}

/// Recognize the end of a parenthesis.
fn paren_end<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(char(')'))(input)
}

/// Recognize the start of a bracket.
fn bracket_start<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(char('['))(input)
}

/// Recognize the end of a bracket.
fn bracket_end<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(char(']'))(input)
}

/// Recognize a reference operator.
fn reference_operator<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(char('&'))(input)
}

/// Recognize a path separator.
fn path_separator<'a, E>(input: Input<'a>) -> IResult<'a, char, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(char('/'))(input)
}

/// Recognize an arithmetic unary operator.
fn arith_operator_unary<'a, E>(input: Input<'a>) -> IResult<'a, UnaryOperator, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(map(tag("~"), |_| UnaryOperator::BitNot))(input)
}

/// Recognize an arithmetic binary operator.
fn arith_operator_binary<'a, E>(input: Input<'a>) -> IResult<'a, BinaryOperator, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(alt((
        map(tag("+"), |_| BinaryOperator::Add),
        map(tag("-"), |_| BinaryOperator::Sub),
        map(tag("*"), |_| BinaryOperator::Mul),
        map(tag("/"), |_| BinaryOperator::Div),
        map(tag("<<"), |_| BinaryOperator::LShift),
        map(tag(">>"), |_| BinaryOperator::RShift),
        map(tag("&"), |_| BinaryOperator::BitAnd),
        map(tag("|"), |_| BinaryOperator::BitOr),
        map(tag("^"), |_| BinaryOperator::BitXor),
    )))(input)
}

/// Parse a signed integer number in base 16, prefixed by `0x`.
fn hex<'a, E>(input: Input<'a>) -> IResult<'a, i64, E>
where
    E: ParseError<Input<'a>>,
{
    map(unsigned_hex, |d| d as i64)(input)
}

/// Parse an usigned integer number in base 16, prefixed by `0x`.
fn unsigned_hex<'a, E>(input: Input<'a>) -> IResult<'a, u64, E>
where
    E: ParseError<Input<'a>>,
{
    map(
        preceded(alt((tag("0x"), tag("0X"))), cut(hex_digit1)),
        |s: &str| u64::from_str_radix(s, 16).unwrap(),
    )(input)
}

/// Parse a signed integer number in base 10.
fn dec<'a, E>(input: Input<'a>) -> IResult<'a, i64, E>
where
    E: ParseError<Input<'a>>,
{
    i64(input)
}

/// Parse an unsigned integer number in base 10.
fn unsigned_dec<'a, E>(input: Input<'a>) -> IResult<'a, u64, E>
where
    E: ParseError<Input<'a>>,
{
    u64(input)
}

/// Parse a byte represented by two hex digits.
fn hex_byte<'a, E>(input: Input<'a>) -> IResult<'a, u8, E>
where
    E: ParseError<Input<'a>>,
{
    map(take_while_m_n(2, 2, |c: char| c.is_digit(16)), |s: &str| {
        u8::from_str_radix(s, 16).unwrap()
    })(input)
}

/// Recognize a sequence of printable ASCII characters.
fn printable_ascii<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    recognize(many0(escaped(
        alt((
            alphanumeric1,
            space1,
            is_a("!#$%&'()*+,-./:;<=>?@[]^_`{|}~"),
        )),
        '\\',
        one_of("\\\""),
    )))(input)
}

/// Recognize a valid path in an `/include/` directive.
fn include_path_str<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    recognize(separated_list1(path_separator, is_not("/\0\"<>")))(input)
}

/// Recognize a valid node name string.
fn node_name_str<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    recognize(many_m_n(1, 31, alt((alphanumeric1, is_a(",._+-")))))(input)
}

/// Recognize a valid node label string.
fn node_label_str<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    recognize(many_m_n(1, 31, alt((alphanumeric1, is_a("_")))))(input)
}

/// Recognize a valid node path.
fn node_path<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    recognize(preceded(
        path_separator,
        separated_list1(path_separator, node_name),
    ))(input)
}

/// Recognize a valid property name string.
fn prop_name_str<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    recognize(many_m_n(1, 31, alt((alphanumeric1, is_a(",._+?#-")))))(input)
}

/// Recognize an include directive prefix.
fn include_keyword<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(tag("/include/"))(input)
}

/// Recognize the `/bits/` keyword.
fn bits_keyword<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(tag("/bits/"))(input)
}

/// Recognize the `/memreserve/` keyword.
fn memreserve_keyword<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(tag("/memreserve/"))(input)
}

/// Recognize the `/delete-node/` keyword.
fn delete_node_keyword<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(tag("/delete-node/"))(input)
}

/// Recognize the `/delete-property/` keyword.
fn delete_property_keyword<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(tag("/delete-property/"))(input)
}

/// Recognize the `/omit-if-no-ref/` keyword.
fn omit_if_no_ref_keyword<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(tag("/omit-if-no-ref/"))(input)
}

/// Recognize the `/dts-v1/` keyword.
fn dts_v1_keyword<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    lexeme(tag("/dts-v1/"))(input)
}

/* === Utility functions === */

/// Parse a lexeme using the combinator passed as its argument,
/// also consuming any whitespaces or comments before or after.
fn lexeme<'a, O, F, E>(f: F) -> impl FnMut(&'a str) -> IResult<'a, O, E>
where
    E: ParseError<Input<'a>>,
    F: FnMut(&'a str) -> IResult<'a, O, E>,
{
    delimited(ws, f, ws)
}

/// Consume zero or more whitespace characters or comments.
fn ws<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    recognize(many0(alt((multispace1, line_comment, block_comment))))(input)
}

/// Parse block comments.
fn block_comment<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    recognize(preceded(tag("/*"), many_till(anychar, tag("*/"))))(input)
}

/// Parse a single line comment.
///
/// The parser stops just before the newline character but doesn't consume the newline.
fn line_comment<'a, E>(input: Input<'a>) -> IResult<'a, &'a str, E>
where
    E: ParseError<Input<'a>>,
{
    recognize(preceded(tag("//"), many_till(anychar, line_ending)))(input)
}

/* === Unit Tests === */

#[cfg(test)]
mod tests {
    use super::*;

    use nom::{
        error::{convert_error, VerboseError},
        Finish,
    };

    #[test]
    fn parse_escaped_strings() {
        for (input, exp) in [(r#""Escaped string: \"\\\"""#, r#"Escaped string: \"\\\""#)] {
            match string_literal::<VerboseError<&str>>(input).finish() {
                Ok(res) => assert_eq!(res, ("", exp)),
                Err(e) => panic!("{}", convert_error(input, e)),
            }
        }
    }

    #[test]
    fn parse_line_comments() {
        assert_eq!(
            line_comment::<VerboseError<&str>>("// This is a comment\n"),
            Ok(("", "// This is a comment\n"))
        );

        assert_eq!(
            line_comment::<VerboseError<&str>>("// Multiline comments\nare not supported"),
            Ok(("are not supported", "// Multiline comments\n"))
        );

        assert!(
            line_comment::<VerboseError<&str>>(r#"prop-name = "value"; // This is a comment"#)
                .is_err()
        );
    }

    #[test]
    fn parse_node_names() {
        for (input, exp) in [
            ("cpus", NodeId::Name("cpus", None)),
            ("cpu@0", NodeId::Name("cpu", Some("0"))),
            ("l2-cache", NodeId::Name("l2-cache", None)),
            ("open-pic", NodeId::Name("open-pic", None)),
            ("soc_gpio1", NodeId::Name("soc_gpio1", None)),
            ("memory@0", NodeId::Name("memory", Some("0"))),
            ("uart@fe001000", NodeId::Name("uart", Some("fe001000"))),
        ] {
            match node_name::<VerboseError<&str>>(input).finish() {
                Ok(res) => assert_eq!(res, ("", exp)),
                Err(e) => panic!("{}", convert_error(input, e),),
            }
        }
    }

    #[test]
    fn parse_node_labels() {
        for label in ["L3", "L2_0", "L2_1", "mmc0", "eth0", "pinctrl_wifi_pin"] {
            match node_label::<VerboseError<&str>>(label).finish() {
                Ok(res) => assert_eq!(res, ("", label)),
                Err(e) => panic!("{}", convert_error(label, e),),
            }
        }
    }

    #[test]
    fn parse_prop_names() {
        for name in [
            "reg",
            "status",
            "compatible",
            "device_type",
            "#size-cells",
            "#address-cells",
            "interrupt-controller",
            "fsl,channel-fifo-len",
            "ibm,ppc-interrupt-server#s",
            "linux,network-index",
        ] {
            match prop_name::<VerboseError<&str>>(name).finish() {
                Ok(res) => assert_eq!(res, ("", name)),
                Err(e) => panic!("{}", convert_error(name, e),),
            }
        }
    }

    #[test]
    fn parse_properties() {
        use IntegerExpression::*;
        use IntegerLiteral::*;
        use PropertyCell::*;
        use PropertyValue::*;

        for (input, prop) in [
            (
                r#"device_type = "cpu";"#,
                Property {
                    name: "device_type",
                    value: Some(vec![Str("cpu")]),
                },
            ),
            (
                r#"compatible = "ns16550", "ns8250";"#,
                Property {
                    name: "compatible",
                    value: Some(vec![Str("ns16550"), Str("ns8250")]),
                },
            ),
            (
                r#"example = <&mpic 0xf00f0000 19>, "a strange property format";"#,
                Property {
                    name: "example",
                    value: Some(vec![
                        CellArray(vec![
                            PropertyCell::Ref(Reference("mpic")),
                            Expr(Lit(Num(0xf00f_0000))),
                            Expr(Lit(Num(19))),
                        ]),
                        Str("a strange property format"),
                    ]),
                },
            ),
            (
                r#"reg = <0>;"#,
                Property {
                    name: "reg",
                    value: Some(vec![CellArray(vec![Expr(Lit(Num(0)))])]),
                },
            ),
            (
                r#"cache-unified;"#,
                Property {
                    name: "cache-unified",
                    value: None,
                },
            ),
            (
                r#"cache-size = <0x8000>;"#,
                Property {
                    name: "cache-size",
                    value: Some(vec![CellArray(vec![Expr(Lit(Num(0x8000)))])]),
                },
            ),
            (
                r#"next-level-cache = <&L2_0>;"#,
                Property {
                    name: "next-level-cache",
                    value: Some(vec![CellArray(vec![PropertyCell::Ref(Reference("L2_0"))])]),
                },
            ),
            (
                r#"interrupts = <17 0xc 'A'>;"#,
                Property {
                    name: "interrupts",
                    value: Some(vec![CellArray(vec![
                        Expr(Lit(Num(17))),
                        Expr(Lit(Num(0xc))),
                        Expr(Lit(Char('A'))),
                    ])]),
                },
            ),
            (
                r#"serial0 = &usart3;"#,
                Property {
                    name: "serial0",
                    value: Some(vec![PropertyValue::Ref(Reference("usart3"))]),
                },
            ),
            (
                r#"cpu = <&{/cpus/cpu@0}>;"#,
                Property {
                    name: "cpu",
                    value: Some(vec![CellArray(vec![PropertyCell::Ref(Reference(
                        "/cpus/cpu@0",
                    ))])]),
                },
            ),
            (
                r#"pinctrl-0 = <>;"#,
                Property {
                    name: "pinctrl-0",
                    value: Some(vec![PropertyValue::CellArray(vec![])]),
                },
            ),
        ] {
            match property::<VerboseError<&str>>(input).finish() {
                Ok(res) => assert_eq!(res, ("", (prop))),
                Err(e) => panic!("{}", convert_error(input, e),),
            }
        }
    }

    #[test]
    fn parse_root_node() {
        use IntegerExpression::*;
        use IntegerLiteral::*;
        use PropertyCell::*;
        use PropertyValue::*;

        let input = r#"/ { #address-cells = <2>; #size-cells = <1>; };"#;

        let exp = Node {
            id: NodeId::Name("/", None),
            contents: NodeContents {
                props: vec![
                    Property {
                        name: "#address-cells",
                        value: Some(vec![CellArray(vec![Expr(Lit(Num(2)))])]),
                    },
                    Property {
                        name: "#size-cells",
                        value: Some(vec![CellArray(vec![Expr(Lit(Num(1)))])]),
                    },
                ],
                ..Default::default()
            },
            ..Default::default()
        };

        match root_node::<VerboseError<&str>>(input).finish() {
            Ok(res) => assert_eq!(res, ("", exp)),
            Err(e) => panic!("{}", convert_error(input, e)),
        }
    }

    #[test]
    fn parse_inner_node() {
        use IntegerExpression::*;
        use IntegerLiteral::*;
        use PropertyCell::*;
        use PropertyValue::*;

        let input = r#"cpus {
            #address-cells = <1>;
            #size-cells = <0>;

            cpu@0 {
                device_type = "cpu";
                reg = <0>;
                cache-unified;
                cache-size = <0x8000>; // L1, 32 KB
                cache-block-size = <32>;
                timebase-frequency = <82500000>; // 82.5 MHz
                next-level-cache = <&L2_0>; // phandle to L2

                L2_0:l2-cache {
                    compatible = "cache";
                };
            };

            cpu@1 {
                device_type = "cpu";
                reg = <1>;

                L2: L2_1: l2-cache {
                    compatible = "cache";
                };
            };
        };
"#;

        let exp = Node {
            id: NodeId::Name("cpus", None),
            contents: NodeContents {
                props: vec![
                    Property {
                        name: "#address-cells",
                        value: Some(vec![CellArray(vec![Expr(Lit(Num(1)))])]),
                    },
                    Property {
                        name: "#size-cells",
                        value: Some(vec![CellArray(vec![Expr(Lit(Num(0)))])]),
                    },
                ],
                children: vec![
                    Node {
                        id: NodeId::Name("cpu", Some("0")),
                        contents: NodeContents {
                            props: vec![
                                Property {
                                    name: "device_type",
                                    value: Some(vec![Str("cpu")]),
                                },
                                Property {
                                    name: "reg",
                                    value: Some(vec![CellArray(vec![Expr(Lit(Num(0)))])]),
                                },
                                Property {
                                    name: "cache-unified",
                                    value: None,
                                },
                                Property {
                                    name: "cache-size",
                                    value: Some(vec![CellArray(vec![Expr(Lit(Num(0x8000)))])]),
                                },
                                Property {
                                    name: "cache-block-size",
                                    value: Some(vec![CellArray(vec![Expr(Lit(Num(32)))])]),
                                },
                                Property {
                                    name: "timebase-frequency",
                                    value: Some(vec![CellArray(vec![Expr(Lit(Num(82_500_000)))])]),
                                },
                                Property {
                                    name: "next-level-cache",
                                    value: Some(vec![CellArray(vec![PropertyCell::Ref(
                                        Reference("L2_0"),
                                    )])]),
                                },
                            ],
                            children: vec![Node {
                                id: NodeId::Name("l2-cache", None),
                                labels: vec!["L2_0"],
                                contents: NodeContents {
                                    props: vec![Property {
                                        name: "compatible",
                                        value: Some(vec![Str("cache")]),
                                    }],
                                    ..Default::default()
                                },
                                ..Default::default()
                            }],
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    Node {
                        id: NodeId::Name("cpu", Some("1")),
                        contents: NodeContents {
                            props: vec![
                                Property {
                                    name: "device_type",
                                    value: Some(vec![Str("cpu")]),
                                },
                                Property {
                                    name: "reg",
                                    value: Some(vec![CellArray(vec![Expr(Lit(Num(1)))])]),
                                },
                            ],
                            children: vec![Node {
                                id: NodeId::Name("l2-cache", None),
                                labels: vec!["L2", "L2_1"],
                                contents: NodeContents {
                                    props: vec![Property {
                                        name: "compatible",
                                        value: Some(vec![Str("cache")]),
                                    }],
                                    ..Default::default()
                                },
                                ..Default::default()
                            }],
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                ],
                ..Default::default()
            },
            ..Default::default()
        };

        match inner_node::<VerboseError<&str>>(input).finish() {
            Ok(res) => assert_eq!(res, ("", exp)),
            Err(e) => panic!("{}", convert_error(input, e)),
        }
    }

    #[test]
    fn parse_includes() {
        for (input, inc) in [
            (r#"/include/ "sama5.dtsi""#, "sama5.dtsi"),
            (r#"/include/ "inner/sample.dtsi""#, "inner/sample.dtsi"),
        ] {
            match include_directive::<VerboseError<&str>>(input).finish() {
                Ok(res) => assert_eq!(res, ("", inc)),
                Err(e) => panic!("{}", convert_error(input, e)),
            }
        }
    }

    #[test]
    fn parse_deleted_properties() {
        for (input, exp) in [
            ("/delete-property/ foo;", Some("foo")),
            ("/delete-property/ foo,bar;", Some("foo,bar")),
            ("/delete-property/ foo,bar", None),
            ("/delete_property/ foo,bar;", None),
            ("/delete-property foo,bar;", None),
            ("foo,bar;", None),
        ] {
            match deleted_property::<VerboseError<&str>>(input).finish() {
                Ok(res) => assert_eq!(res, ("", exp.unwrap())),
                Err(_) => assert!(exp.is_none()),
            }
        }
    }

    #[test]
    fn parse_deleted_node_directives() {
        for (input, exp) in [
            ("/delete-node/ foo;", Some(NodeId::Name("foo", None))),
            (
                "/delete-node/ bar@0,0;",
                Some(NodeId::Name("bar", Some("0,0"))),
            ),
            ("/delete-node/ &baz;", Some(NodeId::Ref(Reference("baz")))),
        ] {
            match deleted_node::<VerboseError<&str>>(input).finish() {
                Ok(res) => assert_eq!(res, ("", exp.unwrap())),
                Err(_) => assert!(exp.is_none()),
            }
        }
    }

    #[test]
    fn parse_memreserve_directives() {
        for (input, exp) in [
            ("/memreserve/ 0x0 0x1000;", Some((0x0, 0x1000))),
            ("/memreserve/ 0x1000 0x2000;", Some((0x1000, 0x2000))),
            ("/memreserve/ 0 65536;", Some((0, 65536))),
            ("/memreserve/ 0x1000;", None),
            ("/memreserve/ 0 0x1000", None),
            ("/memreserve/;", None),
            ("/memreserve/", None),
        ] {
            match memreserve::<VerboseError<&str>>(input).finish() {
                Ok(res) => assert_eq!(res, ("", exp.unwrap())),
                Err(_) => assert!(exp.is_none()),
            }
        }
    }

    #[test]
    fn parse_omit_if_no_ref_directives() {
        for (input, exp) in [
            ("/omit-if-no-ref/ foo;", Some(NodeId::Name("foo", None))),
            (
                "/omit-if-no-ref/ bar@0,0;",
                Some(NodeId::Name("bar", Some("0,0"))),
            ),
            (
                "/omit-if-no-ref/ &baz;",
                Some(NodeId::Ref(Reference("baz"))),
            ),
        ] {
            match omit_if_no_ref::<VerboseError<&str>>(input).finish() {
                Ok(res) => assert_eq!(res, ("", exp.unwrap())),
                Err(_) => assert!(exp.is_none()),
            }
        }
    }

    #[test]
    fn parse_multiple_version_directives() {
        let input = r#"/dts-v1/; /dts-v1/; / { };"#;

        let exp = Dts {
            version: DtsVersion::V1,
            nodes: vec![Node {
                id: NodeId::Name("/", None),
                ..Default::default()
            }],
            ..Default::default()
        };

        match dts_file::<VerboseError<&str>>(input).finish() {
            Ok(res) => assert_eq!(res, ("", exp)),
            Err(e) => panic!("{}", convert_error(input, e)),
        }
    }

    #[test]
    fn parse_simple_file() {
        let input = r#"
/dts-v1/;

/memreserve/ 0x00000000 0x00400000;

/ {
    compatible = "acme,coyotes-revenge";
    #address-cells = <1>;
    #size-cells = <1>;
    interrupt-parent = <&intc>;

    cpus {
        #address-cells = <1>;
        #size-cells = <0>;
        cpu@0 {
            compatible = "arm,cortex-a9";
            reg = <0>;
        };
        cpu@1 {
            compatible = "arm,cortex-a9";
            reg = <1>;
        };
    };

    serial@101f0000 {
        compatible = "arm,pl011";
        reg = <0x101f0000 0x1000 >;
        interrupts = < 1 0 >;
    };

    serial@101f2000 {
        compatible = "arm,pl011";
        reg = <0x101f2000 0X1000 >;
        interrupts = < 2 0 >;
    };

    gpio@101f3000 {
        compatible = "arm,pl061";
        reg = <0x101f3000 0x1000
               0x101f4000 0x0010>;
        interrupts = < 3 0 >;
    };

    intc: interrupt-controller@10140000 {
        compatible = "arm,pl190";
        reg = <0x10140000 0x1000 >;
        interrupt-controller;
        #interrupt-cells = <2>;
    };

    spi@10115000 {
        compatible = "arm,pl022";
        reg = <0x10115000 0x1000 >;
        interrupts = < 4 0 >;
    };

    /delete-node/ &gpio;

    external-bus {
        #address-cells = <2>;
        #size-cells = <1>;
        ranges = <0 0  0x10100000   0x10000     // Chipselect 1, Ethernet
                  1 0  0x10160000   0x10000     // Chipselect 2, i2c controller
                  2 0  0x30000000   0x1000000>; // Chipselect 3, NOR Flash

        ethernet@0,0 {
            compatible = "smc,smc91c111";
            reg = <0 0 0x1000>;
            interrupts = < 5 2 >;
        };

        i2c@1,0 {
            compatible = "acme,a1234-i2c-bus";
            #address-cells = <1>;
            #size-cells = <0>;
            reg = <1 0 0x1000>;
            interrupts = < 6 2 >;
            rtc@58 {
                compatible = "maxim,ds1338";
                reg = <58>;
                interrupts = < 7 3 >;
            };
        };

        flash@2,0 {
            compatible = "samsung,k8f1315ebm", "cfi-flash";
            reg = <2 0 0x4000000>;
        };
    };
};

/ {
    model = "Coyotes Revenge";

    /delete-property/ compatible;
};

/delete-node/ &pioC;
"#;

        // Someday I'll write a proper test for the above file...
        match dts_file::<VerboseError<&str>>(input).finish() {
            Ok((rest, _)) => assert!(rest.is_empty()),
            Err(e) => panic!("{}", convert_error(input, e),),
        }
    }
}
