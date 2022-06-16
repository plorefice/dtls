use std::str;

use nom::{
    branch::alt,
    bytes::complete::{escaped, is_a, is_not, tag, take_while_m_n},
    character::complete::{
        alphanumeric1, anychar, char, hex_digit1, i64, line_ending, multispace1, one_of, space1,
        u64,
    },
    combinator::{all_consuming, cut, map, opt, recognize},
    error::Error,
    multi::{fold_many0, many0, many1, many_m_n, many_till, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    AsChar, Finish,
};
use nom_locate::LocatedSpan;

use crate::ast::*;

type Span<'a> = LocatedSpan<&'a [u8]>;
type Input<'a> = Span<'a>;
type IResult<'a, T> = nom::IResult<Input<'a>, T>;

/// Parse a Device Tree from a string.
pub fn from_str(s: &str) -> Result<Root, Error<Input>> {
    let input = Span::new(s.as_bytes());

    match all_consuming(root)(input).finish() {
        Ok((_, dts)) => Ok(dts),
        Err(e) => Err(e),
    }
}

/// Parse a Device Tree source file.
fn root(input: Input) -> IResult<Root> {
    let (input, items) = fold_many0(
        alt((
            map(root_node, RootItem::Node),
            map(node_override, RootItem::Node),
            map(version_directive, RootItem::Version),
            map(include_directive, RootItem::Include),
            map(deleted_node, RootItem::DeleteNode),
            map(omit_if_no_ref, RootItem::OmitNode),
            map(memreserve, RootItem::MemReserve),
        )),
        Vec::new,
        move |mut items, item| {
            items.push(item);
            items
        },
    )(input)?;

    Ok((input, Root(items)))
}

/// Parse a version directive.
fn version_directive(input: Input) -> IResult<DtsVersion> {
    map(terminated(dts_v1_keyword, cut(terminator)), |_| {
        DtsVersion::V1
    })(input)
}

/// Parse a valid root node.
///
/// The root node is a top-level named node in the file and its name should always be '/'.
fn root_node(input: Input) -> IResult<Node> {
    map(
        tuple((root_node_name, node_body, cut(terminator))),
        |(name, contents, _)| Node {
            id: NodeId::Name(str::from_utf8(name.fragment()).unwrap(), None),
            contents,
            ..Default::default()
        },
    )(input)
}

/// Parse a valid node override.
///
/// Node overrides are only valid in the top-level of the file and their name should be
/// a valid node reference.
fn node_override(input: Input) -> IResult<Node> {
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
            labels: labels
                .into_iter()
                .map(|s| str::from_utf8(s.fragment()).unwrap())
                .collect(),
            contents,
            ommittable: omit.is_some(),
        },
    )(input)
}

/// Parse a valid inner node.
///
/// Inner nodes are only valid within the body of a node. Their name should be a valid node name.
fn inner_node(input: Input) -> IResult<Node> {
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
            labels: labels
                .into_iter()
                .map(|s| str::from_utf8(s.fragment()).unwrap())
                .collect(),
            contents,
            ommittable: omit.is_some(),
        },
    )(input)
}

/// Recognize the name of a root node.
fn root_node_name(input: Input) -> IResult<Input> {
    lexeme(tag("/"))(input)
}

/// Parse the body of a device tree node.
fn node_body(input: Input) -> IResult<Vec<NodeItem>> {
    preceded(left_brace, cut(terminated(node_contents, right_brace)))(input)
}

/// Parse a list of node labels.
fn node_labels(input: Input) -> IResult<Vec<Input>> {
    many0(terminated(node_label, label_separator))(input)
}

/// Parse a node label.
fn node_label(input: Input) -> IResult<Input> {
    lexeme(node_label_str)(input)
}

/// Parse the contents of a node.
fn node_contents(input: Input) -> IResult<Vec<NodeItem>> {
    fold_many0(
        alt((
            map(include_directive, NodeItem::Include),
            map(inner_node, NodeItem::ChildNode),
            map(property, NodeItem::Property),
            map(deleted_property, |s| {
                NodeItem::DeletedProp(str::from_utf8(s.fragment()).unwrap())
            }),
            map(deleted_node, NodeItem::DeletedNode),
        )),
        Vec::new,
        |mut items, item| {
            items.push(item);
            items
        },
    )(input)
}

/// Parse a node property.
fn property(input: Input) -> IResult<Property> {
    map(
        tuple((
            prop_name,
            opt(preceded(assignment, cut(prop_values))),
            cut(terminator),
        )),
        |(name, value, _)| Property {
            name: str::from_utf8(name.fragment()).unwrap(),
            value,
        },
    )(input)
}

/// Parse a propery name.
fn prop_name(input: Input) -> IResult<Input> {
    lexeme(prop_name_str)(input)
}

/// Parse a property value list.
fn prop_values(input: Input) -> IResult<Vec<PropertyValue>> {
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
fn prop_value_bits(input: Input) -> IResult<PropertyValue> {
    map(
        tuple((
            bits_keyword,
            cut(dec),
            cut(delimited(left_chevron, many1(expression), right_chevron)),
        )),
        |(_, n, bits)| PropertyValue::Bits(n as u32, bits),
    )(input)
}

/// Parse a property value corresponding to a reference to another node.
fn prop_value_alias(input: Input) -> IResult<PropertyValue> {
    map(node_reference, PropertyValue::Ref)(input)
}

/// Parse a property value corresponding to a string.
fn prop_value_str(input: Input) -> IResult<PropertyValue> {
    lexeme(map(string_literal, |s: Input| {
        PropertyValue::Str(str::from_utf8(s.fragment()).unwrap())
    }))(input)
}

/// Parse a property value corresponding to a cell array.
///
/// A cell array can be empty.
fn prop_value_cell_array(input: Input) -> IResult<PropertyValue> {
    map(
        preceded(
            left_chevron,
            cut(terminated(
                many0(alt((prop_cell_expr, prop_cell_ref))),
                right_chevron,
            )),
        ),
        PropertyValue::CellArray,
    )(input)
}

/// Parse a property value corresponding to a byte string.
fn prop_value_bytestring(input: Input) -> IResult<PropertyValue> {
    map(
        preceded(
            left_bracket,
            cut(terminated(many1(lexeme(hex_byte)), right_bracket)),
        ),
        PropertyValue::Bytestring,
    )(input)
}

/// Parse a property cell containing a reference to another node.
fn prop_cell_ref(input: Input) -> IResult<PropertyCell> {
    map(node_reference, PropertyCell::Ref)(input)
}

/// Parse a property cell containing an integer expression.
fn prop_cell_expr(input: Input) -> IResult<PropertyCell> {
    lexeme(map(expression, PropertyCell::Expr))(input)
}

/// Parse a deleted node.
fn deleted_node(input: Input) -> IResult<NodeId> {
    delimited(
        delete_node_keyword,
        cut(alt((node_name, map(node_reference, NodeId::Ref)))),
        cut(terminator),
    )(input)
}

/// Parse a deleted property.
fn deleted_property(input: Input) -> IResult<Input> {
    delimited(delete_property_keyword, cut(prop_name), cut(terminator))(input)
}

/// Parse a valid memreserve directive.
fn memreserve(input: Input) -> IResult<(u64, u64)> {
    delimited(
        memreserve_keyword,
        cut(pair(unsigned_literal, unsigned_literal)),
        cut(terminator),
    )(input)
}

/// Parse a valid omitted node directive.
fn omit_if_no_ref(input: Input) -> IResult<NodeId> {
    delimited(
        omit_if_no_ref_keyword,
        cut(alt((node_name, map(node_reference, NodeId::Ref)))),
        cut(terminator),
    )(input)
}

/// Parse a valid node reference.
fn node_reference(input: Input) -> IResult<Reference> {
    let node_ref = map(
        alt((
            node_label_str,
            delimited(left_brace, node_path, right_brace),
        )),
        |s: Input| Reference(str::from_utf8(s.fragment()).unwrap()),
    );

    preceded(reference_operator, cut(node_ref))(input)
}

/// Parse a valid node name.
///
/// A node name is composed of node-name part and an optional unit-address in hex format.
fn node_name(input: Input) -> IResult<NodeId> {
    map(
        tuple((node_name_identifier, opt(node_address_identifier))),
        |(name, address)| {
            NodeId::Name(
                str::from_utf8(name.fragment()).unwrap(),
                address.map(|s| str::from_utf8(s.fragment()).unwrap()),
            )
        },
    )(input)
}

/// Parse a valid node name identifier, i.e. the part of the node name before the unit-address.
fn node_name_identifier(input: Input) -> IResult<Input> {
    lexeme(node_name_str)(input)
}

/// Parse a valid node unit-address identifier.
fn node_address_identifier(input: Input) -> IResult<Input> {
    preceded(char('@'), cut(node_name_str))(input)
}

/// Parse a valid top-level integer expression in a property cell.
///
/// Valid expressions include a single integer literal (e.g. `<0>`) or a parenthesized expression
/// (e.g. `<(1 << 1)>`).
fn expression(input: Input) -> IResult<Expression> {
    alt((expression_lit, expression_parens))(input)
}

/// Parse a valid integer literal expression in a property cell.
fn expression_lit(input: Input) -> IResult<Expression> {
    map(integer_literal, Expression::Lit)(input)
}

/// Parse a valid parenthesized integer expression in a property cell.
///
/// Parenthesized expressions may contain a single term or an inner parenthesized expression.
fn expression_parens(input: Input) -> IResult<Expression> {
    preceded(
        left_paren,
        cut(terminated(
            alt((expression_term, expression_parens)),
            right_paren,
        )),
    )(input)
}

/// Parse a valid integer expression term in a property cell.
///
/// A term may be a single integer literal, a binary operator applied to two terms,
/// a unary operator applied to a single term or a ternaty conditional operator.
fn expression_term(input: Input) -> IResult<Expression> {
    alt((
        expression_ternary,
        expression_binary,
        expression_unary,
        expression_lit,
    ))(input)
}

/// Parse a valid binary integer expression term in a property cell.
fn expression_binary(input: Input) -> IResult<Expression> {
    map(
        tuple((
            expression,
            arith_operator_binary,
            cut(alt((expression_term, expression_parens))),
        )),
        |(left, op, right)| Expression::Binary(Box::new(left), op, Box::new(right)),
    )(input)
}

/// Parse a valid unary integer expression term in a property cell.
fn expression_unary(input: Input) -> IResult<Expression> {
    map(
        tuple((arith_operator_unary, cut(expression))),
        |(op, right)| Expression::Unary(op, Box::new(right)),
    )(input)
}

/// Parse a valid ternary expression in a property cell.
fn expression_ternary(input: Input) -> IResult<Expression> {
    map(
        tuple((
            expression,
            ternary_if_operator,
            cut(alt((expression_term, expression_parens))),
            cut(ternary_else_operator),
            cut(alt((expression_term, expression_parens))),
        )),
        |(cond, _, left, _, right)| Expression::Ternary {
            cond: Box::new(cond),
            left: Box::new(left),
            right: Box::new(right),
        },
    )(input)
}

/// Parse an integer literal valid in the context of an expression.
fn integer_literal(input: Input) -> IResult<IntegerLiteral> {
    alt((
        map(numeric_literal, IntegerLiteral::Num),
        map(char_literal, IntegerLiteral::Char),
    ))(input)
}

/// Parse a valid unsigned integer number in any base.
fn unsigned_literal(input: Input) -> IResult<u64> {
    lexeme(alt((unsigned_hex, unsigned_dec)))(input)
}

/// Parse a valid signed integer number in any base.
fn numeric_literal(input: Input) -> IResult<i64> {
    lexeme(alt((hex, dec)))(input)
}

/// Parse a valid character literal.
fn char_literal(input: Input) -> IResult<char> {
    delimited(char('\''), cut(anychar), cut(char('\'')))(input)
}

/// Parse a valid include directive.
///
/// The parser recognizes both C-style include directives (i.e. `#include "foo.h"`)
/// and Devicetree-style include directives (i.e. `/include/ "foo.h"`).
fn include_directive(input: Input) -> IResult<Include> {
    alt((include_directive_cpp, include_directive_dts))(input)
}

/// Parse a valid C-style include directive.
fn include_directive_cpp(input: Input) -> IResult<Include> {
    let quoted = delimited(double_quote, include_path_str, double_quote);
    let bracketed = delimited(left_chevron, include_path_str, right_chevron);

    map(
        preceded(include_cpp_keyword, cut(alt((quoted, bracketed)))),
        |path: Input| Include::C(str::from_utf8(path.fragment()).unwrap()),
    )(input)
}

/// Parse a valid Devicetree-style include directive.
fn include_directive_dts(input: Input) -> IResult<Include> {
    map(
        preceded(
            include_dts_keyword,
            cut(delimited(double_quote, include_path_str, double_quote)),
        ),
        |path: Input| Include::Dts(str::from_utf8(path.fragment()).unwrap()),
    )(input)
}

/// Parse a valid string literal.
fn string_literal(input: Input) -> IResult<Input> {
    preceded(double_quote, cut(terminated(printable_ascii, double_quote)))(input)
}

/* === Low-level syntax parsers === */

/// Recognize a double-quote character.
fn double_quote(input: Input) -> IResult<char> {
    lexeme(char('"'))(input)
}

/// Recognize an assigment operator.
fn assignment(input: Input) -> IResult<char> {
    lexeme(char('='))(input)
}

/// Recognize a statement terminator.
fn terminator(input: Input) -> IResult<char> {
    lexeme(char(';'))(input)
}

/// Recognize a list separator.
fn list_separator(input: Input) -> IResult<char> {
    lexeme(char(','))(input)
}

/// Recognize a label separator.
fn label_separator(input: Input) -> IResult<char> {
    lexeme(char(':'))(input)
}

/// Recognize an opening brace.
fn left_brace(input: Input) -> IResult<char> {
    lexeme(char('{'))(input)
}

/// Recognize a closing brace.
fn right_brace(input: Input) -> IResult<char> {
    lexeme(char('}'))(input)
}

/// Recognize an opening chevron.
fn left_chevron(input: Input) -> IResult<char> {
    lexeme(char('<'))(input)
}

/// Recognize a closing chevron.
fn right_chevron(input: Input) -> IResult<char> {
    lexeme(char('>'))(input)
}

/// Recognize an opening parenthesis.
fn left_paren(input: Input) -> IResult<char> {
    lexeme(char('('))(input)
}

/// Recognize a closing parenthesis.
fn right_paren(input: Input) -> IResult<char> {
    lexeme(char(')'))(input)
}

/// Recognize an opening bracket.
fn left_bracket(input: Input) -> IResult<char> {
    lexeme(char('['))(input)
}

/// Recognize a closing bracket.
fn right_bracket(input: Input) -> IResult<char> {
    lexeme(char(']'))(input)
}

/// Recognize a reference operator.
fn reference_operator(input: Input) -> IResult<char> {
    lexeme(char('&'))(input)
}

/// Recognize a ternary `?` operator.
fn ternary_if_operator(input: Input) -> IResult<char> {
    lexeme(char('?'))(input)
}

/// Recognize a ternary `:` operator.
fn ternary_else_operator(input: Input) -> IResult<char> {
    lexeme(char(':'))(input)
}

/// Recognize a path separator.
fn path_separator(input: Input) -> IResult<char> {
    lexeme(char('/'))(input)
}

/// Recognize an arithmetic unary operator.
fn arith_operator_unary(input: Input) -> IResult<UnaryOperator> {
    lexeme(map(tag("~"), |_| UnaryOperator::BitNot))(input)
}

/// Recognize an arithmetic binary operator.
fn arith_operator_binary(input: Input) -> IResult<BinaryOperator> {
    lexeme(alt((
        map(tag("<<"), |_| BinaryOperator::LShift),
        map(tag(">>"), |_| BinaryOperator::RShift),
        map(tag("=="), |_| BinaryOperator::Eq),
        map(tag("!="), |_| BinaryOperator::Neq),
        map(tag("<="), |_| BinaryOperator::Le),
        map(tag(">="), |_| BinaryOperator::Ge),
        map(tag("+"), |_| BinaryOperator::Add),
        map(tag("-"), |_| BinaryOperator::Sub),
        map(tag("*"), |_| BinaryOperator::Mul),
        map(tag("/"), |_| BinaryOperator::Div),
        map(tag("&"), |_| BinaryOperator::BitAnd),
        map(tag("|"), |_| BinaryOperator::BitOr),
        map(tag("^"), |_| BinaryOperator::BitXor),
        map(tag("<"), |_| BinaryOperator::Lt),
        map(tag(">"), |_| BinaryOperator::Gt),
    )))(input)
}

/// Parse a signed integer number in base 16, prefixed by `0x`.
fn hex(input: Input) -> IResult<i64> {
    map(unsigned_hex, |d| d as i64)(input)
}

/// Parse an usigned integer number in base 16, prefixed by `0x`.
fn unsigned_hex(input: Input) -> IResult<u64> {
    map(
        preceded(alt((tag("0x"), tag("0X"))), cut(hex_digit1)),
        |s: Input| {
            // SAFETY: The input is guaranteed to be a valid hex string.
            let s = unsafe { str::from_utf8_unchecked(s.fragment()) };
            u64::from_str_radix(s, 16).unwrap()
        },
    )(input)
}

/// Parse a signed integer number in base 10.
fn dec(input: Input) -> IResult<i64> {
    i64(input)
}

/// Parse an unsigned integer number in base 10.
fn unsigned_dec(input: Input) -> IResult<u64> {
    u64(input)
}

/// Parse a byte represented by two hex digits.
fn hex_byte(input: Input) -> IResult<u8> {
    map(
        take_while_m_n(2, 2, |c: u8| c.is_hex_digit()),
        |s: Input| {
            // SAFETY: The input is guaranteed to be a valid hex string.
            let s = unsafe { str::from_utf8_unchecked(s.fragment()) };
            u8::from_str_radix(s, 16).unwrap()
        },
    )(input)
}

/// Recognize a sequence of printable ASCII characters.
fn printable_ascii(input: Input) -> IResult<Input> {
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
fn include_path_str(input: Input) -> IResult<Input> {
    recognize(separated_list1(path_separator, is_not("/\0\"<>")))(input)
}

/// Recognize a valid node name string.
fn node_name_str(input: Input) -> IResult<Input> {
    recognize(many_m_n(1, 31, alt((alphanumeric1, is_a(",._+-")))))(input)
}

/// Recognize a valid node label string.
fn node_label_str(input: Input) -> IResult<Input> {
    recognize(many_m_n(1, 31, alt((alphanumeric1, is_a("_")))))(input)
}

/// Recognize a valid node path.
fn node_path(input: Input) -> IResult<Input> {
    recognize(preceded(
        path_separator,
        separated_list1(path_separator, node_name),
    ))(input)
}

/// Recognize a valid property name string.
fn prop_name_str(input: Input) -> IResult<Input> {
    recognize(many_m_n(1, 31, alt((alphanumeric1, is_a(",._+?#-")))))(input)
}

/// Recognize a C preprocessor include directive prefix.
fn include_cpp_keyword(input: Input) -> IResult<Input> {
    lexeme(tag("#include"))(input)
}

/// Recognize a Devicetree include directive prefix.
fn include_dts_keyword(input: Input) -> IResult<Input> {
    lexeme(tag("/include/"))(input)
}

/// Recognize the `/bits/` keyword.
fn bits_keyword(input: Input) -> IResult<Input> {
    lexeme(tag("/bits/"))(input)
}

/// Recognize the `/memreserve/` keyword.
fn memreserve_keyword(input: Input) -> IResult<Input> {
    lexeme(tag("/memreserve/"))(input)
}

/// Recognize the `/delete-node/` keyword.
fn delete_node_keyword(input: Input) -> IResult<Input> {
    lexeme(tag("/delete-node/"))(input)
}

/// Recognize the `/delete-property/` keyword.
fn delete_property_keyword(input: Input) -> IResult<Input> {
    lexeme(tag("/delete-property/"))(input)
}

/// Recognize the `/omit-if-no-ref/` keyword.
fn omit_if_no_ref_keyword(input: Input) -> IResult<Input> {
    lexeme(tag("/omit-if-no-ref/"))(input)
}

/// Recognize the `/dts-v1/` keyword.
fn dts_v1_keyword(input: Input) -> IResult<Input> {
    lexeme(tag("/dts-v1/"))(input)
}

/* === Utility functions === */

/// Parse a lexeme using the combinator passed as its argument,
/// also consuming any whitespaces or comments before or after.
fn lexeme<'a, O, F>(f: F) -> impl FnMut(Input<'a>) -> IResult<'a, O>
where
    F: FnMut(Input<'a>) -> IResult<'a, O>,
{
    delimited(ws, f, ws)
}

/// Consume zero or more whitespace characters or comments.
fn ws(input: Input) -> IResult<Input> {
    recognize(many0(alt((multispace1, line_comment, block_comment))))(input)
}

/// Parse block comments.
fn block_comment(input: Input) -> IResult<Input> {
    recognize(preceded(tag("/*"), many_till(anychar, tag("*/"))))(input)
}

/// Parse a single line comment.
///
/// The parser stops just before the newline character but doesn't consume the newline.
fn line_comment(input: Input) -> IResult<Input> {
    recognize(preceded(tag("//"), many_till(anychar, line_ending)))(input)
}

/* === Unit Tests === */

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_escaped_strings() {
        for (input, exp) in [(r#""Escaped string: \"\\\"""#, r#"Escaped string: \"\\\""#)] {
            assert_eq!(
                &exp.as_bytes(),
                all_consuming(string_literal)(input.as_bytes().into())
                    .unwrap()
                    .1
                    .fragment()
            );
        }
    }

    #[test]
    fn parse_line_comments() {
        let (_, res) =
            all_consuming(line_comment)((&b"// This is a comment\n"[..]).into()).unwrap();
        assert_eq!(res.fragment(), b"// This is a comment\n");

        let (rest, res) =
            line_comment((&b"// Multiline comments\nare not supported"[..]).into()).unwrap();

        assert_eq!(res.fragment(), b"// Multiline comments\n");
        assert_eq!(rest.fragment(), b"are not supported");

        assert!(
            line_comment((&br#"prop-name = "value"; // This is a comment"#[..]).into()).is_err()
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
            assert_eq!(
                exp,
                all_consuming(node_name)(input.as_bytes().into()).unwrap().1,
            );
        }
    }

    #[test]
    fn parse_node_labels() {
        for label in ["L3", "L2_0", "L2_1", "mmc0", "eth0", "pinctrl_wifi_pin"] {
            assert_eq!(
                &label.as_bytes(),
                all_consuming(node_label)(label.as_bytes().into())
                    .unwrap()
                    .1
                    .fragment()
            );
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
            assert_eq!(
                &name.as_bytes(),
                all_consuming(prop_name)(name.as_bytes().into())
                    .unwrap()
                    .1
                    .fragment()
            );
        }
    }

    #[test]
    fn parse_properties() {
        use Expression::*;
        use IntegerLiteral::*;
        use PropertyCell::*;
        use PropertyValue::*;

        for (input, exp) in [
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
            assert_eq!(
                exp,
                all_consuming(property)(input.as_bytes().into()).unwrap().1,
            );
        }
    }

    #[test]
    fn parse_root_node() {
        use Expression::*;
        use IntegerLiteral::*;
        use PropertyCell::*;
        use PropertyValue::*;

        let input = r#"/ { #address-cells = <2>; #size-cells = <1>; };"#;

        let exp = Node {
            id: NodeId::Name("/", None),
            contents: vec![
                NodeItem::Property(Property {
                    name: "#address-cells",
                    value: Some(vec![CellArray(vec![Expr(Lit(Num(2)))])]),
                }),
                NodeItem::Property(Property {
                    name: "#size-cells",
                    value: Some(vec![CellArray(vec![Expr(Lit(Num(1)))])]),
                }),
            ],
            ..Default::default()
        };

        assert_eq!(
            exp,
            all_consuming(root_node)(input.as_bytes().into()).unwrap().1,
        );
    }

    #[test]
    fn parse_inner_node() {
        use Expression::*;
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

        let l2_0 = Node {
            id: NodeId::Name("l2-cache", None),
            labels: vec!["L2_0"],
            contents: vec![Property {
                name: "compatible",
                value: Some(vec![Str("cache")]),
            }
            .into()],
            ..Default::default()
        };

        let l2_1 = Node {
            id: NodeId::Name("l2-cache", None),
            labels: vec!["L2", "L2_1"],
            contents: vec![Property {
                name: "compatible",
                value: Some(vec![Str("cache")]),
            }
            .into()],
            ..Default::default()
        };

        let cpu_0 = Node {
            id: NodeId::Name("cpu", Some("0")),
            contents: vec![
                Property {
                    name: "device_type",
                    value: Some(vec![Str("cpu")]),
                }
                .into(),
                Property {
                    name: "reg",
                    value: Some(vec![CellArray(vec![Expr(Lit(Num(0)))])]),
                }
                .into(),
                Property {
                    name: "cache-unified",
                    value: None,
                }
                .into(),
                Property {
                    name: "cache-size",
                    value: Some(vec![CellArray(vec![Expr(Lit(Num(0x8000)))])]),
                }
                .into(),
                Property {
                    name: "cache-block-size",
                    value: Some(vec![CellArray(vec![Expr(Lit(Num(32)))])]),
                }
                .into(),
                Property {
                    name: "timebase-frequency",
                    value: Some(vec![CellArray(vec![Expr(Lit(Num(82_500_000)))])]),
                }
                .into(),
                Property {
                    name: "next-level-cache",
                    value: Some(vec![CellArray(vec![PropertyCell::Ref(Reference("L2_0"))])]),
                }
                .into(),
                l2_0.into(),
            ],
            ..Default::default()
        };

        let cpu_1 = Node {
            id: NodeId::Name("cpu", Some("1")),
            contents: vec![
                Property {
                    name: "device_type",
                    value: Some(vec![Str("cpu")]),
                }
                .into(),
                Property {
                    name: "reg",
                    value: Some(vec![CellArray(vec![Expr(Lit(Num(1)))])]),
                }
                .into(),
                l2_1.into(),
            ],
            ..Default::default()
        };

        let exp = Node {
            id: NodeId::Name("cpus", None),
            contents: vec![
                Property {
                    name: "#address-cells",
                    value: Some(vec![CellArray(vec![Expr(Lit(Num(1)))])]),
                }
                .into(),
                Property {
                    name: "#size-cells",
                    value: Some(vec![CellArray(vec![Expr(Lit(Num(0)))])]),
                }
                .into(),
                cpu_0.into(),
                cpu_1.into(),
            ],
            ..Default::default()
        };

        assert_eq!(
            exp,
            all_consuming(inner_node)(input.as_bytes().into())
                .unwrap()
                .1,
        );
    }

    #[test]
    fn parse_includes() {
        for (input, exp) in [
            (r#"/include/ "sama5.dtsi""#, Include::Dts("sama5.dtsi")),
            (
                r#"/include/ "inner/sample.dtsi""#,
                Include::Dts("inner/sample.dtsi"),
            ),
            (r#"#include "sama5.dtsi""#, Include::C("sama5.dtsi")),
            (
                r#"#include "inner/sample.dtsi""#,
                Include::C("inner/sample.dtsi"),
            ),
            (r#"#include <sama5.dtsi>"#, Include::C("sama5.dtsi")),
            (
                r#"#include <inner/sample.dtsi>"#,
                Include::C("inner/sample.dtsi"),
            ),
        ] {
            assert_eq!(
                exp,
                all_consuming(include_directive)(input.as_bytes().into())
                    .unwrap()
                    .1,
            );
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
            match deleted_property(input.as_bytes().into()) {
                Ok((rest, res)) => {
                    assert!(rest.is_empty());
                    assert_eq!(res.fragment(), &exp.unwrap().as_bytes());
                }
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
            match deleted_node(input.as_bytes().into()) {
                Ok((rest, res)) => {
                    assert!(rest.is_empty());
                    assert_eq!(res, exp.unwrap());
                }
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
            match memreserve(input.as_bytes().into()) {
                Ok((rest, res)) => {
                    assert!(rest.is_empty());
                    assert_eq!(res, exp.unwrap());
                }
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
            match omit_if_no_ref(input.as_bytes().into()) {
                Ok((rest, res)) => {
                    assert!(rest.is_empty());
                    assert_eq!(res, exp.unwrap());
                }
                Err(_) => assert!(exp.is_none()),
            }
        }
    }

    #[test]
    fn parse_multiple_version_directives() {
        let input = r#"/dts-v1/; /dts-v1/; / { };"#;

        let exp = Root(vec![
            RootItem::Version(DtsVersion::V1),
            RootItem::Version(DtsVersion::V1),
            RootItem::Node(Node {
                id: NodeId::Name("/", None),
                ..Default::default()
            }),
        ]);

        assert_eq!(exp, all_consuming(root)(input.as_bytes().into()).unwrap().1,);
    }

    #[test]
    fn parse_ternary_operator() {
        let input = r#"
            (
                (35 <= 2) ? (0x00b4 + 4 * 35) :
                    (35 <= 26) ? (0x027c + 4 * (35 - 3)) :
                        (35 <= 98) ? (0x0400 + 4 * (35 - 27)) :
                            (35 <= 127) ? (0x0600 + 4 * (35 - 99)) :
                            0
            )"#;

        match expression(input.as_bytes().into()).finish() {
            Ok((rest, _)) => assert!(rest.is_empty()),
            Err(e) => panic!("{}", std::str::from_utf8(e.input.fragment()).unwrap()),
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
        let (rest, _) = root(input.as_bytes().into()).unwrap();
        assert!(rest.is_empty());
    }
}
