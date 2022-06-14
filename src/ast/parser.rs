use std::{io::Read, str};

use anyhow::{bail, Result};
use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag},
    character::complete::{
        alpha1, alphanumeric1, anychar, digit1, hex_digit1, line_ending, multispace1,
    },
    combinator::{map, map_res, opt, recognize},
    multi::{many0, many1, many_till, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

use crate::ast::*;

/// Parse a Device Tree source file.
pub fn from_reader(mut r: impl Read) -> Result<Dts> {
    let mut dts = String::new();
    r.read_to_string(&mut dts)?;

    let dts = match dts_file(&dts) {
        Ok((_, dts)) => dts,
        Err(e) => bail!("{:?}", e.to_string()),
    };

    Ok(dts)
}

/// Parse a Device Tree source file.
fn dts_file(input: &str) -> IResult<&str, Dts> {
    enum FileContent {
        Include(Include),
        Node(Node),
    }

    // Trim leading commends/spaces
    let (input, _) = sc(input)?;
    let (input, version) = version_directive(input)?;

    map(
        many0(alt((
            map(include, FileContent::Include),
            map(node, FileContent::Node),
        ))),
        move |contents| {
            contents.into_iter().fold(
                Dts {
                    version,
                    includes: vec![],
                    nodes: vec![],
                },
                |mut dts, elem| {
                    match elem {
                        FileContent::Include(i) => dts.includes.push(i),
                        FileContent::Node(n) => dts.nodes.push(n),
                    };
                    dts
                },
            )
        },
    )(input)
}

fn version_directive(input: &str) -> IResult<&str, DtsVersion> {
    map(opt(terminated(symbol("/dts-v1/"), symbol(";"))), |v| {
        if v.is_some() {
            DtsVersion::V1
        } else {
            DtsVersion::V0
        }
    })(input)
}

/// Parse an include directive.
fn include(input: &str) -> IResult<&str, Include> {
    preceded(
        symbol("#include"),
        alt((
            map(include_path, |s| Include::Global(s.to_string())),
            map(string_literal, |s| Include::Local(s.to_string())),
        )),
    )(input)
}

/// Parse a device tree node.
fn node(input: &str) -> IResult<&str, Node> {
    map(
        tuple((
            opt(terminated(node_label, symbol(":"))),
            node_name,
            delimited(symbol("{"), node_contents, symbol("}")),
            symbol(";"),
        )),
        |(label, (name, address), (props, children), _)| Node {
            name: name.to_string(),
            address: address.map(|address| address.to_string()),
            label: label.map(|label| label.to_string()),
            props,
            children,
        },
    )(input)
}

/// Parse the contents of a node.
fn node_contents(input: &str) -> IResult<&str, (Vec<Property>, Vec<Node>)> {
    enum NodeContent {
        Prop(Property),
        Node(Node),
    }

    // A node can contain 0+ properties and 0+ child nodes.
    // To make the parsing easier, wrap them both in a `NodeContent` enum
    // and split them later in the closure.
    map(
        many0(alt((
            map(property, NodeContent::Prop),
            map(node, NodeContent::Node),
        ))),
        |contents| {
            contents
                .into_iter()
                .fold((vec![], vec![]), |(mut props, mut nodes), elem| {
                    match elem {
                        NodeContent::Prop(p) => props.push(p),
                        NodeContent::Node(c) => nodes.push(c),
                    };
                    (props, nodes)
                })
        },
    )(input)
}

/// Parse a node property.
fn property(input: &str) -> IResult<&str, Property> {
    map(
        tuple((
            lexeme(prop_name),
            opt(preceded(
                symbol("="),
                separated_list1(
                    symbol(","),
                    alt((prop_value_cell_array, prop_value_alias, prop_value_str)),
                ),
            )),
            symbol(";"),
        )),
        |(name, value, _)| Property {
            name: name.to_string(),
            value,
        },
    )(input)
}

/// Parse a property value corresponding to a reference to another node.
fn prop_value_alias(input: &str) -> IResult<&str, PropertyValue> {
    map(node_reference, |s| PropertyValue::Alias(s.to_string()))(input)
}

/// Parse a property value corresponding to a string.
fn prop_value_str(input: &str) -> IResult<&str, PropertyValue> {
    map(string_literal, |s| PropertyValue::Str(s.to_string()))(input)
}

/// Parse a property value corresponding to a cell array.
fn prop_value_cell_array(input: &str) -> IResult<&str, PropertyValue> {
    map(
        delimited(
            symbol("<"),
            many1(alt((prop_cell_u32, prop_cell_ref))),
            symbol(">"),
        ),
        PropertyValue::CellArray,
    )(input)
}

/// Parse a property cell containing a reference to another node.
fn prop_cell_ref(input: &str) -> IResult<&str, PropertyCell> {
    map(node_reference, |s| PropertyCell::Ref(s.to_string()))(input)
}

/// Parse a property cell containing a 32-bit integer cell.
fn prop_cell_u32(input: &str) -> IResult<&str, PropertyCell> {
    map(numeric_literal, PropertyCell::U32)(input)
}

/// Parse a valid property name.
fn prop_name(input: &str) -> IResult<&str, &str> {
    recognize(many1(alt((alphanumeric1, is_a(",._+?#-")))))(input)
}

/// Parse a valid node reference.
fn node_reference(input: &str) -> IResult<&str, &str> {
    lexeme(preceded(symbol("&"), node_label))(input)
}

/// Parse a valid node label.
fn node_label(input: &str) -> IResult<&str, &str> {
    lexeme(recognize(many1(alt((alphanumeric1, symbol("_"))))))(input)
}

/// Parse a valid node name.
/// A node name has composed of a node-name part and an optional node-address in hex format.
#[allow(clippy::type_complexity)]
fn node_name(input: &str) -> IResult<&str, (&str, Option<&str>)> {
    lexeme(pair(node_name_identifier, opt(node_address_identifier)))(input)
}

/// Parse a valid node name identifier,
/// ie. the part of the node name before the unit-address.
fn node_name_identifier(input: &str) -> IResult<&str, &str> {
    alt((
        symbol("/"),
        recognize(tuple((alpha1, many0(alt((alphanumeric1, is_a(",._+-"))))))),
    ))(input)
}

/// Parse a valid node unit-address identifier.
fn node_address_identifier(input: &str) -> IResult<&str, &str> {
    preceded(
        symbol("@"),
        recognize(many1(alt((alphanumeric1, is_a(",._+-"))))),
    )(input)
}

/// Parse a valid number in any base.
fn numeric_literal(input: &str) -> IResult<&str, u32> {
    lexeme(alt((hex, dec)))(input)
}

/// Parse a valid global include path.
fn include_path(input: &str) -> IResult<&str, &str> {
    lexeme(delimited(symbol("<"), is_not(">"), symbol(">")))(input)
}

/// Parse a valid string literal.
fn string_literal(input: &str) -> IResult<&str, &str> {
    lexeme(delimited(symbol("\""), is_not("\""), symbol("\"")))(input)
}

/* === Utility functions === */

/// Parse a natural number in base 16, prefixed by `0x`.
fn hex(input: &str) -> IResult<&str, u32> {
    map_res(preceded(symbol("0x"), hex_digit1), |input: &str| {
        u32::from_str_radix(input, 16)
    })(input)
}

/// Parse a natural number in base 10.
fn dec(input: &str) -> IResult<&str, u32> {
    map_res(digit1, |input: &str| input.parse::<u32>())(input)
}

/// Consume a fixed symbol.
fn symbol<'a>(s: &'a str) -> impl FnMut(&'a str) -> IResult<&str, &str> {
    lexeme(tag(s))
}

/// Parse a lexeme using the combinator passed as its argument,
/// also consuming zero or more trailing whitespaces.
fn lexeme<'a, O, F>(f: F) -> impl FnMut(&'a str) -> IResult<&str, O>
where
    F: FnMut(&'a str) -> IResult<&str, O>,
{
    terminated(f, sc)
}

/// Consume zero or more white space characters or line comments.
fn sc(input: &str) -> IResult<&str, &str> {
    recognize(many0(alt((
        multispace1,
        skip_line_comment,
        skip_block_comment,
    ))))(input)
}

/// Return a parser that skips block comments.
fn skip_block_comment(input: &str) -> IResult<&str, &str> {
    recognize(preceded(symbol("/*"), many_till(anychar, symbol("*/"))))(input)
}

/// Return a parser that skips line comments.
/// Note that it stops just before the newline character but doesn't consume the newline.
fn skip_line_comment(input: &str) -> IResult<&str, &str> {
    recognize(preceded(symbol("//"), many_till(anychar, line_ending)))(input)
}

/* === Unit Tests === */

#[cfg(test)]
mod tests {
    use super::*;

    use PropertyCell::*;
    use PropertyValue::*;

    #[test]
    fn parse_line_comments() {
        assert_eq!(
            skip_line_comment("// This is a comment\n"),
            Ok(("", "// This is a comment\n"))
        );

        assert_eq!(
            skip_line_comment("// Multiline comments\nare not supported"),
            Ok(("are not supported", "// Multiline comments\n"))
        );

        assert!(skip_line_comment(r#"prop-name = "value"; // This is a comment"#).is_err());
    }

    #[test]
    fn parse_node_names() {
        for (input, (name, address)) in vec![
            ("/", ("/", None)),
            ("cpus", ("cpus", None)),
            ("cpu@0", ("cpu", Some("0"))),
            ("cpu@1", ("cpu", Some("1"))),
            ("l2-cache", ("l2-cache", None)),
            ("l3-cache", ("l3-cache", None)),
            ("open-pic", ("open-pic", None)),
            ("soc_gpio1", ("soc_gpio1", None)),
            ("memory@0", ("memory", Some("0"))),
            ("uart@fe001000", ("uart", Some("fe001000"))),
            ("ethernet@fe002000", ("ethernet", Some("fe002000"))),
            ("ethernet@fe003000", ("ethernet", Some("fe003000"))),
        ]
        .into_iter()
        {
            assert_eq!(node_name(input), Ok(("", (name, address))));
        }
    }

    #[test]
    fn parse_node_labels() {
        for label in vec!["L3", "L2_0", "L2_1", "mmc0", "eth0", "pinctrl_wifi_pin"].into_iter() {
            assert_eq!(node_label(label), Ok(("", label)));
        }
    }

    #[test]
    fn parse_prop_names() {
        for name in vec![
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
        ]
        .into_iter()
        {
            assert_eq!(prop_name(name), Ok(("", name)));
        }
    }

    #[test]
    fn parse_properties() {
        for (input, prop) in vec![
            (
                r#"device_type = "cpu";"#,
                Property {
                    name: String::from("device_type"),
                    value: Some(vec![Str(String::from("cpu"))]),
                },
            ),
            (
                r#"compatible = "ns16550", "ns8250";"#,
                Property {
                    name: String::from("compatible"),
                    value: Some(vec![
                        Str(String::from("ns16550")),
                        Str(String::from("ns8250")),
                    ]),
                },
            ),
            (
                r#"example = <&mpic 0xf00f0000 19>, "a strange property format";"#,
                Property {
                    name: String::from("example"),
                    value: Some(vec![
                        CellArray(vec![Ref(String::from("mpic")), U32(0xf00f_0000), U32(19)]),
                        Str(String::from("a strange property format")),
                    ]),
                },
            ),
            (
                r#"reg = <0>;"#,
                Property {
                    name: String::from("reg"),
                    value: Some(vec![CellArray(vec![U32(0)])]),
                },
            ),
            (
                r#"cache-unified;"#,
                Property {
                    name: String::from("cache-unified"),
                    value: None,
                },
            ),
            (
                r#"cache-size = <0x8000>;"#,
                Property {
                    name: String::from("cache-size"),
                    value: Some(vec![CellArray(vec![U32(0x8000)])]),
                },
            ),
            (
                r#"next-level-cache = <&L2_0>;"#,
                Property {
                    name: String::from("next-level-cache"),
                    value: Some(vec![CellArray(vec![Ref(String::from("L2_0"))])]),
                },
            ),
            (
                r#"interrupts = <17 0xc>;"#,
                Property {
                    name: String::from("interrupts"),
                    value: Some(vec![CellArray(vec![U32(17), U32(0xc)])]),
                },
            ),
            (
                r#"serial0 = &usart3;"#,
                Property {
                    name: String::from("serial0"),
                    value: Some(vec![Alias(String::from("usart3"))]),
                },
            ),
        ]
        .into_iter()
        {
            assert_eq!(property(input), Ok(("", prop)));
        }
    }

    #[test]
    fn parse_nodes() {
        assert_eq!(
            node(
                r#"cpus {
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

                            L2_1:l2-cache {
                                compatible = "cache";
                            };
                        };
                    };
            "#
            ),
            Ok((
                "",
                Node {
                    name: String::from("cpus"),
                    address: None,
                    label: None,
                    props: vec![
                        Property {
                            name: String::from("#address-cells"),
                            value: Some(vec![CellArray(vec![U32(1)])])
                        },
                        Property {
                            name: String::from("#size-cells"),
                            value: Some(vec![CellArray(vec![U32(0)])])
                        }
                    ],
                    children: vec![
                        Node {
                            name: String::from("cpu"),
                            address: Some(String::from("0")),
                            label: None,
                            props: vec![
                                Property {
                                    name: String::from("device_type"),
                                    value: Some(vec![Str(String::from("cpu"))])
                                },
                                Property {
                                    name: String::from("reg"),
                                    value: Some(vec![CellArray(vec![U32(0)])])
                                },
                                Property {
                                    name: String::from("cache-unified"),
                                    value: None
                                },
                                Property {
                                    name: String::from("cache-size"),
                                    value: Some(vec![CellArray(vec![U32(0x8000)])])
                                },
                                Property {
                                    name: String::from("cache-block-size"),
                                    value: Some(vec![CellArray(vec![U32(32)])])
                                },
                                Property {
                                    name: String::from("timebase-frequency"),
                                    value: Some(vec![CellArray(vec![U32(82_500_000)])])
                                },
                                Property {
                                    name: String::from("next-level-cache"),
                                    value: Some(vec![CellArray(vec![Ref(String::from("L2_0"))])])
                                }
                            ],
                            children: vec![Node {
                                name: String::from("l2-cache"),
                                address: None,
                                label: Some(String::from("L2_0")),
                                props: vec![Property {
                                    name: String::from("compatible"),
                                    value: Some(vec![Str(String::from("cache"))])
                                }],
                                children: vec![],
                            }],
                        },
                        Node {
                            name: String::from("cpu"),
                            address: Some(String::from("1")),
                            label: None,
                            props: vec![
                                Property {
                                    name: String::from("device_type"),
                                    value: Some(vec![Str(String::from("cpu"))])
                                },
                                Property {
                                    name: String::from("reg"),
                                    value: Some(vec![CellArray(vec![U32(1)])])
                                }
                            ],
                            children: vec![Node {
                                name: String::from("l2-cache"),
                                address: None,
                                label: Some(String::from("L2_1")),
                                props: vec![Property {
                                    name: String::from("compatible"),
                                    value: Some(vec![Str(String::from("cache"))])
                                }],
                                children: vec![],
                            }],
                        }
                    ]
                }
            ))
        );
    }

    #[test]
    fn parse_includes() {
        assert_eq!(
            include(r#"#include <arm/pinctrl.h>"#),
            Ok(("", Include::Global(String::from("arm/pinctrl.h"))))
        );
        assert_eq!(
            include(r#"#include "sama5.dtsi""#),
            Ok(("", Include::Local(String::from("sama5.dtsi"))))
        );
    }

    #[test]
    fn parse_simple_file() {
        let dts = r#"/dts-v1/;

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
        reg = <0x101f2000 0x1000 >;
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
};"#;

        let (rest, _) = dts_file(dts).unwrap();

        // Someday I'll write a proper test for the above file...
        assert!(rest.is_empty());
    }
}
