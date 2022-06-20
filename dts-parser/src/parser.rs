use chumsky::prelude::*;

use crate::{
    lexer::{Spanned, Token},
    *,
};

/// Entry point of the parser.
///
/// This function will parse a top-level Devicetree file and return a sequence of statements.
pub(super) fn parser() -> impl Parser<Token, Dts, Error = Simple<Token>> + Clone {
    statements().then_ignore(end()).map(Dts)
}

fn statements() -> impl Parser<Token, Vec<Spanned<Statement>>, Error = Simple<Token>> + Clone {
    recursive(|stmts| {
        (node(stmts).map(Statement::from))
            .or(property().map(Statement::from))
            .or(directive().map(Statement::from))
            .padded()
            .repeated()
    })
}

fn node(
    stmts: impl Parser<Token, Vec<Spanned<Statement>>, Error = Simple<Token>> + Clone,
) -> impl Parser<Token, Spanned<Node>, Error = Simple<Token>> + Clone {
    let labels = node_label()
        .then_ignore(just(Token::Op(":".into())))
        .repeated();

    let omittable = just(Token::OmitIfNoRef).or_not();

    omittable
        .then(labels)
        .then(node_id())
        .then(stmts.delimited_by(Token::Ctrl('{'), Token::Ctrl('}')))
        .then_ignore(Token::Ctrl(';'))
        .map(|(((omit, labels), node_id), contents)| Node {
            id: node_id,
            labels,
            contents,
            ommittable: omit.is_some(),
        })
}

fn property() -> impl Parser<char, Property, Error = Simple<char>> + Clone {
    let value = (cell_array().map(PropertyValue::CellArray))
        .or(byte_string().map(PropertyValue::Bytestring))
        .or(phandle().map(PropertyValue::Phandle))
        .or(string().map(PropertyValue::Str));

    let values = value.padded().separated_by(just(',').padded());

    let bits = just("/bits/").padded().ignore_then(number());

    property_name()
        .then((just('=').ignore_then(bits.or_not().then(values))).or_not())
        .then_ignore(semicolon())
        .map(|(name, rest)| {
            let (storage_modifier, values) = rest
                .map(|(bits, values)| (bits, Some(values)))
                .unwrap_or((None, None));

            Property {
                name,
                values,
                storage_modifier,
            }
        })
}

fn property_name() -> impl Parser<char, String, Error = Simple<char>> + Clone {
    filter(|c: &char| c.is_ascii_alphanumeric() || ",._+?#-".contains(*c))
        .repeated()
        .at_least(1) // there should be a maximum of 31, but apparently it's not enforced
        .collect()
        .padded()
}

fn cell_array() -> impl Parser<char, Vec<PropertyCell>, Error = Simple<char>> + Clone {
    let expr = literal()
        .or(expr().delimited_by(just('('), just(')')))
        .map(PropertyCell::Expr);

    let phandle = phandle().map(PropertyCell::Phandle);

    (phandle.or(expr))
        .padded()
        .repeated()
        .delimited_by(just('<'), just('>'))
        .collect()
}

fn byte_string() -> impl Parser<char, Vec<u8>, Error = Simple<char>> + Clone {
    let byte = filter(char::is_ascii_hexdigit)
        .repeated()
        .exactly(2)
        .collect::<String>()
        .map(|s| u8::from_str_radix(&s, 16))
        .unwrapped()
        .padded();

    byte.repeated()
        .delimited_by(just('['), just(']'))
        .padded()
        .collect()
}

fn expr() -> impl Parser<char, Expression, Error = Simple<char>> + Clone {
    macro_rules! bin_op {
        ($p:expr, $op:expr) => {
            $p.clone()
                .then($op.padded().then($p).repeated())
                .foldl(|lhs, (op, rhs)| Expression::Binary(Box::new(lhs), op, Box::new(rhs)))
                .boxed()
        };
    }

    recursive(|expr| {
        let atom = literal()
            .or(expr.delimited_by(just('('), just(')')))
            .padded();

        let unary = (just('-').to(UnaryOp::Neg))
            .or(just('~').to(UnaryOp::BitNot))
            .or(just('!').to(UnaryOp::LogicalNot))
            .padded()
            .repeated()
            .then(atom.clone())
            .foldr(|op, rhs| Expression::Unary(op, Box::new(rhs)));

        let binary = {
            let product = bin_op!(
                unary,
                (just('*').to(BinaryOp::Mul))
                    .or(just('/').to(BinaryOp::Div))
                    .or(just('/').to(BinaryOp::Mod))
            );

            let sum = bin_op!(
                product,
                (just('+').to(BinaryOp::Add)).or(just('-').to(BinaryOp::Sub))
            );

            let bitshift = bin_op!(
                sum,
                (just("<<").to(BinaryOp::LShift)).or(just(">>").to(BinaryOp::RShift))
            );

            let comparison = bin_op!(
                bitshift,
                (just("<=").to(BinaryOp::Le))
                    .or(just(">=").to(BinaryOp::Ge))
                    .or(just("<").to(BinaryOp::Lt))
                    .or(just(">").to(BinaryOp::Gt))
            );

            let equality = bin_op!(
                comparison,
                (just("==").to(BinaryOp::Eq)).or(just("!=").to(BinaryOp::Neq))
            );

            let bit_and = bin_op!(equality, (just('&').to(BinaryOp::BitAnd)));
            let bit_xor = bin_op!(bit_and, (just('^').to(BinaryOp::BitXor)));
            let bit_or = bin_op!(bit_xor, (just('|').to(BinaryOp::BitOr)));

            let and = bin_op!(bit_or, (just("&&").to(BinaryOp::And)));
            bin_op!(and, (just("||").to(BinaryOp::Or)))
        };

        // A ternary expression (lowest precedence), or a simple expression if not ternary
        recursive(|ternary| {
            binary
                .clone()
                .then(
                    just('?')
                        .ignore_then(binary)
                        .then_ignore(just(':'))
                        .then(ternary)
                        .or_not(),
                )
                .map(|(cond, rest)| match rest {
                    Some((then, else_)) => Expression::Ternary {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        else_: Box::new(else_),
                    },
                    None => cond,
                })
        })
    })
}

fn node_id() -> impl Parser<char, NodeId, Error = Simple<char>> + Clone {
    let root_name = just('/')
        .map(|_| NodeName {
            name: "/".to_string(),
            address: None,
        })
        .padded();

    (node_name().or(root_name).map(NodeId::from))
        .or(phandle().map(NodeId::from))
        .padded()
}

fn phandle() -> impl Parser<char, Phandle, Error = Simple<char>> + Clone {
    let label = node_label().map(Phandle::Label);

    let path = node_path()
        .delimited_by(just('{'), just('}'))
        .map(Phandle::Path);

    just('&').ignore_then(path.or(label))
}

fn node_label() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    select! { Token::Ident(ident) => ident.clone() }
}

fn node_path() -> impl Parser<char, Vec<NodeName>, Error = Simple<char>> + Clone {
    just('/').ignore_then(node_name().separated_by(just('/')))
}

fn node_name() -> impl Parser<char, NodeName, Error = Simple<char>> + Clone {
    let ident = filter(|c: &char| c.is_ascii_alphanumeric() || ",._+-".contains(*c))
        .repeated()
        .at_least(1)
        .collect(); // there should be a maximum of 31, but apparently it's not enforced

    ident
        .then(just('@').ignore_then(ident).or_not())
        .map(|(name, address)| NodeName { name, address })
}

fn directive() -> impl Parser<char, Directive, Error = Simple<char>> + Clone {
    let version = just("/dts-v1/")
        .to(Directive::Version(Version::V1))
        .then_ignore(semicolon());

    let include = just("/include/")
        .padded()
        .ignore_then(file_path().delimited_by(just('"'), just('"')))
        .map(|s| Directive::Include(Include::Dts(s)));

    let delete_property = just("/delete-property/")
        .padded()
        .ignore_then(property_name())
        .then_ignore(semicolon())
        .map(Directive::DeleteProperty);

    let delete_node = just("/delete-node/")
        .padded()
        .ignore_then(node_id())
        .then_ignore(semicolon())
        .map(Directive::DeleteNode);

    let memreserve = just("/memreserve/")
        .padded()
        .ignore_then(number().padded())
        .then(number().padded())
        .then_ignore(semicolon())
        .map(|(a, b)| Directive::MemReserve(a as u64, b as u64));

    version
        .or(include)
        .or(delete_property)
        .or(delete_node)
        .or(memreserve)
}

fn file_path() -> impl Parser<char, String, Error = Simple<char>> + Clone {
    filter(|c| !("\0\"<>".contains(*c)))
        .repeated()
        .at_least(1)
        .collect()
}

fn string() -> impl Parser<char, String, Error = Simple<char>> + Clone {
    let escape = just('\\').ignore_then(one_of("\\/\""));

    just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect()
}

fn literal() -> impl Parser<char, Expression, Error = Simple<char>> + Clone {
    let int = number().map(IntLiteral::Num).padded();

    let char_ = filter(char::is_ascii_graphic)
        .delimited_by(just('\''), just('\''))
        .map(IntLiteral::Char)
        .padded();

    int.or(char_).map(Expression::Lit)
}

fn number() -> impl Parser<char, i64, Error = Simple<char>> + Clone {
    hex().or(dec())
}

fn hex() -> impl Parser<char, i64, Error = Simple<char>> + Clone {
    (just("0x").or(just("0X")))
        .ignore_then(
            // text::int() won't parse hex numbers with leading zeros.
            // In this case, we first try to apply the parser after consuming the leading zeros.
            (just('0').repeated().chain(text::int(16)))
                // If that fails, we fall back on a zero-only parser.
                .or(just('0').repeated().at_least(1)),
        )
        .collect()
        .map(|s: String| i64::from_str_radix(&s, 16).unwrap())
}

fn dec() -> impl Parser<char, i64, Error = Simple<char>> + Clone {
    text::int(10).map(|s: String| s.parse::<i64>().unwrap())
}

fn semicolon() -> impl Parser<char, char, Error = Simple<char>> + Clone {
    just(';').padded()
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! node {
        ($(@$($label:expr),+;)? $name:expr $(, $addr:expr)? => { $( $stmt:expr );* }) => {
            {
                #[allow(unused_assignments, unused_mut)]
                let mut address = None;
                $( address = Some($addr.to_string()); )?

                let id = NodeId::Name(NodeName {
                    name: $name.to_string(),
                    address,
                });

                Node {
                    id,
                    labels: vec![$( $( $label.into() ),* )?],
                    contents: vec![$( $stmt.into() ),*],
                    ommittable: false,
                }
            }
        };
    }

    macro_rules! prop {
        ($name:expr $(, $bits:expr )? => [ $( $v:expr ),* ]) => {
            {
                #[allow(unused_assignments, unused_mut)]
                let mut storage_modifier = None;
                $( storage_modifier = Some($bits); )?

                Property {
                    name: $name.into(),
                    values: Some(vec![$( $v.into() ),*]),
                    storage_modifier,
                }
            }
        };
        ($name:expr) => {
            Property {
                name: $name.into(),
                values: None,
                storage_modifier: None,
            }
        };
    }

    macro_rules! cells {
        [$( $cell:expr ),*] => {
            PropertyValue::CellArray(vec![$( $cell.into() ),*])
        };
    }

    macro_rules! e {
        ($e:expr) => {
            Expression::from($e)
        };
    }

    macro_rules! p {
        ($p:expr) => {
            Phandle::from($p.to_string())
        };
        [$($v:expr),*] => {
            Phandle::from(vec![
                $( $v.into() ),*
            ])
        };
    }

    impl From<i64> for Box<Expression> {
        fn from(i: i64) -> Self {
            Box::new(i.into())
        }
    }

    impl From<char> for Box<Expression> {
        fn from(c: char) -> Self {
            Box::new(c.into())
        }
    }

    #[test]
    fn parse_unsigned_numbers() {
        for (input, expected) in [
            ("0", 0),
            ("1", 1),
            ("25", 25),
            ("0x1", 0x1),
            ("0x25", 0x25),
            ("0x1f", 0x1f),
            ("0x0000", 0),
            ("0x0001", 0x1),
            ("0x0010", 0x10),
            ("0x0f00", 0xf00),
        ] {
            assert_eq!(
                Expression::Lit(expected.into()),
                expr().parse(dbg!(input)).unwrap()
            );
        }
    }

    #[test]
    fn parse_signed_numbers() {
        use Expression::Unary;

        for (input, expected) in [
            ("-0", 0),
            ("-1", 1),
            ("-25", 25),
            ("-0x1", 0x1),
            ("-0x25", 0x25),
            ("-0x1f", 0x1f),
        ] {
            assert_eq!(
                Unary(UnaryOp::Neg, expected.into()),
                expr().parse(dbg!(input)).unwrap()
            );
        }
    }

    #[test]
    fn parse_chars() {
        for c in "0618jaAfJzaiw)k+1'-_!?".chars() {
            assert_eq!(
                Expression::Lit(c.into()),
                expr().parse(format!("'{}'", c)).unwrap()
            );
        }
    }

    #[test]
    fn parse_and_eval_unary_op() {
        use Expression::Unary;
        use UnaryOp::*;

        for (input, res, ast) in [
            ("-1", -1, Unary(Neg, 1.into())),
            ("- ~0xf", 16, Unary(Neg, Unary(BitNot, 0xf.into()).into())),
            (
                "!!0",
                0,
                Unary(LogicalNot, Unary(LogicalNot, 0.into()).into()),
            ),
        ] {
            let expr = expr().parse(dbg!(input)).unwrap();

            assert_eq!(ast, expr);
            assert_eq!(res, expr.eval());
        }
    }

    #[test]
    fn parse_and_eval_binary_op() {
        use BinaryOp::*;
        use Expression::*;
        use UnaryOp::*;

        for (input, res, ast) in [
            ("1+1", 2, Binary(1.into(), Add, 1.into())),
            ("2 * 1", 2, Binary(2.into(), Mul, 1.into())),
            (
                "(3+2) * 1",
                5,
                Binary(Binary(3.into(), Add, 2.into()).into(), Mul, 1.into()),
            ),
            (
                "(1-4*2+~0)",
                -8,
                Binary(
                    Binary(1.into(), Sub, Binary(4.into(), Mul, 2.into()).into()).into(),
                    Add,
                    Unary(BitNot, 0.into()).into(),
                ),
            ),
            (
                "~1 << (2 >> 1)",
                -4,
                Binary(
                    Unary(BitNot, 1.into()).into(),
                    LShift,
                    Binary(2.into(), RShift, 1.into()).into(),
                ),
            ),
            (
                "1 <= 2 > 3 == 5",
                0,
                Binary(
                    Binary(Binary(1.into(), Le, 2.into()).into(), Gt, 3.into()).into(),
                    Eq,
                    5.into(),
                ),
            ),
            (
                "2-1 != 0",
                1,
                Binary(Binary(2.into(), Sub, 1.into()).into(), Neq, 0.into()),
            ),
            (
                "((2 + 'A') != 0) ? (5 << 1) : ~0",
                10,
                Ternary {
                    cond: Binary(Binary(2.into(), Add, 'A'.into()).into(), Neq, 0.into()).into(),
                    then: Binary(5.into(), LShift, 1.into()).into(),
                    else_: Unary(BitNot, 0.into()).into(),
                },
            ),
            (
                "((16 <= 2) ? (0x00b4 + 4 * 16) :
                    (16 <= 26) ? (0x027c + 4 * (16 - 3)) :
                        (16 <= 98) ? (0x0400 + 4 * (16 - 27)) :
                            (16 <= 127) ? (0x0600 + 4 * (16 - 99)) : 0)",
                688,
                Ternary {
                    cond: Binary(16.into(), Le, 2.into()).into(),
                    then: Binary(0x00b4.into(), Add, Binary(4.into(), Mul, 16.into()).into())
                        .into(),
                    else_: Ternary {
                        cond: Binary(16.into(), Le, 26.into()).into(),
                        then: Binary(
                            0x027c.into(),
                            Add,
                            Binary(4.into(), Mul, Binary(16.into(), Sub, 3.into()).into()).into(),
                        )
                        .into(),
                        else_: Ternary {
                            cond: Binary(16.into(), Le, 98.into()).into(),
                            then: Binary(
                                0x0400.into(),
                                Add,
                                Binary(4.into(), Mul, Binary(16.into(), Sub, 27.into()).into())
                                    .into(),
                            )
                            .into(),
                            else_: Ternary {
                                cond: Binary(16.into(), Le, 127.into()).into(),
                                then: Binary(
                                    0x0600.into(),
                                    Add,
                                    Binary(4.into(), Mul, Binary(16.into(), Sub, 99.into()).into())
                                        .into(),
                                )
                                .into(),
                                else_: 0.into(),
                            }
                            .into(),
                        }
                        .into(),
                    }
                    .into(),
                },
            ),
        ] {
            let expr = expr().parse(dbg!(input)).unwrap();

            assert_eq!(ast, expr);
            assert_eq!(res, expr.eval());
        }
    }

    #[test]
    fn parse_node_name() {
        for (input, expected) in [
            ("cpus", ("cpus", None::<&str>).into()),
            ("cpu@0", ("cpu", Some("0")).into()),
            ("l2-cache", ("l2-cache", None::<&str>).into()),
            ("open-pic", ("open-pic", None::<&str>).into()),
            ("soc_gpio1", ("soc_gpio1", None::<&str>).into()),
            ("memory@0", ("memory", Some("0")).into()),
            ("uart@fe001000", ("uart", Some("fe001000")).into()),
        ] as [(_, NodeName); 7]
        {
            assert_eq!(expected, node_name().parse(dbg!(input)).unwrap());
        }
    }

    #[test]
    fn parse_phandles() {
        for (input, expected) in [
            ("&soc", "soc".to_string().into()),
            ("&gpio0", "gpio0".to_string().into()),
            ("&{/cpus}", vec![("cpus", None::<&str>).into()].into()),
            (
                "&{/cpus/cpu@0}",
                vec![("cpus", None::<&str>).into(), ("cpu", Some("0")).into()].into(),
            ),
        ] as [(_, Phandle); 4]
        {
            assert_eq!(expected, phandle().parse(dbg!(input)).unwrap());
        }
    }

    #[test]
    fn parse_cell_array() {
        use BinaryOp::*;
        use Expression::*;
        use Phandle::*;
        use PropertyCell::Expr;

        for (input, expected) in [
            ("<1>", vec![Expr(Lit(1.into()))]),
            ("<1 2>", vec![Expr(Lit(1.into())), Expr(Lit(2.into()))]),
            (
                "<1 (1 << 0)>",
                vec![
                    Expr(Lit(1.into())),
                    Expr(Binary(1.into(), LShift, 0.into())),
                ],
            ),
            ("<&gpio0>", vec![Label("gpio0".into()).into()]),
            (
                "<&gpio0 0 1>",
                vec![
                    Label("gpio0".into()).into(),
                    Expr(Lit(0.into())),
                    Expr(Lit(1.into())),
                ],
            ),
            (
                "<&{/cpus/cpu@0} (1 << 0)>",
                vec![
                    Path(vec![
                        ("cpus", None::<&str>).into(),
                        ("cpu", Some("0")).into(),
                    ])
                    .into(),
                    Expr(Binary(1.into(), LShift, 0.into())),
                ],
            ),
        ] {
            assert_eq!(expected, cell_array().parse(dbg!(input)).unwrap());
        }
    }

    #[test]
    fn parse_directives() {
        for (input, expected) in [
            ("/dts-v1/;", Directive::Version(Version::V1)),
            (
                r#"/include/ "sama5.dtsi""#,
                Directive::Include(Include::Dts("sama5.dtsi".into())),
            ),
            (
                r#"/include/ "overlays/sama5.dtsi""#,
                Directive::Include(Include::Dts("overlays/sama5.dtsi".into())),
            ),
            (
                "/delete-property/ ti,pmic-shutdown-controller;",
                Directive::DeleteProperty("ti,pmic-shutdown-controller".into()),
            ),
            (
                "/delete-node/ &aes1_target;",
                Directive::DeleteNode(Phandle::Label("aes1_target".into()).into()),
            ),
            (
                "/delete-node/regulator-vcc-sdhci0;",
                Directive::DeleteNode(
                    NodeName {
                        name: "regulator-vcc-sdhci0".into(),
                        address: None,
                    }
                    .into(),
                ),
            ),
            (
                "/memreserve/ 0x00000000 0x00400000;",
                Directive::MemReserve(0, 0x400000),
            ),
        ] {
            assert_eq!(expected, directive().parse(dbg!(input)).unwrap());
        }
    }

    #[test]
    fn parse_property() {
        for (input, expected) in [
            (r#"cache-unified;"#, prop! { "cache-unified" }),
            (r#"reg = <0>;"#, prop! { "reg" => [ cells![e!(0)] ] }),
            (
                r#"reg = <0x002ff000 0x2000 0x0 0x01>;"#,
                prop! { "reg" => [ cells![e!(0x002ff000), e!(0x2000), e!(0x0), e!(0x01)] ] },
            ),
            (
                r#"cache-size = /bits/ 16 <0x8000>;"#,
                prop! { "cache-size", 16 => [ cells![e!(0x8000)] ] },
            ),
            (
                r#"next-level-cache = <&L2_0>;"#,
                prop! { "next-level-cache" => [ cells![p!("L2_0")] ] },
            ),
            (
                r#"interrupts = <17 0xc 'A'>;"#,
                prop! { "interrupts" => [ cells![e!(17), e!(0xc), e!('A')] ] },
            ),
            (
                r#"cpu = <&{/cpus/cpu@0}>;"#,
                prop! { "cpu" => [ cells![p![("cpus", None::<&str>), ("cpu", Some("0"))]] ] },
            ),
            (r#"pinctrl-0 = <>;"#, prop! { "pinctrl-0" => [ cells![] ] }),
            (
                r#"device_type = "cpu";"#,
                prop! { "device_type" => [ "cpu" ] },
            ),
            (
                r#"compatible = "ns16550", "ns8250";"#,
                prop! { "compatible" => [ "ns16550", "ns8250" ] },
            ),
            (
                r#"example = <&mpic 0xf00f0000 19>, "a strange property format";"#,
                prop! {
                    "example" => [
                        cells![p!("mpic"), e!(0xf00f0000), e!(19)],
                        "a strange property format"
                    ]
                },
            ),
            (
                r#"serial0 = &usart3;"#,
                prop! { "serial0" => [ p!("usart3") ] },
            ),
            (
                r#"local-mac-address = [ 00 11 22 33 44 55 ];"#,
                prop! { "local-mac-address" => [ vec![0x00, 0x11, 0x22, 0x33, 0x44, 0x55] ] },
            ),
            (
                r#"local-mac-address = [001122334455];"#,
                prop! { "local-mac-address" => [ vec![0x00, 0x11, 0x22, 0x33, 0x44, 0x55] ] },
            ),
            (
                r#"prop = /bits/ 16 <0x1234 0x5678>, <0x9abc 0xdef0>;"#,
                prop! {
                    "prop", 16 => [
                        cells![e!(0x1234), e!(0x5678)],
                        cells![e!(0x9abc), e!(0xdef0)]
                    ]
                },
            ),
            (
                r#"a-very-very-veeeeeery-looooong-property-name-which-should-not-fail;"#,
                prop! { "a-very-very-veeeeeery-looooong-property-name-which-should-not-fail" },
            ),
        ] {
            assert_eq!(expected, property().parse(dbg!(input)).unwrap());
        }
    }

    #[test]
    fn parse_node() {
        for (input, expected) in [
            (
                r#"&tps {};"#,
                Node {
                    id: NodeId::Phandle(Phandle::Label("tps".into())),
                    labels: vec![],
                    contents: vec![],
                    ommittable: false,
                },
            ),
            (
                r#"a-very-very-veeeeeery-looooong-node-name-which-should-not-fail {
                    #address-cells = <1>;
                    #size-cells = <0>;

                    cpu@0 {
                        device_type = "cpu";
                        reg = <0>;
                        cache-unified;
                        cache-size = <0x8000>;
                        cache-block-size = <32>;
                        timebase-frequency = <82500000>;
                        next-level-cache = <&L2_0>;

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
                };"#,
                node! {
                    "a-very-very-veeeeeery-looooong-node-name-which-should-not-fail" => {
                        prop! {"#address-cells" => [ cells!(e!(1)) ] };
                        prop! {"#size-cells" => [ cells!(e!(0)) ] };

                        node! {
                            "cpu","0" => {
                                prop! {"device_type" => [ "cpu" ] };
                                prop! {"reg" => [ cells!(e!(0)) ] };
                                prop! {"cache-unified" };
                                prop! {"cache-size" => [ cells!(e!(0x8000)) ] };
                                prop! {"cache-block-size" => [ cells!(e!(32)) ] };
                                prop! {"timebase-frequency" => [ cells!(e!(82500000)) ] };
                                prop! {"next-level-cache" => [ cells!(p!("L2_0")) ] };

                                node! {
                                    @"L2_0"; "l2-cache" => {
                                       prop! {"compatible" => [ "cache" ] }
                                    }
                                }
                            }
                        };

                        node! {
                            "cpu","1" => {
                                prop! {"device_type" => [ "cpu" ] };
                                prop! {"reg" => [ cells!(e!(1)) ] };

                                node! {
                                    @"L2", "L2_1"; "l2-cache" => {
                                        prop! {"compatible" => [ "cache" ] }
                                    }
                                }
                            }
                        }
                    }
                },
            ),
            (
                r#"/omit-if-no-ref/ label: node@0 {};"#,
                Node {
                    id: NodeId::Name(NodeName {
                        name: "node".into(),
                        address: Some("0".into()),
                    }),
                    labels: vec!["label".into()],
                    contents: vec![],
                    ommittable: true,
                },
            ),
        ] {
            assert_eq!(expected, node(statements()).parse(dbg!(input)).unwrap());
        }
    }
}
