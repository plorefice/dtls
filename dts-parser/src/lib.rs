use std::str;

use chumsky::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Node(Node),
    Property(Property),
    Directive(Directive),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    Version(Version),
    Include(Include),
    DeleteProperty(String),
    DeleteNode(NodeId),
    OmitNode(NodeId),
    MemReserve(u64, u64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    pub id: NodeId,
    pub labels: Vec<String>,
    pub contents: Vec<Statement>,
    pub ommittable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeId {
    Name(NodeName),
    Phandle(Phandle),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeName {
    pub name: String,
    pub address: Option<String>,
}

impl<U, V> From<(U, Option<V>)> for NodeName
where
    U: ToString,
    V: ToString,
{
    fn from((name, address): (U, Option<V>)) -> Self {
        Self {
            name: name.to_string(),
            address: address.map(|v| v.to_string()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Phandle {
    Label(String),
    Path(Vec<NodeName>),
}

impl From<String> for Phandle {
    fn from(s: String) -> Self {
        Self::Label(s)
    }
}

impl From<Vec<NodeName>> for Phandle {
    fn from(v: Vec<NodeName>) -> Self {
        Self::Path(v)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Property {
    pub name: String,
    pub values: Option<Vec<PropertyValue>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropertyValue {
    Str(String),
    Phandle(Phandle),
    Bytestring(Vec<u8>),
    CellArray(Vec<PropertyCell>),
    Bits(u64, Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropertyCell {
    Phandle(Phandle),
    Expr(Expression),
}

impl From<Phandle> for PropertyCell {
    fn from(p: Phandle) -> Self {
        Self::Phandle(p)
    }
}

impl From<Expression> for PropertyCell {
    fn from(e: Expression) -> Self {
        Self::Expr(e)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Lit(IntLiteral),
    Unary(UnaryOp, Box<Expression>),
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    Ternary {
        cond: Box<Expression>,
        then: Box<Expression>,
        else_: Box<Expression>,
    },
}

impl Expression {
    pub fn eval(&self) -> i64 {
        match self {
            Expression::Lit(l) => l.eval(),
            Expression::Unary(op, e) => op.eval(e),
            Expression::Binary(lhs, op, rhs) => op.eval(lhs, rhs),
            Expression::Ternary { cond, then, else_ } => {
                if cond.eval() != 0 {
                    then.eval()
                } else {
                    else_.eval()
                }
            }
        }
    }
}

impl From<i64> for Box<Expression> {
    fn from(i: i64) -> Self {
        Expression::Lit(i.into()).into()
    }
}

impl From<char> for Box<Expression> {
    fn from(c: char) -> Self {
        Expression::Lit(c.into()).into()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntLiteral {
    Num(i64),
    Char(char),
}

impl IntLiteral {
    pub fn eval(&self) -> i64 {
        match self {
            IntLiteral::Num(n) => *n,
            IntLiteral::Char(c) => *c as i64,
        }
    }
}

impl From<i64> for IntLiteral {
    fn from(i: i64) -> Self {
        IntLiteral::Num(i)
    }
}

impl From<char> for IntLiteral {
    fn from(c: char) -> Self {
        IntLiteral::Char(c)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    BitNot,
    LogicalNot,
}

impl UnaryOp {
    pub fn eval<E>(&self, e: E) -> i64
    where
        E: AsRef<Expression>,
    {
        let e = e.as_ref();

        match self {
            UnaryOp::Neg => -e.eval(),
            UnaryOp::BitNot => !e.eval(),
            UnaryOp::LogicalNot => (e.eval() == 0) as i64,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    LShift,
    RShift,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
}

impl BinaryOp {
    pub fn eval<L, R>(&self, l: L, r: R) -> i64
    where
        L: AsRef<Expression>,
        R: AsRef<Expression>,
    {
        let l = l.as_ref();
        let r = r.as_ref();

        match self {
            BinaryOp::Add => l.eval() + r.eval(),
            BinaryOp::Sub => l.eval() - r.eval(),
            BinaryOp::Mul => l.eval() * r.eval(),
            BinaryOp::Div => l.eval() / r.eval(),
            BinaryOp::Mod => l.eval() % r.eval(),
            BinaryOp::BitAnd => l.eval() & r.eval(),
            BinaryOp::BitOr => l.eval() | r.eval(),
            BinaryOp::BitXor => l.eval() ^ r.eval(),
            BinaryOp::LShift => l.eval() << r.eval(),
            BinaryOp::RShift => l.eval() >> r.eval(),
            BinaryOp::And => (l.eval() != 0 && r.eval() != 0) as i64,
            BinaryOp::Or => (l.eval() != 0 || r.eval() != 0) as i64,
            BinaryOp::Eq => (l.eval() == r.eval()) as i64,
            BinaryOp::Neq => (l.eval() != r.eval()) as i64,
            BinaryOp::Lt => (l.eval() < r.eval()) as i64,
            BinaryOp::Gt => (l.eval() > r.eval()) as i64,
            BinaryOp::Le => (l.eval() <= r.eval()) as i64,
            BinaryOp::Ge => (l.eval() >= r.eval()) as i64,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Include {
    C(String),
    Dts(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Version {
    V1,
}

pub fn from_str(s: &str) -> Statement {
    parser().parse(s.as_bytes()).unwrap()
}

fn parser() -> impl Parser<u8, Statement, Error = Simple<u8>> {
    property()
        .then_ignore(semicolon())
        .map(Statement::Property)
        .then_ignore(end())
}

fn property() -> impl Parser<u8, Property, Error = Simple<u8>> {
    let name = filter(|c: &u8| c.is_ascii_alphanumeric() || b",._+?#-".contains(c))
        .repeated()
        .at_least(1)
        .at_most(31)
        .padded();

    let value = (cell_array().map(PropertyValue::CellArray))
        .or(byte_string().map(PropertyValue::Bytestring))
        .or(phandle().map(PropertyValue::Phandle))
        .or(string().map(PropertyValue::Str));

    let values = value.padded().separated_by(just(b',').padded());

    name.then((just(b'=').ignore_then(values)).or_not())
        .map(|(name, values)| Property {
            name: String::from_utf8(name).unwrap(),
            values,
        })
}

fn cell_array() -> impl Parser<u8, Vec<PropertyCell>, Error = Simple<u8>> {
    let expr = literal()
        .or(expr().delimited_by(just(b'('), just(b')')))
        .map(PropertyCell::Expr);

    let phandle = phandle().map(PropertyCell::Phandle);

    (phandle.or(expr))
        .padded()
        .repeated()
        .delimited_by(just(b'<'), just(b'>'))
        .collect()
}

fn byte_string() -> impl Parser<u8, Vec<u8>, Error = Simple<u8>> {
    let byte = filter(|c: &u8| c.is_ascii_hexdigit())
        .repeated()
        .exactly(2)
        .map(|s| u8::from_str_radix(str::from_utf8(&s).unwrap(), 16))
        .unwrapped()
        .padded();

    byte.repeated()
        .delimited_by(just(b'['), just(b']'))
        .padded()
        .collect()
}

fn expr() -> impl Parser<u8, Expression, Error = Simple<u8>> + Clone {
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
            .or(expr.delimited_by(just(b'('), just(b')')))
            .padded();

        let unary = (just(b'-').to(UnaryOp::Neg))
            .or(just(b'~').to(UnaryOp::BitNot))
            .or(just(b'!').to(UnaryOp::LogicalNot))
            .padded()
            .repeated()
            .then(atom)
            .foldr(|op, rhs| Expression::Unary(op, Box::new(rhs)));

        let binary = {
            let product = bin_op!(
                unary,
                (just(b'*').to(BinaryOp::Mul))
                    .or(just(b'/').to(BinaryOp::Div))
                    .or(just(b'/').to(BinaryOp::Mod))
            );

            let sum = bin_op!(
                product,
                (just(b'+').to(BinaryOp::Add)).or(just(b'-').to(BinaryOp::Sub))
            );

            let bitshift = bin_op!(
                sum,
                (just(b"<<").to(BinaryOp::LShift)).or(just(b">>").to(BinaryOp::RShift))
            );

            let comparison = bin_op!(
                bitshift,
                (just(b"<=").to(BinaryOp::Le))
                    .or(just(b">=").to(BinaryOp::Ge))
                    .or(just(b"<").to(BinaryOp::Lt))
                    .or(just(b">").to(BinaryOp::Gt))
            );

            let equality = bin_op!(
                comparison,
                (just(b"==").to(BinaryOp::Eq)).or(just(b"!=").to(BinaryOp::Neq))
            );

            let bit_and = bin_op!(equality, (just(b'&').to(BinaryOp::BitAnd)));
            let bit_xor = bin_op!(bit_and, (just(b'^').to(BinaryOp::BitXor)));
            let bit_or = bin_op!(bit_xor, (just(b'|').to(BinaryOp::BitOr)));

            let and = bin_op!(bit_or, (just(b"&&").to(BinaryOp::And)));
            bin_op!(and, (just(b"||").to(BinaryOp::Or)))
        };

        // A ternary expression (lowest precedence), or a simple expression if not ternary
        (binary.clone())
            .then(
                just(b'?')
                    .ignore_then(binary.clone())
                    .then_ignore(just(b':'))
                    .then(binary)
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
}

fn phandle() -> impl Parser<u8, Phandle, Error = Simple<u8>> + Clone {
    let label = node_label().map(Phandle::Label);

    let path = node_path()
        .delimited_by(just(b'{'), just(b'}'))
        .map(Phandle::Path);

    just(b'&').ignore_then(path.or(label))
}

fn node_label() -> impl Parser<u8, String, Error = Simple<u8>> + Clone {
    text::ident().map(|s| String::from_utf8(s).unwrap())
}

fn node_path() -> impl Parser<u8, Vec<NodeName>, Error = Simple<u8>> + Clone {
    just(b'/').ignore_then(node_name().separated_by(just(b'/')))
}

fn node_name() -> impl Parser<u8, NodeName, Error = Simple<u8>> + Clone {
    let ident = filter(|c: &u8| c.is_ascii_alphanumeric() || b",._+-".contains(c))
        .repeated()
        .at_least(1)
        .at_most(31);

    ident
        .then(just(b'@').ignore_then(ident).or_not())
        .map(|(name, addr)| NodeName {
            name: String::from_utf8(name).unwrap(),
            address: addr.map(|addr| String::from_utf8(addr).unwrap()),
        })
}

fn string() -> impl Parser<u8, String, Error = Simple<u8>> + Clone {
    let escape = just(b'\\').ignore_then(one_of(b"\\/\""));

    just(b'"')
        .ignore_then(
            filter(|c: &u8| *c != b'\\' && *c != b'"')
                .or(escape)
                .repeated(),
        )
        .then_ignore(just(b'"'))
        .map(|v| String::from_utf8(v).unwrap())
}

fn literal() -> impl Parser<u8, Expression, Error = Simple<u8>> + Clone {
    int().or(char_()).map(Expression::Lit)
}

fn int() -> impl Parser<u8, IntLiteral, Error = Simple<u8>> + Clone {
    hex().or(dec()).map(IntLiteral::Num).padded()
}

fn char_() -> impl Parser<u8, IntLiteral, Error = Simple<u8>> + Clone {
    filter(u8::is_ascii_graphic)
        .delimited_by(just(b'\''), just(b'\''))
        .map(|c| IntLiteral::Char(c as char))
        .padded()
}

fn hex() -> impl Parser<u8, i64, Error = Simple<u8>> + Clone {
    just(b"0x")
        .or(just(b"0X"))
        .then(text::int(16))
        .map(|(_, vs): (_, Vec<u8>)| i64::from_str_radix(str::from_utf8(&vs).unwrap(), 16).unwrap())
}

fn dec() -> impl Parser<u8, i64, Error = Simple<u8>> + Clone {
    text::int(10).map(|vs: Vec<u8>| str::from_utf8(&vs).unwrap().parse::<i64>().unwrap())
}

fn semicolon() -> impl Parser<u8, u8, Error = Simple<u8>> + Clone {
    just(b';').padded()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_unsigned_numbers() {
        for (input, expected) in [
            ("0", 0),
            ("1", 1),
            ("25", 25),
            ("0x1", 0x1),
            ("0x25", 0x25),
            ("0x1f", 0x1f),
        ] {
            assert_eq!(
                Expression::Lit(expected.into()),
                expr().parse(dbg!(input.as_bytes())).unwrap()
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
                expr().parse(dbg!(input.as_bytes())).unwrap()
            );
        }
    }

    #[test]
    fn parse_chars() {
        for c in "0618jaAfJzaiw)k+1'-_!?".chars() {
            assert_eq!(
                Expression::Lit(c.into()),
                expr().parse(format!("'{}'", c).as_bytes()).unwrap()
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
            let expr = expr().parse(dbg!(input.as_bytes())).unwrap();

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
        ] {
            let expr = expr().parse(dbg!(input.as_bytes())).unwrap();

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
            assert_eq!(expected, node_name().parse(dbg!(input.as_bytes())).unwrap());
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
            assert_eq!(expected, phandle().parse(dbg!(input.as_bytes())).unwrap());
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
            assert_eq!(
                expected,
                cell_array().parse(dbg!(input).as_bytes()).unwrap()
            );
        }
    }

    #[test]
    fn parse_property() {
        use super::Phandle::*;
        use Expression::*;
        use PropertyCell::*;
        use PropertyValue::*;

        for (input, expected) in [
            (
                r#"cache-unified;"#,
                Property {
                    name: "cache-unified".into(),
                    values: None,
                },
            ),
            (
                r#"reg = <0>;"#,
                Property {
                    name: "reg".into(),
                    values: Some(vec![CellArray(vec![Expr(Lit(0.into()))])]),
                },
            ),
            (
                r#"cache-size = <0x8000>;"#,
                Property {
                    name: "cache-size".into(),
                    values: Some(vec![CellArray(vec![Expr(Lit(0x8000.into()))])]),
                },
            ),
            (
                r#"next-level-cache = <&L2_0>;"#,
                Property {
                    name: "next-level-cache".into(),
                    values: Some(vec![CellArray(vec![Label("L2_0".into()).into()])]),
                },
            ),
            (
                r#"interrupts = <17 0xc 'A'>;"#,
                Property {
                    name: "interrupts".into(),
                    values: Some(vec![CellArray(vec![
                        Expr(Lit(17.into())),
                        Expr(Lit(0xc.into())),
                        Expr(Lit('A'.into())),
                    ])]),
                },
            ),
            (
                r#"cpu = <&{/cpus/cpu@0}>;"#,
                Property {
                    name: "cpu".into(),
                    values: Some(vec![CellArray(vec![Path(vec![
                        ("cpus", None::<&str>).into(),
                        ("cpu", Some("0")).into(),
                    ])
                    .into()])]),
                },
            ),
            (
                r#"pinctrl-0 = <>;"#,
                Property {
                    name: "pinctrl-0".into(),
                    values: Some(vec![PropertyValue::CellArray(vec![])]),
                },
            ),
            (
                r#"device_type = "cpu";"#,
                Property {
                    name: "device_type".into(),
                    values: Some(vec![Str("cpu".into())]),
                },
            ),
            (
                r#"compatible = "ns16550", "ns8250";"#,
                Property {
                    name: "compatible".into(),
                    values: Some(vec![Str("ns16550".into()), Str("ns8250".into())]),
                },
            ),
            (
                r#"example = <&mpic 0xf00f0000 19>, "a strange property format";"#,
                Property {
                    name: "example".into(),
                    values: Some(vec![
                        CellArray(vec![
                            Label("mpic".into()).into(),
                            Expr(Lit(0xf00f_0000.into())),
                            Expr(Lit(19.into())),
                        ]),
                        Str("a strange property format".into()),
                    ]),
                },
            ),
            (
                r#"serial0 = &usart3;"#,
                Property {
                    name: "serial0".into(),
                    values: Some(vec![PropertyValue::Phandle(Label("usart3".into()))]),
                },
            ),
            (
                r#"local-mac-address = [ 00 11 22 33 44 55 ];"#,
                Property {
                    name: "local-mac-address".into(),
                    values: Some(vec![Bytestring(vec![0x00, 0x11, 0x22, 0x33, 0x44, 0x55])]),
                },
            ),
            (
                r#"local-mac-address = [001122334455];"#,
                Property {
                    name: "local-mac-address".into(),
                    values: Some(vec![Bytestring(vec![0x00, 0x11, 0x22, 0x33, 0x44, 0x55])]),
                },
            ),
        ] {
            assert_eq!(expected, property().parse(dbg!(input).as_bytes()).unwrap());
        }
    }
}
