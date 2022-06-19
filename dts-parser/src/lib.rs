use std::str;

use chumsky::prelude::*;
use derive_more::From;

#[derive(Debug, Clone, PartialEq, Eq, From)]
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

#[derive(Debug, Clone, PartialEq, Eq, From)]
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

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum Phandle {
    Label(String),
    Path(Vec<NodeName>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Property {
    pub name: String,
    pub values: Option<Vec<PropertyValue>>,
    pub storage_modifier: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum PropertyValue {
    Str(String),
    Phandle(Phandle),
    Bytestring(Vec<u8>),
    CellArray(Vec<PropertyCell>),
}

impl From<&str> for PropertyValue {
    fn from(s: &str) -> Self {
        Self::from(s.to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum PropertyCell {
    Phandle(Phandle),
    Expr(Expression),
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

impl From<i64> for Expression {
    fn from(i: i64) -> Self {
        Expression::Lit(i.into())
    }
}

impl From<char> for Expression {
    fn from(c: char) -> Self {
        Expression::Lit(c.into())
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

pub fn from_str(s: &str) -> Result<Vec<Statement>, Vec<Simple<char>>> {
    parser().parse(s)
}

fn parser() -> impl Parser<char, Vec<Statement>, Error = Simple<char>> + Clone {
    statements().then_ignore(end())
}

fn statements() -> impl Parser<char, Vec<Statement>, Error = Simple<char>> + Clone {
    recursive(|stmts| {
        (node(stmts).map(Statement::from))
            .or(property().map(Statement::from))
            .or(directive().map(Statement::from))
            .padded()
            .repeated()
    })
}

fn node(
    stmts: impl Parser<char, Vec<Statement>, Error = Simple<char>> + Clone,
) -> impl Parser<char, Node, Error = Simple<char>> + Clone {
    let labels = node_label()
        .then_ignore(just(':').padded())
        .padded()
        .repeated();

    let root_name = just('/')
        .map(|_| NodeName {
            name: "/".to_string(),
            address: None,
        })
        .padded();

    let node_id = (node_name().or(root_name).map(NodeId::from))
        .or(phandle().map(NodeId::from))
        .padded();

    labels
        .then(node_id)
        .then(stmts.delimited_by(just('{').padded(), just('}').padded()))
        .then_ignore(semicolon())
        .map(|((labels, node_id), contents)| Node {
            id: node_id,
            labels,
            contents,
            ommittable: false,
        })
}

fn property() -> impl Parser<char, Property, Error = Simple<char>> + Clone {
    let name = filter(|c: &char| c.is_ascii_alphanumeric() || ",._+?#-".contains(*c))
        .repeated()
        .at_least(1) // there should be a maximum of 31, but apparently it's not enforced
        .collect()
        .padded();

    let value = (cell_array().map(PropertyValue::CellArray))
        .or(byte_string().map(PropertyValue::Bytestring))
        .or(phandle().map(PropertyValue::Phandle))
        .or(string().map(PropertyValue::Str));

    let values = value.padded().separated_by(just(',').padded());

    let bits = just("/bits/").padded().ignore_then(number());

    name.then((just('=').ignore_then(bits.or_not().then(values))).or_not())
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
            .then(atom)
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
        (binary.clone())
            .then(
                just('?')
                    .ignore_then(binary.clone())
                    .then_ignore(just(':'))
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

fn phandle() -> impl Parser<char, Phandle, Error = Simple<char>> + Clone {
    let label = node_label().map(Phandle::Label);

    let path = node_path()
        .delimited_by(just('{'), just('}'))
        .map(Phandle::Path);

    just('&').ignore_then(path.or(label))
}

fn node_label() -> impl Parser<char, String, Error = Simple<char>> + Clone {
    text::ident()
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

    version.or(include)
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
                "/include/ \"sama5.dtsi\"",
                Directive::Include(Include::Dts("sama5.dtsi".into())),
            ),
            (
                "/include/ \"overlays/sama5.dtsi\"",
                Directive::Include(Include::Dts("overlays/sama5.dtsi".into())),
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
        ] {
            assert_eq!(expected, node(statements()).parse(dbg!(input)).unwrap());
        }
    }
}
