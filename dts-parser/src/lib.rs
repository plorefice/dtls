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
    Phandle(String),
    Name(String, Option<String>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Property {
    pub name: String,
    pub value: Option<Vec<PropertyValue>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropertyValue {
    Str(String),
    Phandle(String),
    Bytestring(Vec<u8>),
    CellArray(Vec<PropertyCell>),
    Bits(u64, Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropertyCell {
    Phandle(String),
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

pub fn from_str(s: &str) -> Expression {
    parser().parse(s.as_bytes()).unwrap()
}

macro_rules! bin_op {
    ($p:expr, $op:expr) => {
        $p.clone()
            .then($op.padded().then($p).repeated())
            .foldl(|lhs, (op, rhs)| Expression::Binary(Box::new(lhs), op, Box::new(rhs)))
            .boxed()
    };
}

fn parser() -> impl Parser<u8, Expression, Error = Simple<u8>> {
    // An expression used in a property value
    let expr = recursive(|expr| {
        // Unsigned number in hexadecimal notation
        let hex = just(b"0x")
            .or(just(b"0X"))
            .then(text::int(16))
            .map(|(_, vs): (_, Vec<u8>)| {
                i64::from_str_radix(str::from_utf8(&vs).unwrap(), 16).unwrap()
            });

        // Unsigned number in decimal notation
        let dec =
            text::int(10).map(|vs: Vec<u8>| str::from_utf8(&vs).unwrap().parse::<i64>().unwrap());

        // Unsigned integer literal
        let int = hex.or(dec).map(IntLiteral::Num).padded();

        // Character literal
        let char_ = filter(u8::is_ascii_graphic)
            .delimited_by(just(b'\''), just(b'\''))
            .map(|c| IntLiteral::Char(c as char))
            .padded();

        // Any literal valid in the context of an expression
        let literal = int.or(char_);

        // 'Atoms' are expressions that contain no ambiguity (highest precedence)
        let atom = (literal.map(Expression::Lit))
            .or(expr.delimited_by(just(b'('), just(b')')))
            .padded();

        // A unary expression, i.e. an atom prefixed by a unary operator
        let unary = (just(b'-').to(UnaryOp::Neg))
            .or(just(b'~').to(UnaryOp::BitNot))
            .or(just(b'!').to(UnaryOp::LogicalNot))
            .padded()
            .repeated()
            .then(atom)
            .foldr(|op, rhs| Expression::Unary(op, Box::new(rhs)));

        // A binary expression, accounting for operator precedence
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
    });

    expr.then_ignore(end())
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
                parser().parse(input.as_bytes()).unwrap()
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
                parser().parse(input.as_bytes()).unwrap()
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
            let expr = parser().parse(input.as_bytes()).unwrap();

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
            let expr = parser().parse(input.as_bytes()).unwrap();

            assert_eq!(ast, expr);
            assert_eq!(res, expr.eval());
        }
    }
}
