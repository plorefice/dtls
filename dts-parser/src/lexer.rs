use std::ops::Range;

use chumsky::prelude::*;

pub(crate) type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Token {
    Num(String),
    Str(String),
    Op(String),
    Ctrl(char),
    Ident(String),
    Path(String),
    Version,
    Bits,
    DeleteNode,
    DeleteProp,
    OmitIfNoRef,
    MemReserve,
}

pub(crate) fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    // A parser for numbers in hexadecimal notation
    let hex = (just("0x").or(just("0X")))
        .map(String::from)
        .chain::<char, _, _>(text::digits(16))
        .collect()
        .map(Token::Num);

    // A parser for numbers in decimal notation
    let dec = text::int(10).map(Token::Num);

    // A parser for string literals
    let str_ = just('"')
        .ignore_then(
            filter(|c| *c != '\\' && *c != '"')
                .or(just('\\').ignore_then(one_of(r#"\/""#)))
                .repeated(),
        )
        .then_ignore(just('"'))
        .collect()
        .map(Token::Str);

    // A parser for operators
    let op = (just("<<")
        .or(just(">>"))
        .or(just("<="))
        .or(just(">="))
        .or(just("=="))
        .or(just("!="))
        .or(just("&&"))
        .or(just("||"))
        .map(String::from))
    .or(one_of("+-*/%^&|~!?:").map(String::from))
    .map(Token::Op);

    // A parser for control characters
    let ctrl = one_of("{}[]()<>;").map(Token::Ctrl);

    // A parser for node names
    let node_name = {
        let charset =
            filter(|c: &char| c.is_ascii_alphanumeric() || ",._+-".contains(*c)).repeated();

        let node_name = filter(char::is_ascii_alphabetic)
            .repeated()
            .at_least(1)
            .chain(charset);

        node_name
            .chain(
                just('@')
                    .chain(charset)
                    .or_not()
                    .map(|v| v.unwrap_or_default()),
            )
            .collect::<Vec<char>>()
    };

    // A parser for paths
    let path = (just('/').chain(node_name))
        .repeated()
        .at_least(1)
        .flatten()
        .collect()
        .map(Token::Path);

    // A parser for identifiers (labels, node names, property names, etc.)
    let ident = filter(|c: &char| c.is_ascii_alphanumeric() || ",._+?#-".contains(*c))
        .repeated()
        .at_least(1) // there should be a maximum of 31, but apparently it's not enforced
        .collect()
        .map(Token::Ident);

    // A parser for directives
    let directive = (just("/dts-v1/").to(Token::Version))
        .or(just("/bits/").to(Token::Bits))
        .or(just("/delete-node/").to(Token::DeleteNode))
        .or(just("/delete-prop/").to(Token::DeleteProp))
        .or(just("/omit-if-no-ref/").to(Token::OmitIfNoRef))
        .or(just("/memreserve/").to(Token::MemReserve));

    // A single token can be any of the above. Ordering is important here!
    let token = directive
        .or(path)
        .or(str_)
        .or(hex)
        .or(dec)
        .or(op)
        .or(ctrl)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    token
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn numbers() {
        for input in [
            "0", "1", "25", "0x1", "0x25", "0x1f", "0x0000", "0x0001", "0x0010", "0x0f00",
        ] {
            assert_eq!(
                Token::Num(input.into()),
                lexer().parse(dbg!(input)).unwrap()[0].0
            );
        }
    }

    #[test]
    fn strings() {
        for (input, expected) in [
            (r#""word""#, "word"),
            (r#""more words""#, "more words"),
            (r#""lots and lots of words""#, "lots and lots of words"),
            ("\"words with\nnewline\"", "words with\nnewline"),
            (
                r#""words with nested \"quotes\"""#,
                r#"words with nested "quotes""#,
            ),
            (
                r#""words with escaped \\ delimiters""#,
                r#"words with escaped \ delimiters"#,
            ),
        ] {
            assert_eq!(
                Token::Str(expected.into()),
                lexer().parse(dbg!(input)).unwrap()[0].0
            );
        }
    }

    #[test]
    fn operators() {
        for input in [
            "<<", ">>", "<=", ">=", "==", "!=", "&&", "||", "+", "-", "*", "/", "%", "^", "&", "|",
            "~", "!", "?", ":",
        ] {
            assert_eq!(
                Token::Op(input.into()),
                lexer().parse(dbg!(input)).unwrap()[0].0
            );
        }
    }

    #[test]
    fn control_characters() {
        for input in ["{", "}", "[", "]", "(", ")", "<", ">", ";"] {
            assert_eq!(
                Token::Ctrl(input.chars().next().unwrap()),
                lexer().parse(dbg!(input)).unwrap()[0].0
            );
        }
    }

    #[test]
    fn identifiers() {
        for input in [
            "cpus",
            "L2_0",
            "l2-cache",
            "ti,pmic-shutdown-controller",
            "#address-cells",
            "a-very-very-long-name-which-should-not-fail",
        ] {
            assert_eq!(
                Token::Ident(input.into()),
                lexer().parse(dbg!(input)).unwrap()[0].0
            );
        }
    }

    #[test]
    fn node_names() {
        for (input, expected) in [
            ("/", Token::Op("/".into())), // although it is also the root node
            ("/cpus", Token::Path("/cpus".into())),
            ("/cpus/cpu@0", Token::Path("/cpus/cpu@0".into())),
            (
                "/cpus/cpu@0/cache@0,0",
                Token::Path("/cpus/cpu@0/cache@0,0".into()),
            ),
        ] {
            assert_eq!(expected, lexer().parse(dbg!(input)).unwrap()[0].0);
        }
    }

    #[test]
    fn directives() {
        for (input, expected) in [
            ("/dts-v1/", Token::Version),
            ("/bits/", Token::Bits),
            ("/delete-node/", Token::DeleteNode),
            ("/delete-prop/", Token::DeleteProp),
            ("/omit-if-no-ref/", Token::OmitIfNoRef),
            ("/memreserve/", Token::MemReserve),
        ] {
            assert_eq!(expected, lexer().parse(dbg!(input)).unwrap()[0].0);
        }
    }
}
