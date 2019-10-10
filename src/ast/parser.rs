use super::*;

use nom::{
    branch::alt,
    bytes::complete::{is_a, tag, take_till, take_while1},
    character::complete::{alpha1, alphanumeric1, digit1, hex_digit1},
    combinator::{map_res, opt, recognize},
    multi::many0,
    sequence::{pair, preceded, terminated, tuple},
    AsChar, IResult,
};

/// Parse a valid node label.
fn node_label(input: &[u8]) -> IResult<&[u8], &[u8]> {
    recognize(many0(alt((alphanumeric1, symbol(b"_")))))(input)
}

/// Parse a valid node name.
/// A node name has composed of a node-name part and an optional node-address in hex format.
fn node_name(input: &[u8]) -> IResult<&[u8], (&[u8], Option<u32>)> {
    pair(node_name_identifier, opt(preceded(symbol(b"@"), hex)))(input)
}

/// Parse a valid node name identifier,
/// ie. the part of the node name before the unit-address.
fn node_name_identifier(input: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((
        symbol(b"/"),
        recognize(tuple((alpha1, many0(alt((alphanumeric1, is_a(",._+-"))))))),
    ))(input)
}

/* === Utility functions === */

/// Parse a valid number in any base.
fn number(input: &[u8]) -> IResult<&[u8], u32> {
    lexeme(alt((hex, dec)))(input)
}

/// Parse a natural number in base 16.
fn hex(input: &[u8]) -> IResult<&[u8], u32> {
    let (input, _) = opt(symbol(b"0x"))(input)?;
    map_res(lexeme(hex_digit1), |input| {
        u32::from_str_radix(std::str::from_utf8(input).unwrap(), 16)
    })(input)
}

/// Parse a natural number in base 10.
fn dec(input: &[u8]) -> IResult<&[u8], u32> {
    map_res(lexeme(digit1), |input| {
        u32::from_str_radix(std::str::from_utf8(input).unwrap(), 10)
    })(input)
}

/// Consume a fixed symbol.
fn symbol<'a>(s: &'a [u8]) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], &'a [u8]> + 'a {
    lexeme(tag(s))
}

/// Parse a lexeme using the combinator passed as its argument,
/// also consuming zero or more trailing whitespaces.
fn lexeme<'a, O: 'a, F: 'a>(f: F) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], O> + 'a
where
    F: Fn(&'a [u8]) -> IResult<&'a [u8], O> + 'a,
{
    terminated(f, sc)
}

/// Consume zero or more white space characters or line comments.
fn sc(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = many0(alt((space1, skip_line_comment)))(input)?;
    Ok((input, ()))
}

/// Skip one or more white space characters.
fn space1(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while1(|c: u8| c.as_char().is_whitespace())(input)
}

/// Return a parser that skips line comments.
/// Note that it stops just before the newline character but doesn't consume the newline.
fn skip_line_comment(input: &[u8]) -> IResult<&[u8], &[u8]> {
    preceded(symbol(b"//"), take_till(|c: u8| c == b'\n'))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn match_whitespaces() {
        for (input, (rest, matched)) in vec![
            (" ", ("", " ")),
            ("   ", ("", "   ")),
            (" \t", ("", " \t")),
            ("\n", ("", "\n")),
            (" \n", ("", " \n")),
            (" ba", ("ba", " ")),
            (" \nba", ("ba", " \n")),
        ]
        .into_iter()
        {
            assert_eq!(
                space1(input.as_bytes()),
                Ok((rest.as_bytes(), matched.as_bytes()))
            );
        }

        assert!(space1(b"a b").is_err());
    }

    #[test]
    fn match_line_comments() {
        assert_eq!(
            skip_line_comment(b"// This is a comment"),
            Ok((&b""[..], &b"This is a comment"[..]))
        );

        assert_eq!(
            skip_line_comment(b"// Multiline comments\nare not supported"),
            Ok((&b"\nare not supported"[..], &b"Multiline comments"[..]))
        );

        assert!(skip_line_comment(&br#"prop-name = "value"; // This is a comment"#[..]).is_err());
    }

    #[test]
    fn match_node_name() {
        for (input, (name, address)) in vec![
            ("/", ("/", None)),
            ("cpus", ("cpus", None)),
            ("cpu@0", ("cpu", Some(0x0))),
            ("cpu@1", ("cpu", Some(0x1))),
            ("l2-cache", ("l2-cache", None)),
            ("l3-cache", ("l3-cache", None)),
            ("open-pic", ("open-pic", None)),
            ("soc_gpio1", ("soc_gpio1", None)),
            ("memory@0", ("memory", Some(0x0))),
            ("uart@fe001000", ("uart", Some(0xfe00_1000))),
            ("ethernet@fe002000", ("ethernet", Some(0xfe00_2000))),
            ("ethernet@fe003000", ("ethernet", Some(0xfe00_3000))),
        ]
        .into_iter()
        {
            assert_eq!(
                node_name(input.as_bytes()),
                Ok((&b""[..], (name.as_bytes(), address)))
            );
        }
    }

    #[test]
    fn match_node_label() {
        for label in ["L3", "L2_0", "L2_1", "mmc0", "eth0", "pinctrl_wifi_pin"].iter() {
            assert_eq!(
                node_label(label.as_bytes()),
                Ok((&b""[..], label.as_bytes()))
            );
        }
    }
}
