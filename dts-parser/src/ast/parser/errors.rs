use std::ops::Range;

use nom::{error, Err};

use crate::ast::parser::{IResult, Input, ToRange};

#[derive(Debug)]
pub struct Error(Range<usize>, String);

pub(super) fn expect<'a, O, F, E>(
    parser: F,
    error_msg: E,
) -> impl Fn(Input<'a>) -> IResult<Option<O>>
where
    F: Fn(Input<'a>) -> IResult<O>,
    E: ToString,
{
    move |input| match parser(input) {
        Ok((remaining, output)) => Ok((remaining, Some(output))),
        Err(Err::Error(error::Error { input, .. }))
        | Err(Err::Failure(error::Error { input, .. })) => {
            let err = Error(input.to_range(), error_msg.to_string());
            input.extra.report_error(err);
            Ok((input, None))
        }
        Err(err) => Err(err),
    }
}
