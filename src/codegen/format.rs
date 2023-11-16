use std::io;

use itertools::Itertools;

pub type Result = std::result::Result<(), io::Error>;

#[derive(Clone)]
pub enum Token {
    Str(&'static str),
    String(String),
    Vec(Vec<Token>),
    SpaceSeparated(Vec<Token>),
    LineSeparated(Vec<Token>),
    CommaSeparated(Vec<Token>),
    List(Delimiter, Vec<Token>),
    Block(Vec<Token>),
    // Internals.
    Line,
}

#[derive(Copy, Clone)]
pub enum Delimiter {
    Paren,
    Brace,
}

pub fn format<Writer: io::Write>(token: Token, writer: &mut Writer) -> Result {
    let mut state = State { indent_level: 0 };
    format_token(&mut state, token, writer)
}

struct State {
    indent_level: usize,
}

fn format_token<Writer: io::Write>(state: &mut State, token: Token, writer: &mut Writer) -> Result {
    match token {
        Token::Str(s) => writer.write_all(s.as_bytes()),
        Token::String(s) => writer.write_all(s.as_bytes()),
        Token::Vec(tokens) => format_tokens(state, tokens, writer),
        Token::SpaceSeparated(tokens) => format_space_separated(state, tokens, writer),
        Token::LineSeparated(tokens) => format_line_separated(state, tokens, writer),
        Token::CommaSeparated(tokens) => format_comma_separated(state, tokens, writer),
        Token::List(bracket, tokens) => format_list(state, bracket, tokens, writer),
        Token::Block(tokens) => format_block(state, tokens, writer),
        Token::Line => format_blankline(state, writer),
    }
}

fn format_tokens<Writer: io::Write>(
    state: &mut State,
    tokens: impl IntoIterator<Item = Token>,
    writer: &mut Writer,
) -> Result {
    tokens
        .into_iter()
        .try_for_each(|token| format_token(state, token, writer))
}

fn format_blankline<Writer: io::Write>(state: &mut State, writer: &mut Writer) -> Result {
    writer.write_all(b"\n")?;
    format_newline(state, writer)
}

fn format_newline<Writer: io::Write>(state: &mut State, writer: &mut Writer) -> Result {
    writer.write_all(b"\n")?;
    (0..state.indent_level).try_for_each(|_| writer.write_all(b"\t"))
}

fn format_space_separated<Writer: io::Write>(
    state: &mut State,
    tokens: impl IntoIterator<Item = Token>,
    writer: &mut Writer,
) -> Result {
    format_tokens(
        state,
        Itertools::intersperse(tokens.into_iter(), Token::Str(" ")),
        writer,
    )
}

fn format_line_separated<Writer: io::Write>(
    state: &mut State,
    tokens: impl IntoIterator<Item = Token>,
    writer: &mut Writer,
) -> Result {
    format_tokens(
        state,
        Itertools::intersperse(tokens.into_iter(), Token::Line),
        writer,
    )
}

fn format_comma_separated<Writer: io::Write>(
    state: &mut State,
    tokens: impl IntoIterator<Item = Token>,
    writer: &mut Writer,
) -> Result {
    format_tokens(
        state,
        Itertools::intersperse(tokens.into_iter(), Token::Str(", ")),
        writer,
    )
}

fn format_list<Writer: io::Write>(
    state: &mut State,
    bracket: Delimiter,
    tokens: impl IntoIterator<Item = Token>,
    writer: &mut Writer,
) -> Result {
    format_opening_bracket(bracket, writer)?;
    format_comma_separated(state, tokens, writer)?;
    format_closing_bracket(bracket, writer)
}

fn format_opening_bracket<Writer: io::Write>(bracket: Delimiter, writer: &mut Writer) -> Result {
    let bracket = match bracket {
        Delimiter::Paren => b"(",
        Delimiter::Brace => b"{",
    };
    writer.write_all(bracket)
}

fn format_closing_bracket<Writer: io::Write>(bracket: Delimiter, writer: &mut Writer) -> Result {
    let bracket = match bracket {
        Delimiter::Paren => b")",
        Delimiter::Brace => b"}",
    };
    writer.write_all(bracket)
}

fn format_block<Writer: io::Write>(
    state: &mut State,
    tokens: impl IntoIterator<Item = Token>,
    writer: &mut Writer,
) -> Result {
    format_opening_bracket(Delimiter::Brace, writer)?;
    state.indent_level += 1;
    tokens.into_iter().try_for_each(|token| {
        format_newline(state, writer)?;
        format_token(state, token, writer)
    })?;
    state.indent_level -= 1;
    format_newline(state, writer)?;
    format_closing_bracket(Delimiter::Brace, writer)
}
