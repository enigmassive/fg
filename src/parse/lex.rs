use std::iter;

use chumsky::prelude::Simple;
use chumsky::primitive::{choice, end, filter, just};
use chumsky::text::{self, TextParser};
use chumsky::Parser;

pub fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    token().padded().repeated().then_ignore(end())
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Token {
    String(String),
    Number(String),
    Symbol(Symbol),
    Keyword(Keyword),
    Name(String),
}

fn token() -> impl Parser<char, Token, Error = Simple<char>> {
    let string = just('"')
        .then(filter(|c| *c != '"').repeated())
        .then(just('"'))
        .map(|((l, s), r)| iter::once(l).chain(s).chain(iter::once(r)))
        .collect()
        .map(Token::String);

    let number = text::int(10).map(Token::Number);

    let name = text::ident().map(Token::Name);

    choice((
        string,
        number,
        symbol().map(Token::Symbol),
        keyword_lexer().map(Token::Keyword),
        name,
    ))
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Symbol {
    Ampersand,
    Asterisk,
    Comma,
    ColonEqual,
    Semicolon,
    Equal,
    LeftParen,
    RightParen,
    LeftSquare,
    RightSquare,
    LeftBrace,
    RightBrace,
}

fn symbol() -> impl Parser<char, Symbol, Error = Simple<char>> {
    choice((
        just("&").to(Symbol::Ampersand),
        just("*").to(Symbol::Asterisk),
        just(",").to(Symbol::Comma),
        just(":=").to(Symbol::ColonEqual),
        just(";").to(Symbol::Semicolon),
        just("=").to(Symbol::Equal),
        just("(").to(Symbol::LeftParen),
        just(")").to(Symbol::RightParen),
        just("[").to(Symbol::LeftSquare),
        just("]").to(Symbol::RightSquare),
        just("{").to(Symbol::LeftBrace),
        just("}").to(Symbol::RightBrace),
    ))
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Keyword {
    Else,
    Func,
    If,
    Package,
    Return,
    Var,
}

fn keyword_lexer() -> impl Parser<char, Keyword, Error = Simple<char>> {
    choice((
        text::keyword("else").to(Keyword::Else),
        text::keyword("func").to(Keyword::Func),
        text::keyword("if").to(Keyword::If),
        text::keyword("package").to(Keyword::Package),
        text::keyword("return").to(Keyword::Return),
        text::keyword("var").to(Keyword::Var),
    ))
}
