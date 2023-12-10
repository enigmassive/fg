mod lex;

use chumsky::prelude::Simple;
use chumsky::primitive::{choice, just};
use chumsky::recursive::recursive;
use chumsky::{select, Parser};

use crate::ast::declaration::{FunctionDecl, NamedParam, Parameters};
use crate::ast::expression::{Expression, PrimaryExpr, UnaryExpr};
use crate::ast::literal::Literal;
use crate::ast::name::Name;
use crate::ast::operator::UnaryOp;
use crate::ast::r#type::Type;
use crate::ast::statement::{ElseStmt, IfStmt, Statement};
use crate::ast::Package;

use self::lex::{Keyword, Symbol, Token};

pub fn parse(src: &str) -> Package {
    let tokens = lex::lexer().parse(src).unwrap();
    package().parse(tokens).unwrap()
}

fn package() -> impl Parser<Token, Package, Error = Simple<Token>> {
    let func_decls = function_decl().repeated().collect();

    just(Token::Keyword(Keyword::Package))
        .ignore_then(name())
        .then(func_decls)
        .map(|(name, function_decls)| Package {
            name,
            function_decls,
        })
}

fn name() -> impl Parser<Token, Name, Error = Simple<Token>> {
    select! {
        Token::Name(name) => Name(name)
    }
}

fn function_decl() -> impl Parser<Token, FunctionDecl, Error = Simple<Token>> {
    just(Token::Keyword(Keyword::Func))
        .ignore_then(name())
        .then(parameters(r#type()))
        .then(returns(r#type()))
        .then(block())
        .map(|(((name, params), returns), block)| FunctionDecl {
            name,
            params,
            returns,
            block,
        })
}

fn parameters(
    r#type: impl Parser<Token, Type, Error = Simple<Token>> + Clone,
) -> impl Parser<Token, Parameters, Error = Simple<Token>> {
    let unnamed_params = r#type
        .clone()
        .separated_by(just(Token::Symbol(Symbol::Comma)))
        .collect::<Vec<_>>()
        .map(Parameters::Unnamed);

    let named_params = {
        let named_param = name()
            .then(r#type.clone())
            .map(|(name, r#type)| NamedParam(name, r#type));
        named_param
            .separated_by(just(Token::Symbol(Symbol::Comma)))
            .collect::<Vec<_>>()
            .map(Parameters::Named)
    };

    let l_paren = just(Token::Symbol(Symbol::LeftParen));
    let r_paren = just(Token::Symbol(Symbol::RightParen));
    choice((
        unnamed_params.delimited_by(l_paren.clone(), r_paren.clone()),
        named_params.delimited_by(l_paren, r_paren),
    ))
}

fn returns(
    r#type: impl Parser<Token, Type, Error = Simple<Token>> + Clone,
) -> impl Parser<Token, Parameters, Error = Simple<Token>> {
    let naked_type = r#type
        .clone()
        .map(|r#type| Parameters::Unnamed(vec![r#type]));

    parameters(r#type)
        .or(naked_type)
        .or_not()
        .map(|returns| returns.unwrap_or(Parameters::Unnamed(vec![])))
}

fn block() -> impl Parser<Token, Vec<Statement>, Error = Simple<Token>> {
    recursive(|block| {
        statement(block).repeated().collect().delimited_by(
            just(Token::Symbol(Symbol::LeftBrace)),
            just(Token::Symbol(Symbol::RightBrace)),
        )
    })
}

fn r#type() -> impl Parser<Token, Type, Error = Simple<Token>> + Clone {
    recursive(|r#type| {
        let type_name = name().map(Type::TypeName);

        let reference_type = just(Token::Symbol(Symbol::Ampersand))
            .ignore_then(r#type.clone())
            .map(Box::new)
            .map(Type::ReferenceType);

        let func_type = just(Token::Keyword(Keyword::Func))
            .ignore_then(parameters(r#type.clone()))
            .then(returns(r#type.clone()))
            .map(|(params, returns)| Type::FunctionType {
                param_types: params.into_types(),
                return_types: returns.into_types(),
            });

        choice((type_name, reference_type, func_type))
    })
}

pub fn statement(
    block: impl Parser<Token, Vec<Statement>, Error = Simple<Token>> + Clone + 'static,
) -> impl Parser<Token, Statement, Error = Simple<Token>> {
    let var_decl = just(Token::Keyword(Keyword::Var))
        .ignore_then(name())
        .then(r#type())
        .map(|(name, r#type)| Statement::VarDecl(name, r#type));

    let var_assign = {
        let names = name()
            .separated_by(just(Token::Symbol(Symbol::Comma)))
            .at_least(1);
        just(Token::Keyword(Keyword::Var))
            .ignore_then(names)
            .then(just(Token::Symbol(Symbol::Equal)))
            .then(expression())
            .map(|((names, _), expr)| Statement::VarAssign(names, expr))
    };

    let return_stmt = just(Token::Keyword(Keyword::Return))
        .ignore_then(expression().separated_by(just(Token::Symbol(Symbol::Comma))))
        .map(Statement::ReturnStmt);

    let if_stmt = if_stmt(block.clone()).map(Statement::IfStmt);

    let assignment = {
        let lhs = expression()
            .separated_by(just(Token::Symbol(Symbol::Comma)))
            .at_least(1);
        lhs.then(just(Token::Symbol(Symbol::Equal)))
            .then(expression())
            .map(|((lhs, _), rhs)| Statement::Assignment(lhs, rhs))
    };

    choice((
        block.map(Statement::Block),
        var_decl,
        var_assign,
        return_stmt,
        if_stmt,
        assignment,
        expression().map(Statement::ExpressionStmt),
    ))
}

pub fn if_stmt(
    block: impl Parser<Token, Vec<Statement>, Error = Simple<Token>> + Clone + 'static,
) -> impl Parser<Token, IfStmt, Error = Simple<Token>> {
    recursive(|r#if| {
        let else_stmt = {
            let else_if = just(Token::Keyword(Keyword::Else))
                .ignore_then(r#if)
                .map(Box::new)
                .map(ElseStmt::ElseIf);
            let r#else = just(Token::Keyword(Keyword::Else))
                .ignore_then(block.clone())
                .map(ElseStmt::Else);
            else_if.or(r#else)
        };

        just(Token::Keyword(Keyword::If))
            .ignore_then(expression())
            .then(block.clone())
            .then(else_stmt.or_not())
            .map(|((condition, block), else_stmt)| IfStmt {
                condition,
                block,
                else_stmt,
            })
    })
}

pub fn expression() -> impl Parser<Token, Expression, Error = Simple<Token>> + Clone {
    recursive(|expression| {
        let unary_expr = unary_expr(expression).map(Expression::UnaryExpr);

        unary_expr
    })
}

pub fn unary_expr(
    expression: impl Parser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl Parser<Token, UnaryExpr, Error = Simple<Token>> {
    recursive(|unary_expr| {
        let unary_op = unary_op()
            .then(unary_expr.map(Box::new))
            .map(|(op, expression)| UnaryExpr::UnaryOp(op, expression));

        let primary_expr = primary_expr(expression).map(UnaryExpr::PrimaryExpr);

        unary_op.or(primary_expr)
    })
}

pub fn primary_expr(
    expression: impl Parser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl Parser<Token, PrimaryExpr, Error = Simple<Token>> {
    let literal = literal().map(PrimaryExpr::Literal);

    let operand_name = name().map(PrimaryExpr::OperandName);

    let nested_expr = expression
        .clone()
        .delimited_by(
            just(Token::Symbol(Symbol::LeftParen)),
            just(Token::Symbol(Symbol::RightParen)),
        )
        .map(Box::new)
        .map(PrimaryExpr::NestedExpr);

    enum Tail {
        Arguments(Vec<Expression>),
    }
    let tail = {
        let arguments = expression
            .separated_by(just(Token::Symbol(Symbol::Comma)))
            .collect()
            .delimited_by(
                just(Token::Symbol(Symbol::LeftParen)),
                just(Token::Symbol(Symbol::RightParen)),
            )
            .map(Tail::Arguments);

        arguments
    };

    choice((literal, operand_name, nested_expr))
        .then(tail.repeated())
        .foldl(|primary_expr, tail| match tail {
            Tail::Arguments(args) => PrimaryExpr::Arguments(Box::new(primary_expr), args),
        })
}

fn literal() -> impl Parser<Token, Literal, Error = Simple<Token>> {
    select! {
        Token::String(s) => Literal::StringLit(s),
        Token::Number(n) => Literal::IntLit(n),
    }
}

fn unary_op() -> impl Parser<Token, UnaryOp, Error = Simple<Token>> {
    select! {
        Token::Symbol(Symbol::Ampersand) => UnaryOp::Borrow,
        Token::Symbol(Symbol::Asterisk) => UnaryOp::Deref,
    }
}
