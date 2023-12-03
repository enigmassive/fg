mod format;

use std::io;

use crate::ast::declaration::{FunctionDecl, NamedParam, Parameters};
use crate::ast::expression::{Expression, PrimaryExpr, UnaryExpr};
use crate::ast::literal::Literal;
use crate::ast::name::Name;
use crate::ast::operator::UnaryOp;
use crate::ast::r#type::Type;
use crate::ast::statement::{ElseStmt, IfStmt, Statement};
use crate::ast::Package;

use self::format::{format, Delimiter, Result, Token};

pub fn gen<Writer: io::Write>(package: Package, writer: &mut Writer) -> Result {
    let token = gen_package(package);
    format(token, writer)
}

fn gen_package(package: Package) -> Token {
    Token::LineSeparated(vec![
        Token::SpaceSeparated(vec![Token::Str("package"), Token::String(package.name.0)]),
        gen_function_decls(package.function_decls),
    ])
}

fn gen_function_decls(func_decls: Vec<FunctionDecl>) -> Token {
    let func_decl_tokens = func_decls.into_iter().map(gen_function_decl).collect();
    Token::LineSeparated(func_decl_tokens)
}

fn gen_function_decl(func_decl: FunctionDecl) -> Token {
    let mut tokens = vec![
        Token::Str("func"),
        Token::Vec(vec![
            Token::String(func_decl.name.0),
            gen_parameters(func_decl.params),
        ]),
    ];
    if let Some(returns) = gen_returns(func_decl.returns) {
        tokens.push(returns)
    }
    tokens.push(gen_block(func_decl.block));
    Token::SpaceSeparated(tokens)
}

fn gen_parameters(params: Parameters) -> Token {
    let tokens = match params {
        Parameters::Unnamed(types) => gen_types(types),
        Parameters::Named(named_params) => named_params.into_iter().map(gen_named_param).collect(),
    };
    Token::List(Delimiter::Paren, tokens)
}

fn gen_named_param(named_param: NamedParam) -> Token {
    let NamedParam(name, r#type) = named_param;
    Token::SpaceSeparated(vec![gen_name(name), gen_type(r#type)])
}

fn gen_returns(returns: Parameters) -> Option<Token> {
    if matches!(&returns, Parameters::Unnamed(types) if types.is_empty()) {
        None
    } else {
        Some(gen_parameters(returns))
    }
}

fn gen_name(name: Name) -> Token {
    Token::String(name.0)
}

fn gen_types(types: Vec<Type>) -> Vec<Token> {
    types.into_iter().map(gen_type).collect()
}

fn gen_type(r#type: Type) -> Token {
    match r#type {
        Type::TypeName(name) => gen_name(name),
        Type::ReferenceType(r#type) => gen_reference_type(*r#type),
        Type::FunctionType {
            param_types,
            return_types,
        } => gen_function_type(param_types, return_types),
        Type::ReturnTypes(_) => panic!("matching ReturnTypes on gen_type"),
    }
}

fn gen_reference_type(r#type: Type) -> Token {
    Token::Vec(vec![Token::Str("*"), gen_type(r#type)])
}

fn gen_function_type(param_types: Vec<Type>, mut return_types: Vec<Type>) -> Token {
    let mut tokens = vec![Token::Vec(vec![
        Token::Str("func"),
        Token::List(Delimiter::Paren, gen_types(param_types)),
    ])];

    match return_types.len() {
        1 => tokens.push(gen_type(return_types.swap_remove(0))),
        n if n > 1 => {
            let return_types = Token::List(Delimiter::Paren, gen_types(return_types));
            tokens.push(return_types)
        }
        _ => {}
    }

    Token::SpaceSeparated(tokens)
}

fn gen_block(stmts: Vec<Statement>) -> Token {
    Token::Block(stmts.into_iter().map(gen_statement).collect())
}

fn gen_statement(stmt: Statement) -> Token {
    match stmt {
        Statement::Block(block) => gen_block(block),
        Statement::VarDecl(name, r#type) => gen_var_decl(name, r#type),
        Statement::VarAssign(names, expr) => gen_var_assign(names, expr),
        Statement::ReturnStmt(exprs) => gen_return_stmt(exprs),
        Statement::IfStmt(if_stmt) => gen_if_stmt(if_stmt),
        Statement::Assignment(lhs, rhs) => gen_assignment(lhs, rhs),
        Statement::ExpressionStmt(expr) => gen_expression(expr),
    }
}

fn gen_return_stmt(exprs: Vec<Expression>) -> Token {
    Token::SpaceSeparated(vec![
        Token::Str("return"),
        Token::CommaSeparated(exprs.into_iter().map(gen_expression).collect()),
    ])
}

fn gen_var_decl(name: Name, r#type: Type) -> Token {
    Token::SpaceSeparated(vec![Token::Str("var"), gen_name(name), gen_type(r#type)])
}

fn gen_var_assign(names: Vec<Name>, expr: Expression) -> Token {
    Token::SpaceSeparated(vec![
        Token::Str("var"),
        Token::CommaSeparated(names.into_iter().map(gen_name).collect()),
        Token::Str("="),
        gen_expression(expr),
    ])
}

fn gen_expression(expr: Expression) -> Token {
    match expr {
        Expression::UnaryExpr(unary_expr) => match unary_expr {
            UnaryExpr::UnaryOp(unary_op, unary_expr) => gen_unary_op(unary_op, *unary_expr),
            UnaryExpr::PrimaryExpr(primary_expr) => match primary_expr {
                PrimaryExpr::Literal(lit) => gen_literal(lit),
                PrimaryExpr::OperandName(name) => gen_name(name),
                PrimaryExpr::NestedExpr(expr) => gen_nested_expr(*expr),
                PrimaryExpr::Arguments(primary_expr, args) => gen_arguments(*primary_expr, args),
            },
        },
    }
}

fn gen_assignment(lhs: Vec<Expression>, rhs: Expression) -> Token {
    Token::SpaceSeparated(vec![
        Token::CommaSeparated(lhs.into_iter().map(gen_expression).collect()),
        Token::Str("="),
        gen_expression(rhs),
    ])
}

fn gen_literal(lit: Literal) -> Token {
    match lit {
        Literal::IntLit(int) => Token::String(int),
        Literal::StringLit(string) => Token::String(string),
    }
}

fn gen_nested_expr(expr: Expression) -> Token {
    Token::List(Delimiter::Paren, vec![gen_expression(expr)])
}

fn gen_arguments(callee: PrimaryExpr, args: Vec<Expression>) -> Token {
    let expr = Expression::UnaryExpr(UnaryExpr::PrimaryExpr(callee));
    let callee_token = gen_expression(expr);
    let arg_tokens = args.into_iter().map(gen_expression).collect();
    Token::Vec(vec![
        callee_token,
        Token::List(Delimiter::Paren, arg_tokens),
    ])
}

fn gen_unary_op(op: UnaryOp, expr: UnaryExpr) -> Token {
    let op_token = Token::Str(match op {
        UnaryOp::Deref => "*",
        UnaryOp::Borrow => "&",
    });
    let expr_token = gen_expression(Expression::UnaryExpr(expr));
    Token::Vec(vec![op_token, expr_token])
}

fn gen_if_stmt(if_stmt: IfStmt) -> Token {
    let mut tokens = vec![Token::Str("if")];

    tokens.push(gen_expression(if_stmt.condition));
    tokens.push(gen_block(if_stmt.block));

    if let Some(else_stmt) = if_stmt.else_stmt {
        tokens.push(Token::Str("else"));
        match else_stmt {
            ElseStmt::ElseIf(if_stmt) => tokens.push(gen_if_stmt(*if_stmt)),
            ElseStmt::Else(block) => tokens.push(gen_block(block)),
        }
    }

    Token::SpaceSeparated(tokens)
}
