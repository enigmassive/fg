mod builtin;
mod environment;
mod scope;

use std::iter;

use crate::ast::declaration::FunctionDecl;
use crate::ast::expression::{Expression, PrimaryExpr, UnaryExpr};
use crate::ast::literal::Literal;
use crate::ast::name::Name;
use crate::ast::operator::UnaryOp;
use crate::ast::r#type::Type;
use crate::ast::statement::{Declaration, ElseStmt, IfStmt, ShortVarDecl, Statement};
use crate::ast::Package;

use self::scope::{FunctionScope, TopLevelScope};

type Result<T> = std::result::Result<T, ()>;

pub fn check(package: Package) -> Result<()> {
    check_package(package)
}

fn check_package(package: Package) -> Result<()> {
    let mut scope = TopLevelScope::default();

    register_function_decls(&mut scope, package.function_decls.clone());

    check_function_decls(&scope, package.function_decls)
}

fn register_function_decls(scope: &mut TopLevelScope, func_decls: Vec<FunctionDecl>) {
    func_decls
        .into_iter()
        .for_each(|func_decl| register_function_decl(scope, func_decl));
}

fn register_function_decl(scope: &mut TopLevelScope, func_decl: FunctionDecl) {
    let func_type = Type::FunctionType {
        param_types: func_decl.params.into_types(),
        return_types: func_decl.returns.into_types(),
    };
    scope.env_mut().register_value(func_decl.name, func_type);
}

fn check_function_decls(scope: &TopLevelScope, func_decls: Vec<FunctionDecl>) -> Result<()> {
    func_decls
        .into_iter()
        .try_for_each(|func_decl| check_function_decl(scope, func_decl))
}

fn check_function_decl(scope: &TopLevelScope, func_decl: FunctionDecl) -> Result<()> {
    let scope = scope.to_function_scope(func_decl.params, func_decl.returns);
    check_block(&scope, func_decl.block)
}

fn check_block(scope: &FunctionScope, stmts: Vec<Statement>) -> Result<()> {
    let mut scope = scope.derive_env();
    stmts
        .into_iter()
        .try_for_each(|stmt| check_statement(&mut scope, stmt))
}

fn check_statement(scope: &mut FunctionScope, stmt: Statement) -> Result<()> {
    match stmt {
        Statement::Block(block) => check_block(&scope, block),
        Statement::Declaration(decl) => match decl {
            Declaration::VarDecl(name, r#type, expr) => check_var_decl(scope, name, r#type, expr),
        },
        Statement::ReturnStmt(exprs) => check_return_stmt(&scope, exprs),
        Statement::IfStmt(if_stmt) => check_if_stmt(&scope, if_stmt),
        Statement::Assignment(lhs, rhs) => check_assignment(&scope, lhs, rhs),
        Statement::ExpressionStmt(expr) => check_expression(&scope, expr).map(|_| ()),
    }
}

fn check_var_decl(
    scope: &mut FunctionScope,
    name: Name,
    r#type: Type,
    expr: Expression,
) -> Result<()> {
    let expr_type = check_expression(scope, expr)?;
    (r#type == expr_type).then_some(()).ok_or(())?;

    register_var_decl(scope, name, r#type)
}

fn register_var_decl(scope: &mut FunctionScope, name: Name, r#type: Type) -> Result<()> {
    let existing_name = scope.env_mut().register_value(name, r#type);
    existing_name.map_or(Ok(()), |_| Err(()))
}

fn check_expression(scope: &FunctionScope, expr: Expression) -> Result<Type> {
    match expr {
        Expression::UnaryExpr(unary_expr) => match unary_expr {
            UnaryExpr::UnaryOp(unary_op, unary_expr) => match unary_op {
                UnaryOp::Deref => check_deref(scope, *unary_expr),
                UnaryOp::Borrow => check_borrow(scope, *unary_expr),
            },
            UnaryExpr::PrimaryExpr(primary_expr) => match primary_expr {
                PrimaryExpr::Literal(lit) => Ok(check_literal(lit)),
                PrimaryExpr::OperandName(name) => check_name(scope, name),
                PrimaryExpr::NestedExpr(expr) => check_expression(scope, *expr),
                PrimaryExpr::Arguments(callee, args) => check_arguments(scope, *callee, args),
            },
        },
    }
}

fn check_literal(lit: Literal) -> Type {
    match lit {
        Literal::IntLit(_) => builtin::int(),
        Literal::StringLit(_) => builtin::string(),
    }
}

fn check_name(scope: &FunctionScope, name: Name) -> Result<Type> {
    scope.env().value_type(&name).ok_or(())
}

fn check_arguments(
    scope: &FunctionScope,
    callee: PrimaryExpr,
    args: Vec<Expression>,
) -> Result<Type> {
    let expr = Expression::UnaryExpr(UnaryExpr::PrimaryExpr(callee));
    let callee_type = check_expression(scope, expr)?;

    let (param_types, return_types) = match callee_type {
        Type::FunctionType {
            param_types,
            return_types,
        } => Ok((param_types, return_types)),
        _ => Err(()),
    }?;

    (args.len() == param_types.len()).then_some(()).ok_or(())?;

    let arg_types = check_expressions(scope, args)?;
    iter::zip(arg_types, param_types)
        .all(|(arg_type, param_type)| arg_type == param_type)
        .then_some(())
        .ok_or(())?;

    Ok(Type::ReturnTypes(return_types))
}

fn check_expressions(scope: &FunctionScope, exprs: Vec<Expression>) -> Result<Vec<Type>> {
    exprs
        .into_iter()
        .map(|expr| check_expression(scope, expr))
        .collect()
}

fn check_deref(scope: &FunctionScope, unary_expr: UnaryExpr) -> Result<Type> {
    let expr_type = check_expression(scope, Expression::UnaryExpr(unary_expr))?;
    match expr_type {
        Type::ReferenceType(value_type) => Ok(*value_type),
        _ => Err(()),
    }
}

fn check_borrow(scope: &FunctionScope, unary_expr: UnaryExpr) -> Result<Type> {
    let expr_type = check_expression(scope, Expression::UnaryExpr(unary_expr))?;
    Ok(Type::ReferenceType(Box::new(expr_type)))
}

fn check_assignment(scope: &FunctionScope, lhs: Vec<Expression>, rhs: Expression) -> Result<()> {
    (lhs.iter().all(expression_is_assignable))
        .then_some(())
        .ok_or(())?;

    let mut lhs_types = lhs
        .into_iter()
        .map(|expr| check_expression(scope, expr))
        .collect::<Result<Vec<_>>>()?;

    let rhs_type = check_expression(scope, rhs)?;

    if let Type::ReturnTypes(rhs_types) = rhs_type {
        (lhs_types.len() == rhs_types.len())
            .then_some(())
            .ok_or(())?;

        return iter::zip(lhs_types, rhs_types)
            .all(|(lhs_type, rhs_type)| lhs_type == rhs_type)
            .then_some(())
            .ok_or(());
    }

    (lhs_types.len() == 1).then_some(()).ok_or(())?;
    let lhs_type = lhs_types.swap_remove(0);

    (lhs_type == rhs_type).then_some(()).ok_or(())
}

// TODO: Keep in view.
fn expression_is_assignable(expr: &Expression) -> bool {
    match expr {
        Expression::UnaryExpr(expr) => match expr {
            // Pointer indirection.
            UnaryExpr::UnaryOp(UnaryOp::Borrow, expr) => match **expr {
                UnaryExpr::PrimaryExpr(PrimaryExpr::OperandName(_)) => true,
                _ => false,
            },
            // Operand name.
            UnaryExpr::PrimaryExpr(PrimaryExpr::OperandName(_)) => true,
            _ => false,
        },
    }
}

fn check_return_stmt(scope: &FunctionScope, exprs: Vec<Expression>) -> Result<()> {
    let return_types = scope.return_types().to_vec();
    let expr_types = check_expressions(scope, exprs)?;
    (expr_types == return_types).then_some(()).ok_or(())
}

fn check_if_stmt(scope: &FunctionScope, if_stmt: IfStmt) -> Result<()> {
    let mut scope = scope.derive_env();

    if let Some(ShortVarDecl(name, expr)) = if_stmt.var_decl {
        let expr_type = check_expression(&scope, expr)?;
        register_var_decl(&mut scope, name, expr_type)?;
    }

    let cond_type = check_expression(&scope, if_stmt.condition)?;
    (cond_type == builtin::bool()).then_some(()).ok_or(())?;

    check_block(&scope, if_stmt.block)?;

    if let Some(r#else) = if_stmt.else_stmt {
        match r#else {
            ElseStmt::ElseIf(if_stmt) => check_if_stmt(&scope, *if_stmt)?,
            ElseStmt::Else(block) => check_block(&scope, block)?,
        }
    }
    Ok(())
}
