use super::expression::Expression;
use super::name::Name;
use super::r#type::Type;

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Statement>),
    VarDecl(Name, Type),
    VarAssign(Vec<Name>, Expression),
    ReturnStmt(Vec<Expression>),
    IfStmt(IfStmt),
    Assignment(Vec<Expression>, Expression),
    ExpressionStmt(Expression),
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Expression,
    pub block: Vec<Statement>,
    pub else_stmt: Option<ElseStmt>,
}

#[derive(Debug, Clone)]
pub enum ElseStmt {
    ElseIf(Box<IfStmt>),
    Else(Vec<Statement>),
}
