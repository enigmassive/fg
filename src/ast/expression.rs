use super::literal::Literal;
use super::name::Name;
use super::operator::UnaryOp;

#[derive(Debug, Clone)]
pub enum Expression {
    UnaryExpr(UnaryExpr),
}

#[derive(Debug, Clone)]
pub enum UnaryExpr {
    UnaryOp(UnaryOp, Box<UnaryExpr>),
    PrimaryExpr(PrimaryExpr),
}

#[derive(Debug, Clone)]
pub enum PrimaryExpr {
    Literal(Literal),
    OperandName(Name),
    NestedExpr(Box<Expression>),
    Arguments(Box<PrimaryExpr>, Vec<Expression>),
}
