use crate::ast::name::Name;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    TypeName(Name),
    ReferenceType(Box<Type>),
    FunctionType(Vec<Type>, Vec<Type>),
    ReturnTypes(Vec<Type>),
}
