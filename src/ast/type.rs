use super::name::Name;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    TypeName(Name),
    TypeLit(TypeLit),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeLit {
    ReferenceType(Box<Type>),
    FunctionType(Vec<Type>, Vec<Type>),
}
