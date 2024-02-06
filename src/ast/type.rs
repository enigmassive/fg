use super::name::Name;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    TypeName(Name),
    TypeLit(TypeLit),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeLit {
    ReferenceType(Box<Type>),
    FunctionType {
        param_types: Vec<Type>,
        return_types: Vec<Type>,
    },
}
