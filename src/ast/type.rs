use super::name::Name;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    TypeName(Name),
    ReferenceType(Box<Type>),
    FunctionType {
        param_types: Vec<Type>,
        return_types: Vec<Type>,
    },
    ReturnTypes(Vec<Type>), // Only used in typecheck.
}
