use crate::ast::name::Name;

use super::r#type::Type;

pub fn bool() -> Type {
    Type::TypeName(Name("bool".to_owned()))
}

pub fn int() -> Type {
    Type::TypeName(Name("int".to_owned()))
}

pub fn string() -> Type {
    Type::TypeName(Name("string".to_owned()))
}
