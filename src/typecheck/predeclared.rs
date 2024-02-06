use std::collections::HashMap;
use std::sync::OnceLock;

use crate::ast::name::Name;
use crate::typecheck::builtin;

use super::r#type::Type;

pub fn predeclared() -> &'static HashMap<Name, Type> {
    static PREDECLARED: OnceLock<HashMap<Name, Type>> = OnceLock::new();
    PREDECLARED.get_or_init(|| {
        let mut m = HashMap::new();
        m.insert(Name("true".to_owned()), builtin::bool());
        m.insert(Name("false".to_owned()), builtin::bool());
        m
    })
}
