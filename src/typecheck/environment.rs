use std::collections::HashMap;

use crate::ast::name::Name;
use crate::ast::r#type::Type;

use super::predeclared;

#[derive(Debug, Default)]
pub struct Environment<'package> {
    parent_env: Option<&'package Self>,
    value_types: HashMap<Name, Type>,
}

impl<'package> Environment<'package> {
    pub fn derive(&self) -> Environment {
        Environment {
            parent_env: Some(&self),
            value_types: HashMap::new(),
        }
    }

    pub fn register_value(&mut self, name: Name, r#type: Type) -> Option<Type> {
        self.value_types.insert(name, r#type)
    }

    pub fn value_type(&self, name: &Name) -> Option<Type> {
        if let Some(r#type) = predeclared::predeclared().get(name) {
            return Some(r#type.to_owned());
        }
        self.value_types
            .get(name)
            .map(ToOwned::to_owned)
            .or_else(|| {
                let parent_env = self.parent_env?;
                parent_env.value_type(name)
            })
    }
}
