use crate::ast::declaration::{NamedParam, Parameters};
use crate::ast::r#type::Type;

use super::environment::Environment;

#[derive(Debug, Default)]
pub struct TopLevelScope<'package> {
    env: Environment<'package>,
}

impl<'package> TopLevelScope<'package> {
    pub fn env_mut(&mut self) -> &mut Environment<'package> {
        &mut self.env
    }

    pub fn to_function_scope(&self, params: Parameters, returns: Parameters) -> FunctionScope {
        let mut env = self.env.derive();

        if let Parameters::Named(named_params) = params {
            for NamedParam(name, r#type) in named_params {
                env.register_value(name, r#type);
            }
        }
        if let Parameters::Named(named_params) = returns.clone() {
            for NamedParam(name, r#type) in named_params {
                env.register_value(name, r#type);
            }
        }

        FunctionScope {
            env,
            return_types: returns.into_types(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionScope<'package> {
    env: Environment<'package>,
    return_types: Vec<Type>,
}

impl<'package> FunctionScope<'package> {
    pub fn derive_env(&self) -> FunctionScope {
        FunctionScope {
            env: self.env.derive(),
            return_types: self.return_types.to_owned(),
        }
    }

    pub fn env(&self) -> &Environment {
        &self.env
    }

    pub fn env_mut(&mut self) -> &mut Environment<'package> {
        &mut self.env
    }

    pub fn return_types(&self) -> &[Type] {
        &self.return_types
    }
}
