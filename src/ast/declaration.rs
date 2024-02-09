use super::name::Name;
use super::r#type::Type;
use super::statement::Statement;

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: Name,
    pub params: Parameters,
    pub returns: Parameters,
    pub block: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Parameters {
    Unnamed(Vec<Type>),
    Named(Vec<NamedParam>),
}

#[derive(Debug, Clone)]
pub struct NamedParam {
    pub name: Name,
    pub r#type: Type,
}

impl Parameters {
    pub fn into_types(self) -> Vec<Type> {
        match self {
            Parameters::Unnamed(types) => types,
            Parameters::Named(named_params) => named_params
                .into_iter()
                .map(|NamedParam { r#type, .. }| r#type)
                .collect(),
        }
    }
}
