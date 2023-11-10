pub mod declaration;
pub mod expression;
pub mod literal;
pub mod name;
pub mod operator;
pub mod statement;
pub mod r#type;

use self::declaration::FunctionDecl;
use self::name::Name;

#[derive(Debug, Clone)]
pub struct Package {
    pub name: Name,
    pub function_decls: Vec<FunctionDecl>,
}
