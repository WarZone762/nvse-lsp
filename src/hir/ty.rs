use std::collections::HashMap;

use super::VarDeclId;

#[derive(Debug, Clone)]
pub(crate) struct SymbolTable {
    pub map: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Symbol {
    Local(VarDeclId),
    Global(Type),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Type {
    Bool,
    Number,
    Ref,
    String,
    Array(Box<Type>),
    Function(Box<FunctionSignature>),
    Script,
    Ambiguous,
}

impl Type {
    pub fn from_str(string: &str) -> Self {
        match string {
            "int" | "float" | "double" => Self::Ref,
            "ref" => Self::Ref,
            "string" => Self::String,
            "array" => Self::Array(Type::Ambiguous.into()),
            _ => Self::Ambiguous,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FunctionSignature {
    pub ret: Option<Type>,
    pub params: Vec<Type>,
}
