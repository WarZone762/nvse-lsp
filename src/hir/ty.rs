use std::{collections::HashMap, fmt::Display};

use anyhow::bail;
use itertools::{EitherOrBoth, Itertools};

use super::{
    infer::{Constraint, TypeVar},
    Database, FileId, NameId, VarDeclType,
};
use crate::{
    db::Lookup,
    game_data::{Form, GlobalsDatabaseId},
};

#[derive(Debug, Clone)]
pub(crate) struct SymbolTable {
    pub map: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Symbol {
    Local(FileId, NameId),
    Global(GlobalsDatabaseId, String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct InferredType {
    pub widest: Type,
    pub narrowest: Type,
}

impl InferredType {
    pub fn inferred(narrowest: Type) -> Self {
        Self { widest: Type::Any, narrowest }
    }

    pub fn concrete(narrowest: Type) -> Self {
        Self { widest: narrowest.clone(), narrowest }
    }

    pub fn any() -> Self {
        Self { widest: Type::Any, narrowest: Type::Empty }
    }

    pub fn bool() -> Self {
        Self::concrete(Type::Bool)
    }

    pub fn number() -> Self {
        Self::concrete(Type::Number)
    }

    pub fn ref_() -> Self {
        Self::concrete(Type::Ref)
    }

    pub fn string() -> Self {
        Self::concrete(Type::String)
    }

    pub fn array() -> Self {
        Self {
            widest: Type::Map(Box::new((Type::Number, Type::Any))),
            narrowest: Type::Map(Box::new((Type::Number, Type::Empty))),
        }
    }

    pub fn from_decl_type(decl_type: VarDeclType) -> Self {
        match decl_type {
            VarDeclType::Int => Self::concrete(Type::Number),
            VarDeclType::Double => Self::concrete(Type::Number),
            VarDeclType::Float => Self::concrete(Type::Number),
            VarDeclType::Ref => Self::concrete(Type::Ref),
            VarDeclType::String => Self::concrete(Type::String),
            VarDeclType::Array => Self {
                widest: Type::Map(Box::new((Type::Any, Type::Any))),
                narrowest: Type::Map(Box::new((Type::Empty, Type::Empty))),
            },
            VarDeclType::Unknown => Self::concrete(Type::Any),
        }
    }

    pub fn union(
        &mut self,
        other: &mut Self,
        constraints: &mut HashMap<TypeVar, Vec<Constraint>>,
    ) -> anyhow::Result<()> {
        if !self.widest.is_subtype(&other.widest) && !other.widest.is_subtype(&self.widest) {
            bail!(
                "cannot assign '{}' to '{}'",
                other.widest.to_string(0),
                self.widest.to_string(0),
            );
        }

        let merged = self.narrowest.union(&other.narrowest, constraints);
        if merged.is_subtype(&self.widest) {
            self.narrowest = merged.clone();
        }
        if merged.is_subtype(&other.widest) {
            other.narrowest = merged;
        }
        Ok(())
    }

    pub fn replace_type_vars(
        &mut self,
        type_map: &mut HashMap<TypeVar, InferredType>,
        limit: usize,
        tv_stack: &mut Vec<TypeVar>,
    ) {
        self.widest.replace_type_vars(type_map, limit, tv_stack);
        self.narrowest.replace_type_vars(type_map, limit, tv_stack);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Type {
    Void,
    Bool,
    Number,
    Ref,
    String,
    Form(Form),
    Map(Box<(Type, Type)>),
    Function(Box<FunctionSignature>),
    Record(Record),
    Union(Vec<Type>),
    Any,
    Empty,
    Var(TypeVar),
}

impl From<VarDeclType> for Type {
    fn from(value: VarDeclType) -> Self {
        match value {
            VarDeclType::Int | VarDeclType::Double | VarDeclType::Float => Type::Number,
            VarDeclType::Ref => Type::Ref,
            VarDeclType::String => Type::String,
            VarDeclType::Array => Type::Map(Box::new((Type::Any, Type::Any))),
            VarDeclType::Unknown => Type::Any,
        }
    }
}

impl Type {
    pub fn is_subtype(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Empty, _) => true,
            (_, Self::Any) => true,
            (Self::Bool, Self::Number) => true,
            (Self::Ref, Self::Number) => true,
            (Self::Number, Self::Ref) => true,
            (Self::Map(a), Self::Map(b)) => a.0.is_subtype(&b.0) && a.1.is_subtype(&b.1),
            (Self::Function(a), Self::Function(b)) => {
                a.ret.is_subtype(&b.ret)
                    && a.params.iter().zip(b.params.iter()).all(|(a, b)| a.is_subtype(b))
            }
            (Self::Record(a), Self::Record(b)) => {
                a.fields.iter().all(|(k, v)| b.get(k).is_some_and(|x| v.is_subtype(x)))
            }
            (Self::Union(a), b @ Self::Union(_)) => a.iter().all(|x| x.is_subtype(b)),
            (a, Self::Union(b)) => b.iter().any(|b| a.is_subtype(b)),
            (a, b) => a == b,
        }
    }

    pub fn union(&self, other: &Self, constraints: &mut HashMap<TypeVar, Vec<Constraint>>) -> Self {
        match (self, other) {
            (Self::Empty, x) | (x, Self::Empty) => x.clone(),
            (_, Self::Any) | (Self::Any, _) => Self::Any,
            (Self::Number, Self::Bool) | (Self::Bool, Self::Number) => Self::Number,
            (Self::Number, Self::Ref) | (Self::Ref, Self::Number) => Self::Number,
            (Self::Map(a), Self::Map(b)) => {
                Self::Map(Box::new((a.0.union(&b.0, constraints), a.1.union(&b.1, constraints))))
            }
            (Self::Function(a), Self::Function(b)) => Self::Function(Box::new(FunctionSignature {
                ret: a.ret.union(&b.ret, constraints),
                params: a
                    .params
                    .iter()
                    .zip_longest(b.params.iter())
                    .map(|x| match x {
                        EitherOrBoth::Both(a, b) => a.union(b, constraints),
                        EitherOrBoth::Left(x) | EitherOrBoth::Right(x) => {
                            x.union(&Self::Void, constraints)
                        }
                    })
                    .collect(),
            })),
            (Self::Record(a), Self::Record(b)) => {
                let mut rec = a.clone();
                for (k, v) in &b.fields {
                    if let Some(field) = rec.get_mut(k) {
                        *field = field.union(v, constraints);
                    } else {
                        rec.fields.push((k.clone(), v.clone()));
                    }
                }
                Self::Record(rec)
            }
            (a @ Self::Union(_), Self::Union(b)) => {
                b.iter().fold(a.clone(), |acc, x| acc.union(x, constraints))
            }
            (Self::Union(a), b) | (b, Self::Union(a)) => {
                if matches!(b, Self::Var(_)) {
                    return b.clone();
                }
                let mut arr = a.clone();
                if let Some(e) = arr.iter_mut().find(|x| b.can_merge(x)) {
                    *e = b.union(e, constraints);
                } else if !arr.iter().any(|x| b.is_subtype(x)) {
                    arr.push(b.clone());
                }
                Self::Union(arr)
            }
            (Self::Var(a), Self::Var(b)) => {
                let c = Constraint::Assignable(*a, *b);
                constraints.entry(*a).or_default().push(c.clone());
                constraints.entry(*b).or_default().push(c);
                self.clone()
            }
            (Self::Var(a), b) | (b, Self::Var(a)) => {
                let c = Constraint::AssignableToType(b.clone());
                constraints.entry(*a).or_default().push(c);
                self.clone()
            }
            (a, b) if a == b => a.clone(),
            (a, b) => Self::Union(vec![a.clone(), b.clone()]),
        }
    }

    pub fn can_merge(&self, other: &Self) -> bool {
        matches!((self, other), (Self::Union(_), Self::Union(_)))
    }

    pub fn replace_type_vars(
        &mut self,
        type_map: &mut HashMap<TypeVar, InferredType>,
        mut limit: usize,
        tv_stack: &mut Vec<TypeVar>,
    ) {
        limit += 1;
        if limit > 256 {
            println!("inferrence recursion limit reached");
            *self = Self::Any;
            return;
        }
        match self {
            Self::Map(x) => {
                x.0.replace_type_vars(type_map, limit, tv_stack);
                x.1.replace_type_vars(type_map, limit, tv_stack);
            }
            Self::Function(x) => {
                x.ret.replace_type_vars(type_map, limit, tv_stack);
                if matches!(x.ret, Self::Var(_)) {
                    x.ret = Self::Void;
                }
                for param in &mut x.params {
                    param.replace_type_vars(type_map, limit, tv_stack);
                }
            }
            Self::Record(x) => {
                for (_, v) in x.fields.iter_mut() {
                    v.replace_type_vars(type_map, limit, tv_stack);
                }
            }
            Self::Union(x) => {
                for e in &mut *x {
                    e.replace_type_vars(type_map, limit, tv_stack);
                }
                *self = Self::Union(flatten(x));

                fn flatten(x: &Vec<Type>) -> Vec<Type> {
                    let mut new = vec![];
                    for e in x {
                        match e {
                            Type::Union(x) => {
                                let x = flatten(x);
                                for e in x {
                                    if !new.contains(&e) {
                                        new.push(e);
                                    }
                                }
                            }
                            x => {
                                if !new.contains(x) {
                                    new.push(x.clone());
                                }
                            }
                        }
                    }
                    new.sort();
                    new
                }
            }
            Self::Var(x) => {
                if tv_stack.contains(x) {
                    *self = Self::Any;
                    return;
                }
                if let Some(mut ty) = type_map.get(x).cloned() {
                    tv_stack.push(*x);
                    ty.replace_type_vars(type_map, limit, tv_stack);
                    tv_stack.pop();
                    type_map.insert(*x, ty.clone());
                    *self = ty.narrowest;
                }
            }
            _ => (),
        }
    }

    pub fn to_string(&self, indent: usize) -> String {
        match self {
            Self::Void => "void".into(),
            Self::Bool => "bool".into(),
            Self::Number => "number".into(),
            Self::Ref => "ref".into(),
            Self::String => "string".into(),
            Self::Form(x) => x.to_string(),
            Self::Map(x) => {
                if x.0 == Self::Number {
                    format!("array<{}>", x.1.to_string(indent))
                } else {
                    format!("map<{}, {}>", x.0.to_string(indent), x.1.to_string(indent))
                }
            }
            Self::Function(x) => {
                if x.ret == Self::Void {
                    format!("fn({})", x.params.iter().map(|x| x.to_string(indent)).join(", "))
                } else {
                    format!(
                        "fn({}) -> {}",
                        x.params.iter().map(|x| x.to_string(indent)).join(", "),
                        x.ret.to_string(indent)
                    )
                }
            }
            Self::Record(x) => {
                if x.fields.is_empty() {
                    "{}".into()
                } else {
                    let indent_str = format!("\n{}", "    ".repeat(indent + 1));
                    format!(
                        "{{{indent_str}{}\n{}}}",
                        x.fields
                            .iter()
                            .map(|(k, v)| format!("{k}: {}", v.to_string(indent + 1)))
                            .join(&indent_str),
                        "    ".repeat(indent),
                    )
                }
            }
            Self::Union(x) => x.iter().map(|x| x.to_string(indent)).join(" | "),
            Self::Any => "any".into(),
            Self::Empty => "any".into(),
            // Self::Var(x) => format!("T{}", x.into_raw().into_u32()),
            Self::Var(_) => "any".into(),
        }
    }

    pub fn to_string_with_name(&self, name: &str, indent: usize) -> String {
        match self {
            Self::Function(x) => {
                if x.ret == Self::Void {
                    format!(
                        "fn {name}({})",
                        x.params.iter().map(|x| x.to_string(indent)).join(", ")
                    )
                } else {
                    format!(
                        "fn {name}({}) -> {}",
                        x.params.iter().map(|x| x.to_string(indent)).join(", "),
                        x.ret.to_string(indent)
                    )
                }
            }
            x => format!("{} {name}", x.to_string(indent)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct FunctionSignature {
    pub ret: Type,
    pub params: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Record {
    pub fields: Vec<(String, Type)>,
}

impl Record {
    pub fn get(&self, key: &str) -> Option<&Type> {
        Some(&self.fields.iter().find(|(k, _)| k == key)?.1)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut Type> {
        Some(&mut self.fields.iter_mut().find(|(k, _)| k == key)?.1)
    }
}
