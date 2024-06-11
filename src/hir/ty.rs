use std::collections::HashMap;

use anyhow::bail;
use itertools::{EitherOrBoth, Itertools};

use super::{
    infer::{Constraint, TypeVar, TypeVarStore},
    NameId, VarDeclType,
};
use crate::hir::printer::{Print, SimplePrinter};

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
    Local(NameId),
    Global(InferredType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct InferredType {
    pub widest: Type,
    pub narrowest: Type,
}

impl InferredType {
    pub fn new(widest: Type) -> Self {
        Self { widest, narrowest: Type::Never }
    }

    pub fn inferred(narrowest: Type) -> Self {
        Self { widest: Type::Any, narrowest }
    }

    pub fn concrete(narrowest: Type) -> Self {
        Self { widest: narrowest.clone(), narrowest }
    }

    pub fn any() -> Self {
        Self { widest: Type::Any, narrowest: Type::Never }
    }

    pub fn merge(&mut self, other: &mut Self) -> anyhow::Result<()> {
        // if !self.widest.is_subtype(&other.widest) &&
        // !other.widest.is_subtype(&self.widest) {     bail!(
        //         "cannot assign '{}' to '{}'",
        //         other.print_str(SimplePrinter::new()),
        //         self.print_str(SimplePrinter::new()),
        //     );
        // }

        let merged = self.narrowest.merge(&other.narrowest);
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
    Map(Box<(Type, Type)>),
    Function(Box<FunctionSignature>),
    Record(Record),
    Union(Vec<Type>),
    Any,
    Never,
    TypeVar(TypeVar),
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
    pub fn from_str(string: &str) -> Self {
        match string {
            "int" | "float" | "double" => Self::Number,
            "ref" => Self::Ref,
            "string" => Self::String,
            "array" => Self::Map((Type::Never, Type::Never).into()),
            _ => Self::Any,
        }
    }

    pub fn to_string(&self, indent: usize) -> String {
        match self {
            Type::Void => "void".into(),
            Type::Bool => "bool".into(),
            Type::Number => "number".into(),
            Type::Ref => "ref".into(),
            Type::String => "string".into(),
            Type::Map(x) => {
                if x.0 == Type::Number {
                    format!("array<{}>", x.1.to_string(indent))
                } else {
                    format!("map<{}, {}>", x.0.to_string(indent), x.1.to_string(indent))
                }
            }
            Type::Function(x) => {
                if x.ret == Type::Void {
                    format!("fn({})", x.params.iter().map(|x| x.to_string(indent)).join(", "))
                } else {
                    format!(
                        "fn({}) -> {}",
                        x.params.iter().map(|x| x.to_string(indent)).join(", "),
                        x.ret.to_string(indent)
                    )
                }
            }
            Type::Record(x) => {
                if x.fields.is_empty() {
                    "{}".into()
                } else {
                    let indent_str = "\n    ".repeat(indent + 1);
                    format!(
                        "{{{indent_str}{}{}}}",
                        "\n    ".repeat(indent),
                        x.fields
                            .iter()
                            .map(|(k, v)| format!("{k}: {}", v.to_string(indent + 1)))
                            .join(&indent_str),
                    )
                }
            }
            Type::Union(x) => x.iter().map(|x| x.to_string(indent)).join(" | "),
            Type::Any => "any".into(),
            Type::Never => "unknown".into(),
            Type::TypeVar(x) => format!("T{}", x.into_raw().into_u32()),
        }
    }

    pub fn to_string_with_name(&self, name: &str, indent: usize) -> String {
        match self {
            Type::Function(x) => {
                if x.ret == Type::Void {
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

    pub fn is_subtype(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Never, _) => true,
            (_, Self::Any) => true,
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

    pub fn merge(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Never, x) | (x, Self::Never) => x.clone(),
            (_, Self::Any) | (Self::Any, _) => Self::Any,
            (Self::Map(a), Self::Map(b)) => Self::Map(Box::new((a.0.merge(&b.0), a.1.merge(&b.1)))),
            (Self::Function(a), Self::Function(b)) => Self::Function(Box::new(FunctionSignature {
                ret: a.ret.merge(&b.ret),
                params: a
                    .params
                    .iter()
                    .zip_longest(b.params.iter())
                    .map(|x| match x {
                        EitherOrBoth::Both(a, b) => a.merge(b),
                        EitherOrBoth::Left(x) | EitherOrBoth::Right(x) => x.merge(&Type::Void),
                    })
                    .collect(),
            })),
            (Self::Record(a), Self::Record(b)) => {
                let mut rec = a.clone();
                for (k, v) in &b.fields {
                    if let Some(field) = rec.get_mut(k) {
                        *field = field.merge(v);
                    } else {
                        rec.fields.push((k.clone(), v.clone()));
                    }
                }
                Self::Record(rec)
            }
            (a @ Self::Union(_), Self::Union(b)) => b.iter().fold(a.clone(), |acc, x| acc.merge(x)),
            (Self::Union(a), b) | (b, Self::Union(a)) => {
                if matches!(b, Type::TypeVar(_)) {
                    return b.clone();
                }
                let mut arr = a.clone();
                if let Some(e) = arr.iter_mut().find(|x| b.can_merge(x)) {
                    *e = b.merge(e);
                } else if !arr.iter().any(|x| b.is_subtype(x)) {
                    arr.push(b.clone());
                }
                Self::Union(arr)
            }
            (a, b) if a == b => a.clone(),
            (a, b) => Self::Union(vec![a.clone(), b.clone()]),
        }
    }

    pub fn can_merge(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            // (Self::Map(_), Self::Map(_))
            //     | (Self::Function(_), Self::Function(_))
            //     | (Self::Record(_), Self::Record(_))
            //     | (Self::Union(_), Self::Union(_))
            (Self::Union(_), Self::Union(_))
        )
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
            *self = Type::Any;
            return;
        }
        match self {
            Type::Map(x) => {
                x.0.replace_type_vars(type_map, limit, tv_stack);
                x.1.replace_type_vars(type_map, limit, tv_stack);
            }
            Type::Function(x) => {
                x.ret.replace_type_vars(type_map, limit, tv_stack);
                for param in &mut x.params {
                    param.replace_type_vars(type_map, limit, tv_stack);
                }
            }
            Type::Record(x) => {
                for (_, v) in x.fields.iter_mut() {
                    v.replace_type_vars(type_map, limit, tv_stack);
                }
            }
            Type::Union(x) => {
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
            Type::TypeVar(x) => {
                if tv_stack.contains(x) {
                    *self = Type::Any;
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

    pub fn unify(&self, other: &Self, store: &mut HashMap<TypeVar, Vec<Constraint>>) {
        match (self, other) {
            (Self::Map(a), Self::Map(b)) => {
                a.0.unify(&b.0, store);
                a.1.unify(&b.1, store);
            }
            (Self::Function(a), Self::Function(b)) => {
                a.ret.unify(&b.ret, store);
                for x in a.params.iter().zip_longest(b.params.iter()) {
                    match x {
                        EitherOrBoth::Both(a, b) => a.unify(b, store),
                        EitherOrBoth::Left(x) | EitherOrBoth::Right(x) => {
                            x.unify(&Type::Void, store)
                        }
                    }
                }
            }
            (Self::Record(a), Self::Record(b)) => {
                for (k, _) in a.fields.iter().chain(b.fields.iter()) {
                    match (a.get(k), b.get(k)) {
                        (Some(x), None) | (None, Some(x)) => {
                            x.unify(&Type::Void, store);
                        }
                        (Some(a), Some(b)) => {
                            a.unify(b, store);
                        }
                        _ => unreachable!(),
                    }
                }
            }
            (Self::Union(_), Self::Union(_)) => (), // TODO
            (Self::TypeVar(a), Self::TypeVar(b)) => {
                let c = Constraint::Assignable(*a, *b);
                store.entry(*a).or_default().push(c.clone());
                store.entry(*b).or_default().push(c);
            }
            (Self::TypeVar(a), b) | (b, Self::TypeVar(a)) => {
                let c = Constraint::AssignableToType(b.clone());
                store.entry(*a).or_default().push(c);
            }
            _ => (),
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
