use std::{
    collections::HashMap,
    ops::{Deref, Index, IndexMut},
};

use itertools::{EitherOrBoth, Itertools};
use la_arena::{Arena, Idx, RawIdx};

use super::{FileId, NameId, VarDeclType};
use crate::game_data::{Form, GlobalsDatabaseId};

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
pub(crate) struct Env {
    pub vars: Arena<Type>,
}

impl Env {
    pub fn new() -> Self {
        Env { vars: Arena::new() }
    }

    pub fn type_var(&mut self, ty: Type) -> TypeVarId {
        TypeVarId(self.vars.alloc(ty))
    }

    pub fn replace_type_vars(mut self) -> Arena<Type> {
        for i in self.vars.iter().map(|(i, _)| i).collect::<Vec<_>>() {
            let mut ty = self.vars[i].clone();
            ty.replace_type_vars(&mut self);
            self.vars[i] = ty;
        }

        self.vars
    }
}

impl Index<TypeVarId> for Env {
    type Output = Type;

    fn index(&self, index: TypeVarId) -> &Self::Output {
        &self.vars[*index]
    }
}

impl IndexMut<TypeVarId> for Env {
    fn index_mut(&mut self, index: TypeVarId) -> &mut Self::Output {
        &mut self.vars[*index]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Symbol {
    Local(FileId, NameId),
    Global(GlobalsDatabaseId, String),
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
    Function(Box<Function>),
    OverloadedFunction { combined: Box<Function>, overloads: Vec<Function> },
    Record(Record),
    Union(Vec<Type>),
    Any,
    Empty,
    Var(TypeVarId),
    TemplateVar(String),
}

impl From<TypeVarId> for Type {
    fn from(value: TypeVarId) -> Self {
        Self::Var(value)
    }
}

impl Type {
    pub fn from_decl_type(decl_type: VarDeclType) -> Self {
        match decl_type {
            VarDeclType::Int => Self::Number,
            VarDeclType::Double => Self::Number,
            VarDeclType::Float => Self::Number,
            VarDeclType::Ref => Self::Ref,
            VarDeclType::String => Self::String,
            VarDeclType::Array => Self::Map(Box::new((Self::Any, Self::Any))),
            VarDeclType::Unknown => Self::Any,
        }
    }

    pub fn new_overloaded_func(overloads: Vec<Function>) -> Self {
        let combined = overloads.iter().skip(1).fold(overloads[0].clone(), |acc, e| acc.union(e));
        Self::OverloadedFunction { combined: combined.into(), overloads }
    }

    pub fn is_subtype(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Empty, _) => true,
            (_, Self::Any) => true,
            (Self::Bool, Self::Number) => true,
            (Self::Ref, Self::Number) => true,
            (Self::Number, Self::Ref) => true,
            (Self::Map(a), Self::Map(b)) => a.0.is_subtype(&b.0) && a.1.is_subtype(&b.1),
            (Self::Function(a), Self::Function(b)) => a.is_subtype(b),
            (Self::Record(a), Self::Record(b)) => {
                a.fields.iter().all(|(k, v)| b.get(k).is_some_and(|x| v.is_subtype(x)))
            }
            (Self::Union(a), b @ Self::Union(_)) => a.iter().all(|x| x.is_subtype(b)),
            (a, Self::Union(b)) => b.iter().any(|b| a.is_subtype(b)),
            (a, b) => a == b,
        }
    }

    pub fn union(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Empty, x) | (x, Self::Empty) => x.clone(),
            (_, Self::Any) | (Self::Any, _) => Self::Any,
            (Self::Number, Self::Bool) | (Self::Bool, Self::Number) => Self::Number,
            (Self::Number, Self::Ref) | (Self::Ref, Self::Number) => Self::Number,
            (Self::Map(a), Self::Map(b)) => Self::Map(Box::new((a.0.union(&b.0), a.1.union(&b.1)))),
            (Self::Function(a), Self::Function(b)) => Self::Function(Box::new(a.union(b))),
            (Self::Record(a), Self::Record(b)) => {
                let mut rec = a.clone();
                for (k, v) in &b.fields {
                    if let Some(field) = rec.get_mut(k) {
                        *field = field.union(v);
                    } else {
                        rec.fields.push((k.clone(), v.clone()));
                    }
                }
                Self::Record(rec)
            }
            (a @ Self::Union(_), Self::Union(b)) => b.iter().fold(a.clone(), |acc, x| acc.union(x)),
            (Self::Union(a), b) | (b, Self::Union(a)) => {
                let mut arr = a.clone();
                if let Some(e) = arr.iter_mut().find(|x| b.can_merge(x)) {
                    *e = b.union(e);
                } else if !arr.iter().any(|x| b.is_subtype(x)) {
                    arr.push(b.clone());
                }
                arr.sort();
                arr.dedup();
                Self::Union(arr)
            }
            (a, b) if a == b => a.clone(),
            (a, b) => Self::Union(vec![a.clone(), b.clone()]),
        }
    }

    pub fn can_merge(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (Self::Union(_), Self::Union(_)) | (Self::Record(_), Self::Record(_))
        )
    }

    fn map(&self, mut f: impl FnMut(&Type) -> Type) -> Type {
        match self {
            Self::Void
            | Self::Bool
            | Self::Number
            | Self::Ref
            | Self::String
            | Self::Form(_)
            | Self::Any
            | Self::Empty => self.clone(),
            Self::Map(x) => Self::Map((f(&x.0), f(&x.1)).into()),
            Self::Function(x) => Self::Function(x.map(f).into()),
            Self::OverloadedFunction { combined, overloads } => Self::OverloadedFunction {
                combined: combined.map(&mut f).into(),
                overloads: overloads.iter().map(|x| x.map(&mut f)).collect(),
            },
            Self::Record(x) => Self::Record(Record {
                fields: x.fields.iter().map(|(k, v)| (k.clone(), f(v))).collect(),
            }),
            Self::Union(x) => Self::Union(x.iter().map(f).collect()),
            Self::Var(_) => f(self),
            Self::TemplateVar(_) => f(self),
        }
    }

    fn map_mut(&mut self, mut f: impl FnMut(&mut Type)) {
        match self {
            Self::Void
            | Self::Bool
            | Self::Number
            | Self::Ref
            | Self::String
            | Self::Form(_)
            | Self::Any
            | Self::Empty => (),
            Self::Map(x) => {
                f(&mut x.0);
                f(&mut x.1);
            }
            Self::Function(x) => x.map_mut(f),
            Self::OverloadedFunction { combined, overloads } => {
                combined.map_mut(&mut f);
                for o in overloads {
                    o.map_mut(&mut f);
                }
            }
            Self::Record(x) => {
                for (_, v) in &mut x.fields {
                    f(v);
                }
            }
            Self::Union(x) => {
                let mut new = Self::Empty;
                for p in &mut *x {
                    f(p);
                    new = new.union(p);
                }
                match &mut new {
                    Self::Union(x) if x.len() > 1 => {
                        x.retain(|x| !matches!(x, Self::Empty | Self::Any))
                    }
                    _ => (),
                }
                *self = new;
            }
            Self::Var(_) => f(self),
            Self::TemplateVar(_) => f(self),
        }
    }

    pub fn template_instantiate2(&self, env: &mut Env) -> Type {
        let mut stack = vec![];
        self.template_instantiate_inner2(env, &mut stack)
    }

    fn template_instantiate_inner2(&self, env: &mut Env, stack: &mut Vec<TypeVarId>) -> Type {
        self.map(move |x| match x {
            Self::Var(x) => {
                if stack.contains(x) {
                    return Self::Empty;
                }
                stack.push(*x);
                let ty = env[*x].clone().template_instantiate_inner2(env, stack);
                stack.pop();
                ty
            }
            x => x.clone(),
        })
    }

    pub fn template_instantiate(&self, env: &mut Env) -> Type {
        match self {
            // Self::Var(x) => env[*x].clone().template_instantiate(env),
            Self::Function(x) => {
                let tvs =
                    x.generic_args.iter().map(|x| (x.clone(), env.type_var(Type::Empty))).collect();
                Self::Function(x.template_instantiate_inner(env, &tvs, &mut vec![]).into())
                // x.template_instantiate_inner(env, &tvs)
            }
            x => x.clone(),
        }
    }

    fn template_instantiate_inner(
        &self,
        env: &mut Env,
        tvs: &HashMap<String, TypeVarId>,
        stack: &mut Vec<TypeVarId>,
    ) -> Type {
        match self {
            Self::Void
            | Self::Bool
            | Self::Number
            | Self::Ref
            | Self::String
            | Self::Form(_)
            | Self::Any
            | Self::Empty => self.clone(),
            Self::Map(x) => Self::Map(
                (
                    x.0.template_instantiate_inner(env, tvs, stack),
                    x.1.template_instantiate_inner(env, tvs, stack),
                )
                    .into(),
            ),
            Self::Function(x) => {
                Self::Function(x.template_instantiate_inner(env, tvs, stack).into())
            }
            Self::OverloadedFunction { combined, overloads } => Self::OverloadedFunction {
                combined: combined.template_instantiate_inner(env, tvs, stack).into(),
                overloads: overloads
                    .iter()
                    .map(|x| x.template_instantiate_inner(env, tvs, stack))
                    .collect(),
            },
            Self::Record(x) => Self::Record(Record {
                fields: x
                    .fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.template_instantiate_inner(env, tvs, stack)))
                    .collect(),
            }),
            Self::Union(x) => Self::Union(
                x.iter().map(|x| x.template_instantiate_inner(env, tvs, stack)).collect(),
            ),
            Self::Var(x) => {
                if stack.contains(x) {
                    return Self::Empty;
                }
                stack.push(*x);
                let ty = env[*x].clone().template_instantiate_inner(env, tvs, stack);
                stack.pop();
                ty
            }
            Self::TemplateVar(k) => {
                if let Some(tv) = tvs.get(k) {
                    Self::Var(*tv)
                } else {
                    Self::TemplateVar(k.clone())
                }
            }
        }
    }

    pub fn replace_type_vars(&mut self, env: &mut Env) {
        let mut stack = vec![];
        self.replace_type_vars_inner(env, &mut stack);
    }

    fn replace_type_vars_inner(&mut self, env: &mut Env, stack: &mut Vec<TypeVarId>) {
        match self {
            Self::Void
            | Self::Bool
            | Self::Number
            | Self::Ref
            | Self::String
            | Self::Form(_)
            | Self::Any
            | Self::Empty
            | Self::TemplateVar(_) => (),
            Self::Map(x) => {
                x.0.replace_type_vars_inner(env, stack);
                x.1.replace_type_vars_inner(env, stack);
            }
            Self::Function(x) => {
                x.ret.replace_type_vars_inner(env, stack);
                for p in &mut x.params {
                    p.replace_type_vars_inner(env, stack);
                }
            }
            Self::OverloadedFunction { combined, overloads } => {
                combined.replace_type_vars_inner(env, stack);
                for f in overloads {
                    f.replace_type_vars_inner(env, stack);
                }
            }
            Self::Record(x) => {
                for (_, f) in &mut x.fields {
                    f.replace_type_vars_inner(env, stack);
                }
            }
            Self::Union(x) => {
                let mut new = Self::Empty;
                for p in &mut *x {
                    p.replace_type_vars_inner(env, stack);
                    new = new.union(p);
                }
                match &mut new {
                    Self::Union(x) if x.len() > 1 => {
                        x.retain(|x| !matches!(x, Self::Empty | Self::Any))
                    }
                    _ => (),
                }
                *self = new;
                // let mut arr = vec![];
                // for p in &mut *x {
                //     p.replace_type_vars_inner(env, stack);
                //     match p {
                //         Self::Union(x) => arr.extend(x.clone()),
                //         x => arr.push(x.clone()),
                //     }
                // }
                // arr.sort();
                // arr.dedup();
                // if arr.len() > 1 {
                //     arr.retain(|x| !matches!(x, Self::Empty | Self::Any))
                // }
                // *x = arr;
            }
            Self::Var(x) => {
                if stack.contains(x) {
                    *self = Self::Empty;
                    return;
                }

                stack.push(*x);
                let mut ty = env[*x].clone();
                ty.replace_type_vars_inner(env, stack);
                env[*x] = ty.clone();
                *self = ty;
                stack.pop();
            }
        }
    }

    pub fn replace_type_vars2(&mut self, env: &mut Env) {
        let mut stack = vec![];
        self.replace_type_vars_inner2(env, &mut stack);
    }

    fn replace_type_vars_inner2(&mut self, env: &mut Env, stack: &mut Vec<TypeVarId>) {
        self.map_mut(move |this| {
            if let Self::Var(x) = this {
                if stack.contains(x) {
                    *this = Self::Empty;
                    return;
                }

                stack.push(*x);
                let mut ty = env[*x].clone();
                ty.replace_type_vars_inner2(env, stack);
                env[*x] = ty.clone();
                *this = ty;
                stack.pop();
            }
        });
    }

    pub fn choose_overload(&self, env: &mut Env, params: &[Type]) -> Type {
        match self {
            Self::OverloadedFunction { combined, overloads } => Self::Function(
                overloads
                    .iter()
                    .find(|x| {
                        params.iter().zip_longest(&x.params).all(|x| match x {
                            EitherOrBoth::Both(a, b) => a.is_subtype(b),
                            EitherOrBoth::Left(_) => false,
                            EitherOrBoth::Right(b) => Self::Void.is_subtype(b),
                        })
                    })
                    .cloned()
                    .unwrap_or_else(|| *combined.clone())
                    .into(),
            ),
            x => x.clone(),
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
            Self::Function(x) => x.to_string(indent),
            Self::OverloadedFunction { combined, overloads: _ } => combined.to_string(indent),
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
            Self::Union(x) => {
                if x.len() == 2
                    && x.contains(&Self::Void)
                    && let Some(other) = x.iter().find(|x| *x != &Self::Void)
                {
                    format!("{}?", other.to_string(indent))
                } else {
                    x.iter().map(|x| x.to_string(indent)).join(" | ")
                }
            }
            Self::Any => "any".into(),
            Self::Empty => "!".into(),
            Self::Var(x) => format!("T{}", x.into_raw().into_u32()),
            Self::TemplateVar(x) => x.clone(),
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
pub(crate) struct Function {
    pub generic_args: Vec<String>,
    pub ret: Type,
    pub params: Vec<Type>,
}

impl Function {
    pub fn is_subtype(&self, other: &Self) -> bool {
        self.ret.is_subtype(&other.ret)
            && self.params.iter().zip(other.params.iter()).all(|(a, b)| a.is_subtype(b))
    }

    pub fn union(&self, other: &Self) -> Self {
        Self {
            generic_args: vec![],
            ret: self.ret.union(&other.ret),
            params: self
                .params
                .iter()
                .zip_longest(other.params.iter())
                .map(|x| match x {
                    EitherOrBoth::Both(a, b) => a.union(b),
                    EitherOrBoth::Left(x) | EitherOrBoth::Right(x) => x.union(&Type::Void),
                })
                .collect(),
        }
    }

    fn replace_type_vars_inner(&mut self, env: &mut Env, stack: &mut Vec<TypeVarId>) {
        self.ret.replace_type_vars_inner(env, stack);
        for f in &mut self.params {
            f.replace_type_vars_inner(env, stack);
        }
    }

    fn map(&self, mut f: impl FnMut(&Type) -> Type) -> Function {
        Function {
            generic_args: self.generic_args.clone(),
            ret: f(&self.ret),
            params: self.params.iter().map(f).collect(),
        }
    }

    fn map_mut(&mut self, mut f: impl FnMut(&mut Type)) {
        f(&mut self.ret);
        for p in &mut self.params {
            f(p);
        }
    }

    fn template_instantiate_inner(
        &self,
        env: &mut Env,
        tvs: &HashMap<String, TypeVarId>,
        stack: &mut Vec<TypeVarId>,
    ) -> Function {
        Function {
            generic_args: self.generic_args.clone(),
            ret: self.ret.template_instantiate_inner(env, tvs, stack),
            params: self
                .params
                .iter()
                .map(|x| x.template_instantiate_inner(env, tvs, stack))
                .collect(),
        }
    }

    pub fn to_string(&self, indent: usize) -> String {
        let generic_args = if !self.generic_args.is_empty() {
            format!("<{}>", self.generic_args.iter().join(", "))
        } else {
            "".to_string()
        };
        if self.ret == Type::Void {
            format!(
                "fn{generic_args}({})",
                self.params.iter().map(|x| x.to_string(indent)).join(", ")
            )
        } else {
            format!(
                "fn{generic_args}({}) -> {}",
                self.params.iter().map(|x| x.to_string(indent)).join(", "),
                self.ret.to_string(indent)
            )
        }
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TypeVarId(Idx<Type>);

impl TypeVarId {
    pub fn from_u32(raw: u32) -> Self {
        Self(Idx::from_raw(RawIdx::from_u32(raw)))
    }

    pub fn union(self, other: Self, env: &Env) -> Type {
        env[self].union(&env[other])
    }
}

impl Deref for TypeVarId {
    type Target = Idx<Type>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
