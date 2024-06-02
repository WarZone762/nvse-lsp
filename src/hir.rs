use std::{collections::HashMap, iter, rc::Rc};

use tower_lsp::lsp_types::Url;

use crate::{
    ast::{self, AstNode},
    doc::Doc,
    syntax_node::Node,
};

mod lower;
mod printer;
pub(crate) use lower::*;

#[derive(Debug)]
pub(crate) struct Workspace {
    pub hir_map: HashMap<Url, Script>,
    pub global_symbols: Vec<Symbol>,
}

impl Workspace {
    pub fn new() -> Self {
        Self { hir_map: HashMap::new(), global_symbols: vec![] }
    }

    pub fn syntax_to_hir(&self, uri: &Url, node: Rc<Node>) -> Option<HirNode> {
        if node.parent.is_none() {
            return Some(self.hir_map.get(uri)?.into());
        }

        node.clone()
            .ancestors()
            .filter_map(|x| self.syntax_to_hir(uri, x))
            .flat_map(|x| x.children())
            .find(|x| x.node().is_some_and(|x| *x.syntax() == node))
    }

    pub fn resolve(&self, name_ref: &NameRef) -> Type {
        Type::Ambiguous
    }

    pub fn add_doc(&mut self, doc: &Doc) {
        let mut ctx = LowerCtx::new(self, &doc.text);
        let script = ctx.script(doc.tree.clone());
        self.hir_map.insert(doc.uri.clone(), script);
    }
}

macro_rules! impl_from {
    ($enum:ident, $($variant:ident($type:ty),)*) => {
        $(
            impl From<$type> for $enum {
                fn from(value: $type) -> Self {
                    Self::$variant(value)
                }
            }
        )*
    };
}

macro_rules! impl_from_ref {
    ($enum:ident, $($type:ident,)*) => {
        $(
            impl<'a> From<&'a $type> for $enum<'a> {
                fn from(value: &'a $type) -> Self {
                    Self::$type(value)
                }
            }
        )*
    };
}

macro_rules! impl_try_from_ref {
    ($enum:ident, $($type:ident,)*) => {
        $(
            impl<'a> TryFrom<$enum<'a>> for &'a $type {
                type Error = ();

                fn try_from(value: $enum<'a>) -> Result<Self, Self::Error> {
                    match value {
                        $enum::$type(x) => Ok(x),
                        _ => Err(()),
                    }
                }
            }
        )*
    };
}

#[derive(Debug)]
pub(crate) enum HirNode<'a> {
    Script(&'a Script),
    Item(&'a Item),
    Stmt(&'a Stmt),
    IfStmt(&'a IfStmt),
    Expr(&'a Expr),
    Block(&'a Block),
    ParamList(&'a ParamList),
    ArgList(&'a ArgList),
    VarDecl(&'a VarDecl),
    Name(&'a Name),
    NameRef(&'a NameRef),
    StringShard(&'a StringShard),
    Literal(&'a Literal),
}

impl_from_ref! {
    HirNode,
    Script,
    Item,
    Stmt,
    IfStmt,
    Expr,
    Block,
    ParamList,
    ArgList,
    VarDecl,
    Name,
    NameRef,
    StringShard,
    Literal,
}

impl<'a> From<&'a Box<Expr>> for HirNode<'a> {
    fn from(value: &'a Box<Expr>) -> Self {
        Self::Expr(value.as_ref())
    }
}

impl<'a> From<&'a Box<IfStmt>> for HirNode<'a> {
    fn from(value: &'a Box<IfStmt>) -> Self {
        Self::IfStmt(value.as_ref())
    }
}

impl<'a> From<&'a Rc<VarDecl>> for HirNode<'a> {
    fn from(value: &'a Rc<VarDecl>) -> Self {
        Self::VarDecl(value)
    }
}

impl_try_from_ref! {
    HirNode,
    Script,
    Item,
    Stmt,
    Expr,
    Block,
    ParamList,
    ArgList,
    VarDecl,
    Name,
    NameRef,
    StringShard,
    Literal,
}

impl<'a> HirNode<'a> {
    pub fn children(&self) -> Box<dyn Iterator<Item = HirNode<'a>> + 'a> {
        match self {
            HirNode::Script(x) => Box::new(x.children()),
            HirNode::Item(x) => Box::new(x.children()),
            HirNode::Stmt(x) => Box::new(x.children()),
            HirNode::IfStmt(x) => Box::new(x.children()),
            HirNode::Expr(x) => Box::new(x.children()),
            HirNode::Block(x) => Box::new(x.children()),
            HirNode::ParamList(x) => Box::new(x.children()),
            HirNode::ArgList(x) => Box::new(x.children()),
            HirNode::VarDecl(x) => Box::new(x.children()),
            HirNode::Name(x) => Box::new(iter::once(HirNode::from(*x))),
            HirNode::NameRef(x) => Box::new(iter::once(HirNode::from(*x))),
            HirNode::StringShard(x) => Box::new(iter::once(HirNode::from(*x))),
            HirNode::Literal(x) => Box::new(iter::once(HirNode::from(*x))),
        }
    }

    pub fn node(&self) -> Option<&dyn AstNode> {
        Some(match self {
            HirNode::Script(x) => &x.node,
            HirNode::Item(x) => x.node()?,
            HirNode::Stmt(x) => x.node()?,
            HirNode::IfStmt(x) => &x.node,
            HirNode::Expr(x) => x.node()?,
            HirNode::Block(x) => &x.node,
            HirNode::ParamList(x) => &x.node,
            HirNode::ArgList(x) => &x.node,
            HirNode::VarDecl(x) => &x.node,
            HirNode::Name(x) => &x.node,
            HirNode::NameRef(x) => &x.node,
            HirNode::StringShard(x) => x.node()?,
            HirNode::Literal(x) => x.node()?,
        })
    }
}

macro_rules! hir_children {
    ($name:ident, $($stmts:stmt)*) => {
        impl $name {
            #[allow(unused_macros)]
            pub fn children(&self) -> impl Iterator<Item = HirNode> {
                macro_rules! child {
                    ($expr:ident) => {
                        yield HirNode::from(&self.$expr)
                    };
                }

                macro_rules! child_opt {
                    ($expr:ident) => {
                        if let Some(child) = &self.$expr {
                            yield HirNode::from(child);
                        }
                    };
                }

                macro_rules! child_iter {
                    ($expr:ident) => {
                        for e in &self.$expr {
                            yield HirNode::from(e);
                        }
                    };
                }

                std::iter::from_coroutine(
                    #[coroutine]
                    || {
                        $($stmts)*
                    },
                )
            }
        }
    };
}

#[derive(Debug)]
pub(crate) struct Script {
    pub name: Option<Name>,
    pub items: Vec<Item>,
    pub sym_table: SymbolTable,
    pub node: ast::Script,
}

hir_children! {
    Script,
    child_opt!(name)
    child_iter!(items)
}

#[derive(Debug)]
pub(crate) enum Item {
    FnDecl(FnDeclItem),
    BlockType(BlockTypeItem),
    VarDeclStmt(VarDeclStmt),
}

impl_from! {
    Item,
    FnDecl(FnDeclItem),
    BlockType(BlockTypeItem),
    VarDeclStmt(VarDeclStmt),
}

impl Item {
    pub fn children(&self) -> Box<dyn Iterator<Item = HirNode> + '_> {
        match self {
            Item::FnDecl(x) => Box::new(x.children()),
            Item::BlockType(x) => Box::new(x.children()),
            Item::VarDeclStmt(x) => Box::new(x.children()),
        }
    }

    pub fn node(&self) -> Option<&dyn AstNode> {
        Some(match self {
            Item::FnDecl(x) => &x.node,
            Item::BlockType(x) => &x.node,
            Item::VarDeclStmt(x) => &x.node,
        })
    }
}

#[derive(Debug)]
pub(crate) struct FnDeclItem {
    pub name: Option<Name>,
    pub params: ParamList,
    pub block: Block,
    pub node: ast::FnDeclItem,
}

hir_children! {
    FnDeclItem,
    child_opt!(name)
    child!(params)
    child!(block)
}

#[derive(Debug)]
pub(crate) struct BlockTypeItem {
    pub blocktype_kind: u8,
    pub block: Block,
    pub node: ast::BlockTypeItem,
}

hir_children! {
    BlockTypeItem,
    child!(block)
}

#[derive(Debug)]
pub(crate) enum Stmt {
    For(ForStmt),
    ForEach(ForEachStmt),
    If(IfStmt),
    While(WhileStmt),
    VarDecl(VarDeclStmt),
    Return(ReturnStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Block(Block),
    Expr(ExprStmt),
}

impl_from! {
    Stmt,
    For(ForStmt),
    ForEach(ForEachStmt),
    If(IfStmt),
    While(WhileStmt),
    VarDecl(VarDeclStmt),
    Return(ReturnStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Block(Block),
    Expr(ExprStmt),
}

impl Stmt {
    pub fn children(&self) -> Box<dyn Iterator<Item = HirNode> + '_> {
        match self {
            Stmt::For(x) => Box::new(x.children()),
            Stmt::ForEach(x) => Box::new(x.children()),
            Stmt::If(x) => Box::new(x.children()),
            Stmt::While(x) => Box::new(x.children()),
            Stmt::VarDecl(x) => Box::new(x.children()),
            Stmt::Return(x) => Box::new(x.children()),
            Stmt::Break(x) => Box::new(x.children()),
            Stmt::Continue(x) => Box::new(x.children()),
            Stmt::Block(x) => Box::new(x.children()),
            Stmt::Expr(x) => Box::new(x.children()),
        }
    }

    pub fn node(&self) -> Option<&dyn AstNode> {
        Some(match self {
            Stmt::For(x) => &x.node,
            Stmt::ForEach(x) => &x.node,
            Stmt::If(x) => &x.node,
            Stmt::While(x) => &x.node,
            Stmt::VarDecl(x) => &x.node,
            Stmt::Return(x) => &x.node,
            Stmt::Break(x) => &x.node,
            Stmt::Continue(x) => &x.node,
            Stmt::Block(x) => &x.node,
            Stmt::Expr(x) => &x.node,
        })
    }
}

#[derive(Debug)]
pub(crate) struct ForStmt {
    pub initializer: Option<Expr>,
    pub cond: Option<Expr>,
    pub loop_expr: Option<Expr>,
    pub block: Block,
    pub node: ast::ForStmt,
}

hir_children! {
    ForStmt,
    child_opt!(initializer)
    child_opt!(cond)
    child_opt!(loop_expr)
    child!(block)
}

#[derive(Debug)]
pub(crate) struct ForEachStmt {
    pub var_decl: Rc<VarDecl>,
    pub iterable: Expr,
    pub block: Block,
    pub node: ast::ForEachStmt,
}

hir_children! {
    ForEachStmt,
    child!(var_decl)
    child!(iterable)
}

#[derive(Debug)]
pub(crate) struct IfStmt {
    pub cond: Expr,
    pub true_branch: Block,
    pub false_branch: Option<Box<IfStmt>>,
    pub else_branch: Option<Block>,
    pub node: ast::IfStmt,
}

hir_children! {
    IfStmt,
    child!(cond)
    child!(true_branch)
    child_opt!(false_branch)
    child_opt!(else_branch)
}

#[derive(Debug)]
pub(crate) struct WhileStmt {
    pub cond: Expr,
    pub block: Block,
    pub node: ast::WhileStmt,
}

hir_children! {
    WhileStmt,
    child!(cond)
    child!(block)
}

#[derive(Debug)]
pub(crate) struct VarDeclStmt {
    pub decl: Rc<VarDecl>,
    pub node: ast::VarDeclStmt,
}

hir_children! {
    VarDeclStmt,
    child!(decl)
}

#[derive(Debug)]
pub(crate) struct ReturnStmt {
    pub expr: Option<Expr>,
    pub node: ast::ReturnStmt,
}

hir_children! {
    ReturnStmt,
    child_opt!(expr)
}

#[derive(Debug)]
pub(crate) struct BreakStmt {
    pub node: ast::BreakStmt,
}

hir_children! {
    BreakStmt,
}

#[derive(Debug)]
pub(crate) struct ContinueStmt {
    pub node: ast::ContinueStmt,
}

hir_children! {
    ContinueStmt,
}

#[derive(Debug)]
pub(crate) struct ExprStmt {
    pub expr: Expr,
    pub node: ast::ExprStmt,
}

hir_children! {
    ExprStmt,
    child!(expr)
}

#[derive(Debug)]
pub(crate) enum Expr {
    Missing,
    Bin(BinExpr),
    Ternary(TernaryExpr),
    Unary(UnaryExpr),
    Subscript(SubscriptExpr),
    Call(CallExpr),
    Paren(ParenExpr),
    Lambda(LambdaExpr),
    NameRef(NameRef),
    Str(StrExpr),
    Lit(Literal),
}

impl_from! {
    Expr,
    Bin(BinExpr),
    Ternary(TernaryExpr),
    Unary(UnaryExpr),
    Subscript(SubscriptExpr),
    Call(CallExpr),
    Paren(ParenExpr),
    Lambda(LambdaExpr),
    NameRef(NameRef),
    Str(StrExpr),
    Lit(Literal),
}

impl Expr {
    pub fn children(&self) -> Box<dyn Iterator<Item = HirNode> + '_> {
        match self {
            Expr::Missing => Box::new(iter::empty()),
            Expr::Bin(x) => Box::new(x.children()),
            Expr::Ternary(x) => Box::new(x.children()),
            Expr::Unary(x) => Box::new(x.children()),
            Expr::Subscript(x) => Box::new(x.children()),
            Expr::Call(x) => Box::new(x.children()),
            Expr::Paren(x) => Box::new(x.children()),
            Expr::Lambda(x) => Box::new(x.children()),
            Expr::NameRef(x) => Box::new(iter::once(x.into())),
            Expr::Str(x) => Box::new(x.children()),
            Expr::Lit(x) => Box::new(iter::once(x.into())),
        }
    }

    pub fn node(&self) -> Option<&dyn AstNode> {
        Some(match self {
            Expr::Missing => return None,
            Expr::Bin(x) => &x.node,
            Expr::Ternary(x) => &x.node,
            Expr::Unary(x) => &x.node,
            Expr::Subscript(x) => &x.node,
            Expr::Call(x) => &x.node,
            Expr::Paren(x) => &x.node,
            Expr::Lambda(x) => &x.node,
            Expr::NameRef(x) => &x.node,
            Expr::Str(x) => &x.node,
            Expr::Lit(x) => x.node()?,
        })
    }

    pub fn type_(&self, workspace: &Workspace) -> Type {
        match self {
            Self::Missing => Type::Ambiguous,
            Self::Bin(x) => x.type_.clone(),
            Self::Ternary(x) => x.type_.clone(),
            Self::Unary(x) => x.type_.clone(),
            Self::Subscript(x) => x.type_.clone(),
            Self::Call(x) => x.type_.clone(),
            Self::Paren(x) => x.expr.type_(workspace),
            Self::Lambda(x) => x.type_.clone(),
            Self::NameRef(x) => workspace.resolve(x),
            Self::Str(_) => Type::String,
            Self::Lit(x) => x.type_(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct BinExpr {
    pub lhs: Box<Expr>,
    pub op: BinOpKind,
    pub rhs: Box<Expr>,
    pub type_: Type,
    pub node: ast::BinaryExpr,
}

hir_children! {
    BinExpr,
    child!(lhs)
    child!(rhs)
}

#[derive(Debug)]
pub(crate) enum BinOpKind {
    Plus,
    Minus,
    // TODO
}

#[derive(Debug)]
pub(crate) struct TernaryExpr {
    pub cond: Box<Expr>,
    pub true_expr: Box<Expr>,
    pub false_expr: Box<Expr>,
    pub type_: Type,
    pub node: ast::TernaryExpr,
}

hir_children! {
    TernaryExpr,
    child!(cond)
    child!(true_expr)
    child!(false_expr)
}

#[derive(Debug)]
pub(crate) struct UnaryExpr {
    pub op: UnaryOpKind,
    pub operand: Box<Expr>,
    pub type_: Type,
    pub node: ast::UnaryExpr,
}

hir_children! {
    UnaryExpr,
    child!(operand)
}

#[derive(Debug)]
pub(crate) enum UnaryOpKind {
    Minus,
    // TODO
}

#[derive(Debug)]
pub(crate) struct SubscriptExpr {
    pub lhs: Box<Expr>,
    pub subscript: Box<Expr>,
    pub type_: Type,
    pub node: ast::SubscriptExpr,
}

hir_children! {
    SubscriptExpr,
    child!(lhs)
    child!(subscript)
}

#[derive(Debug)]
pub(crate) struct CallExpr {
    pub lhs: Box<Expr>,
    pub args: ArgList,
    pub type_: Type,
    pub node: ast::CallExpr,
}

hir_children! {
    CallExpr,
    child!(lhs)
    child!(args)
}

#[derive(Debug)]
pub(crate) struct ParenExpr {
    pub expr: Box<Expr>,
    pub node: ast::ParenExpr,
}

hir_children! {
    ParenExpr,
    child!(expr)
}

#[derive(Debug)]
pub(crate) struct LambdaExpr {
    pub params: ParamList,
    pub block: Block,
    pub type_: Type,
    pub node: ast::LambdaExpr,
}

hir_children! {
    LambdaExpr,
    child!(params)
    child!(block)
}

#[derive(Debug)]
pub(crate) struct Block {
    pub stmts: Vec<Stmt>,
    pub sym_table: SymbolTable,
    pub node: ast::BlockStmt,
}

hir_children! {
    Block,
    child_iter!(stmts)
}

#[derive(Debug)]
pub(crate) struct ParamList {
    pub params: Vec<Rc<VarDecl>>,
    pub node: ast::ParamList,
}

hir_children! {
    ParamList,
    child_iter!(params)
}

#[derive(Debug)]
pub(crate) struct ArgList {
    pub args: Vec<Expr>,
    pub node: ast::ArgList,
}

hir_children! {
    ArgList,
    child_iter!(args)
}

#[derive(Debug)]
pub(crate) struct VarDecl {
    pub decl_type: Type,
    pub name: Name,
    pub node: ast::VarDecl,
}

hir_children! {
    VarDecl,
    child!(name)
}

#[derive(Debug)]
pub(crate) struct Name {
    pub name: String,
    pub node: ast::Name,
}

#[derive(Debug)]
pub(crate) struct NameRef {
    pub name: String,
    pub node: ast::NameRef,
}

#[derive(Debug)]
pub(crate) struct StrExpr {
    pub shards: Vec<StringShard>,
    pub node: ast::StrExpr,
}

hir_children! {
    StrExpr,
    child_iter!(shards)
}

#[derive(Debug)]
pub(crate) enum StringShard {
    Str { val: String, node: ast::StringShard },
    Expr(Expr),
}

impl StringShard {
    pub fn node(&self) -> Option<&dyn AstNode> {
        Some(match self {
            StringShard::Str { node, .. } => node,
            StringShard::Expr(x) => x.node()?,
        })
    }
}

#[derive(Debug)]
pub(crate) enum Literal {
    Number(NumberLiteral),
    Bool(BoolLiteral),
}

impl Literal {
    pub fn node(&self) -> Option<&dyn AstNode> {
        Some(match self {
            Literal::Number(x) => &x.node,
            Literal::Bool(x) => &x.node,
        })
    }

    pub fn type_(&self) -> Type {
        match self {
            Literal::Number(_) => Type::Number,
            Literal::Bool(_) => Type::Bool,
        }
    }
}

#[derive(Debug)]
pub(crate) struct NumberLiteral {
    pub value: f32,
    pub node: ast::Literal,
}

#[derive(Debug)]
pub(crate) struct BoolLiteral {
    pub value: bool,
    pub node: ast::Literal,
}

#[derive(Debug)]
pub(crate) struct SymbolTable {
    pub map: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }
}

#[derive(Debug)]
pub(crate) enum Symbol {
    Local(Rc<VarDecl>),
    Global(Type),
}

#[derive(Debug, Clone)]
pub(crate) enum Type {
    Bool,
    Number,
    Ref,
    String,
    Array,
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
            "array" => Self::Array,
            _ => Self::Ambiguous,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionSignature {
    pub ret: Option<Type>,
    pub params: Vec<Type>,
}
