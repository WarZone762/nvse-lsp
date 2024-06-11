use std::{collections::HashMap, iter, rc::Rc};

use crate::{
    ast::{self, AstNode},
    db::{self, Lookup},
    syntax_node::TokenKind,
};

pub(crate) mod infer;
pub(crate) mod lower;
pub(crate) mod printer;
pub(crate) mod ty;

use db::*;
use ty::*;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum HirNode {
    Script(db::FileId),
    Item(ItemId),
    Stmt(StmtId),
    Expr(ExprId),
    Block(BlockId),
    VarDecl(VarDeclId),
    Name(NameId),
    StringShard(StringShardId),
}

impl_from! {
    HirNode,
    Script(db::FileId),
    Item(ItemId),
    Stmt(StmtId),
    Expr(ExprId),
    Block(BlockId),
    VarDecl(VarDeclId),
    Name(NameId),
    StringShard(StringShardId),
}

impl HirNode {
    pub fn dfs<'a>(&self, db: &'a Database, script_db: &'a ScriptDatabase) -> Iter<'a> {
        Iter::new(db, script_db, *self)
    }

    pub fn children<'a>(
        &self,
        db: &'a db::Database,
        script_db: &'a ScriptDatabase,
    ) -> Box<dyn Iterator<Item = HirNode> + 'a> {
        match self {
            HirNode::Script(x) => Box::new(x.hir(db).children()),
            HirNode::Item(x) => x.children(script_db),
            HirNode::Stmt(x) => x.children(script_db),
            HirNode::Expr(x) => x.children(script_db),
            HirNode::Block(x) => Box::new(x.children(script_db)),
            HirNode::VarDecl(x) => Box::new(x.children(script_db)),
            HirNode::Name(_) => Box::new(iter::empty()),
            HirNode::StringShard(x) => x.children(script_db),
        }
    }

    pub fn node<'a>(
        &self,
        db: &'a db::Database,
        script_db: &'a ScriptDatabase,
    ) -> Option<&'a dyn AstNode> {
        Some(match self {
            HirNode::Script(x) => &x.hir(db).node,
            HirNode::Item(x) => x.node(script_db)?,
            HirNode::Stmt(x) => x.node(script_db)?,
            HirNode::Expr(x) => x.node(script_db)?,
            HirNode::Block(x) => x.node(script_db)?,
            HirNode::VarDecl(x) => x.node(script_db)?,
            HirNode::Name(x) => x.node(script_db)?,
            HirNode::StringShard(x) => x.node(script_db)?,
        })
    }
}

#[derive(Debug)]
pub(crate) struct Iter<'a> {
    stack: Vec<HirNode>,
    db: &'a Database,
    script_db: &'a ScriptDatabase,
}

impl<'a> Iter<'a> {
    pub fn new(db: &'a Database, script_db: &'a ScriptDatabase, node: HirNode) -> Self {
        Self { db, script_db, stack: vec![node] }
    }
}

impl<'a> Iterator for Iter<'a> {
    type Item = HirNode;

    fn next(&mut self) -> Option<Self::Item> {
        let top = self.stack.pop()?;
        self.stack.extend(top.children(self.db, self.script_db));
        Some(top)
    }
}

macro_rules! hir_children {
    ($name:ident, $($stmts:stmt)*) => {
        impl $name {
            #[allow(unused_macros, dead_code)]
            pub fn children(&self) -> impl Iterator<Item = HirNode> {
                macro_rules! child {
                    ($expr:ident) => {
                        yield HirNode::from(self.$expr)
                    };
                }

                macro_rules! child_opt {
                    ($expr:ident) => {
                        if let Some(child) = self.$expr {
                            yield HirNode::from(child);
                        }
                    };
                }

                macro_rules! child_iter {
                    ($expr:ident) => {
                        for e in &self.$expr {
                            yield HirNode::from(*e);
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

#[derive(Debug, Clone)]
pub(crate) struct Script {
    pub name: Option<NameId>,
    pub items: Vec<ItemId>,
    pub sym_table: SymbolTable,
    pub node: ast::Script,
}

hir_children! {
    Script,
    child_opt!(name)
    child_iter!(items)
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub(crate) struct FnDeclItem {
    pub name: Option<NameId>,
    pub params: Vec<VarDeclId>,
    pub block: BlockId,
    pub node: ast::FnDeclItem,
}

hir_children! {
    FnDeclItem,
    child_opt!(name)
    child_iter!(params)
    child!(block)
}

#[derive(Debug, Clone)]
pub(crate) struct BlockTypeItem {
    pub blocktype_kind: u8,
    pub block: BlockId,
    pub node: ast::BlockTypeItem,
}

hir_children! {
    BlockTypeItem,
    child!(block)
}

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
    For(ForStmt),
    ForEach(ForEachStmt),
    If(IfStmt),
    While(WhileStmt),
    VarDecl(VarDeclStmt),
    Return(ReturnStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Block(BlockId),
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
    Block(BlockId),
    Expr(ExprStmt),
}

impl Stmt {
    pub fn children<'a>(
        &'a self,
        db: &'a ScriptDatabase,
    ) -> Box<dyn Iterator<Item = HirNode> + 'a> {
        match self {
            Stmt::For(x) => Box::new(x.children()),
            Stmt::ForEach(x) => Box::new(x.children()),
            Stmt::If(x) => Box::new(x.children()),
            Stmt::While(x) => Box::new(x.children()),
            Stmt::VarDecl(x) => Box::new(x.children()),
            Stmt::Return(x) => Box::new(x.children()),
            Stmt::Break(x) => Box::new(x.children()),
            Stmt::Continue(x) => Box::new(x.children()),
            Stmt::Block(x) => Box::new(x.children(db)),
            Stmt::Expr(x) => Box::new(x.children()),
        }
    }

    pub fn node<'a>(&'a self, db: &'a ScriptDatabase) -> Option<&'a dyn AstNode> {
        Some(match self {
            Stmt::For(x) => &x.node,
            Stmt::ForEach(x) => &x.node,
            Stmt::If(x) => &x.node,
            Stmt::While(x) => &x.node,
            Stmt::VarDecl(x) => &x.node,
            Stmt::Return(x) => &x.node,
            Stmt::Break(x) => &x.node,
            Stmt::Continue(x) => &x.node,
            Stmt::Block(x) => return x.node(db),
            Stmt::Expr(x) => &x.node,
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ForStmt {
    pub init: Option<VarDeclId>,
    pub cond: Option<ExprId>,
    pub loop_expr: Option<ExprId>,
    pub block: BlockId,
    pub node: ast::ForStmt,
}

hir_children! {
    ForStmt,
    child_opt!(init)
    child_opt!(cond)
    child_opt!(loop_expr)
    child!(block)
}

#[derive(Debug, Clone)]
pub(crate) struct ForEachStmt {
    pub var_decl: VarDeclId,
    pub iterable: ExprId,
    pub block: BlockId,
    pub node: ast::ForEachStmt,
}

hir_children! {
    ForEachStmt,
    child!(var_decl)
    child!(iterable)
}

#[derive(Debug, Clone)]
pub(crate) struct IfStmt {
    pub cond: ExprId,
    pub true_branch: BlockId,
    pub false_branch: Option<StmtId>,
    pub else_branch: Option<BlockId>,
    pub node: ast::IfStmt,
}

hir_children! {
    IfStmt,
    child!(cond)
    child!(true_branch)
    child_opt!(false_branch)
    child_opt!(else_branch)
}

#[derive(Debug, Clone)]
pub(crate) struct WhileStmt {
    pub cond: ExprId,
    pub block: BlockId,
    pub node: ast::WhileStmt,
}

hir_children! {
    WhileStmt,
    child!(cond)
    child!(block)
}

#[derive(Debug, Clone)]
pub(crate) struct VarDeclStmt {
    pub decl: VarDeclId,
    pub node: ast::VarDeclStmt,
}

hir_children! {
    VarDeclStmt,
    child!(decl)
}

#[derive(Debug, Clone)]
pub(crate) struct ReturnStmt {
    pub expr: Option<ExprId>,
    pub node: ast::ReturnStmt,
}

hir_children! {
    ReturnStmt,
    child_opt!(expr)
}

#[derive(Debug, Clone)]
pub(crate) struct BreakStmt {
    pub node: ast::BreakStmt,
}

hir_children! {
    BreakStmt,
}

#[derive(Debug, Clone)]
pub(crate) struct ContinueStmt {
    pub node: ast::ContinueStmt,
}

hir_children! {
    ContinueStmt,
}

#[derive(Debug, Clone)]
pub(crate) struct ExprStmt {
    pub expr: ExprId,
    pub node: ast::ExprStmt,
}

hir_children! {
    ExprStmt,
    child!(expr)
}

#[derive(Debug, Clone)]
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
            Expr::NameRef(_) => Box::new(iter::empty()),
            Expr::Str(x) => Box::new(x.children()),
            Expr::Lit(_) => Box::new(iter::empty()),
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
            Expr::Lit(x) => x.node(),
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BinExpr {
    pub lhs: ExprId,
    pub op: BinOpKind,
    pub rhs: ExprId,
    pub node: ast::BinaryExpr,
}

hir_children! {
    BinExpr,
    child!(lhs)
    child!(rhs)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BinOpKind {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Circumflex,
    PlusEq,
    MinusEq,
    AsteriskEq,
    SlashEq,
    PercentEq,
    CircumflexEq,
    Vbar,
    VbarEq,
    Ampersand,
    AmpersandEq,
    Eq,
    Eq2,
    Lt,
    LtEq,
    Gt,
    GtEq,
    ExclamationEq,
    Vbar2,
    Ampersand2,
    Lt2,
    Gt2,
    Colon,
    Colon2,
    Dot,
    Unknown,
}

impl BinOpKind {
    pub fn from_token(string: TokenKind) -> Self {
        match string {
            TokenKind::Plus => Self::Plus,
            TokenKind::Minus => Self::Minus,
            TokenKind::Star => Self::Asterisk,
            TokenKind::Slash => Self::Slash,
            TokenKind::Mod => Self::Percent,
            TokenKind::Pow => Self::Circumflex,
            TokenKind::PlusEq => Self::PlusEq,
            TokenKind::MinusEq => Self::MinusEq,
            TokenKind::StarEq => Self::AsteriskEq,
            TokenKind::SlashEq => Self::SlashEq,
            TokenKind::ModEq => Self::PercentEq,
            TokenKind::PowEq => Self::CircumflexEq,
            TokenKind::BitwiseOr => Self::Vbar,
            TokenKind::BitwiseOrEq => Self::VbarEq,
            TokenKind::BitwiseAnd => Self::Ampersand,
            TokenKind::BitwiseAndEq => Self::AmpersandEq,
            TokenKind::Eq => Self::Eq,
            TokenKind::EqEq => Self::Eq2,
            TokenKind::Less => Self::Lt,
            TokenKind::LessEq => Self::LtEq,
            TokenKind::Greater => Self::Gt,
            TokenKind::GreaterEq => Self::GtEq,
            TokenKind::BangEq => Self::ExclamationEq,
            TokenKind::LogicOr => Self::Vbar2,
            TokenKind::LogicAnd => Self::Ampersand2,
            TokenKind::Lt2 => Self::Lt2,
            TokenKind::Gt2 => Self::Gt2,
            TokenKind::Colon => Self::Colon,
            TokenKind::Colon2 => Self::Colon2,
            TokenKind::Dot => Self::Dot,
            _ => Self::Unknown,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct TernaryExpr {
    pub cond: ExprId,
    pub true_expr: ExprId,
    pub false_expr: ExprId,
    pub node: ast::TernaryExpr,
}

hir_children! {
    TernaryExpr,
    child!(cond)
    child!(true_expr)
    child!(false_expr)
}

#[derive(Debug, Clone)]
pub(crate) struct UnaryExpr {
    pub op: UnaryOpKind,
    pub operand: ExprId,
    pub node: ast::UnaryExpr,
}

hir_children! {
    UnaryExpr,
    child!(operand)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum UnaryOpKind {
    Minus,
    // TODO
}

#[derive(Debug, Clone)]
pub(crate) struct SubscriptExpr {
    pub lhs: ExprId,
    pub subscript: ExprId,
    pub node: ast::SubscriptExpr,
}

hir_children! {
    SubscriptExpr,
    child!(lhs)
    child!(subscript)
}

#[derive(Debug, Clone)]
pub(crate) struct CallExpr {
    pub lhs: ExprId,
    pub args: Vec<ExprId>,
    pub node: ast::CallExpr,
}

hir_children! {
    CallExpr,
    child!(lhs)
    child_iter!(args)
}

#[derive(Debug, Clone)]
pub(crate) struct ParenExpr {
    pub expr: ExprId,
    pub node: ast::ParenExpr,
}

hir_children! {
    ParenExpr,
    child!(expr)
}

#[derive(Debug, Clone)]
pub(crate) struct LambdaExpr {
    pub params: Vec<VarDeclId>,
    pub block: BlockId,
    pub node: ast::LambdaExpr,
}

hir_children! {
    LambdaExpr,
    child_iter!(params)
    child!(block)
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub stmts: Vec<StmtId>,
    pub sym_table: SymbolTable,
    pub node: ast::BlockStmt,
}

hir_children! {
    Block,
    child_iter!(stmts)
}

#[derive(Debug, Clone)]
pub(crate) struct VarDecl {
    pub decl_type: VarDeclType,
    pub name: NameId,
    pub init: Option<ExprId>,
    pub node: ast::VarDecl,
}

hir_children! {
    VarDecl,
    child!(name)
    child_opt!(init)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum VarDeclType {
    Int,
    Double,
    Float,
    Ref,
    String,
    Array,
    Unknown,
}

impl From<TokenKind> for VarDeclType {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::IntType => Self::Int,
            TokenKind::DoubleType => Self::Double,
            TokenKind::FloatType => Self::Float,
            TokenKind::RefType => Self::Ref,
            TokenKind::StringType => Self::String,
            TokenKind::ArrayType => Self::Array,
            _ => Self::Unknown,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Name {
    pub name: String,
    pub node: ast::Name,
}

#[derive(Debug, Clone)]
pub(crate) struct NameRef {
    pub name: String,
    pub node: ast::NameRef,
}

#[derive(Debug, Clone)]
pub(crate) struct StrExpr {
    pub shards: Vec<StringShardId>,
    pub node: ast::StrExpr,
}

hir_children! {
    StrExpr,
    child_iter!(shards)
}

#[derive(Debug, Clone)]
pub(crate) enum StringShard {
    Str { val: String, node: ast::StringShard },
    Expr { expr: ExprId, node: ast::StringShard },
}

impl StringShard {
    pub fn children<'a>(&'a self) -> Box<dyn Iterator<Item = HirNode> + 'a> {
        match self {
            StringShard::Str { .. } => Box::new(iter::empty()),
            StringShard::Expr { expr, .. } => Box::new(iter::once((*expr).into())),
        }
    }

    pub fn node<'a>(&'a self) -> Option<&'a dyn AstNode> {
        Some(match self {
            StringShard::Str { node, .. } => node,
            StringShard::Expr { node, .. } => node,
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Literal {
    Number(NumberLiteral),
    Bool(BoolLiteral),
}

impl Literal {
    pub fn node(&self) -> &impl AstNode {
        match self {
            Literal::Number(x) => &x.node,
            Literal::Bool(x) => &x.node,
        }
    }

    pub fn type_(&self) -> InferredType {
        match self {
            Literal::Number(_) => InferredType::new(Type::Number),
            Literal::Bool(_) => InferredType::new(Type::Bool),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct NumberLiteral {
    pub value: f32,
    pub node: ast::Literal,
}

#[derive(Debug, Clone)]
pub(crate) struct BoolLiteral {
    pub value: bool,
    pub node: ast::Literal,
}
