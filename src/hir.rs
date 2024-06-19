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
    StrShard(StrShardId),
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
    StrShard(StrShardId),
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
            HirNode::StrShard(x) => x.children(script_db),
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
            HirNode::StrShard(x) => x.node(script_db)?,
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
    VarDecl(VarDeclStmt),
}

impl_from! {
    Item,
    FnDecl(FnDeclItem),
    BlockType(BlockTypeItem),
    VarDecl(VarDeclStmt),
}

impl Item {
    pub fn children(&self) -> Box<dyn Iterator<Item = HirNode> + '_> {
        match self {
            Item::FnDecl(x) => Box::new(x.children()),
            Item::BlockType(x) => Box::new(x.children()),
            Item::VarDecl(x) => Box::new(x.children()),
        }
    }

    pub fn node(&self) -> Option<&dyn AstNode> {
        Some(match self {
            Item::FnDecl(x) => &x.node,
            Item::BlockType(x) => &x.node,
            Item::VarDecl(x) => &x.node,
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
    ForRange(ForRangeStmt),
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
    ForRange(ForRangeStmt),
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
            Stmt::ForRange(x) => Box::new(x.children()),
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
            Stmt::ForRange(x) => &x.node,
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
pub(crate) struct ForRangeStmt {
    pub pat: Pat,
    pub iterable: ExprId,
    pub block: BlockId,
    pub node: ast::ForRangeStmt,
}

impl ForRangeStmt {
    pub fn children(&self) -> impl Iterator<Item = HirNode> {
        std::iter::from_coroutine(
            #[coroutine]
            || {
                for child in self.pat.children() {
                    yield child;
                }
                yield HirNode::from(self.iterable);
                yield HirNode::from(self.block);
            },
        )
    }
}

#[derive(Debug, Clone)]
pub(crate) struct IfStmt {
    pub cond: ExprId,
    pub true_branch: BlockId,
    pub false_branch: Option<ElseBranch>,
    pub node: ast::IfStmt,
}

hir_children! {
    IfStmt,
    child!(cond)
    child!(true_branch)
    child_opt!(false_branch)
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ElseBranch {
    Block(BlockId),
    If(StmtId),
}

impl From<ElseBranch> for HirNode {
    fn from(value: ElseBranch) -> Self {
        match value {
            ElseBranch::Block(x) => x.into(),
            ElseBranch::If(x) => x.into(),
        }
    }
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
    pub export: bool,
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
    Postfix(PostfixExpr),
    Field(FieldExpr),
    Subscript(SubscriptExpr),
    Call(CallExpr),
    Paren(ParenExpr),
    Lambda(LambdaExpr),
    NameRef(NameRef),
    Str(StrExpr),
    Literal(Literal),
}

impl_from! {
    Expr,
    Bin(BinExpr),
    Ternary(TernaryExpr),
    Unary(UnaryExpr),
    Postfix(PostfixExpr),
    Field(FieldExpr),
    Subscript(SubscriptExpr),
    Call(CallExpr),
    Paren(ParenExpr),
    Lambda(LambdaExpr),
    NameRef(NameRef),
    Str(StrExpr),
    Literal(Literal),
}

impl Expr {
    pub fn children(&self) -> Box<dyn Iterator<Item = HirNode> + '_> {
        match self {
            Expr::Missing => Box::new(iter::empty()),
            Expr::Bin(x) => Box::new(x.children()),
            Expr::Ternary(x) => Box::new(x.children()),
            Expr::Unary(x) => Box::new(x.children()),
            Expr::Postfix(x) => Box::new(x.children()),
            Expr::Field(x) => Box::new(x.children()),
            Expr::Subscript(x) => Box::new(x.children()),
            Expr::Call(x) => Box::new(x.children()),
            Expr::Paren(x) => Box::new(x.children()),
            Expr::Lambda(x) => Box::new(x.children()),
            Expr::NameRef(_) => Box::new(iter::empty()),
            Expr::Str(x) => Box::new(x.children()),
            Expr::Literal(_) => Box::new(iter::empty()),
        }
    }

    pub fn node(&self) -> Option<&dyn AstNode> {
        Some(match self {
            Expr::Missing => return None,
            Expr::Bin(x) => &x.node,
            Expr::Ternary(x) => &x.node,
            Expr::Unary(x) => &x.node,
            Expr::Postfix(x) => &x.node,
            Expr::Field(x) => &x.node,
            Expr::Subscript(x) => &x.node,
            Expr::Call(x) => &x.node,
            Expr::Paren(x) => &x.node,
            Expr::Lambda(x) => &x.node,
            Expr::NameRef(x) => &x.node,
            Expr::Str(x) => &x.node,
            Expr::Literal(x) => x.node(),
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BinExpr {
    pub lhs: ExprId,
    pub op: BinOpKind,
    pub rhs: ExprId,
    pub node: ast::BinExpr,
}

hir_children! {
    BinExpr,
    child!(lhs)
    child!(rhs)
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
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
    #[default]
    Unknown,
}

impl BinOpKind {
    pub fn from_token(string: TokenKind) -> Self {
        match string {
            TokenKind::PLUS => Self::Plus,
            TokenKind::MINUS => Self::Minus,
            TokenKind::ASTERISK => Self::Asterisk,
            TokenKind::SLASH => Self::Slash,
            TokenKind::PERCENT => Self::Percent,
            TokenKind::CIRCUMFLEX => Self::Circumflex,
            TokenKind::PLUS_EQ => Self::PlusEq,
            TokenKind::MINUS_EQ => Self::MinusEq,
            TokenKind::ASTERISK_EQ => Self::AsteriskEq,
            TokenKind::SLASH_EQ => Self::SlashEq,
            TokenKind::PERCENT_EQ => Self::PercentEq,
            TokenKind::CIRCUMFLEX_EQ => Self::CircumflexEq,
            TokenKind::VBAR => Self::Vbar,
            TokenKind::VBAR_EQ => Self::VbarEq,
            TokenKind::AMPERSAND => Self::Ampersand,
            TokenKind::AMPERSAND_EQ => Self::AmpersandEq,
            TokenKind::EQ => Self::Eq,
            TokenKind::EQ_2 => Self::Eq2,
            TokenKind::LT => Self::Lt,
            TokenKind::LT_EQ => Self::LtEq,
            TokenKind::GT => Self::Gt,
            TokenKind::GT_EQ => Self::GtEq,
            TokenKind::EXCLAMATION_EQ => Self::ExclamationEq,
            TokenKind::VBAR_2 => Self::Vbar2,
            TokenKind::AMPERSAND_2 => Self::Ampersand2,
            TokenKind::LT_2 => Self::Lt2,
            TokenKind::GT_2 => Self::Gt2,
            TokenKind::COLON => Self::Colon,
            TokenKind::COLON_2 => Self::Colon2,
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) enum UnaryOpKind {
    Plus,
    Minus,
    Asterisk,
    Ampersand,
    Plus2,
    Minus2,
    Dollar,
    NumSign,
    Exclamation,
    Tilde,
    #[default]
    Unknown,
}

impl UnaryOpKind {
    pub fn from_token(kind: TokenKind) -> Self {
        match kind {
            TokenKind::PLUS => Self::Plus,
            TokenKind::MINUS => Self::Minus,
            TokenKind::ASTERISK => Self::Asterisk,
            TokenKind::AMPERSAND => Self::Ampersand,
            TokenKind::PLUS_2 => Self::Plus2,
            TokenKind::MINUS_2 => Self::Minus2,
            TokenKind::DOLLAR => Self::Dollar,
            TokenKind::NUM_SIGN => Self::NumSign,
            TokenKind::EXCLAMATION => Self::Exclamation,
            TokenKind::TILDE => Self::Tilde,
            _ => Self::Unknown,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct PostfixExpr {
    pub operand: ExprId,
    pub op: PostfixOpKind,
    pub node: ast::PostfixExpr,
}

hir_children! {
    PostfixExpr,
    child!(operand)
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PostfixOpKind {
    Plus2,
    Minus2,
    #[default]
    Unknown,
}

impl PostfixOpKind {
    pub fn from_token(kind: TokenKind) -> Self {
        match kind {
            TokenKind::PLUS_2 => Self::Plus2,
            TokenKind::MINUS_2 => Self::Minus2,
            _ => Self::Plus2,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FieldExpr {
    pub lhs: ExprId,
    pub field: ExprId,
    pub node: ast::FieldExpr,
}

hir_children! {
    FieldExpr,
    child!(lhs)
    child!(field)
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
    pub block_or_expr: BlockOrExpr,
    pub node: ast::LambdaExpr,
}

hir_children! {
    LambdaExpr,
    child_iter!(params)
    child!(block_or_expr)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BlockOrExpr {
    Block(BlockId),
    Expr(ExprId),
}

impl From<BlockOrExpr> for HirNode {
    fn from(value: BlockOrExpr) -> Self {
        match value {
            BlockOrExpr::Block(x) => x.into(),
            BlockOrExpr::Expr(x) => x.into(),
        }
    }
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
pub(crate) enum Pat {
    VarDecl(VarDeclId),
    Arr(Vec<Pat>),
}

impl Pat {
    pub fn children(&self) -> Box<dyn Iterator<Item = HirNode> + '_> {
        match self {
            Pat::VarDecl(x) => Box::new(iter::once(HirNode::from(*x))),
            Pat::Arr(x) => Box::new(x.iter().flat_map(|x| x.children())),
        }
    }
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
            TokenKind::INT_TY => Self::Int,
            TokenKind::DOUBLE_TY => Self::Double,
            TokenKind::FLOAT_TY => Self::Float,
            TokenKind::REF_TY => Self::Ref,
            TokenKind::STRING_TY => Self::String,
            TokenKind::ARRAY_TY => Self::Array,
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
    pub shards: Vec<StrShardId>,
    pub node: ast::StrExpr,
}

hir_children! {
    StrExpr,
    child_iter!(shards)
}

#[derive(Debug, Clone)]
pub(crate) enum StrShard {
    Str { val: String, node: ast::StrShard },
    Expr { expr: ExprId, node: ast::StrShard },
}

impl StrShard {
    pub fn children<'a>(&'a self) -> Box<dyn Iterator<Item = HirNode> + 'a> {
        match self {
            StrShard::Str { .. } => Box::new(iter::empty()),
            StrShard::Expr { expr, .. } => Box::new(iter::once((*expr).into())),
        }
    }

    pub fn node(&self) -> Option<&dyn AstNode> {
        Some(match self {
            StrShard::Str { node, .. } => node,
            StrShard::Expr { node, .. } => node,
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
