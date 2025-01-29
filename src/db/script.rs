use std::{cell::UnsafeCell, collections::HashMap, rc::Rc};

use la_arena::{Arena, Idx};

use super::{Database, FileId, Lookup, Type};
use crate::{ast::AstNode, hir::*, syntax_node::Node};

#[derive(Debug)]
pub(crate) struct ScriptDatabase {
    pub items: Arena<Item>,
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,
    pub blocks: Arena<Block>,
    pub var_decls: Arena<VarDecl>,
    pub names: Arena<Name>,
    pub str_shards: Arena<StrShard>,

    pub syntax_to_hir_cache: UnsafeCell<HashMap<usize, Option<HirNode>>>,
}

impl Clone for ScriptDatabase {
    fn clone(&self) -> Self {
        Self {
            items: self.items.clone(),
            stmts: self.stmts.clone(),
            exprs: self.exprs.clone(),
            blocks: self.blocks.clone(),
            var_decls: self.var_decls.clone(),
            names: self.names.clone(),
            str_shards: self.str_shards.clone(),
            syntax_to_hir_cache: UnsafeCell::new(unsafe {
                (*self.syntax_to_hir_cache.get()).clone()
            }),
        }
    }
}

impl ScriptDatabase {
    pub fn new() -> Self {
        Self {
            items: Arena::new(),
            stmts: Arena::new(),
            exprs: Arena::new(),
            blocks: Arena::new(),
            var_decls: Arena::new(),
            names: Arena::new(),
            str_shards: Arena::new(),
            syntax_to_hir_cache: UnsafeCell::new(HashMap::new()),
        }
    }

    pub fn syntax_to_hir(
        &self,
        db: &Database,
        file_id: FileId,
        syntax: Rc<Node>,
    ) -> Option<HirNode> {
        if let Some(hir_node) =
            unsafe { (*self.syntax_to_hir_cache.get()).get(&(Rc::as_ptr(&syntax) as _)) }
        {
            return *hir_node;
        }

        let hir_node = syntax
            .clone()
            .ancestors()
            .filter_map(|x| db.syntax_to_hir(file_id, x))
            .flat_map(|x| x.children(db, self))
            .find(|x| x.node(db, self).is_some_and(|x| *x.syntax() == syntax));

        unsafe { (*self.syntax_to_hir_cache.get()).insert(Rc::as_ptr(&syntax) as _, hir_node) };
        hir_node
    }

    pub fn add_item(&mut self, item: Item) -> ItemId {
        ItemId(self.items.alloc(item))
    }

    pub fn add_stmt(&mut self, stmt: Stmt) -> StmtId {
        StmtId(self.stmts.alloc(stmt))
    }

    pub fn add_expr(&mut self, expr: Expr) -> ExprId {
        ExprId(self.exprs.alloc(expr))
    }

    pub fn add_block(&mut self, block: Block) -> BlockId {
        BlockId(self.blocks.alloc(block))
    }

    pub fn add_var_decl(&mut self, var_decl: VarDecl) -> VarDeclId {
        VarDeclId(self.var_decls.alloc(var_decl))
    }

    pub fn add_name(&mut self, name: Name) -> NameId {
        NameId(self.names.alloc(name))
    }

    pub fn add_str_shard(&mut self, str_shard: StrShard) -> StrShardId {
        StrShardId(self.str_shards.alloc(str_shard))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ItemId(Idx<Item>);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct StmtId(Idx<Stmt>);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ExprId(Idx<Expr>);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct BlockId(Idx<Block>);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct VarDeclId(Idx<VarDecl>);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct NameId(Idx<Name>);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct StrShardId(Idx<StrShard>);

impl Lookup for ItemId {
    type DB = ScriptDatabase;
    type Output = Item;

    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output {
        &db.items[self.0]
    }
}

impl Lookup for StmtId {
    type DB = ScriptDatabase;
    type Output = Stmt;

    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output {
        &db.stmts[self.0]
    }
}

impl Lookup for ExprId {
    type DB = ScriptDatabase;
    type Output = Expr;

    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output {
        &db.exprs[self.0]
    }
}

impl Lookup for BlockId {
    type DB = ScriptDatabase;
    type Output = Block;

    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output {
        &db.blocks[self.0]
    }
}

impl Lookup for VarDeclId {
    type DB = ScriptDatabase;
    type Output = VarDecl;

    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output {
        &db.var_decls[self.0]
    }
}

impl Lookup for NameId {
    type DB = ScriptDatabase;
    type Output = Name;

    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output {
        &db.names[self.0]
    }
}

impl Lookup for StrShardId {
    type DB = ScriptDatabase;
    type Output = StrShard;

    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output {
        &db.str_shards[self.0]
    }
}

impl ItemId {
    pub fn children<'a>(&self, db: &'a ScriptDatabase) -> Box<dyn Iterator<Item = HirNode> + 'a> {
        self.lookup(db).children()
    }

    pub fn node<'a>(&self, db: &'a ScriptDatabase) -> Option<&'a dyn AstNode> {
        self.lookup(db).node()
    }
}

impl StmtId {
    pub fn children<'a>(&self, db: &'a ScriptDatabase) -> Box<dyn Iterator<Item = HirNode> + 'a> {
        self.lookup(db).children(db)
    }

    pub fn node<'a>(&self, db: &'a ScriptDatabase) -> Option<&'a dyn AstNode> {
        self.lookup(db).node(db)
    }
}

impl ExprId {
    pub fn children<'a>(&self, db: &'a ScriptDatabase) -> Box<dyn Iterator<Item = HirNode> + 'a> {
        self.lookup(db).children()
    }

    pub fn node<'a>(&self, db: &'a ScriptDatabase) -> Option<&'a dyn AstNode> {
        self.lookup(db).node()
    }

    pub fn type_(&self, db: &Database, file_id: FileId) -> Type {
        db.type_maps.get(file_id.0).and_then(|x| x.get(self)).cloned().unwrap_or(Type::Any)
    }
}

impl BlockId {
    pub fn children<'a>(&self, db: &'a ScriptDatabase) -> impl Iterator<Item = HirNode> + 'a {
        Box::new(self.lookup(db).children())
    }

    pub fn node<'a>(&self, db: &'a ScriptDatabase) -> Option<&'a dyn AstNode> {
        Some(&self.lookup(db).node)
    }
}

impl VarDeclId {
    pub fn children<'a>(&self, db: &'a ScriptDatabase) -> Box<dyn Iterator<Item = HirNode> + 'a> {
        Box::new(self.lookup(db).children())
    }

    pub fn node<'a>(&self, db: &'a ScriptDatabase) -> Option<&'a dyn AstNode> {
        Some(&self.lookup(db).node)
    }
}

impl NameId {
    pub fn type_(&self, db: &Database, file_id: FileId) -> Type {
        db.name_type_maps.get(file_id.0).and_then(|x| x.get(self)).cloned().unwrap_or(Type::Any)
    }

    pub fn node<'a>(&self, db: &'a ScriptDatabase) -> Option<&'a dyn AstNode> {
        Some(&self.lookup(db).node)
    }
}

impl StrShardId {
    pub fn children<'a>(&self, db: &'a ScriptDatabase) -> Box<dyn Iterator<Item = HirNode> + 'a> {
        Box::new(self.lookup(db).children())
    }

    pub fn node<'a>(&self, db: &'a ScriptDatabase) -> Option<&'a dyn AstNode> {
        self.lookup(db).node()
    }
}
