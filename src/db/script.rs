use std::{cell::UnsafeCell, collections::HashMap, rc::Rc};

use ty::InferredType;

use super::{Database, FileId, Lookup};
use crate::{ast::AstNode, hir::*, syntax_node::Node};

#[derive(Debug)]
pub(crate) struct ScriptDatabase {
    pub items: Vec<Item>,
    pub stmts: Vec<Stmt>,
    pub exprs: Vec<Expr>,
    pub blocks: Vec<Block>,
    pub var_decls: Vec<VarDecl>,
    pub names: Vec<Name>,
    pub str_shards: Vec<StringShard>,

    pub syntax_to_hir_cache: UnsafeCell<HashMap<usize, HirNode>>,
}

impl ScriptDatabase {
    pub fn new() -> Self {
        Self {
            items: vec![],
            stmts: vec![],
            exprs: vec![],
            blocks: vec![],
            var_decls: vec![],
            names: vec![],
            str_shards: vec![],
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
            return Some(*hir_node);
        }

        let hir_node = syntax
            .clone()
            .ancestors()
            .filter_map(|x| db.syntax_to_hir(file_id, x))
            .flat_map(|x| x.children(db, self))
            .find(|x| x.node(db, self).is_some_and(|x| *x.syntax() == syntax))?;

        unsafe { (*self.syntax_to_hir_cache.get()).insert(Rc::as_ptr(&syntax) as _, hir_node) };
        Some(hir_node)
    }

    pub fn add_item(&mut self, item: Item) -> ItemId {
        self.items.push(item);
        ItemId((self.items.len() - 1) as _)
    }

    pub fn add_stmt(&mut self, stmt: Stmt) -> StmtId {
        self.stmts.push(stmt);
        StmtId((self.stmts.len() - 1) as _)
    }

    pub fn add_expr(&mut self, expr: Expr) -> ExprId {
        self.exprs.push(expr);
        ExprId((self.exprs.len() - 1) as _)
    }

    pub fn add_block(&mut self, block: Block) -> BlockId {
        self.blocks.push(block);
        BlockId((self.blocks.len() - 1) as _)
    }

    pub fn add_var_decl(&mut self, var_decl: VarDecl) -> VarDeclId {
        self.var_decls.push(var_decl);
        VarDeclId((self.var_decls.len() - 1) as _)
    }

    pub fn add_name(&mut self, name: Name) -> NameId {
        self.names.push(name);
        NameId((self.names.len() - 1) as _)
    }

    pub fn add_str_shard(&mut self, str_shard: StringShard) -> StringShardId {
        self.str_shards.push(str_shard);
        StringShardId((self.str_shards.len() - 1) as _)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ItemId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct StmtId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ExprId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct BlockId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct VarDeclId(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct NameId(u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct StringShardId(u32);

impl Lookup for ItemId {
    type DB = ScriptDatabase;
    type Output = Item;

    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output {
        db.items
            .get(self.0 as usize)
            .unwrap_or_else(|| panic!("failed to find {self:?} in Database"))
    }
}

impl Lookup for StmtId {
    type DB = ScriptDatabase;
    type Output = Stmt;

    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output {
        db.stmts
            .get(self.0 as usize)
            .unwrap_or_else(|| panic!("failed to find {self:?} in Database"))
    }
}

impl Lookup for ExprId {
    type DB = ScriptDatabase;
    type Output = Expr;

    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output {
        db.exprs
            .get(self.0 as usize)
            .unwrap_or_else(|| panic!("failed to find {self:?} in Database"))
    }
}

impl Lookup for BlockId {
    type DB = ScriptDatabase;
    type Output = Block;

    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output {
        db.blocks
            .get(self.0 as usize)
            .unwrap_or_else(|| panic!("failed to find {self:?} in Database"))
    }
}

impl Lookup for VarDeclId {
    type DB = ScriptDatabase;
    type Output = VarDecl;

    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output {
        db.var_decls
            .get(self.0 as usize)
            .unwrap_or_else(|| panic!("failed to find {self:?} in Database"))
    }
}

impl Lookup for NameId {
    type DB = ScriptDatabase;
    type Output = Name;

    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output {
        db.names
            .get(self.0 as usize)
            .unwrap_or_else(|| panic!("failed to find {self:?} in Database"))
    }
}

impl Lookup for StringShardId {
    type DB = ScriptDatabase;
    type Output = StringShard;

    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output {
        db.str_shards
            .get(self.0 as usize)
            .unwrap_or_else(|| panic!("failed to find {self:?} in Database"))
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

    pub fn type_(&self, db: &Database, file_id: FileId) -> InferredType {
        db.type_maps
            .get(file_id.0)
            .and_then(|x| x.get(self))
            .cloned()
            .unwrap_or(InferredType::any())
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
    pub fn type_<'a>(&self, db: &'a Database, file_id: FileId) -> InferredType {
        db.name_type_maps
            .get(file_id.0)
            .and_then(|x| x.get(self))
            .cloned()
            .unwrap_or(InferredType::any())
    }

    pub fn node<'a>(&self, db: &'a ScriptDatabase) -> Option<&'a dyn AstNode> {
        Some(&self.lookup(db).node)
    }
}

impl StringShardId {
    pub fn children<'a>(&self, db: &'a ScriptDatabase) -> Box<dyn Iterator<Item = HirNode> + 'a> {
        Box::new(self.lookup(db).children(db))
    }

    pub fn node<'a>(&self, db: &'a ScriptDatabase) -> Option<&'a dyn AstNode> {
        self.lookup(db).node(db)
    }
}
