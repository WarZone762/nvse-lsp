use std::{collections::HashMap, rc::Rc};

use la_arena::{Arena, ArenaMap, Idx, RawIdx};
use tower_lsp::lsp_types::{TextDocumentItem, Url};
mod script;

pub(crate) use script::*;
use ty::{Symbol, Type};

use crate::{
    ast::{self, AstNode},
    doc::{Doc, DocMeta},
    hir::*,
    syntax_node::Node,
    tree_builder::parse_str,
};

#[derive(Debug)]
pub(crate) struct Database {
    pub doc_metas: Arena<DocMeta>,
    pub text_map: ArenaMap<Idx<DocMeta>, Box<str>>,
    pub hir_map: ArenaMap<Idx<DocMeta>, Script>,
    pub script_db_map: ArenaMap<Idx<DocMeta>, ScriptDatabase>,
    pub type_maps: ArenaMap<Idx<DocMeta>, HashMap<ExprId, Type>>,
    pub globals: Vec<Symbol>,
}

unsafe impl Send for Database {}
unsafe impl Sync for Database {}

impl Database {
    pub fn new() -> Self {
        Self {
            doc_metas: Arena::new(),
            text_map: ArenaMap::new(),
            hir_map: ArenaMap::new(),
            script_db_map: ArenaMap::new(),
            type_maps: ArenaMap::new(),
            globals: vec![],
        }
    }

    pub fn script_to_hir(&self, file_id: FileId, node: &ast::Script) -> Option<FileId> {
        match self.syntax_to_hir(file_id, node.syntax().clone())? {
            HirNode::Script(x) => Some(x),
            _ => None,
        }
    }

    pub fn item_to_hir(&self, file_id: FileId, node: &ast::Item) -> Option<ItemId> {
        match self.syntax_to_hir(file_id, node.syntax().clone())? {
            HirNode::Item(x) => Some(x),
            _ => None,
        }
    }

    pub fn stmt_to_hir(&self, file_id: FileId, node: &ast::Stmt) -> Option<StmtId> {
        match self.syntax_to_hir(file_id, node.syntax().clone())? {
            HirNode::Stmt(x) => Some(x),
            _ => None,
        }
    }

    pub fn expr_to_hir(&self, file_id: FileId, node: &ast::Expr) -> Option<ExprId> {
        match self.syntax_to_hir(file_id, node.syntax().clone())? {
            HirNode::Expr(x) => Some(x),
            _ => None,
        }
    }

    pub fn var_decl_to_hir(&self, file_id: FileId, node: ast::VarDecl) -> Option<VarDeclId> {
        match self.syntax_to_hir(file_id, node.syntax().clone())? {
            HirNode::VarDecl(x) => Some(x),
            _ => None,
        }
    }

    pub fn block_to_hir(&self, file_id: FileId, node: ast::BlockStmt) -> Option<BlockId> {
        match self.syntax_to_hir(file_id, node.syntax().clone())? {
            HirNode::Block(x) => Some(x),
            _ => None,
        }
    }

    pub fn name_to_hir(&self, file_id: FileId, node: ast::Name) -> Option<NameId> {
        match self.syntax_to_hir(file_id, node.syntax().clone())? {
            HirNode::Name(x) => Some(x),
            _ => None,
        }
    }

    pub fn str_shard_to_hir(
        &self,
        file_id: FileId,
        node: ast::StringShard,
    ) -> Option<StringShardId> {
        match self.syntax_to_hir(file_id, node.syntax().clone())? {
            HirNode::StringShard(x) => Some(x),
            _ => None,
        }
    }

    pub fn syntax_to_hir(&self, file_id: FileId, node: Rc<Node>) -> Option<HirNode> {
        if node.parent.is_none() {
            return Some(HirNode::Script(file_id));
        }

        let script_db = file_id.script_db(self);
        script_db.syntax_to_hir(self, file_id, node)
    }

    pub fn resolve(&self, file_id: FileId, name_ref: &NameRef) -> Option<&Symbol> {
        let script_db = file_id.script_db(self);
        name_ref.node.syntax().clone().ancestors().find_map(|x| {
            match self.syntax_to_hir(file_id, x)? {
                HirNode::Block(x) => x.lookup(script_db).sym_table.map.get(&name_ref.name),
                HirNode::Script(x) => x.hir(self).sym_table.map.get(&name_ref.name),
                _ => None,
            }
        })
    }

    pub fn add_doc(&mut self, doc: TextDocumentItem) -> Doc {
        if let Some(old_doc) = self.doc(&doc.uri) {
            self.update_doc_text(*old_doc, doc.text);
            return old_doc;
        }
        let TextDocumentItem { uri, language_id, version, text } = doc;
        let (root, diagnostics) = parse_str(&text);
        let meta = DocMeta { uri, language_id, version, diagnostics };
        let id = FileId(self.doc_metas.alloc(meta));
        self.text_map.insert(id.0, text.into_boxed_str());
        lower::lower(self, id, root);
        Doc(id)
    }

    pub fn update_doc_text(&mut self, file_id: FileId, new_text: String) {
        let (root, diagnostics) = parse_str(&new_text);
        self.doc_metas[file_id.0].diagnostics = diagnostics;
        self.text_map.insert(file_id.0, new_text.into_boxed_str());
        lower::lower(self, file_id, root);
    }

    pub fn analyze_workspace(&mut self) -> impl Iterator<Item = Doc> {
        (0..self.doc_metas.len()).map(|id| {
            let id = Idx::from_raw(RawIdx::from_u32(id as u32));
            propagate::propagate(self, id.into());
            Doc(id.into())
        })
    }

    pub fn doc(&self, uri: &Url) -> Option<Doc> {
        Some(Doc(self.doc_metas.iter().find(|(_, x)| x.uri == *uri)?.0.into()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct FileId(pub Idx<DocMeta>);

impl From<Idx<DocMeta>> for FileId {
    fn from(value: Idx<DocMeta>) -> Self {
        Self(value)
    }
}

impl FileId {
    pub(crate) fn meta<'a>(&self, db: &'a Database) -> &'a DocMeta {
        &db.doc_metas[self.0]
    }

    pub(crate) fn text<'a>(&self, db: &'a Database) -> &'a str {
        &db.text_map[self.0]
    }

    pub(crate) fn hir<'a>(&self, db: &'a Database) -> &'a Script {
        &db.hir_map[self.0]
    }

    pub(crate) fn script_db<'a>(&self, db: &'a Database) -> &'a ScriptDatabase {
        &db.script_db_map[self.0]
    }
}

pub(crate) trait Lookup {
    type DB;
    type Output;
    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output;
}
