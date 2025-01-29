use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
    rc::Rc,
};

use la_arena::{Arena, ArenaMap, Idx, RawIdx};
use tower_lsp::lsp_types::{TextDocumentItem, Url};
mod script;

pub(crate) use script::*;
use ty::{Symbol, Type};

use crate::{
    ast::{self, AstNode},
    doc::{Doc, DocMeta},
    game_data::{FnData, FormData, GlobalsDatabase, GlobalsDatabaseId},
    hir::*,
    syntax_node::Node,
    tree_builder::parse_str,
};

#[derive(Debug, Clone)]
pub(crate) struct Database {
    pub doc_metas: Arena<DocMeta>,
    pub text_map: ArenaMap<Idx<DocMeta>, Box<str>>,

    pub hir_map: ArenaMap<Idx<DocMeta>, Script>,
    pub script_db_map: ArenaMap<Idx<DocMeta>, ScriptDatabase>,

    pub type_maps: ArenaMap<Idx<DocMeta>, HashMap<ExprId, Type>>,
    pub name_type_maps: ArenaMap<Idx<DocMeta>, HashMap<NameId, Type>>,

    pub globals: HashMap<String, Symbol>,
    pub globals_dbs: Arena<GlobalsDatabase>,
    pub globals_db_name_map: HashMap<String, GlobalsDatabaseId>,
}

unsafe impl Send for Database {}
unsafe impl Sync for Database {}

impl Database {
    pub fn new() -> Self {
        let mut this = Self {
            doc_metas: Arena::new(),
            text_map: ArenaMap::new(),
            hir_map: ArenaMap::new(),
            script_db_map: ArenaMap::new(),
            type_maps: ArenaMap::new(),
            name_type_maps: ArenaMap::new(),
            globals: HashMap::new(),
            globals_dbs: Arena::new(),
            globals_db_name_map: HashMap::new(),
        };

        this.add_globals_db("Scriptrunner");

        this
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

    pub fn str_shard_to_hir(&self, file_id: FileId, node: ast::StrShard) -> Option<StrShardId> {
        match self.syntax_to_hir(file_id, node.syntax().clone())? {
            HirNode::StrShard(x) => Some(x),
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
        name_ref
            .node
            .syntax()
            .clone()
            .ancestors()
            .find_map(|x| match self.syntax_to_hir(file_id, x)? {
                HirNode::Block(x) => x.lookup(script_db).sym_table.map.get(&name_ref.name),
                HirNode::Script(x) => x.hir(self).sym_table.map.get(&name_ref.name),
                _ => None,
            })
            .or_else(|| self.globals.get(&name_ref.name))
    }

    pub fn load_json(&mut self) {
        macro_rules! load {
            ($name:literal) => {
                let data = include_str!(concat!("../resources/forms/", $name, ".json"));
                let forms = serde_json::from_str::<Vec<FormData>>(data).unwrap();

                let db = self.add_globals_db($name);
                for f in forms {
                    let edid = f.edid.clone();
                    self[db].add_global(edid.clone(), Type::Form(f.into()));
                    self.globals.insert(edid.clone(), Symbol::Global(db, edid));
                }
            };
        }

        load!("FalloutNV.esm");
        load!("DeadMoney.esm");
        load!("HonestHearts.esm");
        load!("OldWorldBlues.esm");
        load!("LonesomeRoad.esm");
        load!("GunRunnersArsenal.esm");
        load!("CaravanPack.esm");
        load!("ClassicPack.esm");
        load!("MercenaryPack.esm");
        load!("TribalPack.esm");

        load!("Fallout3.esm");
        load!("Anchorage.esm");
        load!("ThePitt.esm");
        load!("BrokenSteel.esm");
        load!("PointLookout.esm");
        load!("Zeta.esm");
        load!("TaleOfTwoWastelands.esm");
        load!("YUPTTW.esm");

        macro_rules! load_fns {
            ($name:literal) => {
                let data = include_str!(concat!("../resources/functions/", $name, ".json"));
                let forms = serde_json::from_str::<Vec<FnData>>(data).unwrap();

                let db = self.add_globals_db($name);
                for f in forms {
                    let name = f.name.clone();
                    self[db].add_global(name.clone(), f.into());
                    self.globals.insert(name.clone(), Symbol::Global(db, name));
                }
            };
        }

        load_fns!("Base game");
        load_fns!("JIP LN NVSE");
        load_fns!("JohnnyGuitarNVSE");
        load_fns!("kNVSE");
        load_fns!("lStewieAl's Tweaks");
        load_fns!("MCM Extensions");
        load_fns!("NVSE");
        load_fns!("ShowOffNVSE Plugin");
        load_fns!("ttw_nvse");
    }

    pub fn add_doc(&mut self, doc: TextDocumentItem) -> Doc {
        if let Some(old_doc) = self.doc(&doc.uri) {
            self.update_doc_text(*old_doc, doc.text);
            return old_doc;
        }
        let TextDocumentItem { uri, language_id, version, text } = doc;
        let mut meta = DocMeta::new(uri, language_id);
        meta.version = version;
        let (root, diagnostics) = parse_str(&text);
        meta.diagnostics = diagnostics;
        let id = FileId(self.doc_metas.alloc(meta));
        self.text_map.insert(id.0, text.into_boxed_str());
        lower::lower(self, id, root);
        Doc(id)
    }

    pub fn add_globals_db(&mut self, name: &str) -> GlobalsDatabaseId {
        let db = GlobalsDatabaseId(self.globals_dbs.alloc(GlobalsDatabase::new(name.into())));
        self.globals_db_name_map.insert(name.into(), db);
        db
    }

    pub fn globals_db(&self, name: &str) -> Option<GlobalsDatabaseId> {
        self.globals_db_name_map.get(name).copied()
    }

    pub fn update_doc_text(&mut self, file_id: FileId, new_text: String) {
        let (root, diagnostics) = parse_str(&new_text);
        self.doc_metas[file_id.0].diagnostics = diagnostics;
        self.doc_metas[file_id.0].modified = true;
        self.text_map.insert(file_id.0, new_text.into_boxed_str());
        lower::lower(self, file_id, root);
    }

    pub fn analyze_workspace(&mut self) -> impl Iterator<Item = Doc> {
        let gdb = self.globals_db("Scriptrunner").unwrap();
        for (k, _) in self.globals_dbs[gdb.0].globals.drain() {
            self.globals.remove(&k);
        }
        (0..self.doc_metas.len()).map(|id| {
            let id = Idx::from_raw(RawIdx::from_u32(id as u32));
            if self.doc_metas[id].modified {
                let res = infer_local::infer(self, id.into());
                res.apply(self);

                self.doc_metas[id].modified = false;
            }
            Doc(id.into())
        })
    }

    pub fn doc(&self, uri: &Url) -> Option<Doc> {
        Some(Doc(self.doc_metas.iter().find(|(_, x)| x.uri == *uri)?.0.into()))
    }
}

impl Index<GlobalsDatabaseId> for Database {
    type Output = GlobalsDatabase;

    fn index(&self, index: GlobalsDatabaseId) -> &Self::Output {
        &self.globals_dbs[index.0]
    }
}

impl IndexMut<GlobalsDatabaseId> for Database {
    fn index_mut(&mut self, index: GlobalsDatabaseId) -> &mut Self::Output {
        &mut self.globals_dbs[index.0]
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
