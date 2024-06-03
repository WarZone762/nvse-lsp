use tower_lsp::lsp_types::*;

use crate::{
    ast::AstNode,
    db::{Database, Lookup},
    doc::Doc,
    hir::{ty::Symbol, Expr, HirNode},
};

impl Doc {
    pub fn goto_def(&self, db: &Database, pos: Position) -> Option<GotoDefinitionResponse> {
        let node = db.syntax_to_hir(
            **self,
            self.hir(db).node.syntax().token_at_offset(self.offset_at(db, pos))?.parent()?,
        )?;
        let script_db = self.script_db(db);
        let name_ref = match node {
            HirNode::Expr(x) => match x.lookup(script_db) {
                Expr::NameRef(x) => x,
                _ => return None,
            },
            _ => return None,
        };

        let var_decl = match db.resolve(**self, name_ref)? {
            Symbol::Local(x) => x,
            Symbol::Global(_) => return None,
        };
        let def_node = var_decl.lookup(script_db).name.lookup(script_db).node.syntax();

        let start = self.pos_at(db, def_node.offset);
        let end = self.pos_at(db, def_node.end());
        let res = vec![Location { uri: self.meta(db).uri.clone(), range: Range::new(start, end) }];

        Some(GotoDefinitionResponse::Array(res))
    }
}
