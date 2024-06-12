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

        let (doc, name) = match self.resolve(db, node)? {
            Symbol::Local(file_id, x) => (Doc(file_id), x),
            Symbol::Global(_) => return None,
        };
        let script_db = doc.script_db(db);
        let name_node = name.lookup(script_db).node.syntax();

        let start = doc.pos_at(db, name_node.offset);
        let end = doc.pos_at(db, name_node.end());
        let res = vec![Location { uri: doc.meta(db).uri.clone(), range: Range::new(start, end) }];

        Some(GotoDefinitionResponse::Array(res))
    }

    pub fn refs(&self, db: &Database, pos: Position, incl_def: bool) -> Option<Vec<Location>> {
        let node = db.syntax_to_hir(
            **self,
            self.hir(db).node.syntax().token_at_offset(self.offset_at(db, pos))?.parent()?,
        )?;

        let sym = self.resolve(db, node)?;

        let script_db = self.script_db(db);
        let (doc, name) = match sym {
            Symbol::Local(file_id, x) => (Doc(file_id), x),
            Symbol::Global(_) => return None,
        };

        let uri = doc.meta(db).uri.clone();
        let mut res = vec![];

        for e in doc.references(db, sym) {
            if let HirNode::Name(e) = e
                && e == name
                && incl_def
            {
                let name = e.lookup(script_db).node.syntax();
                let start = doc.pos_at(db, name.offset);
                let end = doc.pos_at(db, name.end());
                res.push(Location { uri: uri.clone(), range: Range::new(start, end) });
            } else {
                let name_ref = match e {
                    HirNode::Expr(x) => match x.lookup(script_db) {
                        Expr::NameRef(x) => x,
                        _ => continue,
                    },
                    _ => continue,
                }
                .node
                .syntax();

                let start = doc.pos_at(db, name_ref.offset);
                let end = doc.pos_at(db, name_ref.end());
                res.push(Location { uri: uri.clone(), range: Range::new(start, end) });
            }
        }

        Some(res)
    }
}
