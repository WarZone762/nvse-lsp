use std::collections::HashMap;

use tower_lsp::lsp_types::*;

use crate::{
    ast::AstNode,
    db::{Database, Lookup},
    doc::Doc,
    hir::{Expr, HirNode},
};

impl Doc {
    pub fn rename(&self, db: &Database, pos: Position, new_name: String) -> Option<WorkspaceEdit> {
        let node = db.syntax_to_hir(
            **self,
            self.hir(db).node.syntax().token_at_offset(self.offset_at(db, pos))?.parent()?,
        )?;

        let sym = self.resolve(db, node)?;

        let script_db = self.script_db(db);

        let mut changes = HashMap::new();
        let mut edits = vec![];

        for e in self.references(db, sym).filter_map(|x| match x {
            HirNode::Expr(x) => match x.lookup(script_db) {
                Expr::NameRef(x) => Some(x.node.syntax()),
                _ => None,
            },
            HirNode::Name(x) => Some(x.lookup(script_db).node.syntax()),
            _ => None,
        }) {
            let start = self.pos_at(db, e.offset);
            let end = self.pos_at(db, e.end());
            edits.push(TextEdit { range: Range::new(start, end), new_text: new_name.clone() })
        }

        changes.insert(self.meta(db).uri.clone(), edits);

        Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        })
    }
}
