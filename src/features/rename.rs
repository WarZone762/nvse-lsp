use std::collections::HashMap;

use tower_lsp::lsp_types::*;

use crate::{
    ast::AstNode,
    db::{Database, Lookup},
    doc::Doc,
    hir::{ty::Symbol, Expr, HirNode},
};

impl Doc {
    pub fn rename(&self, db: &Database, pos: Position, new_name: String) -> Option<WorkspaceEdit> {
        let node = db.syntax_to_hir(
            **self,
            self.hir(db).node.syntax().token_at_offset(self.offset_at(db, pos))?.parent()?,
        )?;

        let var_decl = match self.resolve(db, node)? {
            Symbol::Local(x) => x,
            Symbol::Global(_) => return None,
        };

        let script_db = self.script_db(db);
        let def_node = var_decl.lookup(script_db).name.lookup(script_db).node.syntax();

        let start = self.pos_at(db, def_node.offset);
        let end = self.pos_at(db, def_node.end());
        let mut changes = HashMap::new();

        let mut edits =
            vec![TextEdit { range: Range::new(start, end), new_text: new_name.clone() }];

        for e in HirNode::Script(**self)
            .dfs(db, script_db)
            .filter(|x| matches!(self.resolve(db, *x), Some(Symbol::Local(def)) if def == var_decl))
            .filter_map(|x| match x {
                HirNode::Expr(x) => match x.lookup(script_db) {
                    Expr::NameRef(x) => Some(x.node.syntax()),
                    _ => None,
                },
                _ => None,
            })
        {
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
