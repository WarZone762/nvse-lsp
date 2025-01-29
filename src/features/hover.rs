use tower_lsp::lsp_types::*;

use crate::{
    ast::AstNode,
    db::{Database, Lookup},
    doc::Doc,
    hir::ty::Symbol,
};

impl Doc {
    pub fn hover(&self, db: &Database, pos: Position) -> Option<Hover> {
        let token = self.hir(db).node.syntax().token_at_offset(self.offset_at(db, pos))?;
        let node = db.syntax_to_hir(**self, token.parent()?)?;

        let string = match self.resolve(db, node)? {
            Symbol::Local(file_id, x) => {
                x.type_(db, file_id).to_string_with_name(&x.lookup(file_id.script_db(db)).name, 0)
            }
            Symbol::Global(gdb, name) => gdb
                .lookup(db)
                .globals
                .get(&name)
                .unwrap()
                .to_string_with_name(token.text(self.text(db)), 0),
        };

        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(string)),
            range: Some(Range::new(
                self.pos_at(db, token.offset),
                self.pos_at(db, token.offset + token.len),
            )),
        })
    }
}
