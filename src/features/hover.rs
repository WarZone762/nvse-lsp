use tower_lsp::lsp_types::*;

use crate::{ast::AstNode, db::Database, doc::Doc};

impl Doc {
    pub fn hover(&self, db: &Database, pos: Position) -> Option<Hover> {
        let token = self.hir(db).node.syntax().token_at_offset(self.offset_at(db, pos))?;

        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(token.text(self.text(db)).into())),
            range: Some(Range::new(
                self.pos_at(db, token.offset),
                self.pos_at(db, token.offset + token.len),
            )),
        })
    }
}
