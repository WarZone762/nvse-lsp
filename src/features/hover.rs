use tower_lsp::lsp_types::*;

use crate::{ast::AstNode, doc::Doc};

impl Doc {
    pub fn hover(&self, pos: Position) -> Option<Hover> {
        let token = self.tree.syntax().token_at_offset(self.offset_at(pos))?;

        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(token.text(&self.text).into())),
            range: Some(Range::new(
                self.pos_at(token.offset),
                self.pos_at(token.offset + token.len),
            )),
        })
    }
}
