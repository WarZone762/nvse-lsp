use std::ops::Deref;

use tower_lsp::lsp_types::*;

use crate::{
    db::{Database, FileId},
    tree_builder::{self},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Doc(pub FileId);

impl Deref for Doc {
    type Target = FileId;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Doc {
    pub fn diagnostics<'a>(&'a self, db: &'a Database) -> impl Iterator<Item = Diagnostic> + 'a {
        self.meta(db).diagnostics.iter().map(|d| {
            let start = self.pos_at(db, d.offset);
            let end = self.pos_at(db, d.offset + d.len);
            Diagnostic {
                range: Range::new(start, end),
                severity: Some(d.severity),
                message: d.msg.clone(),
                ..Default::default()
            }
        })
    }

    pub fn offset_at(&self, db: &Database, mut pos: Position) -> u32 {
        let mut offset = 0;
        let mut chars = self.text(db).chars();
        while let Some(c) = chars.next()
            && pos.line != 0
        {
            offset += 1;
            if c == '\n' {
                pos.line -= 1;
            }
        }

        offset + pos.character
    }

    pub fn pos_at(&self, db: &Database, offset: u32) -> Position {
        let (mut line, mut character) = (0, 0);

        for (_, c) in self.text(db).chars().enumerate().take_while(|(i, _)| (*i as u32) < offset) {
            if c == '\n' {
                character = 0;
                line += 1;
            } else {
                character += 1;
            }
        }

        Position { line, character }
    }
}

#[derive(Debug)]
pub(crate) struct DocMeta {
    pub uri: Url,
    pub language_id: String,
    pub version: i32,
    pub diagnostics: Vec<tree_builder::Diagnostic>,
}
