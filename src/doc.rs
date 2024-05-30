use tower_lsp::lsp_types::*;

use crate::{
    ast::{AstNode, Script},
    tree_builder::{self, parse_str},
};

#[derive(Debug)]
pub(crate) struct Doc {
    pub uri: Url,
    pub text: Box<str>,
    pub tree: Script,
    pub diagnostics: Vec<tree_builder::Diagnostic>,
    pub version: i32,
    pub language_id: String,
}

unsafe impl Send for Doc {}
unsafe impl Sync for Doc {}

impl From<TextDocumentItem> for Doc {
    fn from(value: TextDocumentItem) -> Self {
        let text = value.text.into_boxed_str();
        let (tree, diagnostics) = parse_str(&text);
        Self {
            uri: value.uri,
            text,
            tree: AstNode::cast(tree).unwrap(),
            diagnostics,
            version: value.version,
            language_id: value.language_id,
        }
    }
}

impl Doc {
    pub fn update_text(&mut self, new_text: String) {
        let text = new_text.into_boxed_str();
        let (tree, diagnostics) = parse_str(&text);
        self.text = text;
        self.tree = AstNode::cast(tree).unwrap();
        self.diagnostics = diagnostics;
    }

    pub fn diagnostics(&self) -> impl Iterator<Item = Diagnostic> + '_ {
        self.diagnostics.iter().map(|d| {
            let start = self.pos_at(d.offset);
            let end = self.pos_at(d.offset + d.len);
            Diagnostic {
                range: Range::new(start, end),
                severity: Some(d.severity),
                message: d.msg.clone(),
                ..Default::default()
            }
        })
    }

    pub fn offset_at(&self, mut pos: Position) -> u32 {
        let mut offset = 0;
        let mut chars = self.text.chars();
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

    pub fn pos_at(&self, offset: u32) -> Position {
        let (mut line, mut character) = (0, 0);

        for (_, c) in self.text.chars().enumerate().take_while(|(i, _)| (*i as u32) < offset) {
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
