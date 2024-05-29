use tower_lsp::lsp_types::*;

use crate::{
    ast::AstNode,
    syntax_node::{NodeKind, NodeOrToken, TokenKind},
    Doc,
};

pub(crate) fn capabilities() -> SemanticTokensServerCapabilities {
    SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
        legend: SemanticTokensLegend { token_types: LEGEND.into(), token_modifiers: vec![] },
        full: Some(SemanticTokensFullOptions::Bool(true)),
        ..Default::default()
    })
}

impl Doc {
    pub(crate) fn semantic_tokens(&self) -> Vec<SemanticToken> {
        let mut builder = SemanticTokenBuilder::new();

        for t in NodeOrToken::Node(self.tree.syntax().clone()).leafs() {
            let kind = if t
                .parent()
                .and_then(|x| {
                    Some(x.kind == NodeKind::NameRef && x.parent()?.kind == NodeKind::CallExpr)
                })
                .unwrap_or(false)
            {
                SemanticTokenType::FUNCTION
            } else {
                let Some(kind) = t.kind.to_semantic() else { continue };
                kind
            };
            let Position { mut line, character: mut pos } = self.pos_at(t.offset);

            for line_text in t.text(&self.text).split('\n') {
                builder.push(&kind, None, line, pos, line_text.len() as _);
                if t.kind == TokenKind::String {
                    for (i, c) in line_text.chars().enumerate() {
                        let i = i as u32;
                        if t.text(&self.text).chars().nth(i.saturating_sub(1) as _) == Some('\\') {
                            continue;
                        }
                        if c == '\\' {
                            builder.push(
                                &SemanticTokenType::new("escapeSequence"),
                                None,
                                line,
                                i + pos,
                                2,
                            );
                        }
                    }
                }
                line += 1;
                pos = 0;
            }
        }

        builder.finish()
    }
}

static LEGEND: &[SemanticTokenType] = &[
    SemanticTokenType::COMMENT,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::NUMBER,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::STRING,
    SemanticTokenType::TYPE,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::new("punctuation"),
    SemanticTokenType::new("escapeSequence"),
    SemanticTokenType::new("boolean"),
];

pub(crate) fn semantic_token_type_index(value: &SemanticTokenType) -> u32 {
    LEGEND.iter().position(|x| x == value).expect("semantic token type not in legend") as _
}

struct SemanticTokenBuilder {
    last_line: u32,
    last_pos: u32,
    semantic_tokens: Vec<SemanticToken>,
}

impl SemanticTokenBuilder {
    pub fn new() -> Self {
        Self { last_line: 0, last_pos: 0, semantic_tokens: vec![] }
    }

    pub fn push(
        &mut self,
        token_type: &SemanticTokenType,
        _token_modifiers_bitset: Option<SemanticTokenModifier>,
        line: u32,
        pos: u32,
        len: u32,
    ) {
        let token_type = semantic_token_type_index(token_type);

        let delta_line = line - self.last_line;
        let delta_start = if delta_line == 0 { pos - self.last_pos } else { pos };

        if let Some(last) = self.semantic_tokens.last()
            && delta_line == 0
            && delta_start < last.length
        {
            let last = self.semantic_tokens.pop().unwrap();
            self.semantic_tokens.push(SemanticToken {
                delta_line: last.delta_line,
                delta_start: last.delta_start,
                length: delta_start,
                token_type: last.token_type,
                token_modifiers_bitset: last.token_modifiers_bitset,
            });
            self.semantic_tokens.push(SemanticToken {
                delta_line: 0,
                delta_start,
                length: len,
                token_type,
                token_modifiers_bitset: 0,
            });

            self.last_line = line;
            self.last_pos = pos + len;

            self.semantic_tokens.push(SemanticToken {
                delta_line: 0,
                delta_start: len,
                length: last.length - delta_start - len,
                token_type: last.token_type,
                token_modifiers_bitset: last.token_modifiers_bitset,
            });
            return;
        }

        self.last_line = line;
        self.last_pos = pos;

        self.semantic_tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length: len,
            token_type,
            token_modifiers_bitset: 0,
        });
    }

    pub fn finish(self) -> Vec<SemanticToken> {
        self.semantic_tokens
    }
}
