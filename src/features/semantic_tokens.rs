use tower_lsp::lsp_types::*;

use crate::{lexer::Lexer, node::TokenKind, Doc};

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

        for token in Lexer::new(&self.text) {
            let Some(kind) = token.kind.to_semantic() else { continue };
            let Position { line, character: pos } = self.pos_at(token.offset);

            builder.push(kind, None, line, pos, token.len);

            if token.kind == TokenKind::String {
                for (i, c) in token.text(&self.text).chars().enumerate() {
                    let i = i as u32;
                    if token.text(&self.text).chars().nth(i.saturating_sub(1) as _) == Some('\\') {
                        continue;
                    }
                    if c == '\\' {
                        builder.push(
                            SemanticTokenType::new("escapeSequence"),
                            None,
                            line,
                            i + pos,
                            2,
                        );
                    }
                }
            }
        }

        builder.finish()
    }
}

static LEGEND: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::NUMBER,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::STRING,
    SemanticTokenType::TYPE,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::new("escapeSequence"),
];

pub(crate) fn semantic_token_type_index(value: SemanticTokenType) -> u32 {
    LEGEND.iter().position(|x| *x == value).expect("semantic token type not in legend") as _
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
        token_type: SemanticTokenType,
        token_modifiers_bitset: Option<SemanticTokenModifier>,
        line: u32,
        pos: u32,
        len: u32,
    ) {
        let token_type = semantic_token_type_index(token_type);

        let delta_line = line - self.last_line;
        let delta_start = if delta_line == 0 { pos - self.last_pos } else { pos };

        self.last_line = line;
        self.last_pos = pos;

        self.semantic_tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length: len,
            token_type,
            token_modifiers_bitset: 0,
        })
    }

    pub fn finish(self) -> Vec<SemanticToken> {
        self.semantic_tokens
    }
}
