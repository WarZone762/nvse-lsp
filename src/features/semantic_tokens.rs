use tower_lsp::lsp_types::*;

use crate::{
    ast::{self, AstNode},
    db::{Database, Lookup},
    hir::ty::Symbol,
    syntax_node::{NodeKind, TokenKind},
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
    pub(crate) fn semantic_tokens(&self, db: &Database) -> Vec<SemanticToken> {
        let mut builder = SemanticTokenBuilder::new();

        for t in self.hir(db).node.syntax().leafs() {
            let kind = if t
                .parent()
                .and_then(|x| {
                    Some(x.kind == NodeKind::NAME_REF && x.parent()?.kind == NodeKind::CALL_EXPR)
                })
                .is_some_and(|x| x)
            {
                SemanticTokenType::FUNCTION
            } else if t
                .parent()
                .and_then(|x| {
                    Some(
                        x.kind == NodeKind::NAME_REF
                            && ast::FieldExpr::cast(x.parent()?)?.field()?.syntax() == &x,
                    )
                })
                .is_some_and(|x| x)
            {
                if t.parent()
                    .and_then(|x| x.parent()?.parent())
                    .is_some_and(|x| x.kind == NodeKind::CALL_EXPR)
                {
                    SemanticTokenType::METHOD
                } else {
                    SemanticTokenType::PROPERTY
                }
            } else if t
                .parent()
                .and_then(|x| match self.resolve(db, db.syntax_to_hir(**self, x)?)? {
                    Symbol::Local(file_id, x) => {
                        let doc = Doc(file_id);
                        Some(
                            x.lookup(doc.script_db(db)).node.syntax().parent()?.kind
                                == NodeKind::PARAM_LIST,
                        )
                    }
                    _ => None,
                })
                .is_some_and(|x| x)
            {
                SemanticTokenType::PARAMETER
            } else if t.kind == TokenKind::RBRACK
                && t.parent().is_some_and(|x| x.kind == NodeKind::STR_SHARD_EXPR)
            {
                SemanticTokenTypeCustom::ESCAPE_SEQUENCE
            } else {
                let Some(kind) = t.kind.to_semantic() else { continue };
                kind
            };
            let Position { mut line, character: mut pos } = self.pos_at(db, t.offset);

            for line_text in t.text(self.text(db)).split('\n') {
                builder.push(&kind, None, line, pos, line_text.len() as _);
                if t.kind == TokenKind::STR_SHARD {
                    for (i, c) in line_text.chars().enumerate() {
                        let i = i as u32;
                        if t.text(self.text(db)).chars().nth(i.saturating_sub(1) as _) == Some('\\')
                        {
                            continue;
                        }
                        if c == '\\' {
                            builder.push(
                                &SemanticTokenTypeCustom::ESCAPE_SEQUENCE,
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
    SemanticTokenType::METHOD,
    SemanticTokenType::NUMBER,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::STRING,
    SemanticTokenType::TYPE,
    SemanticTokenType::VARIABLE,
    SemanticTokenTypeCustom::BOOLEAN,
    SemanticTokenTypeCustom::ESCAPE_SEQUENCE,
    SemanticTokenTypeCustom::PUNCTUATION,
];

pub(crate) struct SemanticTokenTypeCustom;

impl SemanticTokenTypeCustom {
    pub const BOOLEAN: SemanticTokenType = SemanticTokenType::new("boolean");
    pub const ESCAPE_SEQUENCE: SemanticTokenType = SemanticTokenType::new("escapeSequence");
    pub const PUNCTUATION: SemanticTokenType = SemanticTokenType::new("punctuation");
}

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
