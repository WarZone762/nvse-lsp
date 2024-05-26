#![feature(let_chains)]

mod lexer;

use std::collections::HashMap;

use lexer::{Lexer, TokenKind};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use dashmap::DashMap;

static LEGEND: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,
    SemanticTokenType::NUMBER,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::STRING,
    SemanticTokenType::TYPE,
    SemanticTokenType::VARIABLE,
];

fn semantic_token_type_index(value: SemanticTokenType) -> u32 {
    LEGEND
        .iter()
        .position(|x| *x == value)
        .expect("semantic token type not in legend") as _
}

#[derive(Debug)]
struct Backend {
    client: Client,
    docs: DashMap<Url, Doc>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            docs: DashMap::new(),
        }
    }

    pub fn add_doc(&self, doc: TextDocumentItem) {
        self.docs.insert(doc.uri.clone(), doc.into());
    }

    pub async fn update_doc(&self, uri: Url, changes: Vec<TextDocumentContentChangeEvent>) {
        if let Some(mut doc) = self.docs.get_mut(&uri) {
            for change in changes {
                doc.text = change.text;
            }
        } else {
            self.client
                .show_message(
                    MessageType::ERROR,
                    format!("unknown document {}", uri.as_str()),
                )
                .await;
        }
    }
}

#[derive(Debug)]
struct Doc {
    uri: Url,
    text: String,
    version: i32,
    language_id: String,
}

impl From<TextDocumentItem> for Doc {
    fn from(value: TextDocumentItem) -> Self {
        Self {
            uri: value.uri,
            text: value.text,
            version: value.version,
            language_id: value.language_id,
        }
    }
}

impl Doc {
    pub fn pos_at(&self, offset: usize) -> (usize, usize) {
        let (mut line, mut pos) = (0, 0);

        for (_, c) in self
            .text
            .chars()
            .enumerate()
            .take_while(|(i, _)| *i < offset)
        {
            if c == '\n' {
                pos = 0;
                line += 1;
            } else {
                pos += 1;
            }
        }

        (line, pos)
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: LEGEND.into(),
                                token_modifiers: vec![],
                            },
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            ..Default::default()
                        },
                    ),
                ),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: env!("CARGO_PKG_NAME").into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::WARNING, "server initialized!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.add_doc(params.text_document);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.update_doc(params.text_document.uri, params.content_changes)
            .await;
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let doc = self.docs.get(&uri).unwrap();

        let mut last_line = 0;
        let mut last_pos = 0;

        let data = Lexer::new(&doc.text)
            .filter_map(|x| {
                let token_type = semantic_token_type_index(x.kind.to_semantic()?);
                let (line, pos) = doc.pos_at(x.offset);

                let delta_line = (line - last_line) as _;
                let delta_start = if delta_line == 0 { pos - last_pos } else { pos } as _;

                last_line = line;
                last_pos = pos;

                Some(SemanticToken {
                    delta_line,
                    delta_start,
                    length: x.len() as _,
                    token_type,
                    token_modifiers_bitset: 0,
                })
            })
            .collect();

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            data,
            ..Default::default()
        })))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
