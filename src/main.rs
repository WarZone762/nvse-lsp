#![feature(let_chains, get_mut_unchecked, debug_closure_helpers)]

mod doc;
mod features;
mod lexer;
mod node;
mod parser;
mod tree_builder;

use dashmap::DashMap;
use doc::Doc;
use features::*;
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{notification::Notification, *},
    Client, LanguageServer, LspService, Server,
};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[derive(Debug)]
struct Backend {
    client: Client,
    docs: DashMap<Url, Doc>,
}

struct AstNotification;

impl Notification for AstNotification {
    type Params = String;

    const METHOD: &'static str = "geckscript-nvse/ast";
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self { client, docs: DashMap::new() }
    }

    pub async fn add_doc(&self, doc: TextDocumentItem) {
        let doc = doc.into();
        self.publish_diagnostics(&doc).await;
        self.client.send_notification::<AstNotification>(doc.tree.tree_string(&doc.text)).await;
        self.docs.insert(doc.uri.clone(), doc);
    }

    pub async fn update_doc(&self, uri: Url, changes: Vec<TextDocumentContentChangeEvent>) {
        if let Some(mut doc) = self.docs.get_mut(&uri) {
            for change in changes {
                doc.update_text(change.text);
            }
            self.publish_diagnostics(&doc).await;
            self.client.send_notification::<AstNotification>(doc.tree.tree_string(&doc.text)).await;
        } else {
            self.client
                .show_message(MessageType::ERROR, format!("unknown document {}", uri.as_str()))
                .await;
        }
    }

    pub async fn publish_diagnostics(&self, doc: &Doc) {
        self.client.publish_diagnostics(doc.uri.clone(), doc.diagnostics().collect(), None).await;
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
                semantic_tokens_provider: Some(semantic_tokens::capabilities()),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: env!("CARGO_PKG_NAME").into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::WARNING, "server initialized!").await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.add_doc(params.text_document).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.update_doc(params.text_document.uri, params.content_changes).await;
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let doc = self.docs.get(&uri).unwrap();

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            data: doc.semantic_tokens(),
            ..Default::default()
        })))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}
