#![feature(let_chains, coroutines, iter_from_coroutine, get_mut_unchecked, debug_closure_helpers)]

mod ast;
mod doc;
mod features;
mod hir;
mod lexer;
mod parser;
mod syntax_node;
mod tree_builder;

use std::borrow::Cow;

use ast::AstNode;
use dashmap::DashMap;
use doc::Doc;
use features::*;
use hir::Workspace;
use serde::{Deserialize, Serialize};
use tokio::sync::RwLock;
use tower_lsp::{
    jsonrpc::{self, Result},
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
    workspace: RwLock<Workspace>,
}

unsafe impl Send for Workspace {}
unsafe impl Sync for Workspace {}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self { client, docs: DashMap::new(), workspace: Workspace::new().into() }
    }

    pub async fn add_doc(&self, doc: TextDocumentItem) {
        let doc = doc.into();
        self.publish_diagnostics(&doc).await;
        self.client.send_notification::<AstNotification>(AstNotificationParams::new(&doc)).await;
        self.workspace.write().await.add_doc(&doc);
        self.docs.insert(doc.uri.clone(), doc);
    }

    pub async fn update_doc(&self, uri: Url, changes: Vec<TextDocumentContentChangeEvent>) {
        if let Some(mut doc) = self.docs.get_mut(&uri) {
            for change in changes {
                doc.update_text(change.text);
            }
            self.publish_diagnostics(&doc).await;
            self.client
                .send_notification::<AstNotification>(AstNotificationParams::new(&doc))
                .await;
            self.workspace.write().await.add_doc(&doc);
        } else {
            self.client
                .show_message(MessageType::ERROR, format!("unknown document {}", uri.as_str()))
                .await;
        }
    }

    pub async fn publish_diagnostics(&self, doc: &Doc) {
        self.client.publish_diagnostics(doc.uri.clone(), doc.diagnostics().collect(), None).await;
    }

    pub fn get_doc(&self, uri: &Url) -> Result<dashmap::mapref::one::Ref<Url, Doc>> {
        self.docs.get(uri).ok_or_else(|| request_failed(format!("failed to get document {uri}")))
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
                hover_provider: Some(HoverProviderCapability::Simple(true)),
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

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let pos_params = params.text_document_position_params;
        let doc = self.get_doc(&pos_params.text_document.uri)?;

        Ok(doc.hover(pos_params.position))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let doc = self.get_doc(&params.text_document.uri)?;

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            data: doc.semantic_tokens(),
            ..Default::default()
        })))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

fn request_failed(msg: impl Into<Cow<'static, str>>) -> jsonrpc::Error {
    jsonrpc::Error {
        code: jsonrpc::ErrorCode::ServerError(lsp_types::error_codes::REQUEST_FAILED),
        message: msg.into(),
        data: None,
    }
}

struct AstNotification;

impl Notification for AstNotification {
    type Params = AstNotificationParams;

    const METHOD: &'static str = "geckscript-nvse/ast";
}

#[derive(Serialize, Deserialize)]
struct AstNotificationParams {
    uri: Url,
    ast: String,
}

impl AstNotificationParams {
    pub fn new(doc: &Doc) -> Self {
        Self { uri: doc.uri.clone(), ast: doc.tree.syntax().tree_string(&doc.text) }
    }
}
