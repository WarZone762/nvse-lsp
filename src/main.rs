#![feature(let_chains, coroutines, iter_from_coroutine, get_mut_unchecked, debug_closure_helpers)]

mod ast;
mod db;
mod doc;
mod features;
mod hir;
mod lexer;
mod parser;
mod syntax_node;
mod tree_builder;

use std::borrow::Cow;

use ast::AstNode;
use db::Database;
use doc::Doc;
use features::*;
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
    db: RwLock<Database>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self { client, db: Database::new().into() }
    }

    pub async fn add_doc(&self, doc: TextDocumentItem) {
        let mut db = self.db.write().await;
        let doc = db.add_doc(doc);
        self.publish_diagnostics(&db, doc).await;
        self.client
            .send_notification::<AstNotification>(AstNotificationParams::new(&db, doc))
            .await;
    }

    pub async fn update_doc(
        &self,
        uri: Url,
        changes: Vec<TextDocumentContentChangeEvent>,
    ) -> Result<()> {
        let mut db = self.db.write().await;

        let doc = Self::doc(&db, &uri)?;
        for change in changes {
            db.update_doc_text(*doc, change.text)
        }
        self.publish_diagnostics(&db, doc).await;
        self.client
            .send_notification::<AstNotification>(AstNotificationParams::new(&db, doc))
            .await;
        Ok(())
    }

    pub async fn publish_diagnostics(&self, db: &Database, doc: Doc) {
        self.client
            .publish_diagnostics(doc.meta(db).uri.clone(), doc.diagnostics(db).collect(), None)
            .await;
    }

    pub fn doc(db: &Database, uri: &Url) -> Result<Doc> {
        db.doc(uri).ok_or_else(|| request_failed(format!("failed to get document {uri}")))
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
        if let Err(err) = self.update_doc(params.text_document.uri, params.content_changes).await {
            self.client.show_message(MessageType::ERROR, err.message).await;
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let db = self.db.read().await;
        let pos_params = params.text_document_position_params;
        let doc = Self::doc(&db, &pos_params.text_document.uri)?;

        Ok(doc.hover(&db, pos_params.position))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let db = self.db.read().await;
        let doc = Self::doc(&db, &params.text_document.uri)?;

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            data: doc.semantic_tokens(&db),
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
    pub fn new(db: &Database, doc: Doc) -> Self {
        Self {
            uri: doc.meta(db).uri.clone(),
            ast: doc.hir(db).node.syntax().tree_string(doc.text(db)),
        }
    }
}
