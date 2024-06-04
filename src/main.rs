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

use std::{
    borrow::Cow,
    fs,
    path::{Path, PathBuf},
    sync::Arc,
    time,
};

use ast::AstNode;
use db::Database;
use doc::Doc;
use features::*;
use serde::{Deserialize, Serialize};
use tokio::sync::RwLock;
use tower_lsp::{
    jsonrpc::{self, Result},
    lsp_types::{
        notification::{Notification, Progress},
        *,
    },
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
    root_path: Arc<RwLock<Option<PathBuf>>>,
    db: Arc<RwLock<Database>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self { client, root_path: Arc::new(None.into()), db: Arc::new(Database::new().into()) }
    }

    pub async fn load_folder(&self, path: impl AsRef<Path>) {
        let mut db = self.db.write().await;
        let files = walkdir::WalkDir::new(path)
            .into_iter()
            .filter_map(|x| x.ok())
            .filter(|x| {
                let path = x.path();
                path.is_file()
                    && path.extension().is_some_and(|x| {
                        let bytes = x.as_encoded_bytes();
                        bytes.ends_with(b"gek")
                            || bytes.ends_with(b"geck")
                            || bytes.ends_with(b"txt")
                    })
            })
            .collect::<Vec<_>>();
        let total = files.len();
        let start = time::Instant::now();
        let progress = ProgressReporter::start(
            &self.client,
            "loadWorkspace".into(),
            "Loading Workspace".into(),
            Some(format!("0 / {total}",)),
            Some(0),
        )
        .await;
        for (i, e) in files.iter().enumerate() {
            let path = e.path();

            progress
                .report(
                    Some(format!("{}: {i} / {total}", path.display())),
                    Some((i * 100 / total) as u32),
                )
                .await;

            match fs::read_to_string(path) {
                Ok(text) => {
                    if text.starts_with("name")
                        || text
                            .get(0.."scriptname".len())
                            .is_some_and(|x| x.to_lowercase() == "scriptname")
                        || text.get(0.."scn".len()).is_some_and(|x| x.to_lowercase() == "scn")
                    {
                        db.add_doc(TextDocumentItem {
                            uri: Url::from_file_path(path).unwrap(),
                            language_id: "NVSEScript".into(),
                            version: 0,
                            text,
                        });
                    }
                }
                Err(err) => {
                    self.client
                        .log_message(
                            MessageType::ERROR,
                            format!("failed to load '{}': {err}", path.display()),
                        )
                        .await;
                }
            }
        }

        progress
            .end(Some(format!("Finished loading workspace {} ms", start.elapsed().as_millis())))
            .await;

        for (doc, _) in db.doc_metas.iter() {
            self.publish_diagnostics(&db, Doc(doc.into())).await;
        }

        let start = time::Instant::now();
        let total = db.doc_metas.len();
        let progress = ProgressReporter::start(
            &self.client,
            "analyzeWorkspace".into(),
            "Analyzing Workspace".into(),
            Some(format!("0 / {total}")),
            Some(0),
        )
        .await;

        for (i, _) in db.analyze_workspace().enumerate() {
            progress.report(Some(format!("{i} / {total}")), Some((i * 100 / total) as _)).await;
        }

        progress
            .end(Some(format!("Finished analyzing workspace {} ms", start.elapsed().as_millis())))
            .await;
    }

    pub async fn add_doc(&self, doc: TextDocumentItem) {
        let mut db = self.db.write().await;
        let doc = db.add_doc(doc);
        self.publish_diagnostics(&db, doc).await;
        for _ in db.analyze_workspace() {}
        #[cfg(debug_assertions)]
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
        for _ in db.analyze_workspace() {}
        #[cfg(debug_assertions)]
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
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        *self.root_path.write().await = params.root_uri.and_then(|x| x.to_file_path().ok());

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                definition_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
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
        if let Some(root_path) = self.root_path.read().await.as_ref() {
            self.load_folder(root_path).await;
        }

        self.client.log_message(MessageType::INFO, "NVSE LSP initialized").await;
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

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let db = self.db.read().await;
        let pos_params = params.text_document_position_params;
        let doc = Self::doc(&db, &pos_params.text_document.uri)?;

        Ok(doc.goto_def(&db, pos_params.position))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let db = self.db.read().await;
        let doc = Self::doc(&db, &params.text_document_position.text_document.uri)?;

        Ok(doc.refs(
            &db,
            params.text_document_position.position,
            params.context.include_declaration,
        ))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let db = self.db.read().await;
        let doc = Self::doc(&db, &params.text_document_position.text_document.uri)?;

        Ok(doc.rename(&db, params.text_document_position.position, params.new_name))
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

#[derive(Debug)]
struct ProgressReporter<'a> {
    token: String,
    client: &'a Client,
}

impl<'a> ProgressReporter<'a> {
    pub async fn start(
        client: &'a Client,
        token: String,
        title: String,
        message: Option<String>,
        percentage: Option<u32>,
    ) -> Self {
        client
            .send_notification::<Progress>(ProgressParams {
                token: NumberOrString::String(token.clone()),
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(
                    WorkDoneProgressBegin { title, cancellable: Some(false), message, percentage },
                )),
            })
            .await;
        Self { token, client }
    }

    pub async fn report(&self, message: Option<String>, percentage: Option<u32>) {
        self.client
            .send_notification::<Progress>(ProgressParams {
                token: NumberOrString::String(self.token.clone()),
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::Report(
                    WorkDoneProgressReport { cancellable: Some(false), message, percentage },
                )),
            })
            .await;
    }

    pub async fn end(self, message: Option<String>) {
        self.client
            .send_notification::<Progress>(ProgressParams {
                token: NumberOrString::String(self.token),
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(WorkDoneProgressEnd {
                    message,
                })),
            })
            .await;
    }
}

struct AstNotification;

impl Notification for AstNotification {
    type Params = AstNotificationParams;

    const METHOD: &'static str = "nvse-lsp/ast";
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
