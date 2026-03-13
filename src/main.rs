#![feature(
    coroutines,
    debug_closure_helpers,
    get_mut_unchecked,
    iter_from_coroutine,
    iter_map_windows
)]

mod ast;
mod backend;
mod db;
mod doc;
mod features;
mod game_data;
mod hir;
mod lexer;
mod parser;
mod syntax_node;
mod tree_builder;

use std::{fs, path::PathBuf};

use clap::{Parser, Subcommand};
use doc::Doc;
use features::*;
use tower_lsp::{LspService, Server};

use crate::backend::Backend;

#[derive(Parser)]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    /// Genarate AST for a file
    GenerateAst {
        /// Input file path
        file: PathBuf,
        /// Output file path
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
}

#[tokio::main]
async fn main() {
    let args = Args::parse();
    if let Some(Command::GenerateAst { file, output }) = args.command {
        let text = fs::read_to_string(&file).expect("failed to read input file");
        let out = tree_builder::generate_ast(&text);
        let out_file = output.unwrap_or_else(|| PathBuf::from(format!("{}.ast", file.display())));
        if let Some(parent) = out_file.parent() {
            fs::create_dir_all(parent).expect("failed to create parent directories");
        }

        fs::write(out_file, out).expect("failed to write to the output file");

        return;
    }

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
