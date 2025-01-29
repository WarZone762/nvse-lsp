use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionList, CompletionResponse, Position,
};

use crate::{
    ast::AstNode,
    db::{Database, Lookup},
    doc::Doc,
    hir::{
        ty::{Symbol, Type},
        HirNode,
    },
    syntax_node::TokenKind,
};

impl Doc {
    pub fn completion(&self, db: &Database, pos: Position) -> Option<CompletionResponse> {
        let root = self.hir(db);
        let offset = self.offset_at(db, pos);
        let token = root.node.syntax().nearest_token(offset)?;
        let hir_node = db.syntax_to_hir(**self, token.parent()?)?;

        let mut items = vec![];

        let offset = offset.saturating_sub(1);
        let prev_token = root.node.syntax().token_at_offset(offset);
        let prev_text = prev_token
            .filter(|x| x.kind == TokenKind::IDENT)
            .map(|x| &x.text(self.text(db))[..(offset - x.offset + 1) as usize]);

        for (name, sym) in self.collect_visible_symbols(db, &hir_node, prev_text) {
            let (ty, detail) = match sym {
                Symbol::Local(file_id, x) => {
                    let ty = x.type_(db, file_id);
                    let detail = ty.to_string_with_name(&name, 0);
                    (ty, detail)
                }
                Symbol::Global(gdb, name) => {
                    let ty = gdb.lookup(db).globals.get(&name).unwrap().clone();
                    let detail =
                        format!("{}\n\nFrom: {}", ty.to_string_with_name(&name, 0), db[gdb].name);
                    (ty, detail)
                }
            };
            items.push(CompletionItem {
                kind: Some(match ty {
                    Type::Function(_) => CompletionItemKind::FUNCTION,
                    Type::Record(_) => CompletionItemKind::STRUCT,
                    Type::Form(_) => CompletionItemKind::CONSTANT,
                    _ => CompletionItemKind::VARIABLE,
                }),
                detail: Some(detail),
                label: name,
                ..Default::default()
            })
        }

        Some(CompletionResponse::List(CompletionList { is_incomplete: false, items }))
    }

    fn collect_visible_symbols(
        &self,
        db: &Database,
        hir_node: &HirNode,
        starts_with: Option<&str>,
    ) -> Vec<(String, Symbol)> {
        let script_db = self.script_db(db);

        let mut symbols = vec![];

        match hir_node {
            HirNode::Script(x) => symbols.extend(x.hir(db).sym_table.map.clone()),
            HirNode::Block(x) => symbols.extend(x.lookup(script_db).sym_table.map.clone()),
            _ => (),
        }

        if let Some(ast_node) = hir_node.node(db, script_db) {
            for ancestor in ast_node.syntax().clone().ancestors() {
                match db.syntax_to_hir(**self, ancestor) {
                    Some(HirNode::Script(x)) => symbols.extend(x.hir(db).sym_table.map.clone()),
                    Some(HirNode::Block(x)) => {
                        symbols.extend(x.lookup(script_db).sym_table.map.clone())
                    }
                    _ => (),
                }
            }
        }

        symbols.extend(
            db.globals
                .iter()
                .filter(|x| x.0.starts_with(starts_with.unwrap_or("")))
                .map(|(k, v)| (k.clone(), v.clone())),
        );

        symbols
    }
}
