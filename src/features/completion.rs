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
};

impl Doc {
    pub fn completion(&self, db: &Database, pos: Position) -> Option<CompletionResponse> {
        let token = self.hir(db).node.syntax().nearest_token(self.offset_at(db, pos))?;
        let hir_node = db.syntax_to_hir(**self, token.parent()?)?;

        let mut items = vec![];

        for (name, sym) in self.collect_visible_symbols(db, &hir_node) {
            let ty = match sym {
                Symbol::Local(file_id, x) => x.type_(db, file_id).narrowest,
                Symbol::Global(x) => x.narrowest,
            };
            items.push(CompletionItem {
                kind: Some(match ty {
                    Type::Function(_) => CompletionItemKind::FUNCTION,
                    Type::Record(_) => CompletionItemKind::STRUCT,
                    _ => CompletionItemKind::VARIABLE,
                }),
                detail: Some(ty.to_string_with_name(&name, 0)),
                label: name,
                ..Default::default()
            })
        }

        Some(CompletionResponse::List(CompletionList { is_incomplete: false, items }))
    }

    fn collect_visible_symbols(&self, db: &Database, hir_node: &HirNode) -> Vec<(String, Symbol)> {
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

        symbols.extend(db.globals.clone());

        symbols
    }
}
