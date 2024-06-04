use tower_lsp::lsp_types::*;

use crate::{
    ast::AstNode,
    db::{Database, Lookup},
    doc::Doc,
    hir::{ty::Symbol, Expr, HirNode},
};

impl Doc {
    pub fn goto_def(&self, db: &Database, pos: Position) -> Option<GotoDefinitionResponse> {
        let node = db.syntax_to_hir(
            **self,
            self.hir(db).node.syntax().token_at_offset(self.offset_at(db, pos))?.parent()?,
        )?;

        let var_decl = match self.resolve(db, node)? {
            Symbol::Local(x) => x,
            Symbol::Global(_) => return None,
        };
        let script_db = self.script_db(db);
        let def_node = var_decl.lookup(script_db).name.lookup(script_db).node.syntax();

        let start = self.pos_at(db, def_node.offset);
        let end = self.pos_at(db, def_node.end());
        let res = vec![Location { uri: self.meta(db).uri.clone(), range: Range::new(start, end) }];

        Some(GotoDefinitionResponse::Array(res))
    }

    pub fn refs(&self, db: &Database, pos: Position, incl_def: bool) -> Option<Vec<Location>> {
        let node = db.syntax_to_hir(
            **self,
            self.hir(db).node.syntax().token_at_offset(self.offset_at(db, pos))?.parent()?,
        )?;

        let var_decl = match self.resolve(db, node)? {
            Symbol::Local(x) => x,
            Symbol::Global(_) => return None,
        };

        let script_db = self.script_db(db);
        let uri = self.meta(db).uri.clone();
        let mut res = vec![];

        for e in HirNode::Script(**self).dfs(db, script_db) {
            if let HirNode::VarDecl(e) = e
                && e == var_decl
            {
                if incl_def {
                    let name = e.lookup(script_db).name.lookup(script_db).node.syntax();
                    let start = self.pos_at(db, name.offset);
                    let end = self.pos_at(db, name.end());
                    res.push(Location { uri: uri.clone(), range: Range::new(start, end) });
                }
                continue;
            }

            if let Some(Symbol::Local(def)) = self.resolve(db, e)
                && def == var_decl
            {
                let name_ref = match e {
                    HirNode::Expr(x) => match x.lookup(script_db) {
                        Expr::NameRef(x) => x,
                        _ => continue,
                    },
                    _ => continue,
                }
                .node
                .syntax();

                let start = self.pos_at(db, name_ref.offset);
                let end = self.pos_at(db, name_ref.end());
                res.push(Location { uri: uri.clone(), range: Range::new(start, end) });
            }
        }

        Some(res)
    }
}
