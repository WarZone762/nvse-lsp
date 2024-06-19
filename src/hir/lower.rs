use db::{Database, FileId};

use super::*;
use crate::{
    ast,
    syntax_node::{Token, TokenKind},
};

pub(crate) fn lower(db: &mut db::Database, file_id: db::FileId, script: ast::Script) {
    let mut ctx = LowerCtx::new(db, file_id);
    let script = ctx.script(script);
    let script_db = ctx.finish();
    if let Some(name) = &script.name {
        db.globals.insert(name.lookup(&script_db).name.clone(), Symbol::Local(file_id, *name));
    }
    db.hir_map.insert(file_id.0, script);
    db.script_db_map.insert(file_id.0, script_db);
}

#[derive(Debug)]
struct LowerCtx<'a> {
    db: &'a Database,
    file_id: FileId,
    script_db: ScriptDatabase,
    sym_tbl_stack: Vec<SymbolTable>,
}

impl<'a> LowerCtx<'a> {
    fn new(db: &'a Database, file_id: FileId) -> Self {
        Self { db, file_id, script_db: ScriptDatabase::new(), sym_tbl_stack: vec![] }
    }

    fn finish(self) -> ScriptDatabase {
        self.script_db
    }

    fn script(&mut self, node: ast::Script) -> Script {
        self.push_sym_tbl();
        Script {
            name: self.name(node.name()),
            items: self.items(node.items()),
            sym_table: self.pop_sym_tbl(),
            node,
        }
    }

    fn items(&mut self, items: ast::AstChildren<ast::Item>) -> Vec<ItemId> {
        items.filter_map(|x| self.item(x)).collect()
    }

    fn item(&mut self, node: ast::Item) -> Option<ItemId> {
        let item = match node {
            ast::Item::FnDecl(x) => self.fn_decl(x)?.into(),
            ast::Item::BlockType(x) => self.block_type(x)?.into(),
            ast::Item::VarDecl(x) => self.stmt_var_decl(x)?.into(),
        };
        Some(self.script_db.add_item(item))
    }

    fn fn_decl(&mut self, node: ast::FnDeclItem) -> Option<FnDeclItem> {
        Some(FnDeclItem {
            name: self.name(node.name()),
            params: node
                .param_list()
                .as_ref()?
                .params()
                .filter_map(|x| self.var_decl(Some(x)))
                .collect(),
            block: self.block(node.block())?,
            node,
        })
    }

    fn block_type(&mut self, node: ast::BlockTypeItem) -> Option<BlockTypeItem> {
        Some(BlockTypeItem { blocktype_kind: 0, block: self.block(node.block())?, node })
    }

    fn stmt(&mut self, node: ast::Stmt) -> Option<StmtId> {
        let stmt = match node {
            ast::Stmt::Block(x) => self.block(Some(x))?.into(),
            ast::Stmt::VarDecl(x) => self.stmt_var_decl(x)?.into(),
            ast::Stmt::Expr(x) => self.stmt_expr(x)?.into(),
            ast::Stmt::For(x) => self.stmt_for(x)?.into(),
            ast::Stmt::ForEach(x) => self.stmt_for_range(x)?.into(),
            ast::Stmt::If(x) => self.stmt_if(x)?.into(),
            ast::Stmt::While(x) => self.stmt_while(x)?.into(),
            ast::Stmt::Return(x) => self.stmt_return(x).into(),
            ast::Stmt::Break(x) => self.stmt_break(x).into(),
            ast::Stmt::Continue(x) => self.stmt_contiue(x).into(),
            ast::Stmt::Empty(_) => return None,
        };
        Some(self.script_db.add_stmt(stmt))
    }

    fn stmt_var_decl(&mut self, node: ast::VarDeclStmt) -> Option<VarDeclStmt> {
        Some(VarDeclStmt {
            export: node.export().is_some(),
            decl: self.var_decl(node.var_decl())?,
            node,
        })
    }

    fn stmt_expr(&mut self, node: ast::ExprStmt) -> Option<ExprStmt> {
        Some(ExprStmt { expr: self.expr(node.expr()), node })
    }

    fn stmt_for(&mut self, node: ast::ForStmt) -> Option<ForStmt> {
        Some(ForStmt {
            init: node.init().and_then(|x| self.var_decl(Some(x))),
            cond: node.cond().map(|x| self.expr(Some(x))),
            loop_expr: node.loop_expr().map(|x| self.expr(Some(x))),
            block: self.block(node.block())?,
            node,
        })
    }

    fn stmt_for_range(&mut self, node: ast::ForRangeStmt) -> Option<ForRangeStmt> {
        Some(ForRangeStmt {
            pat: self.pat(node.pat())?,
            iterable: self.expr(node.iterable()),
            block: self.block(node.block())?,
            node,
        })
    }

    fn stmt_if(&mut self, node: ast::IfStmt) -> Option<IfStmt> {
        Some(IfStmt {
            cond: self.expr(node.cond()),
            true_branch: self.block(node.true_branch())?,
            false_branch: match node.false_branch() {
                Some(ast::ElseBranch::Block(x)) => self.block(Some(x)).map(ElseBranch::Block),
                Some(ast::ElseBranch::IfStmt(x)) => self.stmt(ast::Stmt::If(x)).map(ElseBranch::If),
                None => None,
            },
            node,
        })
    }

    fn stmt_while(&mut self, node: ast::WhileStmt) -> Option<WhileStmt> {
        Some(WhileStmt { cond: self.expr(node.cond()), block: self.block(node.block())?, node })
    }

    fn stmt_return(&mut self, node: ast::ReturnStmt) -> ReturnStmt {
        ReturnStmt { expr: node.expr().map(|x| self.expr(Some(x))), node }
    }

    fn stmt_break(&mut self, node: ast::BreakStmt) -> BreakStmt {
        BreakStmt { node }
    }

    fn stmt_contiue(&mut self, node: ast::ContinueStmt) -> ContinueStmt {
        ContinueStmt { node }
    }

    fn expr(&mut self, node: Option<ast::Expr>) -> ExprId {
        let expr = node
            .and_then(|x| {
                Some(match x {
                    ast::Expr::Binary(x) => self.expr_bin(x).into(),
                    ast::Expr::Ternary(x) => self.expr_ternary(x).into(),
                    ast::Expr::Unary(x) => self.expr_unary(x).into(),
                    ast::Expr::Postfix(x) => self.expr_postfix(x).into(),
                    ast::Expr::Field(x) => self.expr_field(x).into(),
                    ast::Expr::Subscript(x) => self.expr_subscript(x).into(),
                    ast::Expr::Call(x) => self.expr_call(x)?.into(),
                    ast::Expr::Paren(x) => self.expr_paren(x).into(),
                    ast::Expr::Lambda(x) => self.expr_lambda(x)?.into(),
                    ast::Expr::NameRef(x) => self.name_ref(x)?.into(),
                    ast::Expr::Str(x) => self.str(x).into(),
                    ast::Expr::Literal(x) => self.literal(x)?.into(),
                })
            })
            .unwrap_or(Expr::Missing);
        self.script_db.add_expr(expr)
    }

    fn expr_bin(&mut self, node: ast::BinExpr) -> BinExpr {
        BinExpr {
            lhs: self.expr(node.lhs()),
            op: node.op().map(|x| BinOpKind::from_token(x.kind)).unwrap_or_default(),
            rhs: self.expr(node.rhs()),
            node,
        }
    }

    fn expr_ternary(&mut self, node: ast::TernaryExpr) -> TernaryExpr {
        TernaryExpr {
            cond: self.expr(node.cond()),
            true_expr: self.expr(node.true_expr()),
            false_expr: self.expr(node.false_expr()),
            node,
        }
    }

    fn expr_unary(&mut self, node: ast::UnaryExpr) -> UnaryExpr {
        UnaryExpr {
            op: node.op().map(|x| UnaryOpKind::from_token(x.kind)).unwrap_or_default(),
            operand: self.expr(node.operand()),
            node,
        }
    }

    fn expr_postfix(&mut self, node: ast::PostfixExpr) -> PostfixExpr {
        PostfixExpr {
            operand: self.expr(node.operand()),
            op: node.op().map(|x| PostfixOpKind::from_token(x.kind)).unwrap_or_default(),
            node,
        }
    }

    fn expr_field(&mut self, node: ast::FieldExpr) -> FieldExpr {
        FieldExpr {
            lhs: self.expr(node.lhs()),
            field: self.expr(node.field().map(ast::Expr::NameRef)),
            node,
        }
    }

    fn expr_subscript(&mut self, node: ast::SubscriptExpr) -> SubscriptExpr {
        SubscriptExpr { lhs: self.expr(node.lhs()), subscript: self.expr(node.subscript()), node }
    }

    fn expr_call(&mut self, node: ast::CallExpr) -> Option<CallExpr> {
        Some(CallExpr {
            lhs: self.expr(node.lhs()),
            args: node.args().as_ref()?.args().map(|x| self.expr(Some(x))).collect(),
            node,
        })
    }

    fn expr_paren(&mut self, node: ast::ParenExpr) -> ParenExpr {
        ParenExpr { expr: self.expr(node.expr()), node }
    }

    fn expr_lambda(&mut self, node: ast::LambdaExpr) -> Option<LambdaExpr> {
        Some(LambdaExpr {
            params: node
                .params()
                .as_ref()?
                .params()
                .filter_map(|x| self.var_decl(Some(x)))
                .collect(),
            block_or_expr: self.block_or_expr(node.block_or_expr())?,
            node,
        })
    }

    fn block_or_expr(&mut self, node: Option<ast::BlockOrExpr>) -> Option<BlockOrExpr> {
        Some(match node? {
            ast::BlockOrExpr::Block(x) => BlockOrExpr::Block(self.block(Some(x))?),
            ast::BlockOrExpr::Expr(x) => BlockOrExpr::Expr(self.expr(Some(x))),
        })
    }

    fn block(&mut self, node: Option<ast::BlockStmt>) -> Option<BlockId> {
        self.push_sym_tbl();
        let block = Block {
            stmts: node
                .as_ref()
                .into_iter()
                .flat_map(|x| x.stmts())
                .filter_map(|x| self.stmt(x))
                .collect(),
            sym_table: self.pop_sym_tbl(),
            node: node?,
        };
        Some(self.script_db.add_block(block))
    }

    fn pat(&mut self, node: Option<ast::Pat>) -> Option<Pat> {
        Some(match node? {
            ast::Pat::VarDecl(x) => Pat::VarDecl(self.var_decl(Some(x))?),
            ast::Pat::Arr(x) => {
                Pat::Arr(x.patts().map(|x| self.pat(Some(x))).collect::<Option<_>>()?)
            }
        })
    }

    fn var_decl(&mut self, node: Option<ast::VarDecl>) -> Option<VarDeclId> {
        let var_decl = VarDecl {
            decl_type: VarDeclType::from(node.as_ref()?.type_()?.kind),
            name: self.name(node.as_ref()?.name())?,
            init: node.as_ref()?.init().map(|x| self.expr(Some(x))),
            node: node?,
        };
        let name = var_decl.name.lookup(&self.script_db).name.clone();
        self.sym_tbl_stack
            .last_mut()
            .expect("LowerCtx::var_decl: symbol table stack empty")
            .map
            .insert(name, Symbol::Local(self.file_id, var_decl.name));

        Some(self.script_db.add_var_decl(var_decl))
    }

    fn name(&mut self, node: Option<ast::Name>) -> Option<NameId> {
        let name =
            Name { name: self.token_text(node.as_ref()?.ident().as_ref()?).into(), node: node? };
        Some(self.script_db.add_name(name))
    }

    fn name_ref(&mut self, node: ast::NameRef) -> Option<NameRef> {
        Some(NameRef { name: self.token_text(node.ident().as_ref()?).into(), node })
    }

    fn str(&mut self, node: ast::StrExpr) -> StrExpr {
        StrExpr { shards: node.shards().filter_map(|x| self.str_shard(x)).collect(), node }
    }

    fn str_shard(&mut self, node: ast::StrShard) -> Option<StrShardId> {
        let str_shard = match &node {
            ast::StrShard::Literal(x) => {
                StrShard::Str { val: self.token_text(x.token()?.as_ref()).into(), node }
            }
            ast::StrShard::Expr(x) => StrShard::Expr { expr: self.expr(x.expr()), node },
        };
        Some(self.script_db.add_str_shard(str_shard))
    }

    fn literal(&mut self, node: ast::Literal) -> Option<Literal> {
        let token = node.literal()?;
        Some(match token.kind {
            TokenKind::NUMBER => Literal::Number(NumberLiteral {
                value: self.token_text(&token).parse().ok()?,
                node,
            }),
            TokenKind::BOOL => {
                Literal::Bool(BoolLiteral { value: self.token_text(&token).parse().ok()?, node })
            }
            _ => unreachable!(),
        })
    }

    fn token_text(&mut self, token: &Token) -> &str {
        token.text(self.file_id.text(self.db))
    }

    fn push_sym_tbl(&mut self) {
        self.sym_tbl_stack.push(SymbolTable::new())
    }

    fn pop_sym_tbl(&mut self) -> SymbolTable {
        self.sym_tbl_stack.pop().expect("LowerCtx::pop_sym_tbl: symbol table stack empty")
    }
}
