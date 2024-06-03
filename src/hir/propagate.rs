use db::{Database, FileId};

use super::*;

pub(crate) fn propagate(db: &mut Database, file_id: FileId) {
    let ctx = PropagateCtx::new(db, file_id);
    let mut type_map = HashMap::new();
    ctx.script(&mut type_map, file_id.hir(db));
    db.type_maps.insert(file_id.0, type_map);
}

#[derive(Debug)]
pub(crate) struct PropagateCtx<'a> {
    db: &'a db::Database,
    script_db: &'a ScriptDatabase,
    file_id: db::FileId,
}

type TypeMap = HashMap<ExprId, Type>;

impl<'a> PropagateCtx<'a> {
    pub fn new(db: &'a Database, file_id: FileId) -> Self {
        Self { script_db: file_id.script_db(db), db, file_id }
    }

    fn script(&self, type_map: &mut TypeMap, node: &Script) {
        for item in &node.items {
            self.item(type_map, *item);
        }
    }

    fn item(&self, type_map: &mut TypeMap, node: ItemId) {
        match node.lookup(self.script_db) {
            Item::FnDecl(x) => {
                self.block(type_map, x.block);
            }
            Item::BlockType(x) => self.block(type_map, x.block),
            Item::VarDeclStmt(_) => (),
        }
    }

    fn block(&self, type_map: &mut TypeMap, node: BlockId) {
        for stmt in &node.lookup(self.script_db).stmts {
            self.stmt(type_map, *stmt);
        }
    }

    fn stmt(&self, type_map: &mut TypeMap, node: StmtId) {
        match node.lookup(self.script_db) {
            Stmt::For(x) => self.stmt_for(type_map, x),
            Stmt::ForEach(x) => self.stmt_for_each(type_map, x),
            Stmt::If(x) => self.stmt_if(type_map, x),
            Stmt::While(x) => self.stmt_while(type_map, x),
            Stmt::VarDecl(x) => self.var_decl(type_map, x.decl),
            Stmt::Return(x) => {
                if let Some(x) = x.expr {
                    self.expr(type_map, x);
                }
            }

            Stmt::Break(_) => (),
            Stmt::Continue(_) => (),
            Stmt::Block(x) => self.block(type_map, *x),
            Stmt::Expr(x) => {
                self.expr(type_map, x.expr);
            }
        }
    }

    fn stmt_for(&self, type_map: &mut TypeMap, node: &ForStmt) {
        if let Some(init) = &node.init {
            self.var_decl(type_map, *init);
        }
        if let Some(cond) = &node.cond {
            self.expr(type_map, *cond);
        }
        if let Some(loop_expr) = &node.loop_expr {
            self.expr(type_map, *loop_expr);
        }
        self.block(type_map, node.block);
    }

    fn stmt_for_each(&self, type_map: &mut TypeMap, node: &ForEachStmt) {
        self.var_decl(type_map, node.var_decl);
        self.expr(type_map, node.iterable);
        self.block(type_map, node.block);
    }

    fn stmt_if(&self, type_map: &mut TypeMap, node: &IfStmt) {
        self.expr(type_map, node.cond);
        self.block(type_map, node.true_branch);
        if let Some(false_branch) = &node.false_branch {
            self.stmt(type_map, *false_branch);
        }
        if let Some(else_branch) = &node.else_branch {
            self.block(type_map, *else_branch);
        }
    }

    fn stmt_while(&self, type_map: &mut TypeMap, node: &WhileStmt) {
        self.expr(type_map, node.cond);
        self.block(type_map, node.block);
    }

    fn var_decl(&self, type_map: &mut TypeMap, node: VarDeclId) {
        if let Some(init) = &node.lookup(self.script_db).init {
            self.expr(type_map, *init);
        }
    }

    fn expr(&self, type_map: &mut TypeMap, node: ExprId) -> Type {
        let type_ = match node.lookup(self.script_db) {
            Expr::Missing => Type::Ambiguous,
            Expr::Bin(x) => self.expr_bin(type_map, x),
            Expr::Ternary(x) => self.expr_ternary(type_map, x),
            Expr::Unary(x) => self.expr(type_map, x.operand),
            Expr::Subscript(x) => self.expr_subscript(type_map, x),
            Expr::Call(x) => self.expr_call(type_map, x),
            Expr::Paren(x) => self.expr(type_map, x.expr),
            Expr::Lambda(x) => self.expr_lambda(type_map, x),
            Expr::NameRef(x) => self
                .db
                .resolve(self.file_id, x)
                .map(|x| match x {
                    Symbol::Local(x) => x.lookup(self.script_db).decl_type.clone(),
                    Symbol::Global(x) => x.clone(),
                })
                .unwrap_or(Type::Ambiguous),
            Expr::Str(x) => {
                for shard in &x.shards {
                    match shard.lookup(self.script_db) {
                        StringShard::Str { .. } => (),
                        StringShard::Expr(x) => {
                            self.expr(type_map, *x);
                        }
                    }
                }
                Type::String
            }
            Expr::Lit(x) => match x {
                Literal::Number(_) => Type::Number,
                Literal::Bool(_) => Type::Bool,
            },
        };
        type_map.insert(node, type_.clone());
        type_
    }

    fn expr_bin(&self, type_map: &mut TypeMap, node: &BinExpr) -> Type {
        let lhs = self.expr(type_map, node.lhs);
        let rhs = self.expr(type_map, node.rhs);
        if lhs == rhs {
            lhs
        } else {
            // TODO: error reporting
            Type::Ambiguous
        }
    }

    fn expr_ternary(&self, type_map: &mut TypeMap, node: &TernaryExpr) -> Type {
        self.expr(type_map, node.cond);
        let true_expr = self.expr(type_map, node.true_expr);
        let false_expr = self.expr(type_map, node.false_expr);
        if true_expr == false_expr {
            true_expr
        } else {
            Type::Ambiguous
        }
    }

    fn expr_subscript(&self, type_map: &mut TypeMap, node: &SubscriptExpr) -> Type {
        self.expr(type_map, node.subscript);
        match self.expr(type_map, node.lhs) {
            Type::Array(x) => *x,
            _ => Type::Ambiguous,
        }
    }

    fn expr_call(&self, type_map: &mut TypeMap, node: &CallExpr) -> Type {
        for arg in &node.args {
            self.expr(type_map, *arg);
        }
        match node.lhs.type_(self.db, self.file_id) {
            Type::Function(x) => x.ret.clone().unwrap_or(Type::Ambiguous),
            _ => Type::Ambiguous,
        }
    }

    fn expr_lambda(&self, type_map: &mut TypeMap, node: &LambdaExpr) -> Type {
        let params =
            node.params.iter().map(|x| x.lookup(self.script_db).decl_type.clone()).collect();
        self.block(type_map, node.block);
        Type::Function(FunctionSignature { ret: None, params }.into())
    }
}
