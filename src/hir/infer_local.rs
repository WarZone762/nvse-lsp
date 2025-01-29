use std::collections::HashMap;

use super::*;
use crate::game_data::{Form, FormType, GlobalsDatabaseId};

pub(crate) fn infer(db: &mut Database, file_id: FileId) -> InferenceResult {
    let mut ctx = InferCtx::new(db, file_id);
    let mut env = Env::new();
    ctx.script(&mut env, file_id.hir(db));

    let InferCtx { file_id, name_map, expr_map, globals_map, .. } = ctx;

    InferenceResult { env, file_id, name_map, expr_map, globals_map }
}

#[derive(Debug)]
pub(crate) struct InferCtx<'a> {
    db: &'a Database,
    script_db: &'a ScriptDatabase,
    file_id: FileId,
    return_stack: Vec<TypeVarId>,
    name_map: HashMap<NameId, TypeVarId>,
    expr_map: HashMap<ExprId, TypeVarId>,
    globals_map: HashMap<String, (TypeVarId, GlobalsDatabaseId)>,
}

impl<'a> InferCtx<'a> {
    pub fn new(db: &'a Database, file_id: FileId) -> Self {
        Self {
            script_db: file_id.script_db(db),
            db,
            file_id,
            return_stack: vec![],
            name_map: HashMap::new(),
            expr_map: HashMap::new(),
            globals_map: HashMap::new(),
        }
    }

    fn script(&mut self, env: &mut Env, node: &Script) {
        if let Some(name) = node.name {
            self.name_map.insert(name, env.type_var(Type::Form(Form::Other(FormType::SCPT))));
            //     let tv = env.type_var();
            //     env.constraint_subtype(Type::Form(Form::Other(FormType::SCPT)),
            // tv.into());     self.name_map.insert(name, tv);
        }
        for item in &node.items {
            self.item(env, *item);
        }
    }

    fn item(&mut self, env: &mut Env, node: ItemId) {
        match node.lookup(self.script_db) {
            Item::FnDecl(x) => {
                self.return_stack.push(env.type_var(Type::Void));
                self.block(env, x.block);
                let _ret = self.return_stack.pop().expect("InferCtx::item: return stack empty");
            }
            Item::BlockType(x) => self.block(env, x.block),
            Item::VarDecl(x) => self.var_decl(env, x.decl),
        }
    }

    fn block(&mut self, env: &mut Env, node: BlockId) {
        for stmt in &node.lookup(self.script_db).stmts {
            self.stmt(env, *stmt);
        }
    }

    fn stmt(&mut self, env: &mut Env, node: StmtId) {
        match node.lookup(self.script_db) {
            Stmt::For(x) => self.stmt_for(env, x),
            Stmt::ForRange(x) => self.stmt_for_range(env, x),
            Stmt::If(x) => self.stmt_if(env, x),
            Stmt::While(x) => self.stmt_while(env, x),
            Stmt::VarDecl(x) => self.var_decl(env, x.decl),
            Stmt::Return(x) => {
                if let Some(x) = x.expr {
                    let ret = self.expr(env, x, None);
                    if let Some(&top) = self.return_stack.last() {
                        let union = top.union(ret, env);
                        env[top] = union;
                    }
                }
            }

            Stmt::Break(_) => (),
            Stmt::Continue(_) => (),
            Stmt::Block(x) => self.block(env, *x),
            Stmt::Expr(x) => {
                self.expr(env, x.expr, None);
            }
        }
    }

    fn stmt_for(&mut self, env: &mut Env, node: &ForStmt) {
        if let Some(init) = &node.init {
            self.var_decl(env, *init);
        }
        if let Some(cond) = &node.cond {
            self.expr(env, *cond, Some(Type::Bool));
        }
        if let Some(loop_expr) = &node.loop_expr {
            self.expr(env, *loop_expr, None);
        }
        self.block(env, node.block);
    }

    fn stmt_for_range(&mut self, env: &mut Env, node: &ForRangeStmt) {
        self.pat(env, &node.pat);
        self.expr(env, node.iterable, None);
        self.block(env, node.block);
    }

    fn stmt_if(&mut self, env: &mut Env, node: &IfStmt) {
        self.expr(env, node.cond, Some(Type::Bool));
        self.block(env, node.true_branch);
        match &node.false_branch {
            Some(ElseBranch::Block(x)) => self.block(env, *x),
            Some(ElseBranch::If(x)) => self.stmt(env, *x),
            None => (),
        }
    }

    fn stmt_while(&mut self, env: &mut Env, node: &WhileStmt) {
        self.expr(env, node.cond, Some(Type::Bool));
        self.block(env, node.block);
    }

    fn pat(&mut self, env: &mut Env, node: &Pat) {
        match node {
            Pat::VarDecl(x) => self.var_decl(env, *x),
            Pat::Arr(x) => {
                for pat in x {
                    self.pat(env, pat)
                }
            }
        }
    }

    fn var_decl(&mut self, env: &mut Env, node: VarDeclId) {
        let var_decl = node.lookup(self.script_db);
        let tv = env.type_var(Type::from_decl_type(var_decl.decl_type));
        if let Some(init) = &var_decl.init {
            self.expr(env, *init, Some(tv.into()));
        }
        self.name_map.insert(var_decl.name, tv);
    }

    fn expr(&mut self, env: &mut Env, node: ExprId, expect: Option<Type>) -> TypeVarId {
        let tv = match node.lookup(self.script_db) {
            Expr::Missing => env.type_var(Type::Void),
            Expr::Bin(x) => self.expr_bin(env, x, expect),
            Expr::Ternary(x) => self.expr_ternary(env, x, expect),
            Expr::Unary(x) => self.expr(env, x.operand, expect),
            Expr::Postfix(x) => self.expr(env, x.operand, expect),
            Expr::Field(x) => self.expr_field(env, x, expect),
            Expr::Subscript(x) => self.expr_subscript(env, x, expect),
            Expr::Call(x) => self.expr_call(env, x, expect),
            Expr::Paren(x) => self.expr(env, x.expr, expect),
            Expr::Lambda(x) => self.expr_lambda(env, x, expect),
            Expr::NameRef(x) => self.resolve(env, x, expect),
            Expr::Str(x) => {
                for shard in &x.shards {
                    match shard.lookup(self.script_db) {
                        StrShard::Str { .. } => (),
                        StrShard::Expr { expr, .. } => {
                            self.expr(env, *expr, expect.clone());
                        }
                    }
                }
                env.type_var(Type::String)
            }
            Expr::LitArr(x) => self.lit_arr(env, x, expect),
            Expr::LitMap(x) => self.lit_map(env, x, expect),
            Expr::Literal(x) => env.type_var(match x {
                Literal::Number(_) => Type::Number,
                Literal::Bool(_) => Type::Bool,
            }),
        };

        self.expr_map.insert(node, tv);
        tv
    }

    fn expr_bin(&mut self, env: &mut Env, node: &BinExpr, expect: Option<Type>) -> TypeVarId {
        match node.op {
            BinOpKind::PlusEq
            | BinOpKind::MinusEq
            | BinOpKind::AsteriskEq
            | BinOpKind::SlashEq
            | BinOpKind::PercentEq
            | BinOpKind::CircumflexEq
            | BinOpKind::VbarEq
            | BinOpKind::AmpersandEq
            | BinOpKind::Eq => {
                let lhs = self.expr(env, node.lhs, expect);
                let rhs = self.expr(env, node.rhs, Some(lhs.into()));

                env[lhs] = lhs.union(rhs, env);
                lhs
            }

            BinOpKind::Eq2
            | BinOpKind::Lt
            | BinOpKind::LtEq
            | BinOpKind::Gt
            | BinOpKind::GtEq
            | BinOpKind::ExclamationEq => env.type_var(Type::Bool),
            BinOpKind::Vbar2 | BinOpKind::Ampersand2 => {
                self.expr(env, node.lhs, Some(Type::Bool));
                self.expr(env, node.rhs, Some(Type::Bool));
                env.type_var(Type::Bool)
            }
            BinOpKind::Plus
            | BinOpKind::Minus
            | BinOpKind::Asterisk
            | BinOpKind::Slash
            | BinOpKind::Percent
            | BinOpKind::Circumflex
            | BinOpKind::Vbar
            | BinOpKind::Ampersand
            | BinOpKind::Lt2
            | BinOpKind::Gt2 => {
                let lhs = self.expr(env, node.lhs, Some(Type::Number));
                let rhs = self.expr(env, node.rhs, Some(Type::Number));
                env.type_var(lhs.union(rhs, env))
            }
            BinOpKind::Colon | BinOpKind::Colon2 | BinOpKind::Unknown => {
                let lhs = self.expr(env, node.lhs, expect.clone());
                let rhs = self.expr(env, node.rhs, expect);
                env.type_var(lhs.union(rhs, env))
            }
        }
    }

    fn expr_ternary(
        &mut self,
        env: &mut Env,
        node: &TernaryExpr,
        expect: Option<Type>,
    ) -> TypeVarId {
        let cond = self.expr(env, node.cond, Some(Type::Bool));

        let true_expr = self.expr(env, node.true_expr, expect.clone());
        let false_expr = self.expr(env, node.false_expr, expect);

        env.type_var(true_expr.union(false_expr, env))
    }

    fn expr_field(&mut self, env: &mut Env, node: &FieldExpr, expect: Option<Type>) -> TypeVarId {
        // let lhs = self.expr(node.lhs);
        // let field = match node.field.lookup(self.script_db) {
        //     Expr::NameRef(x) => x,
        //     _ => return env.type_var(),
        // };
        // let tv = self.expr(node.field);
        //
        // env.constraint_subtype(
        //     Type::Record(Record { fields: vec![(field.name.clone(), Type::Var(tv))]
        // }),     lhs.into(),
        // );
        //
        // tv
        self.expr(env, node.field, expect)
    }

    fn expr_subscript(
        &mut self,
        env: &mut Env,
        node: &SubscriptExpr,
        expect: Option<Type>,
    ) -> TypeVarId {
        let lhs = self.expr(
            env,
            node.lhs,
            Some(Type::Map(Box::new((Type::Empty, expect.clone().unwrap_or(Type::Empty))))),
        );
        let rhs = self.expr(env, node.subscript, None);

        env[lhs] =
            env[lhs].union(&Type::Map(Box::new((rhs.into(), expect.unwrap_or(Type::Empty)))));

        env.type_var(match &env[lhs] {
            Type::Map(x) => x.1.clone(),
            _ => Type::Empty,
        })
    }

    fn expr_call(&mut self, env: &mut Env, node: &CallExpr, expect: Option<Type>) -> TypeVarId {
        let lhs = self.expr(
            env,
            node.lhs,
            Some(Type::Function(Box::new(Function {
                generic_args: vec![],
                ret: expect.clone().unwrap_or(Type::Void),
                params: vec![Type::Empty; node.args.len()],
            }))),
        );
        let params = node.args.iter().map(|x| self.expr(env, *x, None)).map(Into::into).collect();
        env[lhs].clone().template_instantiate(env);

        env[lhs] = env[lhs].union(&Type::Function(Box::new(Function {
            generic_args: vec![],
            ret: expect.unwrap_or(Type::Void),
            params,
        })));

        env.type_var(match &env[lhs] {
            Type::Function(x) => x.ret.clone(),
            _ => Type::Empty,
        })
    }

    fn expr_lambda(&mut self, env: &mut Env, node: &LambdaExpr, expect: Option<Type>) -> TypeVarId {
        // let params = node
        //     .params
        //     .iter()
        //     .map(|x| {e
        //         self.var_decl(*x);
        //         Type::Var(
        //
        // self.name_map.get(&x.lookup(self.script_db).name).copied().unwrap_or_else(
        // || {                     let tv = env.type_var();
        //                     self.name_map.insert(x.lookup(self.script_db).name, tv);
        //                     tv
        //                 },
        //             ),
        //         )
        //     })
        //     .collect();
        // let ret = match node.block_or_expr {
        //     BlockOrExpr::Block(x) => {
        //         self.return_stack.push(env.type_var());
        //         self.block(x);
        //         self.return_stack.pop().expect("InferCtx::expr_lambda: return stack
        // is empty")     }
        //     BlockOrExpr::Expr(x) => self.expr(x),
        // };
        // let tv = env.type_var();
        // env.constraint_subtype(
        //     tv.into(),
        //     Type::Function(Box::new(FunctionType { ret: Type::Var(ret), params })),
        // );
        // tv
        env.type_var(Type::Any)
    }

    fn lit_arr(&mut self, env: &mut Env, node: &LitArr, expect: Option<Type>) -> TypeVarId {
        let ty = Type::Map(
            (
                Type::Number,
                node.exprs
                    .iter()
                    .map(|x| self.expr(env, *x, None))
                    .collect::<Vec<_>>()
                    .into_iter()
                    .fold(Type::Empty, |acc, e| acc.union(&env[e])),
            )
                .into(),
        );

        env.type_var(ty)
    }

    fn lit_map(&mut self, env: &mut Env, node: &LitMap, expect: Option<Type>) -> TypeVarId {
        let (k_ty, v_ty) = node
            .kv_pairs
            .iter()
            .map(|(k, v)| (self.expr(env, *k, None), self.expr(env, *v, None)))
            .collect::<Vec<_>>()
            .into_iter()
            .fold((Type::Empty, Type::Empty), |(acc_k, acc_v), (k, v)| {
                (acc_k.union(&env[k]), acc_v.union(&env[v]))
            });
        let ty = Type::Map((k_ty, v_ty).into());

        env.type_var(ty)
    }

    fn resolve(&mut self, env: &mut Env, name_ref: &NameRef, expect: Option<Type>) -> TypeVarId {
        match self.db.resolve(self.file_id, name_ref) {
            Some(Symbol::Local(_, name)) => self.name_map.get(name).copied().unwrap_or_else(|| {
                let tv = env.type_var(Type::Empty);
                self.name_map.insert(*name, tv);
                tv
            }),
            Some(Symbol::Global(gdb, name)) => {
                self.globals_map.get(&name_ref.name).copied().map(|x| x.0).unwrap_or_else(|| {
                    let ty = &self.db.globals_dbs[gdb.0].globals[name];
                    let tv = env.type_var(ty.clone());
                    self.globals_map.insert(name_ref.name.clone(), (tv, *gdb));
                    tv
                })
            }
            None => {
                self.globals_map.get(&name_ref.name).copied().map(|x| x.0).unwrap_or_else(|| {
                    let tv = env.type_var(Type::Empty);
                    let gdb = self.db.globals_db("Scriptrunner").unwrap();
                    self.globals_map.insert(name_ref.name.clone(), (tv, gdb));
                    tv
                })
            }
        }
    }
}

#[derive(Debug)]
pub(crate) struct InferenceResult {
    env: Env,
    file_id: FileId,
    name_map: HashMap<NameId, TypeVarId>,
    expr_map: HashMap<ExprId, TypeVarId>,
    globals_map: HashMap<String, (TypeVarId, GlobalsDatabaseId)>,
}

impl InferenceResult {
    pub fn apply(self, db: &mut Database) {
        let Self { env, file_id, name_map, expr_map, globals_map } = self;

        let ty_map = env.replace_type_vars();

        for (k, (tv, gdb)) in globals_map {
            db[gdb]
                .globals
                .entry(k.clone())
                .and_modify(|e| {
                    *e = e.union(&ty_map[*tv]);
                })
                .or_insert(ty_map[*tv].clone());

            db.globals.insert(k.clone(), Symbol::Global(gdb, k));
        }

        db.name_type_maps.insert(file_id.0, HashMap::new());
        for (k, v) in name_map {
            db.name_type_maps[file_id.0].insert(k, ty_map[*v].clone());
        }

        db.type_maps.insert(file_id.0, HashMap::new());
        for (k, v) in expr_map {
            db.type_maps[file_id.0].insert(k, ty_map[*v].clone());
        }
    }
}

#[cfg(test)]
mod test {
    use tower_lsp::lsp_types::{TextDocumentItem, Url};

    use super::*;
    use crate::hir::printer::{Print, Printer};

    #[test]
    fn a() {
        let mut db = Database::new();
        let doc = db.add_doc(TextDocumentItem {
            uri: Url::parse("file://test").unwrap(),
            language_id: "NVSEScript".into(),
            version: 0,
            // text: "name foo; fn main() {int bar = 123; int foo = a(bar, \"123\")}".into(),
            text: include_str!("../../test_data/cases/c6.gek").into(),
            // text: include_str!("../../test_data/cases/test.gek").into(),
        });

        for _ in db.analyze_workspace() {}

        println!("{}", doc.hir(&db).print_str(Printer::new(*doc, &db, doc.script_db(&db))));
    }
}
