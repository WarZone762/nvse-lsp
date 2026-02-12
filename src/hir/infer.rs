use db::{Database, FileId};
use itertools::Itertools;
use la_arena::{Arena, Idx};

use super::*;
use crate::game_data::{Form, FormType, GlobalsDatabaseId};

pub(crate) fn infer(db: &mut Database, file_id: FileId) -> TypeVarStore {
    let mut ctx = InferCtx::new(db, file_id);
    let mut store = TypeVarStore::new();
    ctx.script(&mut store, file_id.hir(db));

    store
}

#[derive(Debug)]
pub(crate) struct InferCtx<'a> {
    db: &'a db::Database,
    script_db: &'a ScriptDatabase,
    file_id: db::FileId,
    return_stack: Vec<TypeVar>,
}

impl<'a> InferCtx<'a> {
    pub fn new(db: &'a Database, file_id: FileId) -> Self {
        Self { script_db: file_id.script_db(db), db, file_id, return_stack: vec![] }
    }

    fn script(&mut self, store: &mut TypeVarStore, node: &Script) {
        if let Some(name) = node.name {
            let tv = store.type_var();
            store
                .concrete_type(tv, InferredType::concrete(Type::Form(Form::Other(FormType::SCPT))));
            store.name_map.insert(name, tv);
        }
        for item in &node.items {
            self.item(store, *item);
        }
    }

    fn item(&mut self, store: &mut TypeVarStore, node: ItemId) {
        match node.lookup(self.script_db) {
            Item::FnDecl(x) => {
                self.return_stack.push(store.type_var());
                self.block(store, x.block);
                let _ret = self.return_stack.pop().expect("InferCtx::item: return stack empty");
            }
            Item::BlockType(x) => self.block(store, x.block),
            Item::VarDecl(x) => self.var_decl(store, x.decl),
        }
    }

    fn block(&mut self, store: &mut TypeVarStore, node: BlockId) {
        for stmt in &node.lookup(self.script_db).stmts {
            self.stmt(store, *stmt);
        }
    }

    fn stmt(&mut self, store: &mut TypeVarStore, node: StmtId) {
        match node.lookup(self.script_db) {
            Stmt::For(x) => self.stmt_for(store, x),
            Stmt::ForRange(x) => self.stmt_for_range(store, x),
            Stmt::If(x) => self.stmt_if(store, x),
            Stmt::While(x) => self.stmt_while(store, x),
            Stmt::VarDecl(x) => self.var_decl(store, x.decl),
            Stmt::Return(x) => {
                if let Some(x) = x.expr {
                    let ret = self.expr(store, x);
                    if let Some(top) = self.return_stack.last() {
                        store.assignable(ret, *top);
                    }
                } else if let Some(top) = self.return_stack.last() {
                    store.assignable_to_type(*top, Type::Void);
                }
            }

            Stmt::Break(_) => (),
            Stmt::Continue(_) => (),
            Stmt::Block(x) => self.block(store, *x),
            Stmt::Expr(x) => {
                self.expr(store, x.expr);
            }
        }
    }

    fn stmt_for(&mut self, store: &mut TypeVarStore, node: &ForStmt) {
        if let Some(init) = &node.init {
            self.var_decl(store, *init);
        }
        if let Some(cond) = &node.cond {
            let tv = self.expr(store, *cond);
            store.concrete_type(tv, InferredType::bool());
        }
        if let Some(loop_expr) = &node.loop_expr {
            self.expr(store, *loop_expr);
        }
        self.block(store, node.block);
    }

    fn stmt_for_range(&mut self, store: &mut TypeVarStore, node: &ForRangeStmt) {
        self.pat(store, &node.pat);
        self.expr(store, node.iterable);
        self.block(store, node.block);
    }

    fn stmt_if(&mut self, store: &mut TypeVarStore, node: &IfStmt) {
        let tv = self.expr(store, node.cond);
        store.concrete_type(tv, InferredType::bool());
        self.block(store, node.true_branch);
        match &node.false_branch {
            Some(ElseBranch::Block(x)) => self.block(store, *x),
            Some(ElseBranch::If(x)) => self.stmt(store, *x),
            None => (),
        }
    }

    fn stmt_while(&mut self, store: &mut TypeVarStore, node: &WhileStmt) {
        let tv = self.expr(store, node.cond);
        store.concrete_type(tv, InferredType::bool());
        self.block(store, node.block);
    }

    fn pat(&mut self, store: &mut TypeVarStore, node: &Pat) {
        match node {
            Pat::VarDecl(x) => self.var_decl(store, *x),
            Pat::Arr(x) => {
                for pat in x {
                    self.pat(store, pat)
                }
            }
        }
    }

    fn var_decl(&mut self, store: &mut TypeVarStore, node: VarDeclId) {
        let var_decl = node.lookup(self.script_db);
        let name_tv = store.name_map.get(&var_decl.name).copied().unwrap_or_else(|| {
            let tv = store.type_var();
            store.name_map.insert(var_decl.name, tv);
            tv
        });
        store.concrete_type(name_tv, InferredType::from_decl_type(var_decl.decl_type));
        if let Some(init) = &var_decl.init {
            let tv = self.expr(store, *init);
            store.assignable(name_tv, tv);
        }
    }

    fn expr(&mut self, store: &mut TypeVarStore, node: ExprId) -> TypeVar {
        let tv = match node.lookup(self.script_db) {
            Expr::Missing => store.type_var(),
            Expr::Bin(x) => self.expr_bin(store, x),
            Expr::Ternary(x) => self.expr_ternary(store, x),
            Expr::Unary(x) => self.expr(store, x.operand),
            Expr::Postfix(x) => self.expr(store, x.operand),
            Expr::Field(x) => self.expr_field(store, x),
            Expr::Subscript(x) => self.expr_subscript(store, x),
            Expr::Call(x) => self.expr_call(store, x),
            Expr::Paren(x) => self.expr(store, x.expr),
            Expr::Lambda(x) => self.expr_lambda(store, x),
            Expr::NameRef(x) => self.resolve(store, x),
            Expr::Str(x) => {
                for shard in &x.shards {
                    match shard.lookup(self.script_db) {
                        StrShard::Str { .. } => (),
                        StrShard::Expr { expr, .. } => {
                            self.expr(store, *expr);
                        }
                    }
                }
                let tv = store.type_var();
                store.concrete_type(tv, InferredType::string());
                tv
            }
            Expr::LitArr(x) => {
                let tv = store.type_var();
                let val = store.type_var();

                for e in &x.exprs {
                    let e = self.expr(store, *e);
                    store.assignable(val, e);
                }

                store.assignable_to_type(tv, Type::Map(Box::new((Type::Number, Type::Var(val)))));
                tv
            }
            Expr::LitMap(x) => {
                let tv = store.type_var();
                let key = store.type_var();
                let val = store.type_var();

                for (k, v) in &x.kv_pairs {
                    let k = self.expr(store, *k);
                    let v = self.expr(store, *v);

                    store.assignable(key, k);
                    store.assignable(val, v);
                }

                store.assignable_to_type(tv, Type::Map(Box::new((Type::Var(key), Type::Var(val)))));
                tv
            }
            Expr::Literal(x) => {
                let tv = store.type_var();
                store.concrete_type(tv, match x {
                    Literal::Number(_) => InferredType::number(),
                    Literal::Bool(_) => InferredType::bool(),
                });
                tv
            }
        };

        store.expr_map.insert(node, tv);
        tv
    }

    fn expr_bin(&mut self, store: &mut TypeVarStore, node: &BinExpr) -> TypeVar {
        let lhs = self.expr(store, node.lhs);
        let rhs = self.expr(store, node.rhs);
        store.assignable(lhs, rhs);
        let tv = store.type_var();
        store.assignable(tv, lhs);
        store.assignable(tv, rhs);
        tv
    }

    fn expr_ternary(&mut self, store: &mut TypeVarStore, node: &TernaryExpr) -> TypeVar {
        let cond = self.expr(store, node.cond);
        store.concrete_type(cond, InferredType::bool());
        let true_expr = self.expr(store, node.true_expr);
        let false_expr = self.expr(store, node.false_expr);
        store.assignable(true_expr, false_expr);
        let tv = store.type_var();
        store.assignable(tv, true_expr);
        store.assignable(tv, false_expr);
        tv
    }

    fn expr_field(&mut self, store: &mut TypeVarStore, node: &FieldExpr) -> TypeVar {
        let lhs = self.expr(store, node.lhs);
        let field = match node.field.lookup(self.script_db) {
            Expr::NameRef(x) => x,
            _ => return store.type_var(),
        };
        let tv = self.expr(store, node.field);

        store.assignable_to_type(
            lhs,
            Type::Record(Record { fields: vec![(field.name.clone(), Type::Var(tv))] }),
        );

        tv
    }

    fn expr_subscript(&mut self, store: &mut TypeVarStore, node: &SubscriptExpr) -> TypeVar {
        let lhs = self.expr(store, node.lhs);
        let subscript = self.expr(store, node.subscript);
        let tv = store.type_var();
        store.assignable(subscript, tv);
        let value = store.type_var();
        store.assignable_to_type(lhs, Type::Map(Box::new((Type::Var(tv), Type::Var(value)))));
        value
    }

    fn expr_call(&mut self, store: &mut TypeVarStore, node: &CallExpr) -> TypeVar {
        let lhs = self.expr(store, node.lhs);
        let args = node.args.iter().map(|x| Type::Var(self.expr(store, *x))).collect();
        let tv = store.type_var();
        store.assignable_to_type(
            lhs,
            Type::Function(Box::new(FunctionSignature { ret: Type::Var(tv), params: args })),
        );
        tv
    }

    fn expr_lambda(&mut self, store: &mut TypeVarStore, node: &LambdaExpr) -> TypeVar {
        let params = node
            .params
            .iter()
            .map(|x| {
                self.var_decl(store, *x);
                Type::Var(
                    store.name_map.get(&x.lookup(self.script_db).name).copied().unwrap_or_else(
                        || {
                            let tv = store.type_var();
                            store.name_map.insert(x.lookup(self.script_db).name, tv);
                            tv
                        },
                    ),
                )
            })
            .collect();
        let ret = match node.block_or_expr {
            BlockOrExpr::Block(x) => {
                self.return_stack.push(store.type_var());
                self.block(store, x);
                self.return_stack.pop().expect("InferCtx::expr_lambda: return stack is empty")
            }
            BlockOrExpr::Expr(x) => self.expr(store, x),
        };
        let tv = store.type_var();
        store.assignable_to_type(
            tv,
            Type::Function(Box::new(FunctionSignature { ret: Type::Var(ret), params })),
        );
        tv
    }

    fn resolve(&self, store: &mut TypeVarStore, name_ref: &NameRef) -> TypeVar {
        match self.db.resolve(self.file_id, name_ref) {
            Some(Symbol::Local(_, name)) => {
                store.name_map.get(name).copied().unwrap_or_else(|| {
                    let tv = store.type_var();
                    store.name_map.insert(*name, tv);
                    tv
                })
            }
            Some(Symbol::Global(gdb, _)) => {
                store.globals_map.get(&name_ref.name).copied().map(|x| x.0).unwrap_or_else(|| {
                    let tv = store.type_var();
                    store.globals_map.insert(name_ref.name.clone(), (tv, *gdb));
                    tv
                })
            }
            None => {
                store.globals_map.get(&name_ref.name).copied().map(|x| x.0).unwrap_or_else(|| {
                    let tv = store.type_var();
                    let gdb = self.db.globals_db("Scriptrunner").unwrap();
                    store.globals_map.insert(name_ref.name.clone(), (tv, gdb));
                    tv
                })
            }
        }
    }
}

#[derive(Debug)]
pub(crate) struct TypeVarStore {
    type_vars: Arena<Vec<Idx<Constraint>>>,
    expr_map: HashMap<ExprId, TypeVar>,
    name_map: HashMap<NameId, TypeVar>,
    globals_map: HashMap<String, (TypeVar, GlobalsDatabaseId)>,
    constraints: Arena<Constraint>,
}

impl TypeVarStore {
    pub fn new() -> Self {
        Self {
            type_vars: Arena::new(),
            expr_map: HashMap::new(),
            name_map: HashMap::new(),
            globals_map: HashMap::new(),
            constraints: Arena::new(),
        }
    }

    fn type_var(&mut self) -> TypeVar {
        self.type_vars.alloc(vec![])
    }

    fn constraint(&mut self, c: Constraint) -> ConstraintId {
        self.constraints.alloc(c)
    }

    pub fn concrete_type(&mut self, tv: TypeVar, ty: InferredType) {
        let c = self.constraint(Constraint::ConcreteType(ty));
        self.type_vars[tv].push(c);
    }

    pub fn assignable(&mut self, tv1: TypeVar, tv2: TypeVar) {
        let c = self.constraint(Constraint::Assignable(tv1, tv2));
        self.type_vars[tv1].push(c);
        self.type_vars[tv2].push(c);
    }

    pub fn assignable_to_type(&mut self, tv: TypeVar, ty: Type) {
        let c = self.constraint(Constraint::AssignableToType(ty));
        self.type_vars[tv].push(c);
    }

    pub fn apply(mut self, db: &mut Database, file_id: FileId) {
        let mut type_map = HashMap::<TypeVar, InferredType>::new();

        let mut constraint_map = HashMap::new();

        for step in 0..256 {
            for (tv, constraints) in self.type_vars.clone().iter() {
                let constraints_map = self.constraints.clone();
                for c in constraints.iter().filter_map(|x| match &constraints_map[*x] {
                    Constraint::ConcreteType(x) => Some(x),
                    _ => None,
                }) {
                    if let Some(mut x) = type_map.get_mut(&tv).cloned() {
                        match x.union(&mut c.clone(), &mut constraint_map) {
                            Ok(_) => (),
                            Err(x) => eprintln!("{x}"),
                        }
                        type_map.insert(tv, x);
                    } else {
                        type_map.insert(tv, c.clone());
                    }
                }
            }

            let mut changed = false;
            for (tv, cs) in constraint_map.drain() {
                for c in cs {
                    if !self.type_vars[tv].iter().map(|x| &self.constraints[*x]).contains(&c) {
                        let c = self.constraints.alloc(c);
                        self.type_vars[tv].push(c);
                        changed = true;
                    }
                }
            }

            if !changed {
                break;
            }
            if step > 256 {
                eprintln!("inference iteration limit reached");
                break;
            }
        }

        for step in 0..256 {
            for (tv, constraints) in self.type_vars.iter() {
                for c in constraints {
                    match &self.constraints[*c] {
                        Constraint::ConcreteType(_) => (),
                        Constraint::Assignable(lhs, rhs) => {
                            match (type_map.get(lhs).cloned(), type_map.get(rhs).cloned()) {
                                (None, Some(mut b)) => {
                                    let mut ty = InferredType::any();
                                    match ty.union(&mut b, &mut constraint_map) {
                                        Ok(_) => {
                                            type_map.insert(*lhs, ty);
                                        }
                                        Err(x) => eprintln!("{x}"),
                                    }
                                }
                                (Some(mut a), None) => {
                                    let mut ty = InferredType::any();
                                    match ty.union(&mut a, &mut constraint_map) {
                                        Ok(_) => {
                                            type_map.insert(*rhs, ty);
                                        }
                                        Err(x) => eprintln!("{x}"),
                                    }
                                }
                                (Some(mut a), Some(mut b)) => {
                                    match a.union(&mut b, &mut constraint_map) {
                                        Ok(_) => {
                                            type_map.insert(*lhs, a.clone());
                                            type_map.insert(*rhs, b);
                                        }
                                        Err(x) => eprintln!("{x}"),
                                    }
                                }
                                _ => (),
                            }
                        }
                        Constraint::AssignableToType(ty) => {
                            if let Some(mut x) = type_map.get(&tv).cloned() {
                                match x.union(
                                    &mut InferredType::inferred(ty.clone()),
                                    &mut constraint_map,
                                ) {
                                    Ok(_) => {
                                        type_map.insert(tv, x);
                                    }
                                    Err(x) => eprintln!("{x}"),
                                }
                            } else {
                                type_map.insert(tv, InferredType::inferred(ty.clone()));
                            }
                        }
                    }
                }
            }

            let mut changed = false;
            for (tv, cs) in constraint_map.drain() {
                for c in cs {
                    if !self.type_vars[tv].iter().map(|x| &self.constraints[*x]).contains(&c) {
                        let c = self.constraints.alloc(c);
                        self.type_vars[tv].push(c);
                        changed = true;
                    }
                }
            }

            if !changed {
                break;
            }
            if step > 256 {
                eprintln!("inference iteration limit reached");
                break;
            }
        }

        for (tv, _) in self.type_vars.iter() {
            if let Some(mut ty) = type_map.get(&tv).cloned() {
                ty.replace_type_vars(&mut type_map, 0, &mut vec![tv]);
                type_map.insert(tv, ty);
            }
        }

        for (k, (v, gdb)) in self.globals_map {
            let mut ty =
                type_map.get(&v).cloned().unwrap_or_else(|| InferredType::concrete(Type::Var(v)));
            if let Some(old_ty) = db[gdb].globals.get_mut(&k) {
                if let Err(x) = old_ty.union(&mut ty, &mut constraint_map) {
                    eprintln!("{x}")
                }
            } else {
                db[gdb].add_global(k.clone(), ty);
            }
            db.globals.insert(k.clone(), Symbol::Global(gdb, k));
        }

        db.name_type_maps.insert(file_id.0, HashMap::new());
        for (k, v) in self.name_map {
            let ty =
                type_map.get(&v).cloned().unwrap_or_else(|| InferredType::concrete(Type::Var(v)));
            db.name_type_maps[file_id.0].insert(k, ty);
        }

        db.type_maps.insert(file_id.0, HashMap::new());
        for (k, v) in self.expr_map {
            let ty =
                type_map.get(&v).cloned().unwrap_or_else(|| InferredType::concrete(Type::Var(v)));
            db.type_maps[file_id.0].insert(k, ty);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Constraint {
    ConcreteType(InferredType),
    Assignable(TypeVar, TypeVar),
    AssignableToType(Type),
}

pub(crate) type TypeVar = Idx<Vec<Idx<Constraint>>>;
pub(crate) type ConstraintId = Idx<Constraint>;
