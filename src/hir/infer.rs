use db::{Database, FileId};
use itertools::Itertools;
use la_arena::{Arena, Idx};

use super::*;

pub(crate) fn infer(db: &mut Database, file_id: FileId) -> TypeVarStore {
    let ctx = InferCtx::new(db, file_id);
    let mut store = TypeVarStore::new();
    ctx.script(&mut store, file_id.hir(db));

    store
}

#[derive(Debug)]
pub(crate) struct InferCtx<'a> {
    db: &'a db::Database,
    script_db: &'a ScriptDatabase,
    file_id: db::FileId,
}

impl<'a> InferCtx<'a> {
    pub fn new(db: &'a Database, file_id: FileId) -> Self {
        Self { script_db: file_id.script_db(db), db, file_id }
    }

    fn script(&self, store: &mut TypeVarStore, node: &Script) {
        for item in &node.items {
            self.item(store, *item);
        }
    }

    fn item(&self, store: &mut TypeVarStore, node: ItemId) {
        match node.lookup(self.script_db) {
            Item::FnDecl(x) => {
                self.block(store, x.block);
            }
            Item::BlockType(x) => self.block(store, x.block),
            Item::VarDeclStmt(x) => self.var_decl(store, x.decl),
        }
    }

    fn block(&self, store: &mut TypeVarStore, node: BlockId) {
        for stmt in &node.lookup(self.script_db).stmts {
            self.stmt(store, *stmt);
        }
    }

    fn stmt(&self, store: &mut TypeVarStore, node: StmtId) {
        match node.lookup(self.script_db) {
            Stmt::For(x) => self.stmt_for(store, x),
            Stmt::ForEach(x) => self.stmt_for_each(store, x),
            Stmt::If(x) => self.stmt_if(store, x),
            Stmt::While(x) => self.stmt_while(store, x),
            Stmt::VarDecl(x) => self.var_decl(store, x.decl),
            Stmt::Return(x) => {
                if let Some(x) = x.expr {
                    self.expr(store, x);
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

    fn stmt_for(&self, store: &mut TypeVarStore, node: &ForStmt) {
        if let Some(init) = &node.init {
            self.var_decl(store, *init);
        }
        if let Some(cond) = &node.cond {
            let tv = self.expr(store, *cond);
            store.concrete_type(tv, Type::Bool);
        }
        if let Some(loop_expr) = &node.loop_expr {
            self.expr(store, *loop_expr);
        }
        self.block(store, node.block);
    }

    fn stmt_for_each(&self, store: &mut TypeVarStore, node: &ForEachStmt) {
        self.var_decl(store, node.var_decl);
        self.expr(store, node.iterable);
        self.block(store, node.block);
    }

    fn stmt_if(&self, store: &mut TypeVarStore, node: &IfStmt) {
        let tv = self.expr(store, node.cond);
        store.concrete_type(tv, Type::Bool);
        self.block(store, node.true_branch);
        if let Some(false_branch) = &node.false_branch {
            self.stmt(store, *false_branch);
        }
        if let Some(else_branch) = &node.else_branch {
            self.block(store, *else_branch);
        }
    }

    fn stmt_while(&self, store: &mut TypeVarStore, node: &WhileStmt) {
        let tv = self.expr(store, node.cond);
        store.concrete_type(tv, Type::Bool);
        self.block(store, node.block);
    }

    fn var_decl(&self, store: &mut TypeVarStore, node: VarDeclId) {
        let var_decl = node.lookup(self.script_db);
        let name_tv = store.name_map.get(&var_decl.name).copied().unwrap_or_else(|| {
            let tv = store.type_var();
            store.name_map.insert(var_decl.name, tv);
            tv
        });
        store.concrete_type(name_tv, var_decl.decl_type.into());
        if let Some(init) = &var_decl.init {
            let tv = self.expr(store, *init);
            store.assignable(name_tv, tv);
        }
    }

    fn expr(&self, store: &mut TypeVarStore, node: ExprId) -> TypeVar {
        let tv = match node.lookup(self.script_db) {
            Expr::Missing => store.type_var(),
            Expr::Bin(x) => self.expr_bin(store, x),
            Expr::Ternary(x) => self.expr_ternary(store, x),
            Expr::Unary(x) => self.expr(store, x.operand),
            Expr::Subscript(x) => self.expr_subscript(store, x),
            Expr::Call(x) => self.expr_call(store, x),
            Expr::Paren(x) => self.expr(store, x.expr),
            Expr::Lambda(x) => self.expr_lambda(store, x),
            Expr::NameRef(x) => self.resolve(store, x),
            Expr::Str(x) => {
                for shard in &x.shards {
                    match shard.lookup(self.script_db) {
                        StringShard::Str { .. } => (),
                        StringShard::Expr { expr, .. } => {
                            self.expr(store, *expr);
                        }
                    }
                }
                let tv = store.type_var();
                store.concrete_type(tv, Type::String);
                tv
            }
            Expr::Lit(x) => {
                let tv = store.type_var();
                store.concrete_type(tv, match x {
                    Literal::Number(_) => Type::Number,
                    Literal::Bool(_) => Type::Bool,
                });
                tv
            }
        };

        store.expr_map.insert(node, tv);
        tv
    }

    fn expr_bin(&self, store: &mut TypeVarStore, node: &BinExpr) -> TypeVar {
        let lhs = self.expr(store, node.lhs);
        let rhs = self.expr(store, node.rhs);
        store.assignable(lhs, rhs);
        let tv = store.type_var();
        store.assignable(tv, lhs);
        store.assignable(tv, rhs);
        tv
    }

    fn expr_ternary(&self, store: &mut TypeVarStore, node: &TernaryExpr) -> TypeVar {
        let cond = self.expr(store, node.cond);
        store.concrete_type(cond, Type::Bool);
        let true_expr = self.expr(store, node.true_expr);
        let false_expr = self.expr(store, node.false_expr);
        store.assignable(true_expr, false_expr);
        let tv = store.type_var();
        store.assignable(tv, true_expr);
        store.assignable(tv, false_expr);
        tv
    }

    fn expr_subscript(&self, store: &mut TypeVarStore, node: &SubscriptExpr) -> TypeVar {
        let lhs = self.expr(store, node.lhs);
        let subscript = self.expr(store, node.subscript);
        let value = store.type_var();
        store.assignable_to_type(
            lhs,
            Type::Map(Box::new((Type::TypeVar(subscript), Type::TypeVar(value)))),
        );
        value
    }

    fn expr_call(&self, store: &mut TypeVarStore, node: &CallExpr) -> TypeVar {
        let lhs = self.expr(store, node.lhs);
        let args = node.args.iter().map(|x| Type::TypeVar(self.expr(store, *x))).collect();
        let tv = store.type_var();
        store.assignable_to_type(
            lhs,
            Type::Function(Box::new(FunctionSignature { ret: Type::TypeVar(tv), params: args })),
        );
        tv
    }

    fn expr_lambda(&self, store: &mut TypeVarStore, node: &LambdaExpr) -> TypeVar {
        let params = node
            .params
            .iter()
            .map(|x| {
                Type::TypeVar(
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
        let tv = store.type_var();
        let ret = store.type_var();
        store.assignable_to_type(
            tv,
            Type::Function(Box::new(FunctionSignature { ret: Type::TypeVar(ret), params })),
        );
        ret
    }

    fn resolve(&self, store: &mut TypeVarStore, name_ref: &NameRef) -> TypeVar {
        match self.db.resolve(self.file_id, name_ref) {
            Some(Symbol::Local(name)) => store.name_map.get(name).copied().unwrap_or_else(|| {
                let tv = store.type_var();
                store.name_map.insert(*name, tv);
                tv
            }),
            _ => store.globals_map.get(&name_ref.name).copied().unwrap_or_else(|| {
                let tv = store.type_var();
                store.globals_map.insert(name_ref.name.clone(), tv);
                tv
            }),
        }
    }
}

#[derive(Debug)]
pub(crate) struct TypeVarStore {
    type_vars: Arena<Vec<Idx<Constraint>>>,
    expr_map: HashMap<ExprId, TypeVar>,
    name_map: HashMap<NameId, TypeVar>,
    globals_map: HashMap<String, TypeVar>,
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

    pub fn concrete_type(&mut self, tv: TypeVar, ty: Type) {
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
                    if let Some(x) = type_map.get(&tv).cloned() {
                        x.narrowest.unify(c, &mut constraint_map);
                        type_map.insert(tv, x);
                    } else {
                        type_map.insert(tv, InferredType::concrete(c.clone()));
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
                println!("inference iteration limit reached");
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
                                    match ty.merge(&mut b) {
                                        Ok(_) => {
                                            type_map.insert(*lhs, ty);
                                        }
                                        Err(x) => println!("{x}"),
                                    }
                                }
                                (Some(mut a), None) => {
                                    let mut ty = InferredType::any();
                                    match ty.merge(&mut a) {
                                        Ok(_) => {
                                            type_map.insert(*rhs, ty);
                                        }
                                        Err(x) => println!("{x}"),
                                    }
                                }
                                (Some(mut a), Some(mut b)) => {
                                    a.narrowest.unify(&b.narrowest, &mut constraint_map);
                                    match a.merge(&mut b) {
                                        Ok(_) => {
                                            type_map.insert(*lhs, a.clone());
                                            type_map.insert(*rhs, b);
                                        }
                                        Err(x) => println!("{x}"),
                                    }
                                }
                                _ => (),
                            }
                        }
                        Constraint::AssignableToType(ty) => {
                            if let Some(mut x) = type_map.get(&tv).cloned() {
                                x.narrowest.unify(ty, &mut constraint_map);
                                match x.merge(&mut InferredType::inferred(ty.clone())) {
                                    Ok(_) => {
                                        type_map.insert(tv, x);
                                    }
                                    Err(x) => println!("{x}"),
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
                println!("inference iteration limit reached");
                break;
            }
        }

        for (tv, _) in self.type_vars.iter() {
            if let Some(mut ty) = type_map.get(&tv).cloned() {
                ty.replace_type_vars(&mut type_map, 0, &mut vec![tv]);
                type_map.insert(tv, ty);
            }
        }

        for (k, v) in self.globals_map {
            let ty = type_map
                .get(&v)
                .cloned()
                .unwrap_or_else(|| InferredType::concrete(Type::TypeVar(v)));
            db.globals.insert(k, Symbol::Global(ty.clone()));
        }

        db.name_type_maps.insert(file_id.0, HashMap::new());
        for (k, v) in self.name_map {
            let ty = type_map
                .get(&v)
                .cloned()
                .unwrap_or_else(|| InferredType::concrete(Type::TypeVar(v)));
            db.name_type_maps[file_id.0].insert(k, ty);
        }

        db.type_maps.insert(file_id.0, HashMap::new());
        for (k, v) in self.expr_map {
            let ty = type_map
                .get(&v)
                .cloned()
                .unwrap_or_else(|| InferredType::concrete(Type::TypeVar(v)));
            db.type_maps[file_id.0].insert(k, ty);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Constraint {
    ConcreteType(Type),
    Assignable(TypeVar, TypeVar),
    AssignableToType(Type),
}

pub(crate) type TypeVar = Idx<Vec<Idx<Constraint>>>;
pub(crate) type ConstraintId = Idx<Constraint>;
