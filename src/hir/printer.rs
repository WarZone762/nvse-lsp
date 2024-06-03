use db::{Database, FileId};

use super::*;

#[derive(Debug)]
pub(crate) struct Printer<'a> {
    db: &'a db::Database,
    script_db: &'a ScriptDatabase,
    file_id: db::FileId,
    indent: usize,
    buf: String,
}

impl<'a> Printer<'a> {
    pub fn new(file_id: db::FileId, db: &'a Database, script_db: &'a ScriptDatabase) -> Self {
        Self { db, script_db, file_id, indent: 0, buf: String::new() }
    }

    pub fn push(&mut self, string: &str) {
        self.buf.push_str(string);
    }

    pub fn push_indent(&mut self) {
        self.buf.push_str(&"    ".repeat(self.indent));
    }

    pub fn finish(self) -> String {
        self.buf
    }
}

pub(crate) trait Print {
    fn print(&self, p: &mut Printer);
    fn print_str(&self, file_id: FileId, db: &Database) -> String {
        let mut p = Printer::new(file_id, db, file_id.script_db(db));
        self.print(&mut p);
        p.finish()
    }
}

impl<P: Print> Print for &P {
    fn print(&self, p: &mut Printer) {
        Print::print(*self, p);
    }
}

impl<P: Print> Print for Rc<P> {
    fn print(&self, p: &mut Printer) {
        Rc::as_ref(self).print(p);
    }
}

pub(crate) trait PrintDelim {
    fn print_delimited(self, p: &mut Printer, delim: &str);
}

impl<I: IntoIterator<Item = P>, P: Print> PrintDelim for I {
    fn print_delimited(self, p: &mut Printer, delim: &str) {
        let mut is_first = true;
        for e in self.into_iter() {
            if is_first {
                is_first = false;
            } else {
                p.push(delim);
            }
            e.print(p);
        }
    }
}

impl Print for Script {
    fn print(&self, p: &mut Printer) {
        p.push("name ");
        if let Some(name) = &self.name {
            name.print(p);
        } else {
            p.push("?");
        }
        p.push(";\n\n");
        for item in &self.items {
            item.print(p);
        }
    }
}

impl Print for ItemId {
    fn print(&self, p: &mut Printer) {
        p.push_indent();
        match self.lookup(p.script_db) {
            Item::FnDecl(x) => x.print(p),
            Item::BlockType(x) => x.print(p),
            Item::VarDeclStmt(x) => x.print(p),
        }
    }
}

impl Print for FnDeclItem {
    fn print(&self, p: &mut Printer) {
        p.push("fn ");
        if let Some(name) = &self.name {
            name.print(p);
        }
        p.push("(");
        self.params.iter().print_delimited(p, ", ");
        p.push(") ");
        self.block.print(p);
    }
}

impl Print for BlockTypeItem {
    fn print(&self, p: &mut Printer) {
        p.push(&self.blocktype_kind.to_string());
        p.push(" ");
        self.block.print(p);
    }
}

impl Print for StmtId {
    fn print(&self, p: &mut Printer) {
        p.push_indent();
        match self.lookup(p.script_db) {
            Stmt::For(x) => x.print(p),
            Stmt::ForEach(x) => x.print(p),
            Stmt::If(x) => x.print(p),
            Stmt::While(x) => x.print(p),
            Stmt::VarDecl(x) => x.print(p),
            Stmt::Return(x) => x.print(p),
            Stmt::Break(x) => x.print(p),
            Stmt::Continue(x) => x.print(p),
            Stmt::Block(x) => x.print(p),
            Stmt::Expr(x) => x.print(p),
        }
    }
}

impl Print for ForStmt {
    fn print(&self, p: &mut Printer) {
        p.push("for (");
        if let Some(init) = &self.init {
            init.print(p);
        }
        p.push(";");
        if let Some(cond) = &self.cond {
            cond.print(p);
        }
        p.push(";");
        if let Some(loop_expr) = &self.loop_expr {
            loop_expr.print(p);
        }
        p.push(") ");
        self.block.print(p);
    }
}

impl Print for ForEachStmt {
    fn print(&self, p: &mut Printer) {
        p.push("for (");
        self.var_decl.print(p);
        p.push(" : ");
        self.iterable.print(p);
        p.push(") ");
        self.block.print(p);
    }
}

impl Print for IfStmt {
    fn print(&self, p: &mut Printer) {
        p.push("if (");
        self.cond.print(p);
        p.push(") ");
        self.true_branch.print(p);
        if let Some(false_branch) = &self.false_branch {
            p.push(" else ");
            false_branch.print(p);
        }
        if let Some(else_branch) = &self.else_branch {
            p.push(" else ");
            else_branch.print(p);
        }
    }
}

impl Print for WhileStmt {
    fn print(&self, p: &mut Printer) {
        p.push("while (");
        self.cond.print(p);
        p.push(") ");
        self.block.print(p);
    }
}

impl Print for VarDeclStmt {
    fn print(&self, p: &mut Printer) {
        self.decl.print(p);
        p.push(";")
    }
}

impl Print for ReturnStmt {
    fn print(&self, p: &mut Printer) {
        p.push("return");
        if let Some(expr) = &self.expr {
            p.push(" ");
            expr.print(p);
        }
        p.push(";");
    }
}

impl Print for BreakStmt {
    fn print(&self, p: &mut Printer) {
        p.push("break;");
    }
}

impl Print for ContinueStmt {
    fn print(&self, p: &mut Printer) {
        p.push("continue;");
    }
}

impl Print for ExprStmt {
    fn print(&self, p: &mut Printer) {
        self.expr.print(p);
        p.push(";");
    }
}

impl Print for ExprId {
    fn print(&self, p: &mut Printer) {
        p.push("(");
        self.type_(p.db, p.file_id).print(p);
        p.push(")");
        p.push("(");
        match self.lookup(p.script_db) {
            Expr::Missing => p.push("?"),
            Expr::Bin(x) => x.print(p),
            Expr::Ternary(x) => x.print(p),
            Expr::Unary(x) => x.print(p),
            Expr::Subscript(x) => x.print(p),
            Expr::Call(x) => x.print(p),
            Expr::Paren(x) => x.print(p),
            Expr::Lambda(x) => x.print(p),
            Expr::NameRef(x) => x.print(p),
            Expr::Str(x) => x.print(p),
            Expr::Lit(x) => x.print(p),
        }
        p.push(")");
    }
}

impl Print for BinExpr {
    fn print(&self, p: &mut Printer) {
        self.lhs.print(p);
        p.push(" ");
        self.op.print(p);
        p.push(" ");
        self.rhs.print(p);
    }
}

impl Print for BinOpKind {
    fn print(&self, p: &mut Printer) {
        match self {
            BinOpKind::Plus => p.push("+"),
            BinOpKind::Minus => p.push("-"),
        }
    }
}

impl Print for TernaryExpr {
    fn print(&self, p: &mut Printer) {
        self.cond.print(p);
        p.push(" ? ");
        self.true_expr.print(p);
        p.push(" : ");
        self.false_expr.print(p);
    }
}

impl Print for UnaryExpr {
    fn print(&self, p: &mut Printer) {
        self.op.print(p);
        self.operand.print(p);
    }
}

impl Print for UnaryOpKind {
    fn print(&self, p: &mut Printer) {
        match self {
            UnaryOpKind::Minus => p.push("-"),
        }
    }
}

impl Print for SubscriptExpr {
    fn print(&self, p: &mut Printer) {
        self.lhs.print(p);
        p.push("[");
        self.subscript.print(p);
        p.push("]");
    }
}

impl Print for CallExpr {
    fn print(&self, p: &mut Printer) {
        self.lhs.print(p);
        p.push("(");
        self.args.iter().print_delimited(p, ", ");
        p.push(")");
    }
}

impl Print for ParenExpr {
    fn print(&self, p: &mut Printer) {
        p.push("(");
        self.expr.print(p);
        p.push(")");
    }
}

impl Print for LambdaExpr {
    fn print(&self, p: &mut Printer) {
        p.push("fn (");
        self.params.iter().print_delimited(p, ", ");
        p.push(") ");
        self.block.print(p);
    }
}

impl Print for StrExpr {
    fn print(&self, p: &mut Printer) {
        p.push("\"");
        for shard in &self.shards {
            shard.print(p);
        }
        p.push("\"");
    }
}

impl Print for StringShardId {
    fn print(&self, p: &mut Printer) {
        match self.lookup(p.script_db) {
            StringShard::Str { val, .. } => p.push(val),
            StringShard::Expr(x) => {
                p.push("${");
                x.print(p);
                p.push("}");
            }
        }
    }
}

impl Print for Literal {
    fn print(&self, p: &mut Printer) {
        p.push(&match self {
            Literal::Number(x) => x.value.to_string(),
            Literal::Bool(x) => x.value.to_string(),
        })
    }
}

impl Print for VarDeclId {
    fn print(&self, p: &mut Printer) {
        let var_decl = self.lookup(p.script_db);
        var_decl.decl_type.print(p);
        p.push(" ");
        var_decl.name.print(p);
        if let Some(init) = &var_decl.init {
            p.push(" = ");
            init.print(p);
        }
    }
}

impl Print for BlockId {
    fn print(&self, p: &mut Printer) {
        let block = self.lookup(p.script_db);
        p.push("{\n");
        p.indent += 1;
        block.sym_table.print(p);
        block.stmts.iter().print_delimited(p, "\n");
        p.indent -= 1;
        p.push("\n");
        p.push_indent();
        p.push("}");
    }
}

impl Print for NameId {
    fn print(&self, p: &mut Printer) {
        p.push(&self.lookup(p.script_db).name);
    }
}

impl Print for NameRef {
    fn print(&self, p: &mut Printer) {
        p.push(&self.name);
    }
}

impl Print for SymbolTable {
    fn print(&self, p: &mut Printer) {
        if self.map.is_empty() {
            return;
        }

        p.push_indent();
        p.push("{\n");
        p.indent += 1;
        for (k, v) in self.map.iter() {
            p.push_indent();
            p.push(k);
            p.push(": ");
            match v {
                Symbol::Local(x) => x.lookup(p.script_db).decl_type.print(p),
                Symbol::Global(x) => x.print(p),
            }
            p.push(",\n");
        }
        p.indent -= 1;
        p.push_indent();
        p.push("}\n");
    }
}

impl Print for Type {
    fn print(&self, p: &mut Printer) {
        match self {
            Type::Bool => p.push("bool"),
            Type::Number => p.push("num"),
            Type::Ref => p.push("ref"),
            Type::String => p.push("str"),
            Type::Array(x) => {
                p.push("arr<");
                x.print(p);
                p.push(">")
            }
            Type::Function(x) => {
                x.print(p);
            }
            Type::Script => p.push("script"),
            Type::Ambiguous => p.push("?"),
        }
    }
}

impl Print for FunctionSignature {
    fn print(&self, p: &mut Printer) {
        p.push("fn(");
        self.params.iter().print_delimited(p, ", ");
        p.push(")");
        if let Some(ret) = &self.ret {
            p.push(" -> ");
            ret.print(p);
        }
    }
}
