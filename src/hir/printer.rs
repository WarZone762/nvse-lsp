use db::Database;

use super::*;

#[derive(Debug, Clone)]
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

    fn push(&mut self, string: &str) {
        self.buf.push_str(string);
    }

    fn push_indent(&mut self) {
        self.buf.push_str(&"    ".repeat(self.indent));
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn unindent(&mut self) {
        self.indent -= 1;
    }

    fn finish(self) -> String {
        self.buf
    }
}

pub(crate) trait Print {
    fn print(&self, p: &mut Printer<'_>);
    fn print_str(&self, mut p: Printer<'_>) -> String {
        self.print(&mut p);
        p.finish()
    }
}

impl<P: Print> Print for &P {
    fn print(&self, p: &mut Printer<'_>) {
        Print::print(*self, p);
    }
}

impl<P: Print> Print for Rc<P> {
    fn print(&self, p: &mut Printer<'_>) {
        Rc::as_ref(self).print(p);
    }
}

pub(crate) trait PrintDelim {
    fn print_delimited(self, p: &mut Printer<'_>, delim: &str);
}

impl<I: IntoIterator<Item = P>, P: Print> PrintDelim for I {
    fn print_delimited(self, p: &mut Printer<'_>, delim: &str) {
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
    fn print(&self, p: &mut Printer<'_>) {
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
    fn print(&self, p: &mut Printer<'_>) {
        p.push_indent();
        match self.lookup(p.script_db) {
            Item::FnDecl(x) => x.print(p),
            Item::BlockType(x) => x.print(p),
            Item::VarDecl(x) => x.print(p),
        }
        p.push("\n");
    }
}

impl Print for FnDeclItem {
    fn print(&self, p: &mut Printer<'_>) {
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
    fn print(&self, p: &mut Printer<'_>) {
        p.push(&self.blocktype_kind.to_string());
        p.push(" ");
        self.block.print(p);
    }
}

impl Print for StmtId {
    fn print(&self, p: &mut Printer<'_>) {
        p.push_indent();
        match self.lookup(p.script_db) {
            Stmt::For(x) => x.print(p),
            Stmt::ForRange(x) => x.print(p),
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
    fn print(&self, p: &mut Printer<'_>) {
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

impl Print for ForRangeStmt {
    fn print(&self, p: &mut Printer<'_>) {
        p.push("for (");
        self.pat.print(p);
        p.push(" in ");
        self.iterable.print(p);
        p.push(") ");
        self.block.print(p);
    }
}

impl Print for IfStmt {
    fn print(&self, p: &mut Printer<'_>) {
        p.push("if (");
        self.cond.print(p);
        p.push(") ");
        self.true_branch.print(p);
        match &self.false_branch {
            Some(x) => {
                p.push(" ");
                x.print(p);
            }
            None => (),
        }
    }
}

impl Print for ElseBranch {
    fn print(&self, p: &mut Printer<'_>) {
        p.push("else ");
        match self {
            ElseBranch::Block(x) => x.print(p),
            ElseBranch::If(x) => x.print(p),
        }
    }
}

impl Print for WhileStmt {
    fn print(&self, p: &mut Printer<'_>) {
        p.push("while (");
        self.cond.print(p);
        p.push(") ");
        self.block.print(p);
    }
}

impl Print for VarDeclStmt {
    fn print(&self, p: &mut Printer<'_>) {
        if self.export {
            p.push("export ");
        }
        self.decl.print(p);
        p.push(";")
    }
}

impl Print for ReturnStmt {
    fn print(&self, p: &mut Printer<'_>) {
        p.push("return");
        if let Some(expr) = &self.expr {
            p.push(" ");
            expr.print(p);
        }
        p.push(";");
    }
}

impl Print for BreakStmt {
    fn print(&self, p: &mut Printer<'_>) {
        p.push("break;");
    }
}

impl Print for ContinueStmt {
    fn print(&self, p: &mut Printer<'_>) {
        p.push("continue;");
    }
}

impl Print for ExprStmt {
    fn print(&self, p: &mut Printer<'_>) {
        self.expr.print(p);
        p.push(";");
    }
}

impl Print for ExprId {
    fn print(&self, p: &mut Printer<'_>) {
        p.push("(");
        self.type_(p.db, p.file_id).print(p);
        p.push(")");
        p.push("(");
        match self.lookup(p.script_db) {
            Expr::Missing => p.push("?"),
            Expr::Bin(x) => x.print(p),
            Expr::Ternary(x) => x.print(p),
            Expr::Unary(x) => x.print(p),
            Expr::Postfix(x) => x.print(p),
            Expr::Field(x) => x.print(p),
            Expr::Subscript(x) => x.print(p),
            Expr::Call(x) => x.print(p),
            Expr::Paren(x) => x.print(p),
            Expr::Lambda(x) => x.print(p),
            Expr::NameRef(x) => x.print(p),
            Expr::Str(x) => x.print(p),
            Expr::Literal(x) => x.print(p),
        }
        p.push(")");
    }
}

impl Print for BinExpr {
    fn print(&self, p: &mut Printer<'_>) {
        self.lhs.print(p);
        p.push(" ");
        self.op.print(p);
        p.push(" ");
        self.rhs.print(p);
    }
}

impl Print for BinOpKind {
    fn print(&self, p: &mut Printer<'_>) {
        match self {
            BinOpKind::Plus => p.push("+"),
            BinOpKind::Minus => p.push("-"),
            BinOpKind::Asterisk => p.push("*"),
            BinOpKind::Slash => p.push("/"),
            BinOpKind::Percent => p.push("%"),
            BinOpKind::Circumflex => p.push("^"),
            BinOpKind::PlusEq => p.push("+="),
            BinOpKind::MinusEq => p.push("-="),
            BinOpKind::AsteriskEq => p.push("*="),
            BinOpKind::SlashEq => p.push("/="),
            BinOpKind::PercentEq => p.push("%="),
            BinOpKind::CircumflexEq => p.push("^="),
            BinOpKind::Vbar => p.push("|"),
            BinOpKind::VbarEq => p.push("|="),
            BinOpKind::Ampersand => p.push("&"),
            BinOpKind::AmpersandEq => p.push("&="),
            BinOpKind::Eq => p.push("="),
            BinOpKind::Eq2 => p.push("=="),
            BinOpKind::Lt => p.push("<"),
            BinOpKind::LtEq => p.push("<="),
            BinOpKind::Gt => p.push(">"),
            BinOpKind::GtEq => p.push(">="),
            BinOpKind::ExclamationEq => p.push("!="),
            BinOpKind::Vbar2 => p.push("||"),
            BinOpKind::Ampersand2 => p.push("&&"),
            BinOpKind::Lt2 => p.push("<<"),
            BinOpKind::Gt2 => p.push(">>"),
            BinOpKind::Colon => p.push(":"),
            BinOpKind::Colon2 => p.push("::"),
            BinOpKind::Unknown => p.push("<?>"),
        }
    }
}

impl Print for TernaryExpr {
    fn print(&self, p: &mut Printer<'_>) {
        self.cond.print(p);
        p.push(" ? ");
        self.true_expr.print(p);
        p.push(" : ");
        self.false_expr.print(p);
    }
}

impl Print for UnaryExpr {
    fn print(&self, p: &mut Printer<'_>) {
        self.op.print(p);
        self.operand.print(p);
    }
}

impl Print for PostfixExpr {
    fn print(&self, p: &mut Printer<'_>) {
        self.operand.print(p);
        self.op.print(p);
    }
}

impl Print for PostfixOpKind {
    fn print(&self, p: &mut Printer<'_>) {
        match self {
            PostfixOpKind::Plus2 => p.push("++"),
            PostfixOpKind::Minus2 => p.push("--"),
            PostfixOpKind::Unknown => p.push("<?>"),
        }
    }
}

impl Print for FieldExpr {
    fn print(&self, p: &mut Printer<'_>) {
        self.lhs.print(p);
        p.push(".");
        self.field.print(p);
    }
}

impl Print for UnaryOpKind {
    fn print(&self, p: &mut Printer<'_>) {
        match self {
            UnaryOpKind::Plus => p.push("+"),
            UnaryOpKind::Minus => p.push("-"),
            UnaryOpKind::Asterisk => p.push("*"),
            UnaryOpKind::Ampersand => p.push("&"),
            UnaryOpKind::Plus2 => p.push("++"),
            UnaryOpKind::Minus2 => p.push("--"),
            UnaryOpKind::Dollar => p.push("$"),
            UnaryOpKind::NumSign => p.push("#"),
            UnaryOpKind::Exclamation => p.push("!"),
            UnaryOpKind::Tilde => p.push("~"),
            UnaryOpKind::Unknown => p.push("<?>"),
        }
    }
}

impl Print for SubscriptExpr {
    fn print(&self, p: &mut Printer<'_>) {
        self.lhs.print(p);
        p.push("[");
        self.subscript.print(p);
        p.push("]");
    }
}

impl Print for CallExpr {
    fn print(&self, p: &mut Printer<'_>) {
        self.lhs.print(p);
        p.push("(");
        self.args.iter().print_delimited(p, ", ");
        p.push(")");
    }
}

impl Print for ParenExpr {
    fn print(&self, p: &mut Printer<'_>) {
        p.push("(");
        self.expr.print(p);
        p.push(")");
    }
}

impl Print for LambdaExpr {
    fn print(&self, p: &mut Printer<'_>) {
        p.push("fn (");
        self.params.iter().print_delimited(p, ", ");
        p.push(") ");
        match self.block_or_expr {
            BlockOrExpr::Block(x) => x.print(p),
            BlockOrExpr::Expr(x) => x.print(p),
        }
    }
}

impl Print for StrExpr {
    fn print(&self, p: &mut Printer<'_>) {
        p.push("\"");
        for shard in &self.shards {
            shard.print(p);
        }
        p.push("\"");
    }
}

impl Print for StrShardId {
    fn print(&self, p: &mut Printer<'_>) {
        match self.lookup(p.script_db) {
            StrShard::Str { val, .. } => p.push(val),
            StrShard::Expr { expr, .. } => {
                p.push("${");
                expr.print(p);
                p.push("}");
            }
        }
    }
}

impl Print for Literal {
    fn print(&self, p: &mut Printer<'_>) {
        p.push(&match self {
            Literal::Number(x) => x.value.to_string(),
            Literal::Bool(x) => x.value.to_string(),
        })
    }
}

impl Print for VarDeclId {
    fn print(&self, p: &mut Printer<'_>) {
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

impl Print for VarDeclType {
    fn print(&self, p: &mut Printer<'_>) {
        match self {
            VarDeclType::Int => p.push("int"),
            VarDeclType::Double => p.push("double"),
            VarDeclType::Float => p.push("float"),
            VarDeclType::Ref => p.push("ref"),
            VarDeclType::String => p.push("string"),
            VarDeclType::Array => p.push("array"),
            VarDeclType::Unknown => p.push("unknown"),
        }
    }
}

impl Print for Pat {
    fn print(&self, p: &mut Printer<'_>) {
        match self {
            Pat::VarDecl(x) => x.print(p),
            Pat::Arr(x) => {
                p.push("[");
                x.print_delimited(p, ", ");
                p.push("]");
            }
        }
    }
}

impl Print for BlockId {
    fn print(&self, p: &mut Printer<'_>) {
        let block = self.lookup(p.script_db);
        p.push("{\n");
        p.indent();
        block.sym_table.print(p);
        block.stmts.iter().print_delimited(p, "\n");
        p.unindent();
        p.push("\n");
        p.push_indent();
        p.push("}");
    }
}

impl Print for NameId {
    fn print(&self, p: &mut Printer<'_>) {
        p.push(&self.lookup(p.script_db).name);
    }
}

impl Print for NameRef {
    fn print(&self, p: &mut Printer<'_>) {
        p.push(&self.name);
    }
}

impl Print for SymbolTable {
    fn print(&self, p: &mut Printer<'_>) {
        if self.map.is_empty() {
            return;
        }

        p.push_indent();
        p.push("{\n");
        p.indent();
        for (k, v) in self.map.iter() {
            p.push_indent();
            p.push(k);
            p.push(": ");
            match v {
                Symbol::Local(_, x) => x.type_(p.db, p.file_id).print(p),
                Symbol::Global(gdb, name) => gdb.lookup(p.db).globals.get(name).unwrap().print(p),
            }
            p.push(",\n");
        }
        p.unindent();
        p.push_indent();
        p.push("}\n");
    }
}

impl Print for InferredType {
    fn print(&self, p: &mut Printer<'_>) {
        self.narrowest.print(p);
    }
}

impl Print for Type {
    fn print(&self, p: &mut Printer<'_>) {
        p.push(&self.to_string(p.indent));
    }
}
