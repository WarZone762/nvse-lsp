use db::Database;

use super::*;

pub(crate) trait Printer<'a> {
    fn lookup<L: Lookup<DB = ScriptDatabase>>(&self, node: L) -> &'a L::Output;
    fn type_(&self, expr: ExprId) -> InferredType;
    fn name_type(&self, name: NameId) -> InferredType;
    fn push(&mut self, string: &str);
    fn indent_level(&self) -> usize;
    fn indent(&mut self);
    fn unindent(&mut self);
    fn finish(self) -> String;

    fn push_indent(&mut self) {
        self.push(&"    ".repeat(self.indent_level()));
    }
}

#[derive(Debug)]
pub(crate) struct SimplePrinter {
    buf: String,
    indent: usize,
}

impl SimplePrinter {
    pub fn new() -> Self {
        Self { buf: String::new(), indent: 0 }
    }
}

impl Printer<'static> for SimplePrinter {
    fn lookup<L: Lookup<DB = ScriptDatabase>>(&self, _node: L) -> &'static L::Output {
        unimplemented!()
    }

    fn type_(&self, _expr: ExprId) -> InferredType {
        unimplemented!()
    }

    fn name_type(&self, name: NameId) -> InferredType {
        unimplemented!()
    }

    fn push(&mut self, string: &str) {
        self.buf.push_str(string);
    }

    fn indent_level(&self) -> usize {
        self.indent
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

#[derive(Debug, Clone)]
pub(crate) struct HirPrinter<'a> {
    db: &'a db::Database,
    script_db: &'a ScriptDatabase,
    file_id: db::FileId,
    indent: usize,
    buf: String,
}

impl<'a> HirPrinter<'a> {
    pub fn new(file_id: db::FileId, db: &'a Database, script_db: &'a ScriptDatabase) -> Self {
        Self { db, script_db, file_id, indent: 0, buf: String::new() }
    }
}

impl<'a> Printer<'a> for HirPrinter<'a> {
    fn lookup<L: Lookup<DB = ScriptDatabase>>(&self, node: L) -> &'a L::Output {
        node.lookup(self.script_db)
    }

    fn type_(&self, expr: ExprId) -> InferredType {
        expr.type_(self.db, self.file_id)
    }

    fn name_type(&self, name: NameId) -> InferredType {
        name.type_(&self.db, self.file_id)
    }

    fn push(&mut self, string: &str) {
        self.buf.push_str(string);
    }

    fn indent_level(&self) -> usize {
        self.indent
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
    fn print<'a>(&self, p: &mut impl Printer<'a>);
    fn print_str<'a>(&self, mut p: impl Printer<'a>) -> String {
        self.print(&mut p);
        p.finish()
    }
}

impl<P: Print> Print for &P {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        Print::print(*self, p);
    }
}

impl<P: Print> Print for Rc<P> {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        Rc::as_ref(self).print(p);
    }
}

pub(crate) trait PrintDelim {
    fn print_delimited<'a>(self, p: &mut impl Printer<'a>, delim: &str);
}

impl<I: IntoIterator<Item = P>, P: Print> PrintDelim for I {
    fn print_delimited<'a>(self, p: &mut impl Printer<'a>, delim: &str) {
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
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
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
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push_indent();
        match p.lookup(*self) {
            Item::FnDecl(x) => x.print(p),
            Item::BlockType(x) => x.print(p),
            Item::VarDeclStmt(x) => x.print(p),
        }
        p.push("\n");
    }
}

impl Print for FnDeclItem {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
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
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push(&self.blocktype_kind.to_string());
        p.push(" ");
        self.block.print(p);
    }
}

impl Print for StmtId {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push_indent();
        match p.lookup(*self) {
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
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
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
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push("for (");
        self.var_decl.print(p);
        p.push(" : ");
        self.iterable.print(p);
        p.push(") ");
        self.block.print(p);
    }
}

impl Print for IfStmt {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
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
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push("else ");
        match self {
            ElseBranch::Block(x) => x.print(p),
            ElseBranch::If(x) => x.print(p),
        }
    }
}

impl Print for WhileStmt {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push("while (");
        self.cond.print(p);
        p.push(") ");
        self.block.print(p);
    }
}

impl Print for VarDeclStmt {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        self.decl.print(p);
        p.push(";")
    }
}

impl Print for ReturnStmt {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push("return");
        if let Some(expr) = &self.expr {
            p.push(" ");
            expr.print(p);
        }
        p.push(";");
    }
}

impl Print for BreakStmt {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push("break;");
    }
}

impl Print for ContinueStmt {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push("continue;");
    }
}

impl Print for ExprStmt {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        self.expr.print(p);
        p.push(";");
    }
}

impl Print for ExprId {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push("(");
        p.type_(*self).print(p);
        p.push(")");
        p.push("(");
        match p.lookup(*self) {
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
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        self.lhs.print(p);
        p.push(" ");
        self.op.print(p);
        p.push(" ");
        self.rhs.print(p);
    }
}

impl Print for BinOpKind {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
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
            BinOpKind::Dot => p.push("."),
            BinOpKind::Unknown => p.push("<?>"),
        }
    }
}

impl Print for TernaryExpr {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        self.cond.print(p);
        p.push(" ? ");
        self.true_expr.print(p);
        p.push(" : ");
        self.false_expr.print(p);
    }
}

impl Print for UnaryExpr {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        self.op.print(p);
        self.operand.print(p);
    }
}

impl Print for UnaryOpKind {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        match self {
            UnaryOpKind::Minus => p.push("-"),
        }
    }
}

impl Print for SubscriptExpr {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        self.lhs.print(p);
        p.push("[");
        self.subscript.print(p);
        p.push("]");
    }
}

impl Print for CallExpr {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        self.lhs.print(p);
        p.push("(");
        self.args.iter().print_delimited(p, ", ");
        p.push(")");
    }
}

impl Print for ParenExpr {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push("(");
        self.expr.print(p);
        p.push(")");
    }
}

impl Print for LambdaExpr {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push("fn (");
        self.params.iter().print_delimited(p, ", ");
        p.push(") ");
        self.block.print(p);
    }
}

impl Print for StrExpr {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push("\"");
        for shard in &self.shards {
            shard.print(p);
        }
        p.push("\"");
    }
}

impl Print for StringShardId {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        match p.lookup(*self) {
            StringShard::Str { val, .. } => p.push(val),
            StringShard::Expr { expr, .. } => {
                p.push("${");
                expr.print(p);
                p.push("}");
            }
        }
    }
}

impl Print for Literal {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push(&match self {
            Literal::Number(x) => x.value.to_string(),
            Literal::Bool(x) => x.value.to_string(),
        })
    }
}

impl Print for VarDeclId {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        let var_decl = p.lookup(*self);
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
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
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

impl Print for BlockId {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        let block = p.lookup(*self);
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
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push(&p.lookup(*self).name);
    }
}

impl Print for NameRef {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push(&self.name);
    }
}

impl Print for SymbolTable {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
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
                Symbol::Local(x) => p.name_type(*x).print(p),
                Symbol::Global(x) => x.print(p),
            }
            p.push(",\n");
        }
        p.unindent();
        p.push_indent();
        p.push("}\n");
    }
}

impl Print for InferredType {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        self.narrowest.print(p);
    }
}

impl Print for Type {
    fn print<'a>(&self, p: &mut impl Printer<'a>) {
        p.push(&self.to_string(p.indent_level()));
    }
}
