use std::rc::Rc;

use itertools::Itertools;
use tower_lsp::lsp_types::{FormattingOptions, Range, TextEdit};

use crate::{
    ast::{self, AsSyntax, AstChildren, AstNode},
    db::Database,
    doc::Doc,
    syntax_node::{Token, TokenKind},
};

impl Doc {
    pub fn format(&self, db: &Database, opts: FormattingOptions) -> Vec<TextEdit> {
        let root = self.hir(db);

        let syntax = root.node.syntax();
        let start = self.pos_at(db, syntax.offset);
        let end = self.pos_at(db, syntax.end());

        vec![TextEdit {
            range: Range::new(start, end),
            new_text: Formatter::format(db, *self, opts, &root.node),
        }]
    }
}

#[derive(Debug)]
struct Formatter<'a> {
    indent: usize,
    db: &'a Database,
    doc: Doc,
    opts: FormattingOptions,
}

impl<'a> Formatter<'a> {
    pub fn format(
        db: &'a Database,
        doc: Doc,
        opts: FormattingOptions,
        root: &ast::Script,
    ) -> String {
        let mut this = Self::new(db, doc, opts);
        this.script(root)
    }

    pub fn new(db: &'a Database, doc: Doc, opts: FormattingOptions) -> Self {
        Self { indent: 0, db, doc, opts }
    }

    fn script(&mut self, script: &ast::Script) -> String {
        format!(
            "name{} {}{};{}\n{}",
            self.comments_between(script.name_token().as_deref(), script.name().as_ref()),
            self.name(script.name().as_ref()),
            self.comments_between(script.name().as_ref(), script.semi().as_deref()),
            self.comments_between(script.semi().as_deref(), script.items().next().as_ref()),
            self.item_list(script.items()),
        )
    }

    fn item_list(&mut self, items: AstChildren<ast::Item>) -> String {
        items
            .map(|x| match x {
                ast::Item::FnDecl(x) => format!("\n{}", self.fn_decl(&x)),
                ast::Item::BlockType(x) => format!("\n{}", self.block_type(&x)),
                ast::Item::VarDeclStmt(x) => {
                    format!(
                        "{}{};",
                        self.var_decl(x.var_decl().as_ref()),
                        self.comments_between(x.var_decl().as_ref(), x.semi().as_deref()),
                    )
                }
            })
            .join("\n")
    }

    fn fn_decl(&mut self, fn_decl: &ast::FnDeclItem) -> String {
        let name = self.name(fn_decl.name().as_ref());
        if name.is_empty() {
            format!(
                "fn{} {}{} {}",
                self.comments_between(fn_decl.fn_token().as_deref(), fn_decl.param_list().as_ref()),
                self.param_list(fn_decl.param_list().as_ref()),
                self.comments_between(fn_decl.param_list().as_ref(), fn_decl.block().as_ref()),
                self.block(fn_decl.block().as_ref())
            )
        } else {
            format!(
                "fn{} {name}{}{}{} {}",
                self.comments_between(fn_decl.fn_token().as_deref(), fn_decl.name().as_ref()),
                self.comments_between(fn_decl.name().as_ref(), fn_decl.param_list().as_ref()),
                self.param_list(fn_decl.param_list().as_ref()),
                self.comments_between(fn_decl.param_list().as_ref(), fn_decl.block().as_ref()),
                self.block(fn_decl.block().as_ref())
            )
        }
    }

    fn block_type(&mut self, block_type: &ast::BlockTypeItem) -> String {
        format!(
            "{}{} {}",
            self.token(block_type.blocktype().as_ref()),
            self.comments_between(block_type.blocktype().as_deref(), block_type.block().as_ref()),
            self.block(block_type.block().as_ref())
        )
    }

    fn stmt(&mut self, stmt: &ast::Stmt) -> String {
        let indent = self.indent_str();
        match stmt {
            ast::Stmt::Block(x) => format!("\n{indent}{}", self.block(Some(x))),
            ast::Stmt::VarDecl(x) => {
                if x.export().is_some() {
                    format!(
                        "\n{indent}export{} {}{};",
                        self.comments_between(x.export().as_deref(), x.var_decl().as_ref()),
                        self.var_decl(x.var_decl().as_ref()),
                        self.comments_between(x.var_decl().as_ref(), x.semi().as_deref(),),
                    )
                } else {
                    format!(
                        "\n{indent}{}{};",
                        self.var_decl(x.var_decl().as_ref()),
                        self.comments_between(x.var_decl().as_ref(), x.semi().as_deref(),),
                    )
                }
            }
            ast::Stmt::Expr(x) => format!(
                "\n{indent}{}{};",
                self.expr(x.expr().as_ref()),
                self.comments_between(x.expr().as_ref(), x.semi().as_deref(),),
            ),
            ast::Stmt::For(x) => format!("\n{indent}{}", self.stmt_for(x)),
            ast::Stmt::ForEach(x) => format!("\n{indent}{}", self.stmt_for_each(x)),
            ast::Stmt::If(x) => format!("\n{indent}{}", self.stmt_if(x)),
            ast::Stmt::While(x) => format!("\n{indent}{}", self.stmt_while(x)),
            ast::Stmt::Return(x) => {
                if let Some(expr) = &x.expr() {
                    format!(
                        "\n{indent}return{} {}{};",
                        self.comments_between(x.ret_kw().as_deref(), Some(expr)),
                        self.expr(Some(expr)),
                        self.comments_between(Some(expr), x.semi().as_deref()),
                    )
                } else {
                    format!(
                        "\n{indent}return{};",
                        self.comments_between(x.ret_kw().as_deref(), x.semi().as_deref()),
                    )
                }
            }
            ast::Stmt::Break(x) => format!(
                "\n{indent}break{};",
                self.comments_between(x.break_kw().as_deref(), x.semi().as_deref()),
            ),
            ast::Stmt::Continue(x) => {
                format!(
                    "\n{indent}continue{};",
                    self.comments_between(x.continue_kw().as_deref(), x.semi().as_deref()),
                )
            }
            ast::Stmt::Empty(_) => "".into(),
        }
    }

    fn stmt_for(&mut self, stmt_for: &ast::ForStmt) -> String {
        let init = if let Some(init) = &stmt_for.init() {
            format!(
                "{}{}{}",
                self.comments_between(stmt_for.lparen().as_deref(), Some(init)),
                self.var_decl(Some(init)),
                self.comments_between(Some(init), stmt_for.semi_1().as_deref()),
            )
        } else {
            self.comments_between(stmt_for.lparen().as_deref(), stmt_for.semi_1().as_deref())
        };
        let cond = if let Some(cond) = &stmt_for.cond() {
            format!(
                "{} {}{}",
                self.comments_between(stmt_for.semi_1().as_deref(), Some(cond)),
                self.expr(Some(cond)),
                self.comments_between(Some(cond), stmt_for.semi_2().as_deref()),
            )
        } else {
            self.comments_between(stmt_for.lparen().as_deref(), stmt_for.semi_1().as_deref())
        };
        let loop_expr = if let Some(loop_expr) = &stmt_for.loop_expr() {
            format!(
                "{} {}{}",
                self.comments_between(stmt_for.semi_2().as_deref(), Some(loop_expr)),
                self.expr(Some(loop_expr)),
                self.comments_between(Some(loop_expr), stmt_for.rparen().as_deref()),
            )
        } else {
            self.comments_between(stmt_for.semi_1().as_deref(), stmt_for.semi_2().as_deref())
        };
        format!(
            "for{} ({init};{cond};{loop_expr}){} {}",
            self.comments_between(stmt_for.for_kw().as_deref(), stmt_for.lparen().as_deref()),
            self.comments_between(stmt_for.rparen().as_deref(), stmt_for.block().as_ref()),
            self.block(stmt_for.block().as_ref()),
        )
    }

    fn stmt_for_each(&mut self, stmt_for_each: &ast::ForEachStmt) -> String {
        format!(
            "for{} ({}{}{} in{} {}{}){} {}",
            self.comments_between(
                stmt_for_each.for_kw().as_deref(),
                stmt_for_each.lparen().as_deref(),
            ),
            self.comments_between(stmt_for_each.lparen().as_deref(), stmt_for_each.pat().as_ref(),),
            self.pat(stmt_for_each.pat().as_ref()),
            self.comments_between(stmt_for_each.pat().as_ref(), stmt_for_each.in_kw().as_deref(),),
            self.comments_between(
                stmt_for_each.in_kw().as_deref(),
                stmt_for_each.iterable().as_ref(),
            ),
            self.expr(stmt_for_each.iterable().as_ref()),
            self.comments_between(
                stmt_for_each.iterable().as_ref(),
                stmt_for_each.rparen().as_deref(),
            ),
            self.comments_between(
                stmt_for_each.rparen().as_deref(),
                stmt_for_each.block().as_ref(),
            ),
            self.block(stmt_for_each.block().as_ref()),
        )
    }

    fn stmt_if(&mut self, stmt_if: &ast::IfStmt) -> String {
        let mut base = format!(
            "if{} ({}{}{}){} {}",
            self.comments_between(stmt_if.if_kw().as_deref(), stmt_if.lparen().as_deref()),
            self.comments_between(stmt_if.lparen().as_deref(), stmt_if.cond().as_ref()),
            self.expr(stmt_if.cond().as_ref()),
            self.comments_between(stmt_if.cond().as_ref(), stmt_if.rparen().as_deref()),
            self.comments_between(stmt_if.rparen().as_deref(), stmt_if.true_branch().as_ref()),
            self.block(stmt_if.true_branch().as_ref()),
        );

        match &stmt_if.false_branch() {
            Some(ast::ElseBranch::IfStmt(x)) => base.push_str(&format!(
                "{} else{} {}",
                self.comments_between(stmt_if.true_branch().as_ref(), stmt_if.else_kw().as_deref()),
                self.comments_between(stmt_if.else_kw().as_deref(), Some(x)),
                self.stmt_if(x),
            )),
            Some(ast::ElseBranch::Block(x)) => base.push_str(&format!(
                "{} else{} {}",
                self.comments_between(stmt_if.true_branch().as_ref(), stmt_if.else_kw().as_deref()),
                self.comments_between(stmt_if.else_kw().as_deref(), Some(x)),
                self.block(Some(x)),
            )),
            None => (),
        }

        base
    }

    fn stmt_while(&mut self, stmt_while: &ast::WhileStmt) -> String {
        format!(
            "while{} ({}{}{}){} {}",
            self.comments_between(stmt_while.while_kw().as_deref(), stmt_while.lparen().as_deref()),
            self.comments_between(stmt_while.lparen().as_deref(), stmt_while.cond().as_ref()),
            self.expr(stmt_while.cond().as_ref()),
            self.comments_between(stmt_while.cond().as_ref(), stmt_while.rparen().as_deref()),
            self.comments_between(stmt_while.rparen().as_deref(), stmt_while.block().as_ref()),
            self.block(stmt_while.block().as_ref()),
        )
    }

    fn expr(&mut self, expr: Option<&ast::Expr>) -> String {
        expr.map(|x| match x {
            ast::Expr::Binary(x) => self.expr_bin(x),
            ast::Expr::Ternary(x) => self.expr_ternary(x),
            ast::Expr::Unary(x) => self.expr_unary(x),
            ast::Expr::Postfix(x) => self.expr_postfix(x),
            ast::Expr::Field(x) => self.expr_field(x),
            ast::Expr::Subscript(x) => self.expr_subscript(x),
            ast::Expr::Call(x) => self.expr_call(x),
            ast::Expr::Paren(x) => self.expr_paren(x),
            ast::Expr::Lambda(x) => self.expr_lambda(x),
            ast::Expr::NameRef(x) => self.expr_name_ref(x).into(),
            ast::Expr::Str(x) => self.expr_str(x),
            ast::Expr::Lit(x) => self.expr_lit(x).into(),
        })
        .unwrap_or_default()
    }

    fn expr_bin(&mut self, expr: &ast::BinaryExpr) -> String {
        format!(
            "{}{} {}{} {}",
            self.expr(expr.lhs().as_ref()),
            self.comments_between(expr.lhs().as_ref(), expr.op().as_deref()),
            self.token(expr.op().as_ref()),
            self.comments_between(expr.op().as_deref(), expr.rhs().as_ref()),
            self.expr(expr.rhs().as_ref()),
        )
    }

    fn expr_ternary(&mut self, expr: &ast::TernaryExpr) -> String {
        format!(
            "{}{} ?{} {}{} :{} {}",
            self.comments_between(expr.cond().as_ref(), expr.question_mark().as_deref()),
            self.expr(expr.cond().as_ref()),
            self.comments_between(expr.question_mark().as_deref(), expr.true_expr().as_ref()),
            self.expr(expr.true_expr().as_ref()),
            self.comments_between(expr.true_expr().as_ref(), expr.colon().as_deref()),
            self.expr(expr.false_expr().as_ref()),
            self.comments_between(expr.colon().as_deref(), expr.false_expr().as_ref()),
        )
    }

    fn expr_unary(&mut self, expr: &ast::UnaryExpr) -> String {
        format!(
            "{}{}{}",
            self.token(expr.op().as_ref()),
            self.comments_between(expr.op().as_deref(), expr.operand().as_ref()),
            self.expr(expr.operand().as_ref()),
        )
    }

    fn expr_postfix(&mut self, expr: &ast::PostfixExpr) -> String {
        format!(
            "{}{}{}",
            self.expr(expr.operand().as_ref()),
            self.comments_between(expr.operand().as_ref(), expr.op().as_deref()),
            self.token(expr.op().as_ref()),
        )
    }

    fn expr_field(&mut self, expr: &ast::FieldExpr) -> String {
        format!(
            "{}{}.{}{}",
            self.expr(expr.lhs().as_ref()),
            self.comments_between(expr.lhs().as_ref(), expr.dot().as_deref()),
            self.comments_between(expr.dot().as_deref(), expr.field().as_ref()),
            expr.field().as_ref().map(|x| self.expr_name_ref(x)).unwrap_or_default(),
        )
    }

    fn expr_subscript(&mut self, expr: &ast::SubscriptExpr) -> String {
        format!(
            "{}{}[{}{}{}]",
            self.expr(expr.lhs().as_ref()),
            self.comments_between(expr.lhs().as_ref(), expr.lsqbracket().as_deref()),
            self.comments_between(expr.lsqbracket().as_deref(), expr.subscript().as_ref()),
            self.expr(expr.subscript().as_ref()),
            self.comments_between(expr.subscript().as_ref(), expr.rsqbracket().as_deref()),
        )
    }

    fn expr_call(&mut self, expr: &ast::CallExpr) -> String {
        format!(
            "{}{}{}",
            self.expr(expr.lhs().as_ref()),
            self.comments_between(expr.lhs().as_ref(), expr.args().as_ref()),
            self.arg_list(expr.args().as_ref()),
        )
    }

    fn expr_paren(&mut self, expr: &ast::ParenExpr) -> String {
        format!(
            "({}{}{})",
            self.comments_between(expr.lparen().as_deref(), expr.expr().as_ref()),
            self.expr(expr.expr().as_ref()),
            self.comments_between(expr.expr().as_ref(), expr.rparen().as_deref()),
        )
    }

    fn expr_lambda(&mut self, expr: &ast::LambdaExpr) -> String {
        format!(
            "fn{} {}{} {}",
            self.comments_between(expr.fn_kw().as_deref(), expr.params().as_ref()),
            self.param_list(expr.params().as_ref()),
            self.comments_between(expr.params().as_ref(), expr.block().as_ref()),
            self.block(expr.block().as_ref()),
        )
    }

    fn expr_name_ref(&mut self, expr: &ast::NameRef) -> &'a str {
        self.token(expr.ident().as_ref())
    }

    fn expr_str(&mut self, expr: &ast::StrExpr) -> String {
        format!(
            "\"{}\"",
            expr.shards()
                .map(|x| match &x {
                    ast::StringShard::Literal(x) => self.token(x.token().as_ref()).to_string(),
                    ast::StringShard::Expr(x) => format!(
                        "${{{}{}{}}}",
                        self.comments_between(x.dollar_lbrace().as_deref(), x.expr().as_ref()),
                        self.expr(x.expr().as_ref()),
                        self.comments_between(x.expr().as_ref(), x.rbrace().as_deref()),
                    ),
                })
                .collect::<String>()
        )
    }

    fn expr_lit(&mut self, expr: &ast::Literal) -> &'a str {
        self.token(expr.lit().as_ref())
    }

    fn block(&mut self, block: Option<&ast::BlockStmt>) -> String {
        block
            .map(|x| {
                if x.stmts().len() == 0 {
                    format!(
                        "{{{}}}",
                        self.comments_between(x.lbrack().as_deref(), x.rbrack().as_deref())
                    )
                } else {
                    self.indent += 1;
                    let first_stmt = x.stmts().next().unwrap();
                    format!(
                        "{{{}{}{}\n{}{}}}",
                        self.comments_between(x.lbrack().as_deref(), Some(&first_stmt)).trim_end(),
                        self.stmt(&first_stmt),
                        &x.stmts()
                            .map_windows(|[a, b]| {
                                format!(
                                    "{}{}",
                                    self.comments_in_range(a.syntax().end(), b.syntax().offset),
                                    self.stmt(b)
                                )
                            })
                            .collect::<String>(),
                        self.comments_between(x.stmts().last().as_ref(), x.rbrack().as_deref(),)
                            .trim_end(),
                        {
                            self.indent -= 1;
                            self.indent_str()
                        },
                    )
                }
            })
            .unwrap_or_default()
    }

    fn param_list(&mut self, param_list: Option<&ast::ParamList>) -> String {
        param_list
            .map(|x| {
                format!(
                    "({}{}{})",
                    self.comments_between(x.lparen().as_deref(), x.params().next().as_ref()),
                    x.params().map(|x| self.var_decl(Some(&x))).join(", "),
                    self.comments_between(x.params().last().as_ref(), x.rparen().as_deref()),
                )
            })
            .unwrap_or_default()
    }

    fn arg_list(&mut self, arg_list: Option<&ast::ArgList>) -> String {
        arg_list
            .map(|x| {
                format!(
                    "({}{}{})",
                    self.comments_between(x.lparen().as_deref(), x.args().next().as_ref()),
                    x.args().map(|x| self.expr(Some(&x))).join(", "),
                    self.comments_between(x.args().last().as_ref(), x.rparen().as_deref()),
                )
            })
            .unwrap_or_default()
    }

    fn pat(&mut self, pat: Option<&ast::Pat>) -> String {
        pat.map(|x| match x {
            ast::Pat::VarDecl(x) => self.var_decl(Some(x)),
            ast::Pat::Arr(x) => self.pat_arr(x),
        })
        .unwrap_or_default()
    }

    fn pat_arr(&mut self, pat_arr: &ast::PatArr) -> String {
        format!(
            "[{}{}{}]",
            self.comments_between(pat_arr.lsqbrack().as_deref(), pat_arr.patts().next().as_ref()),
            pat_arr.patts().map(|x| self.pat(Some(&x))).join(", "),
            self.comments_between(pat_arr.patts().last().as_ref(), pat_arr.rsqbrack().as_deref()),
        )
    }

    fn var_decl(&mut self, var_decl: Option<&ast::VarDecl>) -> String {
        var_decl
            .map(|x| {
                if let Some(init) = &x.init() {
                    format!(
                        "{}{} {}{} ={} {}",
                        self.token(x.r#type().as_ref()),
                        self.comments_between(x.r#type().as_deref(), x.name().as_ref()),
                        self.name(x.name().as_ref()),
                        self.comments_between(x.name().as_ref(), x.eq().as_deref()),
                        self.comments_between(x.eq().as_deref(), Some(init)),
                        self.expr(Some(init)),
                    )
                } else {
                    format!(
                        "{}{} {}",
                        self.token(x.r#type().as_ref()),
                        self.comments_between(x.r#type().as_deref(), x.name().as_ref()),
                        self.name(x.name().as_ref()),
                    )
                }
            })
            .unwrap_or_default()
    }

    fn name(&mut self, name: Option<&ast::Name>) -> &'a str {
        name.map(|x| self.token(x.ident().as_ref())).unwrap_or_default()
    }

    fn token(&mut self, token: Option<&Rc<Token>>) -> &'a str {
        token.map(|x| x.text(self.text())).unwrap_or_default()
    }

    fn comments_between(&self, start: Option<impl AsSyntax>, end: Option<impl AsSyntax>) -> String {
        match (start, end) {
            (Some(a), Some(b)) => {
                self.comments_in_range(a.as_syntax().end(), b.as_syntax().offset())
            }
            _ => "".into(),
        }
    }

    fn comments_in_range(&self, start: u32, end: u32) -> String {
        let mut buf = String::new();
        let mut last_end = start;
        for comment in self
            .doc
            .hir(self.db)
            .node
            .syntax()
            .tokens_within_range(start, end)
            .filter(|x| matches!(x.kind, TokenKind::Comment))
        {
            let text_before = &self.text()[last_end as usize..comment.offset as usize];

            if text_before.contains('\n') {
                buf.push_str(text_before.trim_end_matches(' '));
                buf.push_str(&self.indent_str());
                buf.push_str(comment.text(self.text()));
            } else {
                let mut next = comment;
                let is_last_comment = loop {
                    if let Some(next_token) = next.next_token() {
                        if next_token.kind != TokenKind::Whitespace
                            && next_token.kind != TokenKind::Comment
                        {
                            break false;
                        }
                        if next_token.kind == TokenKind::Whitespace
                            && next_token.text(self.text()).contains('\n')
                        {
                            break true;
                        }
                        next = next_token;
                    } else {
                        break true;
                    }
                };
                if is_last_comment {
                    if text_before.is_empty() {
                        buf.push(' ');
                    } else {
                        buf.push_str(text_before);
                    }
                } else {
                    buf.push(' ');
                }
                buf.push_str(comment.text(self.text()));
            }
            last_end = comment.end();
        }

        buf.push_str(&self.newlines_between(last_end, end));

        buf
    }

    fn newlines_between(&self, start: u32, end: u32) -> String {
        if end < start {
            return "".into();
        }
        "\n".repeat(
            self.text()
                .chars()
                .skip(start as _)
                .take((end - start) as _)
                .filter(|x| *x == '\n')
                .count()
                .saturating_sub(1)
                .min(1),
        )
    }

    fn text(&self) -> &'a str {
        self.doc.text(self.db)
    }

    fn indent_str(&self) -> String {
        if self.opts.insert_spaces {
            " ".repeat(self.opts.tab_size as _).repeat(self.indent)
        } else {
            "\t".repeat(self.indent)
        }
    }
}
