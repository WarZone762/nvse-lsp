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
            "name{} {}{};{}{}{}",
            self.comments_between(&script.name_kw(), &script.name()),
            self.name(&script.name()),
            self.comments_between(&script.name(), &script.semi()),
            self.comments_between(&script.semi(), &script.items().next()),
            self.item_list(script.items()),
            if let Some(last_item) = script.items().last() {
                self.comments_in_range(last_item.syntax().end(), script.syntax().end())
            } else {
                "".to_string()
            }
        )
    }

    fn item_list(&mut self, items: AstChildren<ast::Item>) -> String {
        let Some(first_item) = items.clone().next() else {
            return "".into();
        };

        format!(
            "\n{}{}",
            self.item(&first_item),
            items
                .map_windows(|[a, b]| {
                    format!(
                        "{}\n{}",
                        self.comments_in_range(a.syntax().end(), b.syntax().offset),
                        self.item(b),
                    )
                })
                .collect::<String>()
        )
    }

    fn item(&mut self, item: &ast::Item) -> String {
        match item {
            ast::Item::FnDecl(x) => self.fn_decl(x),
            ast::Item::BlockType(x) => self.block_type(x),
            ast::Item::VarDecl(x) => {
                format!(
                    "{}{};",
                    self.var_decl(&x.var_decl()),
                    self.comments_between(&x.var_decl(), &x.semi()),
                )
            }
        }
    }

    fn fn_decl(&mut self, fn_decl: &ast::FnDeclItem) -> String {
        let name = self.name(&fn_decl.name());
        if name.is_empty() {
            format!(
                "fn{} {}{} {}",
                self.comments_between(&fn_decl.fn_kw(), &fn_decl.param_list()),
                self.param_list(&fn_decl.param_list()),
                self.comments_between(&fn_decl.param_list(), &fn_decl.block()),
                self.block(&fn_decl.block())
            )
        } else {
            format!(
                "fn{} {name}{}{}{} {}",
                self.comments_between(&fn_decl.fn_kw(), &fn_decl.name()),
                self.comments_between(&fn_decl.name(), &fn_decl.param_list()),
                self.param_list(&fn_decl.param_list()),
                self.comments_between(&fn_decl.param_list(), &fn_decl.block()),
                self.block(&fn_decl.block())
            )
        }
    }

    fn block_type(&mut self, block_type: &ast::BlockTypeItem) -> String {
        format!(
            "{}{} {}",
            self.token(&block_type.blocktype()),
            self.comments_between(&block_type.blocktype(), &block_type.block()),
            self.block(&block_type.block())
        )
    }

    fn stmt(&mut self, stmt: &ast::Stmt) -> String {
        let indent = self.indent_str();
        match stmt {
            ast::Stmt::Block(x) => format!("\n{indent}{}", self.block(x)),
            ast::Stmt::VarDecl(x) => {
                if x.export().is_some() {
                    format!(
                        "\n{indent}export{} {}{};",
                        self.comments_between(&x.export(), &x.var_decl()),
                        self.var_decl(&x.var_decl()),
                        self.comments_between(&x.var_decl(), &x.semi(),),
                    )
                } else {
                    format!(
                        "\n{indent}{}{};",
                        self.var_decl(&x.var_decl()),
                        self.comments_between(&x.var_decl(), &x.semi(),),
                    )
                }
            }
            ast::Stmt::Expr(x) => format!(
                "\n{indent}{}{};",
                self.expr(&x.expr()),
                self.comments_between(&x.expr(), &x.semi(),),
            ),
            ast::Stmt::For(x) => format!("\n{indent}{}", self.stmt_for(x)),
            ast::Stmt::ForEach(x) => format!("\n{indent}{}", self.stmt_for_range(x)),
            ast::Stmt::If(x) => format!("\n{indent}{}", self.stmt_if(x)),
            ast::Stmt::Match(x) => format!("\n{indent}{}", self.stmt_match(x)),
            ast::Stmt::While(x) => format!("\n{indent}{}", self.stmt_while(x)),
            ast::Stmt::Return(x) => {
                if let Some(expr) = &x.expr() {
                    format!(
                        "\n{indent}return{} {}{};",
                        self.comments_between(&x.ret_kw(), expr),
                        self.expr(expr),
                        self.comments_between(expr, &x.semi()),
                    )
                } else {
                    format!("\n{indent}return{};", self.comments_between(&x.ret_kw(), &x.semi()),)
                }
            }
            ast::Stmt::Break(x) => {
                format!("\n{indent}break{};", self.comments_between(&x.break_kw(), &x.semi()),)
            }
            ast::Stmt::Continue(x) => {
                format!("\n{indent}continue{};", self.comments_between(&x.continue_kw(), &x.semi()),)
            }
            ast::Stmt::Empty(_) => "".into(),
        }
    }

    fn stmt_for(&mut self, stmt_for: &ast::ForStmt) -> String {
        let init = if let Some(init) = &stmt_for.init() {
            format!(
                "{}{}{}",
                self.comments_between(&stmt_for.lparen(), init),
                self.var_decl(init),
                self.comments_between(init, &stmt_for.semi_1()),
            )
        } else {
            self.comments_between(&stmt_for.lparen(), &stmt_for.semi_1())
        };
        let cond = if let Some(cond) = &stmt_for.cond() {
            format!(
                "{} {}{}",
                self.comments_between(&stmt_for.semi_1(), cond),
                self.expr(cond),
                self.comments_between(cond, &stmt_for.semi_2()),
            )
        } else {
            self.comments_between(&stmt_for.lparen(), &stmt_for.semi_1())
        };
        let loop_expr = if let Some(loop_expr) = &stmt_for.loop_expr() {
            format!(
                "{} {}{}",
                self.comments_between(&stmt_for.semi_2(), loop_expr),
                self.expr(loop_expr),
                self.comments_between(loop_expr, &stmt_for.rparen()),
            )
        } else {
            self.comments_between(&stmt_for.semi_1(), &stmt_for.semi_2())
        };
        format!(
            "for{} ({init};{cond};{loop_expr}){} {}",
            self.comments_between(&stmt_for.for_kw(), &stmt_for.lparen()),
            self.comments_between(&stmt_for.rparen(), &stmt_for.block()),
            self.block(&stmt_for.block()),
        )
    }

    fn stmt_for_range(&mut self, stmt_for_each: &ast::ForRangeStmt) -> String {
        format!(
            "for{} ({}{}{} in{} {}{}){} {}",
            self.comments_between(&stmt_for_each.for_kw(), &stmt_for_each.lparen(),),
            self.comments_between(&stmt_for_each.lparen(), &stmt_for_each.pat(),),
            self.pat(&stmt_for_each.pat()),
            self.comments_between(&stmt_for_each.pat(), &stmt_for_each.in_kw(),),
            self.comments_between(&stmt_for_each.in_kw(), &stmt_for_each.iterable(),),
            self.expr(&stmt_for_each.iterable()),
            self.comments_between(&stmt_for_each.iterable(), &stmt_for_each.rparen(),),
            self.comments_between(&stmt_for_each.rparen(), &stmt_for_each.block(),),
            self.block(&stmt_for_each.block()),
        )
    }

    fn stmt_if(&mut self, stmt_if: &ast::IfStmt) -> String {
        let mut base = format!(
            "if{} ({}{}{}){} {}",
            self.comments_between(&stmt_if.if_kw(), &stmt_if.lparen()),
            self.comments_between(&stmt_if.lparen(), &stmt_if.cond()),
            self.expr(&stmt_if.cond()),
            self.comments_between(&stmt_if.cond(), &stmt_if.rparen()),
            self.comments_between(&stmt_if.rparen(), &stmt_if.true_branch()),
            self.block(&stmt_if.true_branch()),
        );

        match &stmt_if.false_branch() {
            Some(ast::ElseBranch::IfStmt(x)) => base.push_str(&format!(
                "{} else{} {}",
                self.comments_between(&stmt_if.true_branch(), &stmt_if.else_kw()),
                self.comments_between(&stmt_if.else_kw(), x),
                self.stmt_if(x),
            )),
            Some(ast::ElseBranch::Block(x)) => base.push_str(&format!(
                "{} else{} {}",
                self.comments_between(&stmt_if.true_branch(), &stmt_if.else_kw()),
                self.comments_between(&stmt_if.else_kw(), x),
                self.block(x),
            )),
            None => (),
        }

        base
    }

    fn stmt_match(&mut self, stmt_match: &ast::MatchStmt) -> String {
        self.indent += 1;
        format!(
            "match{} ({}{}{}){} {{{}{}{}\n{}}}",
            self.comments_between(&stmt_match.match_kw(), &stmt_match.lparen()),
            self.comments_between(&stmt_match.lparen(), &stmt_match.expr()),
            self.expr(&stmt_match.expr()),
            self.comments_between(&stmt_match.expr(), &stmt_match.rparen()),
            self.comments_between(&stmt_match.rparen(), &stmt_match.lbrack()),
            self.comments_between(&stmt_match.lbrack(), &stmt_match.arms().next()),
            self.match_arms(stmt_match.arms()),
            self.comments_between(&stmt_match.arms().last(), &stmt_match.rbrack()),
            {
                self.indent -= 1;
                self.indent_str()
            }
        )
    }

    fn match_arms(&mut self, arms: AstChildren<ast::MatchArm>) -> String {
        let Some(first_arm) = arms.clone().next() else {
            return "".into();
        };

        format!(
            "\n{}{}{}",
            self.indent_str(),
            self.match_arm(&first_arm),
            arms.map_windows(|[a, b]| {
                format!(
                    "{}\n{}{}",
                    self.comments_between(&a.block(), &b.pat()),
                    self.indent_str(),
                    self.match_arm(b),
                )
            })
            .collect::<String>()
        )
    }

    fn match_arm(&mut self, arm: &ast::MatchArm) -> String {
        format!(
            "{}{} ->{} {}",
            self.pat(&arm.pat()),
            self.comments_between(&arm.pat(), &arm.rarrow()),
            self.comments_between(&arm.rarrow(), &arm.block()),
            self.block(&arm.block()),
        )
    }

    fn stmt_while(&mut self, stmt_while: &ast::WhileStmt) -> String {
        format!(
            "while{} ({}{}{}){} {}",
            self.comments_between(&stmt_while.while_kw(), &stmt_while.lparen()),
            self.comments_between(&stmt_while.lparen(), &stmt_while.cond()),
            self.expr(&stmt_while.cond()),
            self.comments_between(&stmt_while.cond(), &stmt_while.rparen()),
            self.comments_between(&stmt_while.rparen(), &stmt_while.block()),
            self.block(&stmt_while.block()),
        )
    }

    fn expr<'b>(&'b mut self, expr: impl Into<Option<&'b ast::Expr>>) -> String {
        expr.into()
            .map(|x| match x {
                ast::Expr::Binary(x) => self.expr_bin(x),
                ast::Expr::Ternary(x) => self.expr_ternary(x),
                ast::Expr::Unary(x) => self.expr_unary(x),
                ast::Expr::Postfix(x) => self.expr_postfix(x),
                ast::Expr::Field(x) => self.expr_field(x),
                ast::Expr::Subscript(x) => self.expr_subscript(x),
                ast::Expr::Call(x) => self.expr_call(x),
                ast::Expr::Paren(x) => self.expr_paren(x),
                ast::Expr::Lambda(x) => self.expr_lambda(x),
                ast::Expr::NameRef(x) => self.name_ref(x).into(),
                ast::Expr::Str(x) => self.expr_str(x),
                ast::Expr::LitArr(x) => self.lit_arr(x),
                ast::Expr::LitMap(x) => self.lit_map(x),
                ast::Expr::Literal(x) => self.literal(x).into(),
            })
            .unwrap_or_default()
    }

    fn expr_bin(&mut self, expr: &ast::BinExpr) -> String {
        format!(
            "{}{} {}{} {}",
            self.expr(&expr.lhs()),
            self.comments_between(&expr.lhs(), &expr.op()),
            self.token(&expr.op()),
            self.comments_between(&expr.op(), &expr.rhs()),
            self.expr(&expr.rhs()),
        )
    }

    fn expr_ternary(&mut self, expr: &ast::TernaryExpr) -> String {
        format!(
            "{}{} ?{} {}{} :{} {}",
            self.comments_between(&expr.cond(), &expr.question_mark()),
            self.expr(&expr.cond()),
            self.comments_between(&expr.question_mark(), &expr.true_expr()),
            self.expr(&expr.true_expr()),
            self.comments_between(&expr.true_expr(), &expr.colon()),
            self.comments_between(&expr.colon(), &expr.false_expr()),
            self.expr(&expr.false_expr()),
        )
    }

    fn expr_unary(&mut self, expr: &ast::UnaryExpr) -> String {
        format!(
            "{}{}{}",
            self.token(&expr.op()),
            self.comments_between(&expr.op(), &expr.operand()),
            self.expr(&expr.operand()),
        )
    }

    fn expr_postfix(&mut self, expr: &ast::PostfixExpr) -> String {
        format!(
            "{}{}{}",
            self.expr(&expr.operand()),
            self.comments_between(&expr.operand(), &expr.op()),
            self.token(&expr.op()),
        )
    }

    fn expr_field(&mut self, expr: &ast::FieldExpr) -> String {
        format!(
            "{}{}.{}{}",
            self.expr(&expr.lhs()),
            self.comments_between(&expr.lhs(), &expr.dot()),
            self.comments_between(&expr.dot(), &expr.field()),
            expr.field().map(|x| self.name_ref(&x)).unwrap_or_default(),
        )
    }

    fn expr_subscript(&mut self, expr: &ast::SubscriptExpr) -> String {
        format!(
            "{}{}[{}{}{}]",
            self.expr(&expr.lhs()),
            self.comments_between(&expr.lhs(), &expr.lsqbrack()),
            self.comments_between(&expr.lsqbrack(), &expr.subscript()),
            self.expr(&expr.subscript()),
            self.comments_between(&expr.subscript(), &expr.rsqbrack()),
        )
    }

    fn expr_call(&mut self, expr: &ast::CallExpr) -> String {
        format!(
            "{}{}{}",
            self.expr(&expr.lhs()),
            self.comments_between(&expr.lhs(), &expr.args()),
            self.arg_list(&expr.args()),
        )
    }

    fn expr_paren(&mut self, expr: &ast::ParenExpr) -> String {
        format!(
            "({}{}{})",
            self.comments_between(&expr.lparen(), &expr.expr()),
            self.expr(&expr.expr()),
            self.comments_between(&expr.expr(), &expr.rparen()),
        )
    }

    fn expr_lambda(&mut self, expr: &ast::LambdaExpr) -> String {
        format!(
            "fn{} {}{} {}",
            self.comments_between(&expr.fn_kw(), &expr.params()),
            self.param_list(&expr.params()),
            self.comments_between(&expr.params(), &expr.block_or_expr()),
            self.block_or_expr(&expr.block_or_expr()),
        )
    }

    fn expr_str(&mut self, expr: &ast::StrExpr) -> String {
        format!(
            "\"{}\"",
            expr.shards()
                .map(|x| match &x {
                    ast::StrShard::Literal(x) => self.token(&x.token()).to_string(),
                    ast::StrShard::Expr(x) => format!(
                        "${{{}{}{}}}",
                        self.comments_between(&x.dollar_lbrack(), &x.expr()),
                        self.expr(&x.expr()),
                        self.comments_between(&x.expr(), &x.rbrack()),
                    ),
                })
                .collect::<String>()
        )
    }

    fn lit_arr(&mut self, lit: &ast::LitArr) -> String {
        format!(
            "[{}{}{}]",
            self.comments_between(&lit.lsq_brack(), &lit.exprs().next()),
            lit.exprs().map(|x| self.expr(&x)).join(", "),
            self.comments_between(&lit.exprs().last(), &lit.rsq_brack()),
        )
    }

    fn lit_map(&mut self, lit: &ast::LitMap) -> String {
        format!(
            "{{{}{}{}}}",
            self.comments_between(&lit.lbrack(), &lit.kv_pairs().next()),
            lit.kv_pairs().map(|x| self.kv_pair(&x)).join(", "),
            self.comments_between(&lit.kv_pairs().last(), &lit.rbrack()),
        )
    }

    fn kv_pair(&mut self, kv_pair: &ast::KVPair) -> String {
        format!(
            "{}{}::{}{}",
            self.expr(&kv_pair.key()),
            self.comments_between(&kv_pair.key(), &kv_pair.col2()),
            self.comments_between(&kv_pair.col2(), &kv_pair.value()),
            self.expr(&kv_pair.value()),
        )
    }

    fn literal(&mut self, expr: &ast::Literal) -> &'a str {
        self.token(&expr.literal())
    }

    fn block_or_expr<'b>(
        &mut self,
        block_or_expr: impl Into<Option<&'b ast::BlockOrExpr>>,
    ) -> String {
        block_or_expr
            .into()
            .map(|x| match x {
                ast::BlockOrExpr::Block(x) => self.block(x),
                ast::BlockOrExpr::Expr(x) => self.expr(x),
            })
            .unwrap_or_default()
    }

    fn block<'b>(&'b mut self, block: impl Into<Option<&'b ast::BlockStmt>>) -> String {
        block
            .into()
            .map(|x| {
                if x.stmts().len() == 0 {
                    format!("{{{}}}", self.comments_between(&x.lbrack(), &x.rbrack()))
                } else {
                    self.indent += 1;
                    let first_stmt = x.stmts().next().unwrap();
                    format!(
                        "{{{}{}{}{}\n{}}}",
                        self.comments_between(&x.lbrack(), &first_stmt).trim_end(),
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
                        self.comments_between(&x.stmts().last(), &x.rbrack(),).trim_end(),
                        {
                            self.indent -= 1;
                            self.indent_str()
                        },
                    )
                }
            })
            .unwrap_or_default()
    }

    fn param_list<'b>(&'b mut self, param_list: impl Into<Option<&'b ast::ParamList>>) -> String {
        param_list
            .into()
            .map(|x| {
                format!(
                    "({}{}{})",
                    self.comments_between(&x.lparen(), &x.params().next()),
                    x.params().map(|x| self.var_decl(&x)).join(", "),
                    self.comments_between(&x.params().last(), &x.rparen()),
                )
            })
            .unwrap_or_default()
    }

    fn arg_list<'b>(&'b mut self, arg_list: impl Into<Option<&'b ast::ArgList>>) -> String {
        arg_list
            .into()
            .map(|x| {
                format!(
                    "({}{}{})",
                    self.comments_between(&x.lparen(), &x.args().next()),
                    x.args().map(|x| self.expr(&x)).join(", "),
                    self.comments_between(&x.args().last(), &x.rparen()),
                )
            })
            .unwrap_or_default()
    }

    fn pat<'b>(&'b mut self, pat: impl Into<Option<&'b ast::Pat>>) -> String {
        pat.into()
            .map(|x| match x {
                ast::Pat::StrExpr(x) => self.expr_str(x),
                ast::Pat::Literal(x) => self.literal(x).to_string(),
                ast::Pat::VarDecl(x) => self.var_decl(x),
                ast::Pat::Arr(x) => self.arr_pat(x),
            })
            .unwrap_or_default()
    }

    fn arr_pat(&mut self, pat_arr: &ast::ArrPat) -> String {
        format!(
            "[{}{}{}]",
            self.comments_between(&pat_arr.lsqbrack(), &pat_arr.patts().next()),
            pat_arr.patts().map(|x| self.pat(&x)).join(", "),
            self.comments_between(&pat_arr.patts().last(), &pat_arr.rsqbrack()),
        )
    }

    fn var_decl<'b>(&'b mut self, var_decl: impl Into<Option<&'b ast::VarDecl>>) -> String {
        var_decl
            .into()
            .map(|x| {
                if let Some(init) = &x.init() {
                    format!(
                        "{}{} {}{} ={} {}",
                        self.token(&x.type_()),
                        self.comments_between(&x.type_(), &x.name()),
                        self.name(&x.name()),
                        self.comments_between(&x.name(), &x.eq()),
                        self.comments_between(&x.eq(), init),
                        self.expr(init),
                    )
                } else {
                    format!(
                        "{}{} {}",
                        self.token(&x.type_()),
                        self.comments_between(&x.type_(), &x.name()),
                        self.name(&x.name()),
                    )
                }
            })
            .unwrap_or_default()
    }

    fn name_ref(&mut self, expr: &ast::NameRef) -> &'a str {
        self.token(&expr.ident())
    }

    fn name<'b>(&'b mut self, name: impl Into<Option<&'b ast::Name>>) -> &'a str {
        name.into().map(|x| self.token(&x.ident())).unwrap_or_default()
    }

    fn token<'b>(&'b mut self, token: impl Into<Option<&'b Rc<Token>>>) -> &'a str {
        token.into().map(|x| x.text(self.text())).unwrap_or_default()
    }

    fn comments_between(&self, start: impl MaybeAsSyntax, end: impl MaybeAsSyntax) -> String {
        match (start.into_opt(), end.into_opt()) {
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
            .filter(|x| matches!(x.kind, TokenKind::COMMENT))
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
                        if next_token.kind != TokenKind::WHITESPACE
                            && next_token.kind != TokenKind::COMMENT
                        {
                            break false;
                        }
                        if next_token.kind == TokenKind::WHITESPACE
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

/// Helper trait for `comments_between`
trait MaybeAsSyntax {
    type AsSyntax: AsSyntax;

    fn into_opt(self) -> Option<Self::AsSyntax>;
}

impl<T: AsSyntax> MaybeAsSyntax for T {
    type AsSyntax = T;

    fn into_opt(self) -> Option<Self::AsSyntax> {
        Some(self)
    }
}

impl<'a, T> MaybeAsSyntax for &'a Option<T>
where
    &'a T: AsSyntax,
{
    type AsSyntax = &'a T;

    fn into_opt(self) -> Option<Self::AsSyntax> {
        self.as_ref()
    }
}

impl<'a, T> MaybeAsSyntax for &'a Option<Rc<T>>
where
    &'a T: AsSyntax,
{
    type AsSyntax = &'a T;

    fn into_opt(self) -> Option<Self::AsSyntax> {
        self.as_deref()
    }
}
