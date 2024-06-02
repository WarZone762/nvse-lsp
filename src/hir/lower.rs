use super::*;
use crate::{
    ast,
    syntax_node::{Token, TokenKind},
};

#[derive(Debug)]
pub(crate) struct LowerCtx<'a> {
    workspace: &'a mut Workspace,
    text: &'a str,
    sym_tbl_stack: Vec<SymbolTable>,
}

impl<'a> LowerCtx<'a> {
    pub fn new(workspace: &'a mut Workspace, text: &'a str) -> Self {
        Self { workspace, text, sym_tbl_stack: vec![] }
    }

    pub fn script(&mut self, node: ast::Script) -> Script {
        self.push_sym_tbl();
        Script {
            name: self.name(node.name()),
            items: self.items(node.items()),
            sym_table: self.pop_sym_tbl(),
            node,
        }
    }

    pub fn items(&mut self, items: ast::AstChildren<ast::Item>) -> Vec<Item> {
        items.filter_map(|x| self.item(x)).collect()
    }

    pub fn item(&mut self, node: ast::Item) -> Option<Item> {
        Some(match node {
            ast::Item::FnDecl(x) => self.fn_decl(x)?.into(),
            ast::Item::BlockType(x) => self.block_type(x)?.into(),
            ast::Item::VarDeclStmt(x) => self.stmt_var_decl(x)?.into(),
        })
    }

    pub fn fn_decl(&mut self, node: ast::FnDeclItem) -> Option<FnDeclItem> {
        Some(FnDeclItem {
            name: self.name(node.name()),
            params: self.param_list(node.param_list())?,
            block: self.block(node.block())?,
            node,
        })
    }

    pub fn block_type(&mut self, node: ast::BlockTypeItem) -> Option<BlockTypeItem> {
        Some(BlockTypeItem { blocktype_kind: 0, block: self.block(node.block())?, node })
    }

    pub fn stmt(&mut self, node: ast::Stmt) -> Option<Stmt> {
        Some(match node {
            ast::Stmt::Block(x) => self.block(Some(x))?.into(),
            ast::Stmt::VarDecl(x) => self.stmt_var_decl(x)?.into(),
            ast::Stmt::Expr(x) => self.stmt_expr(x)?.into(),
            ast::Stmt::For(x) => self.stmt_for(x)?.into(),
            ast::Stmt::ForEach(x) => self.stmt_for_each(x)?.into(),
            ast::Stmt::If(x) => self.stmt_if(x)?.into(),
            ast::Stmt::While(x) => self.stmt_while(x)?.into(),
            ast::Stmt::Return(x) => self.stmt_return(x).into(),
            ast::Stmt::Break(x) => self.stmt_break(x).into(),
            ast::Stmt::Continue(x) => self.stmt_contiue(x).into(),
            ast::Stmt::Empty(_) => return None,
        })
    }

    pub fn stmt_var_decl(&mut self, node: ast::VarDeclStmt) -> Option<VarDeclStmt> {
        Some(VarDeclStmt { decl: self.var_decl(node.var_decl())?, node })
    }

    pub fn stmt_expr(&mut self, node: ast::ExprStmt) -> Option<ExprStmt> {
        Some(ExprStmt { expr: self.expr(node.expr()), node })
    }

    pub fn stmt_for(&mut self, node: ast::ForStmt) -> Option<ForStmt> {
        Some(ForStmt {
            initializer: node.init_expr().map(|x| self.expr(Some(x))),
            cond: node.cond().map(|x| self.expr(Some(x))),
            loop_expr: node.loop_expr().map(|x| self.expr(Some(x))),
            block: self.block(node.block())?,
            node,
        })
    }

    pub fn stmt_for_each(&mut self, node: ast::ForEachStmt) -> Option<ForEachStmt> {
        Some(ForEachStmt {
            var_decl: self.var_decl(node.var_decl())?,
            iterable: self.expr(node.iterable()),
            block: self.block(node.block())?,
            node,
        })
    }

    pub fn stmt_if(&mut self, node: ast::IfStmt) -> Option<IfStmt> {
        Some(IfStmt {
            cond: self.expr(node.cond()),
            true_branch: self.block(node.true_branch())?,
            false_branch: node.false_branch().and_then(|x| Some(self.stmt_if(x)?.into())),
            else_branch: node.else_branch().and_then(|x| Some(self.block(Some(x))?.into())),
            node,
        })
    }

    pub fn stmt_while(&mut self, node: ast::WhileStmt) -> Option<WhileStmt> {
        Some(WhileStmt { cond: self.expr(node.cond()), block: self.block(node.block())?, node })
    }

    pub fn stmt_return(&mut self, node: ast::ReturnStmt) -> ReturnStmt {
        ReturnStmt { expr: node.expr().map(|x| self.expr(Some(x))), node }
    }

    pub fn stmt_break(&mut self, node: ast::BreakStmt) -> BreakStmt {
        BreakStmt { node }
    }

    pub fn stmt_contiue(&mut self, node: ast::ContinueStmt) -> ContinueStmt {
        ContinueStmt { node }
    }

    pub fn expr(&mut self, node: Option<ast::Expr>) -> Expr {
        node.and_then(|x| {
            Some(match x {
                ast::Expr::Binary(x) => self.expr_bin(x).into(),
                ast::Expr::Ternary(x) => self.expr_ternary(x).into(),
                ast::Expr::Unary(x) => self.expr_unary(x).into(),
                ast::Expr::Subscript(x) => self.expr_subscript(x).into(),
                ast::Expr::Call(x) => self.expr_call(x)?.into(),
                ast::Expr::Paren(x) => self.expr_paren(x).into(),
                ast::Expr::Lambda(x) => self.expr_lambda(x)?.into(),
                ast::Expr::NameRef(x) => self.name_ref(x)?.into(),
                ast::Expr::Str(x) => self.str(x).into(),
                ast::Expr::Lit(x) => self.lit(x)?.into(),
            })
        })
        .unwrap_or(Expr::Missing)
    }

    pub fn expr_bin(&mut self, node: ast::BinaryExpr) -> BinExpr {
        BinExpr {
            lhs: self.expr(node.lhs()).into(),
            // TODO
            op: BinOpKind::Plus,
            rhs: self.expr(node.rhs()).into(),
            type_: Type::Ambiguous,
            node,
        }
    }

    pub fn expr_ternary(&mut self, node: ast::TernaryExpr) -> TernaryExpr {
        TernaryExpr {
            cond: self.expr(node.cond()).into(),
            true_expr: self.expr(node.true_expr()).into(),
            false_expr: self.expr(node.false_expr()).into(),
            type_: Type::Ambiguous,
            node,
        }
    }

    pub fn expr_unary(&mut self, node: ast::UnaryExpr) -> UnaryExpr {
        UnaryExpr {
            // TODO
            op: UnaryOpKind::Minus,
            operand: self.expr(node.operand()).into(),
            type_: Type::Ambiguous,
            node,
        }
    }

    pub fn expr_subscript(&mut self, node: ast::SubscriptExpr) -> SubscriptExpr {
        SubscriptExpr {
            lhs: self.expr(node.lhs()).into(),
            subscript: self.expr(node.subscript()).into(),
            type_: Type::Ambiguous,
            node,
        }
    }

    pub fn expr_call(&mut self, node: ast::CallExpr) -> Option<CallExpr> {
        Some(CallExpr {
            lhs: self.expr(node.lhs()).into(),
            args: self.arg_list(node.args())?,
            type_: Type::Function(FunctionSignature { ret: None, params: vec![] }.into()),
            node,
        })
    }

    pub fn expr_paren(&mut self, node: ast::ParenExpr) -> ParenExpr {
        ParenExpr { expr: self.expr(node.expr()).into(), node }
    }

    pub fn expr_lambda(&mut self, node: ast::LambdaExpr) -> Option<LambdaExpr> {
        Some(LambdaExpr {
            params: self.param_list(node.params())?,
            block: self.block(node.block())?,
            type_: Type::Ambiguous,
            node,
        })
    }

    pub fn block(&mut self, node: Option<ast::BlockStmt>) -> Option<Block> {
        self.push_sym_tbl();
        Some(Block {
            stmts: node
                .as_ref()
                .into_iter()
                .flat_map(|x| x.stmts())
                .filter_map(|x| self.stmt(x))
                .collect(),
            sym_table: self.pop_sym_tbl(),
            node: node?,
        })
    }

    pub fn param_list(&mut self, node: Option<ast::ParamList>) -> Option<ParamList> {
        Some(ParamList {
            params: node.as_ref()?.params().filter_map(|x| self.var_decl(Some(x))).collect(),
            node: node?,
        })
    }

    pub fn arg_list(&mut self, node: Option<ast::ArgList>) -> Option<ArgList> {
        Some(ArgList {
            args: node.as_ref()?.args().filter_map(|x| self.expr(Some(x)).into()).collect(),
            node: node?,
        })
    }

    pub fn var_decl(&mut self, node: Option<ast::VarDecl>) -> Option<Rc<VarDecl>> {
        let var_decl = Rc::new(VarDecl {
            decl_type: Type::from_str(self.token_text(node.as_ref()?.r#type().as_ref()?)),
            name: self.name(node.as_ref()?.name())?,
            node: node?,
        });
        self.sym_tbl_stack
            .last_mut()
            .expect("LowerCtx::var_decl: symbol table stack empty")
            .map
            .insert(var_decl.name.name.clone(), Symbol::Local(var_decl.clone()));

        Some(var_decl)
    }

    pub fn name(&mut self, node: Option<ast::Name>) -> Option<Name> {
        Some(Name { name: self.token_text(node.as_ref()?.ident().as_ref()?).into(), node: node? })
    }

    pub fn name_ref(&mut self, node: ast::NameRef) -> Option<NameRef> {
        Some(NameRef { name: self.token_text(node.ident().as_ref()?).into(), node })
    }

    pub fn str(&mut self, node: ast::StrExpr) -> StrExpr {
        StrExpr { shards: node.shards().filter_map(|x| self.string_shard(x)).collect(), node }
    }

    pub fn string_shard(&mut self, node: ast::StringShard) -> Option<StringShard> {
        Some(match &node {
            ast::StringShard::Literal(x) => {
                StringShard::Str { val: self.token_text(x.token()?.as_ref()).into(), node }
            }
            ast::StringShard::Expr(x) => StringShard::Expr(self.expr(x.expr())),
        })
    }

    pub fn lit(&mut self, node: ast::Literal) -> Option<Literal> {
        let token = node.lit()?;
        Some(match token.kind {
            TokenKind::Number => Literal::Number(NumberLiteral {
                value: self.token_text(&token).parse().ok()?,
                node,
            }),
            TokenKind::Bool => {
                Literal::Bool(BoolLiteral { value: self.token_text(&token).parse().ok()?, node })
            }
            _ => unreachable!(),
        })
    }

    fn token_text(&mut self, token: &Token) -> &str {
        token.text(self.text)
    }

    fn push_sym_tbl(&mut self) {
        self.sym_tbl_stack.push(SymbolTable::new())
    }

    fn pop_sym_tbl(&mut self) -> SymbolTable {
        self.sym_tbl_stack.pop().expect("LowerCtx::pop_sym_tbl: symbol table stack empty")
    }
}
