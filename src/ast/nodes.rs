use super::*;

macro_rules! enum_ {
    ($name:ident, $($member:ident($member_struct: ident, $member_kind:ident),)*) => {
        #[derive(Debug, Clone)]
        pub(crate) enum $name {
            $(
                $member($member_struct),
            )*
        }

        impl AstNode for $name {
            fn can_cast(kind: NodeKind) -> bool {
                match kind {
                    $(
                        NodeKind::$member_kind => true,
                    )*
                    _ => false,
                }
            }

            fn cast(syntax_node: Rc<Node>) -> Option<Self>
            where
                Self: Sized,
            {
                match syntax_node.kind {
                    $(
                        NodeKind::$member_kind => Some(Self::$member($member_struct{syntax_node})),
                    )*
                    _ => None,
                }
            }

            fn syntax(&self) -> &Rc<Node> {
                match self {
                    $(
                        Self::$member(x) => &x.syntax_node,
                    )*
                }
            }
        }
    };
}

macro_rules! node {
    ($name:ident, $pat:pat, $($items:item)*) => {
        #[derive(Debug, Clone)]
        pub(crate) struct $name {
            pub syntax_node: Rc<Node>,
        }

        impl $name {
            $(
                $items
            )*
        }

        ast_node!($name, $pat);
    };
}

macro_rules! ast_node {
    ($name:ident, $pat:pat) => {
        impl AstNode for $name {
            fn can_cast(kind: NodeKind) -> bool {
                matches!(kind, $pat)
            }

            fn cast(syntax_node: Rc<Node>) -> Option<Self>
            where
                Self: Sized,
            {
                if Self::can_cast(syntax_node.kind) { Some(Self { syntax_node }) } else { None }
            }

            fn syntax(&self) -> &Rc<Node> {
                &self.syntax_node
            }
        }
    };
}

macro_rules! children {
    ($name:ident, $type:ident) => {
        pub fn $name(&self) -> AstChildren<'_, $type> {
            AstChildren::new(&self.syntax_node)
        }
    };
}

macro_rules! child {
    ($name:ident, $type:ident) => {
        pub fn $name(&self) -> Option<$type> {
            self.syntax_node.children.iter().find_map(|x| match x {
                NodeOrToken::Node(x) => AstNode::cast(x.clone()),
                NodeOrToken::Token(_) => None,
            })
        }
    };
    ($name:ident, $type:ident, $n:literal) => {
        pub fn $name(&self) -> Option<$type> {
            self.syntax_node
                .children
                .iter()
                .filter_map(|x| AstNode::cast(x.node()?.clone()))
                .nth($n)
        }
    };
}

macro_rules! token {
    ($name:ident, $kinds:pat) => {
        pub fn $name(&self) -> Option<Rc<Token>> {
            self.syntax_node
                .children
                .iter()
                .filter_map(|x| match x {
                    NodeOrToken::Node(_) => None,
                    NodeOrToken::Token(x) => Some(x),
                })
                .find(|x| matches!(x.kind, $kinds))
                .cloned()
        }
    };
    ($name:ident, $pat:pat, $n:literal) => {
        pub fn $name(&self) -> Option<Rc<Token>> {
            self.syntax_node
                .children
                .iter()
                .filter_map(|x| x.token())
                .filter(|x| matches!(x.kind, $pat))
                .nth($n)
                .cloned()
        }
    };
    ($name:ident, $test:expr) => {
        pub fn $name(&self) -> Option<Rc<Token>> {
            self.syntax_node
                .children
                .iter()
                .filter_map(|x| match x {
                    NodeOrToken::Node(_) => None,
                    NodeOrToken::Token(x) => Some(x),
                })
                .find($test)
                .cloned()
        }
    };
}

node! {
    Script,
    NodeKind::SCRIPT,
    token!(name_kw, TokenKind::NAME_KW);
    child!(name, Name);
    token!(semi, TokenKind::SEMICOLON);
    children!(items, Item);
}

enum_! {
    Item,
    FnDecl(FnDeclItem, FN_DECL_ITEM),
    BlockType(BlockTypeItem, BLOCK_TYPE_ITEM),
    VarDecl(VarDeclStmt, VAR_DECL_STMT),
}

node! {
    FnDeclItem,
    NodeKind::FN_DECL_ITEM,
    token!(fn_kw, TokenKind::FN_KW);
    child!(name, Name);
    child!(param_list, ParamList);
    child!(block, BlockStmt);
}

node! {
    BlockTypeItem,
    NodeKind::BLOCK_TYPE_ITEM,
    token!(blocktype, TokenKind::BLOCK_TYPE);
    child!(block, BlockStmt);
    child!(param, Expr);
}

enum_! {
    Stmt,
    Block(BlockStmt, BLOCK_STMT),
    VarDecl(VarDeclStmt, VAR_DECL_STMT),
    Expr(ExprStmt, EXPR_STMT),
    For(ForStmt, FOR_STMT),
    ForEach(ForRangeStmt, FOR_RANGE_STMT),
    If(IfStmt, IF_STMT),
    While(WhileStmt, WHILE_STMT),
    Return(ReturnStmt, RETURN_STMT),
    Break(BreakStmt, BREAK_STMT),
    Continue(ContinueStmt, CONTINUE_STMT),
    Empty(EmptyStmt, EMPTY_EXPR),
}

node! {
    BlockStmt,
    NodeKind::BLOCK_STMT,
    token!(lbrack, TokenKind::LBRACK);
    children!(stmts, Stmt);
    token!(rbrack, TokenKind::RBRACK);
}

node! {
    VarDeclStmt,
    NodeKind::VAR_DECL_STMT,
    token!(export, TokenKind::EXPORT_KW);
    child!(var_decl, VarDecl);
    token!(semi, TokenKind::SEMICOLON);
}

node! {
    ExprStmt,
    NodeKind::EXPR_STMT,
    child!(expr, Expr);
    token!(semi, TokenKind::SEMICOLON);
}

node! {
    ForStmt,
    NodeKind::FOR_STMT,
    token!(for_kw, TokenKind::FOR_KW);
    token!(lparen, TokenKind::LPAREN);
    child!(init, VarDecl);
    token!(semi_1, TokenKind::SEMICOLON);
    child!(cond, Expr);
    token!(semi_2, TokenKind::SEMICOLON, 1);
    child!(loop_expr, Expr, 1);
    token!(rparen, TokenKind::RPAREN);
    child!(block, BlockStmt);
}

node! {
    ForRangeStmt,
    NodeKind::FOR_RANGE_STMT,
    token!(for_kw, TokenKind::FOR_KW);
    token!(lparen, TokenKind::LPAREN);
    child!(pat, Pat);
    token!(in_kw, TokenKind::IN_KW);
    child!(iterable, Expr);
    token!(rparen, TokenKind::RPAREN);
    child!(block, BlockStmt);
}

node! {
    IfStmt,
    NodeKind::IF_STMT,
    token!(if_kw, TokenKind::IF_KW);
    token!(lparen, TokenKind::LPAREN);
    child!(cond, Expr);
    token!(rparen, TokenKind::RPAREN);
    child!(true_branch, BlockStmt);
    token!(else_kw, TokenKind::ELSE_KW);
}

impl IfStmt {
    pub fn false_branch(&self) -> Option<ElseBranch> {
        self.syntax_node
            .children
            .iter()
            .find_map(|x| match x {
                NodeOrToken::Node(x) => Some(ElseBranch::IfStmt(IfStmt::cast(x.clone())?)),
                NodeOrToken::Token(_) => None,
            })
            .or_else(|| {
                self.syntax_node
                    .children
                    .iter()
                    .filter_map(|x| Some(ElseBranch::Block(BlockStmt::cast(x.node()?.clone())?)))
                    .nth(1)
            })
    }
}

enum_! {
    ElseBranch,
    Block(BlockStmt, BLOCK_STMT),
    IfStmt(IfStmt, IF_STMT),
}

node! {
    WhileStmt,
    NodeKind::WHILE_STMT,
    token!(while_kw, TokenKind::WHILE_KW);
    token!(lparen, TokenKind::LPAREN);
    child!(cond, Expr);
    token!(rparen, TokenKind::RPAREN);
    child!(block, BlockStmt);
}

node! {
    ReturnStmt,
    NodeKind::RETURN_STMT,
    token!(ret_kw, TokenKind::RETURN_KW);
    child!(expr, Expr);
    token!(semi, TokenKind::SEMICOLON);
}

node! {
    BreakStmt,
    NodeKind::BREAK_STMT,
    token!(break_kw, TokenKind::BREAK_KW);
    token!(semi, TokenKind::SEMICOLON);
}

node! {
    ContinueStmt,
    NodeKind::CONTINUE_STMT,
    token!(continue_kw, TokenKind::CONTINUE_KW);
    token!(semi, TokenKind::SEMICOLON);
}

node! {
    EmptyStmt,
    NodeKind::EMPTY_EXPR,
    token!(semi, TokenKind::SEMICOLON);
}

enum_! {
    Expr,
    Binary(BinExpr, BIN_EXPR),
    Ternary(TernaryExpr, TERNARY_EXPR),
    Unary(UnaryExpr, UNARY_EXPR),
    Postfix(PostfixExpr, POSTFIX_EXPR),
    Field(FieldExpr, FIELD_EXPR),
    Subscript(SubscriptExpr, SUBSCRIPT_EXPR),
    Call(CallExpr, CALL_EXPR),
    Paren(ParenExpr, PAREN_EXPR),
    Lambda(LambdaExpr, LAMBDA_EXPR),
    NameRef(NameRef, NAME_REF),
    Str(StrExpr, STR_EXPR),
    LitArr(LitArr, LIT_ARR),
    LitMap(LitMap, LIT_MAP),
    Literal(Literal, LITERAL),
}

node! {
    BinExpr,
    NodeKind::BIN_EXPR,
    child!(lhs, Expr);
    token!(op, |x| x.kind.is_bin_op());
    child!(rhs, Expr, 1);
}

node! {
    TernaryExpr,
    NodeKind::TERNARY_EXPR,
    child!(cond, Expr);
    token!(question_mark, TokenKind::QUESTION_MARK);
    child!(true_expr, Expr, 1);
    token!(colon, TokenKind::COLON);
    child!(false_expr, Expr, 2);
}

node! {
    UnaryExpr,
    NodeKind::UNARY_EXPR,
    token!(op, |x| x.kind.is_unary_op());
    child!(operand, Expr);
}

node! {
    PostfixExpr,
    NodeKind::POSTFIX_EXPR,
    child!(operand, Expr);
    token!(op, |x| x.kind == TokenKind::PLUS_2 || x.kind == TokenKind::MINUS_2);
}

node! {
    FieldExpr,
    NodeKind::FIELD_EXPR,
    child!(lhs, Expr);
    token!(dot, TokenKind::DOT);
}

impl FieldExpr {
    pub fn field(&self) -> Option<NameRef> {
        self.syntax_node
            .children
            .iter()
            .filter_map(|x| Expr::cast(x.node()?.clone()))
            .skip(1)
            .find_map(|x| match x {
                Expr::NameRef(x) => Some(x),
                _ => None,
            })
    }
}

node! {
    SubscriptExpr,
    NodeKind::SUBSCRIPT_EXPR,
    child!(lhs, Expr);
    token!(lsqbrack, TokenKind::LSQ_BRACK);
    child!(subscript, Expr, 1);
    token!(rsqbrack, TokenKind::RSQ_BRACK);
}

node! {
    CallExpr,
    NodeKind::CALL_EXPR,
    child!(lhs, Expr);
    child!(args, ArgList);
}

node! {
    ParenExpr,
    NodeKind::PAREN_EXPR,
    token!(lparen, TokenKind::LPAREN);
    child!(expr, Expr);
    token!(rparen, TokenKind::RPAREN);
}

node! {
    LambdaExpr,
    NodeKind::LAMBDA_EXPR,
    token!(fn_kw, TokenKind::FN_KW);
    child!(params, ParamList);
    child!(block_or_expr, BlockOrExpr);
}

#[derive(Debug, Clone)]
pub(crate) enum BlockOrExpr {
    Block(BlockStmt),
    Expr(Expr),
}

impl AstNode for BlockOrExpr {
    fn can_cast(kind: NodeKind) -> bool {
        Expr::can_cast(kind) || BlockStmt::can_cast(kind)
    }

    fn cast(syntax_node: Rc<Node>) -> Option<Self>
    where
        Self: Sized,
    {
        Some(if Expr::can_cast(syntax_node.kind) {
            Self::Expr(Expr::cast(syntax_node).unwrap())
        } else if BlockStmt::can_cast(syntax_node.kind) {
            Self::Block(BlockStmt::cast(syntax_node).unwrap())
        } else {
            return None;
        })
    }

    fn syntax(&self) -> &Rc<Node> {
        match self {
            Self::Block(x) => &x.syntax_node,
            Self::Expr(x) => x.syntax(),
        }
    }
}

node! {
    ParamList,
    NodeKind::PARAM_LIST,
    token!(lparen, TokenKind::LPAREN);
    children!(params, VarDecl);
    token!(rparen, TokenKind::RPAREN);
}

node! {
    ArgList,
    NodeKind::ARG_LIST,
    token!(lparen, TokenKind::LPAREN);
    children!(args, Expr);
    token!(rparen, TokenKind::RPAREN);
}

enum_! {
    Pat,
    VarDecl(VarDecl, VAR_DECL),
    Arr(ArrPat, ARR_PAT),
}

node! {
    ArrPat,
    NodeKind::ARR_PAT,
    token!(lsqbrack, TokenKind::LSQ_BRACK);
    children!(patts, Pat);
    token!(rsqbrack, TokenKind::RSQ_BRACK);
}

node! {
    VarDecl,
    NodeKind::VAR_DECL,
    token!(type_, |x| x.kind.is_type());
    child!(name, Name);
    token!(eq, TokenKind::EQ);
    child!(init, Expr);
}

node! {
    Name,
    NodeKind::NAME,
    token!(ident, TokenKind::IDENT);
}

node! {
    NameRef,
    NodeKind::NAME_REF,
    token!(ident, TokenKind::IDENT);
}

node! {
    StrExpr,
    NodeKind::STR_EXPR,
    token!(lquote, TokenKind::QUOTE_DOUBLE);
    children!(shards, StrShard);
    token!(rquote, TokenKind::QUOTE_DOUBLE, 1);
}

enum_! {
    StrShard,
    Literal(StrShardLiteral, STR_SHARD_LITERAL),
    Expr(StrShardExpr, STR_SHARD_EXPR),
}

node! {
    StrShardLiteral,
    NodeKind::STR_SHARD_LITERAL,
    token!(token, TokenKind::STR_SHARD);
}

node! {
    StrShardExpr,
    NodeKind::STR_SHARD_EXPR,
    token!(dollar_lbrack, TokenKind::DOLLAR_LBRACK);
    child!(expr, Expr);
    token!(rbrack, TokenKind::RBRACK);
}

node! {
    LitArr,
    NodeKind::LIT_ARR,
    token!(lsq_brack, TokenKind::LSQ_BRACK);
    children!(exprs, Expr);
    token!(rsq_brack, TokenKind::RSQ_BRACK);
}

node! {
    LitMap,
    NodeKind::LIT_MAP,
    token!(lbrack, TokenKind::LBRACK);
    children!(kv_pairs, KVPair);
    token!(rbrack, TokenKind::RBRACK);
}

node! {
    KVPair,
    NodeKind::KV_PAIR,
    child!(key, Expr);
    token!(col2, TokenKind::COLON_2);
    child!(value, Expr, 1);
}

node! {
    Literal,
    NodeKind::LITERAL,
    token!(literal, TokenKind::NUMBER | TokenKind::BOOL);
}
