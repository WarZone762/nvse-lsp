use super::*;

macro_rules! enum_ {
    ($name:ident, $($member:ident($member_kind:ident),)*) => {
        #[derive(Debug, Clone)]
        pub(crate) enum $name {
            $(
                $member($member_kind),
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
                        NodeKind::$member_kind => Some(Self::$member($member_kind{syntax_node})),
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
                if Self::can_cast(syntax_node.kind) {
                    Some(Self { syntax_node })
                } else {
                    None
                }
            }

            fn syntax(&self) -> &Rc<Node> {
                &self.syntax_node
            }
        }
    };
}

macro_rules! children {
    ($name:ident, $type:ident) => {
        pub fn $name(&self) -> AstChildren<$type> {
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
    NodeKind::Script,
    token!(name_token, TokenKind::Name);
    child!(name, Name);
    token!(semi, TokenKind::Semicolon);
    children!(items, Item);
}

enum_! {
    Item,
    FnDecl(FnDeclItem),
    BlockType(BlockTypeItem),
    VarDeclStmt(VarDeclStmt),
}

node! {
    FnDeclItem,
    NodeKind::FnDeclItem,
    token!(fn_token, TokenKind::Fn);
    child!(name, Name);
    child!(param_list, ParamList);
    child!(block, BlockStmt);
}

node! {
    BlockTypeItem,
    NodeKind::BlockTypeItem,
    token!(blocktype, TokenKind::BlockType);
    child!(block, BlockStmt);
    child!(param, Expr);
}

enum_! {
    Stmt,
    Block(BlockStmt),
    VarDecl(VarDeclStmt),
    Expr(ExprStmt),
    For(ForStmt),
    ForEach(ForEachStmt),
    If(IfStmt),
    While(WhileStmt),
    Return(ReturnStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Empty(EmptyStmt),
}

node! {
    BlockStmt,
    NodeKind::BlockStmt,
    token!(lbrack, TokenKind::LeftBrace);
    children!(stmts, Stmt);
    token!(rbrack, TokenKind::RightBrace);
}

node! {
    VarDeclStmt,
    NodeKind::VarDeclStmt,
    token!(export, TokenKind::Export);
    child!(var_decl, VarDecl);
    token!(semi, TokenKind::Semicolon);
}

node! {
    ExprStmt,
    NodeKind::ExprStmt,
    child!(expr, Expr);
    token!(semi, TokenKind::Semicolon);
}

node! {
    ForStmt,
    NodeKind::ForStmt,
    token!(for_kw, TokenKind::For);
    token!(lparen, TokenKind::LeftParen);
    child!(init, VarDecl);
    token!(semi_1, TokenKind::Semicolon);
    child!(cond, Expr);
    token!(semi_2, TokenKind::Semicolon, 1);
    child!(loop_expr, Expr, 1);
    token!(rparen, TokenKind::RightParen);
    child!(block, BlockStmt);
}

node! {
    ForEachStmt,
    NodeKind::ForEachStmt,
    token!(for_kw, TokenKind::For);
    token!(lparen, TokenKind::LeftParen);
    child!(pat, Pat);
    token!(in_kw, TokenKind::In);
    child!(iterable, Expr);
    token!(rparen, TokenKind::RightParen);
    child!(block, BlockStmt);
}

node! {
    IfStmt,
    NodeKind::IfStmt,
    token!(if_kw, TokenKind::If);
    token!(lparen, TokenKind::LeftParen);
    child!(cond, Expr);
    token!(rparen, TokenKind::RightParen);
    child!(true_branch, BlockStmt);
    token!(else_kw, TokenKind::Else);
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
    Block(BlockStmt),
    IfStmt(IfStmt),
}

node! {
    WhileStmt,
    NodeKind::WhileStmt,
    token!(while_kw, TokenKind::While);
    token!(lparen, TokenKind::LeftParen);
    child!(cond, Expr);
    token!(rparen, TokenKind::RightParen);
    child!(block, BlockStmt);
}

node! {
    ReturnStmt,
    NodeKind::ReturnStmt,
    token!(ret_kw, TokenKind::Return);
    child!(expr, Expr);
    token!(semi, TokenKind::Semicolon);
}

node! {
    BreakStmt,
    NodeKind::BreakStmt,
    token!(break_kw, TokenKind::Break);
    token!(semi, TokenKind::Semicolon);
}

node! {
    ContinueStmt,
    NodeKind::ContinueStmt,
    token!(continue_kw, TokenKind::Continue);
    token!(semi, TokenKind::Semicolon);
}

node! {
    EmptyStmt,
    NodeKind::EmptyStmt,
    token!(semi, TokenKind::Semicolon);
}

enum_! {
    Expr,
    Binary(BinaryExpr),
    Ternary(TernaryExpr),
    Unary(UnaryExpr),
    Postfix(PostfixExpr),
    Field(FieldExpr),
    Subscript(SubscriptExpr),
    Call(CallExpr),
    Paren(ParenExpr),
    Lambda(LambdaExpr),
    NameRef(NameRef),
    Str(StrExpr),
    Lit(Literal),
}

node! {
    BinaryExpr,
    NodeKind::BinaryExpr,
    child!(lhs, Expr);
    token!(op, |x| x.kind.is_bin_op());
    child!(rhs, Expr, 1);
}

node! {
    TernaryExpr,
    NodeKind::TernaryExpr,
    child!(cond, Expr);
    token!(question_mark, TokenKind::Ternary);
    child!(true_expr, Expr, 1);
    token!(colon, TokenKind::Colon);
    child!(false_expr, Expr, 2);
}

node! {
    UnaryExpr,
    NodeKind::UnaryExpr,
    token!(op, |x| x.kind.is_unary_op());
    child!(operand, Expr);
}

node! {
    PostfixExpr,
    NodeKind::PostfixExpr,
    child!(operand, Expr);
    token!(op, |x| x.kind == TokenKind::PlusPlus || x.kind == TokenKind::MinusMinus);
}

node! {
    FieldExpr,
    NodeKind::FieldExpr,
    child!(lhs, Expr);
    token!(dot, TokenKind::Dot);
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
    NodeKind::SubscriptExpr,
    child!(lhs, Expr);
    token!(lsqbracket, TokenKind::LeftBracket);
    child!(subscript, Expr, 1);
    token!(rsqbracket, TokenKind::RightBracket);
}

node! {
    CallExpr,
    NodeKind::CallExpr,
    child!(lhs, Expr);
    child!(args, ArgList);
}

node! {
    ParenExpr,
    NodeKind::ParenExpr,
    token!(lparen, TokenKind::LeftParen);
    child!(expr, Expr);
    token!(rparen, TokenKind::RightParen);
}

node! {
    LambdaExpr,
    NodeKind::LambdaExpr,
    token!(fn_kw, TokenKind::Fn);
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
    NodeKind::ParamList,
    token!(lparen, TokenKind::LeftParen);
    children!(params, VarDecl);
    token!(rparen, TokenKind::RightParen);
}

node! {
    ArgList,
    NodeKind::ArgList,
    token!(lparen, TokenKind::LeftParen);
    children!(args, Expr);
    token!(rparen, TokenKind::RightParen);
}

enum_! {
    Pat,
    VarDecl(VarDecl),
    Arr(PatArr),
}

node! {
    PatArr,
    NodeKind::PatArr,
    token!(lsqbrack, TokenKind::LeftBracket);
    children!(patts, Pat);
    token!(rsqbrack, TokenKind::RightBracket);
}

node! {
    VarDecl,
    NodeKind::VarDecl,
    token!(r#type, |x| x.kind.is_type());
    child!(name, Name);
    token!(eq, TokenKind::Eq);
    child!(init, Expr);
}

node! {
    Name,
    NodeKind::Name,
    token!(ident, TokenKind::Identifier);
}

node! {
    NameRef,
    NodeKind::NameRef,
    token!(ident, TokenKind::Identifier);
}

node! {
    StrExpr,
    NodeKind::StrExpr,
    token!(lquote, TokenKind::QuoteDouble);
    children!(shards, StringShard);
    token!(rquote, TokenKind::QuoteDouble, 1);
}

enum_! {
    StringShard,
    Literal(StringShardLiteral),
    Expr(StringShardExpr),
}

node! {
    StringShardLiteral,
    NodeKind::StringShardLiteral,
    token!(token, TokenKind::StringShard);
}

node! {
    StringShardExpr,
    NodeKind::StringShardExpr,
    token!(dollar_lbrace, TokenKind::DollarLeftBrace);
    child!(expr, Expr);
    token!(rbrace, TokenKind::RightBrace);
}

node! {
    Literal,
    NodeKind::Literal,
    token!(lit, TokenKind::Number | TokenKind::Bool);
}
