use super::*;

macro_rules! enum_ {
    ($name:ident, $($member:ident($member_kind:ident),)*) => {
        #[derive(Debug)]
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
        #[derive(Debug)]
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
    token!(name, TokenKind::Identifier);
}

enum_! {
    Stmt,
    Begin(BeginStmt),
    FnDecl(FnDeclStmt),
    Block(BlockStmt),
    VarDecl(VarDeclStmt),
    Expr(ExprStmt),
    For(ForStmt),
    If(IfStmt),
    While(WhileStmt),
    Return(ReturnStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Empty(EmptyStmt),
}

node! {
    BeginStmt,
    NodeKind::BeginStmt,
    token!(blocktype, TokenKind::BlockType);
    child!(block, BlockStmt);
    child!(param, Expr);
}

node! {
    FnDeclStmt,
    NodeKind::FnDeclStmt,
    token!(fn_token, TokenKind::Fn);
    child!(param_list, ParamList);
    child!(block, BlockStmt);
}

node! {
    BlockStmt,
    NodeKind::BlockStmt,
    children!(stmts, Stmt);
}

node! {
    VarDeclStmt,
    NodeKind::VarDeclStmt,
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
    child!(init_expr, Expr);
    token!(semi_1, TokenKind::Semicolon);
    child!(cond, Expr, 1);
    token!(semi_2, TokenKind::Semicolon, 1);
    child!(loop_expr, Expr, 2);
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
    child!(false_branch, BlockStmt, 1);
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
    token!(question_mark, TokenKind::Ternary);
    child!(cond, Expr);
    token!(colon, TokenKind::Colon);
    child!(expr, Expr, 1);
}

node! {
    UnaryExpr,
    NodeKind::UnaryExpr,
    token!(op, |x| x.kind.is_unary_op());
    child!(operand, Expr);
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
    token!(lparen, TokenKind::LeftParen);
    child!(args, ArgList);
    token!(rparen, TokenKind::RightParen);
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
    child!(block, BlockStmt);
}

node! {
    ParamList,
    NodeKind::ParamList,
    children!(params, VarDecl);
}

node! {
    ArgList,
    NodeKind::ArgList,
    children!(args, Expr);
}

node! {
    VarDecl,
    NodeKind::VarDecl,
    token!(r#type, |x| x.kind.is_type());
    child!(name, Name);
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
