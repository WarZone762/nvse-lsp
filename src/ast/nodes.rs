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
}

node! {
    BeginStmt,
    NodeKind::BeginStmt,
    token!(blocktype, TokenKind::BlockType);
    child!(block, BlockStmt);
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
    token!(semicolon, TokenKind::Semicolon);
}

node! {
    ExprStmt,
    NodeKind::ExprStmt,
    child!(expr, Expr);
    token!(semicolon, TokenKind::Semicolon);
}

node! {
    ForStmt,
    NodeKind::ForStmt,
    token!(for_kw, TokenKind::For);
    token!(lparen, TokenKind::LeftParen);
    token!(rparen, TokenKind::RightParen);
    child!(initializer, Expr);
    token!(semicolon_1, TokenKind::Semicolon);
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
}

node! {
    BreakStmt,
    NodeKind::BreakStmt,
}

node! {
    ContinueStmt,
    NodeKind::ContinueStmt,
}

enum_! {
    Expr,
    Binary(BinaryExpr),
    Ternary(TernaryExpr),
    Unary(UnaryExpr),
    Subscript(SubscriptExpr),
    Call(CallExpr),
    Lambda(LambdaExpr),
}

node! {
    BinaryExpr,
    NodeKind::BinaryExpr,
    child!(lhs, Expr);
    token!(op, TokenKind::Plus | TokenKind::Minus);
}

node! {
    TernaryExpr,
    NodeKind::TernaryExpr,
    token!(question_mark, TokenKind::Ternary);
    child!(cond, Expr);
    token!(colon, TokenKind::Colon);
}

node! {
    UnaryExpr,
    NodeKind::UnaryExpr,
    child!(operand, Expr);
    token!(op, TokenKind::Star);
}

node! {
    SubscriptExpr,
    NodeKind::SubscriptExpr,
    token!(lsqbracket, TokenKind::LeftBracket);
    child!(cond, Expr);
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
    NodeKind::GroupingExpr,
    token!(lparen, TokenKind::LeftParen);
    child!(expr, Expr);
    token!(rparen, TokenKind::RightParen);
}

node! {
    LambdaExpr,
    NodeKind::LambdaExpr,
    token!(fn_kw, TokenKind::Fn);
    child!(expr, ParamList);
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
    token!(
        r#type,
        TokenKind::IntType
          | TokenKind::DoubleType
          | TokenKind::RefType
          | TokenKind::StringType
          | TokenKind::ArrayType
    );
    token!(name_ref, TokenKind::Identifier);
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
    Literal,
    NodeKind::Literal,
    token!(lit, TokenKind::Number | TokenKind::String | TokenKind::Bool);
}
