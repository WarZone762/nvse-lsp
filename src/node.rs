use std::{fmt::Debug, rc::Rc};

use tower_lsp::lsp_types::SemanticTokenType;

#[derive(Debug)]
pub(crate) enum NodeOrToken<'a> {
    Node(Rc<Node<'a>>),
    Token(Token<'a>),
}

impl<'a> From<Rc<Node<'a>>> for NodeOrToken<'a> {
    fn from(value: Rc<Node<'a>>) -> Self {
        Self::Node(value)
    }
}

impl<'a> From<Token<'a>> for NodeOrToken<'a> {
    fn from(value: Token<'a>) -> Self {
        Self::Token(value)
    }
}

impl<'a> NodeOrToken<'a> {
    pub fn offset(&self) -> usize {
        match self {
            NodeOrToken::Node(x) => x.offset,
            NodeOrToken::Token(x) => x.offset,
        }
    }

    pub fn end(&self) -> usize {
        match self {
            NodeOrToken::Node(x) => x.end(),
            NodeOrToken::Token(x) => x.end(),
        }
    }

    pub fn parent(&self) -> &Option<Rc<Node<'a>>> {
        match self {
            NodeOrToken::Node(x) => &x.parent,
            NodeOrToken::Token(x) => &x.parent,
        }
    }

    pub unsafe fn parent_mut(&mut self) -> &mut Option<Rc<Node<'a>>> {
        match self {
            NodeOrToken::Node(x) => &mut Rc::get_mut_unchecked(x).parent,
            NodeOrToken::Token(x) => &mut x.parent,
        }
    }
}

pub(crate) struct Node<'a> {
    pub kind: NodeKind,
    pub offset: usize,
    pub children: Vec<NodeOrToken<'a>>,
    pub parent: Option<Rc<Node<'a>>>,
}

impl Debug for Node<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = f.debug_struct("Node");
        out.field("kind", &self.kind)
            .field("offset", &self.offset)
            .field("children", &self.children);
        if let Some(parent) = &self.parent {
            out.field_with("parent", |f| {
                f.debug_struct("Node")
                    .field("kind", &parent.kind)
                    .field("offset", &parent.offset)
                    .finish()
            });
        } else {
            out.field("parent", &self.parent);
        }

        out.finish()
    }
}

impl Node<'_> {
    pub fn new(kind: NodeKind, offset: usize) -> Self {
        Self {
            kind,
            offset,
            children: Vec::new(),
            parent: None,
        }
    }

    pub fn end(&self) -> usize {
        self.offset + self.children.last().map(|x| x.end()).unwrap_or(0)
    }
}

#[derive(Clone)]
pub(crate) struct Token<'a> {
    pub kind: TokenKind,
    pub offset: usize,
    pub text: &'a str,
    pub parent: Option<Rc<Node<'a>>>,
}

impl Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = f.debug_struct("Token");
        out.field("kind", &self.kind)
            .field("offset", &self.offset)
            .field("text", &self.text);
        if let Some(parent) = &self.parent {
            out.field_with("parent", |f| {
                f.debug_struct("Node")
                    .field("kind", &parent.kind)
                    .field("offset", &parent.offset)
                    .finish()
            });
        } else {
            out.field("parent", &self.parent);
        }

        out.finish()
    }
}

impl PartialEq for Token<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.offset == other.offset && self.text == other.text
    }
}

impl Eq for Token<'_> {}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, offset: usize, text: &'a str) -> Self {
        Self {
            kind,
            offset,
            text,
            parent: None,
        }
    }
}

impl Token<'_> {
    pub fn len(&self) -> usize {
        self.text.len()
    }

    pub fn end(&self) -> usize {
        self.offset + self.text.len()
    }
}

macro_rules! tokens {
    ($(($group:ident) $($($lit:literal =>)? $ident:ident,)*)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[allow(dead_code)]
        pub(crate) enum TokenKind {
            $($(
                $ident,
            )*)*
        }

        impl TokenKind {
            pub fn from_str(string: &str) -> Option<Self> {
                match string {
                    $($(
                        $($lit => Some(Self::$ident),)?
                    )*)*
                    "true" | "false" => Some(Self::Bool),
                    _ => None,
                }
            }
        }

        $(
            #[allow(dead_code)]
            impl TokenKind {
                pub fn $group(&self) -> bool {
                    match self {
                        $(
                            Self::$ident => true,
                        )*
                        _ => false,
                    }
                }
            }
        )*
    };
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum NodeKind {
    Script,
    BeginStmt,
    FnDeclStmt,
    VarDeclStmt,
    ExprStmt,
    ForStmt,
    IfStmt,
    ReturnStmt,
    WhileStmt,
    BlockStmt,
    AssignmentExpr,
    TernaryExpr,
    BinayExpr,
    UnaryExpr,
    SubscriptExpr,
    CallExpr,
    GetExpr,
    BoolExpr,
    NuberExpr,
    StringExpr,
    IdentExpr,
    GroupingExpr,
    LambdaExpr,
    Tombstone,
}

tokens! {
    (is_keyword)
    "if" => If,
    "else" => Else,
    "while" => While,
    "fn" => Fn,
    "return" => Return,
    "for" => For,
    "name" => Name,
    "begin" => Begin,
    BlockType,

    (is_type)
    "int" => IntType,
    "double" => DoubleType,
    "ref" => RefType,
    "string" => StringType,
    "array" => ArrayType,

    (is_operator)
    "+" => Plus,
    "+=" => PlusEq,
    "++" => PlusPlus,
    "-" => Minus,
    "-=" => MinusEq,
    "--" => MinusMinus,
    "*" => Star,
    "*=" => StarEq,
    "/" => Slash,
    "/=" => SlashEq,
    "=" => Eq,
    "==" => EqEq,
    "<" => Less,
    ">" => Greater,
    "<=" => LessEq,
    ">=" => GreaterEq,
    "!" => Bang,
    "!=" => BangEq,
    "||" => LogicOr,
    "&&" => LogicAnd,
    "|" => BitwiseOr,
    "&" => BitwiseAnd,
    "~" => Tilde,
    "$" => Dollar,
    "?" => Ternary,
    ":" => Colon,

    (is_brace)
    "{" => LeftBrace,
    "}" => RigthBrace,
    "[" => LeftBracket,
    "]" => RigthBracket,
    "(" => LeftParen,
    ")" => RigthParne,

    (is_literal)
    String,
    Number,
    Identifier,
    Bool,

    (is_misc)
    "," => Comma,
    ";" => Semicolon,
    "." => Dot,
    Whitespace,
    Eof,
    Error,
}

impl TokenKind {
    pub fn to_semantic(self) -> Option<SemanticTokenType> {
        Some(if self.is_keyword() {
            SemanticTokenType::KEYWORD
        } else if self.is_type() {
            SemanticTokenType::TYPE
        } else if self.is_operator() {
            SemanticTokenType::OPERATOR
        } else {
            match self {
                Self::Number => SemanticTokenType::NUMBER,
                Self::String => SemanticTokenType::STRING,
                Self::Identifier => SemanticTokenType::VARIABLE,
                _ => return None,
            }
        })
    }
}
