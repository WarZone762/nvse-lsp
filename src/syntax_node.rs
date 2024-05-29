use std::{
    fmt::Debug,
    rc::{Rc, Weak},
};

use tower_lsp::lsp_types::SemanticTokenType;

mod api;

#[derive(Debug, Clone)]
pub(crate) enum NodeOrToken {
    Node(Rc<Node>),
    Token(Rc<Token>),
}

impl From<Rc<Node>> for NodeOrToken {
    fn from(value: Rc<Node>) -> Self {
        Self::Node(value)
    }
}

impl From<Rc<Token>> for NodeOrToken {
    fn from(value: Rc<Token>) -> Self {
        Self::Token(value)
    }
}

impl NodeOrToken {
    pub fn node(&self) -> Option<&Rc<Node>> {
        match self {
            Self::Node(x) => Some(x),
            Self::Token(_) => None,
        }
    }

    pub fn token(&self) -> Option<&Rc<Token>> {
        match self {
            Self::Node(_) => None,
            Self::Token(x) => Some(x),
        }
    }

    pub fn offset(&self) -> u32 {
        match self {
            Self::Node(x) => x.offset,
            Self::Token(x) => x.offset,
        }
    }

    pub fn end(&self) -> u32 {
        match self {
            Self::Node(x) => x.end(),
            Self::Token(x) => x.end(),
        }
    }

    pub fn parent(&self) -> &Option<Weak<Node>> {
        match self {
            Self::Node(x) => &x.parent,
            Self::Token(x) => &x.parent,
        }
    }

    pub unsafe fn parent_mut(&mut self) -> &mut Option<Weak<Node>> {
        match self {
            Self::Node(x) => &mut Rc::get_mut_unchecked(x).parent,
            Self::Token(x) => &mut Rc::get_mut_unchecked(x).parent,
        }
    }
}

pub(crate) struct Node {
    pub kind: NodeKind,
    pub offset: u32,
    pub children: Vec<NodeOrToken>,
    pub parent: Option<Weak<Node>>,
}

impl Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = f.debug_struct("Node");
        out.field("kind", &self.kind)
            .field("offset", &self.offset)
            .field("children", &self.children);
        if let Some(parent) = &self.parent {
            out.field_with("parent", |f| {
                f.debug_struct("Node")
                    .field("kind", &parent.upgrade().unwrap().kind)
                    .field("offset", &parent.upgrade().unwrap().offset)
                    .finish()
            });
        } else {
            out.field("parent", &self.parent);
        }

        out.finish()
    }
}

impl Node {
    pub fn new(kind: NodeKind, offset: u32) -> Self {
        Self { kind, offset, children: Vec::new(), parent: None }
    }

    pub fn tree_string(&self, text: &str) -> String {
        self.tree_string_inner(text, 0)
    }

    fn tree_string_inner(&self, text: &str, ident: usize) -> String {
        let mut string =
            format!("{}{:?}@{}..{}", "    ".repeat(ident), self.kind, self.offset, self.end());
        for child in &self.children {
            string.push('\n');
            string.push_str(&match child {
                NodeOrToken::Node(x) => x.tree_string_inner(text, ident + 1),
                NodeOrToken::Token(x) => {
                    format!(
                        "{}{:?}@{}..{} {:?}",
                        "    ".repeat(ident + 1),
                        x.kind,
                        x.offset,
                        x.end(),
                        x.text(text)
                    )
                }
            })
        }
        string
    }

    pub fn parent(&self) -> Option<Rc<Node>> {
        Some(self.parent.as_ref()?.upgrade().unwrap())
    }

    pub fn end(&self) -> u32 {
        self.children.last().map(|x| x.end()).unwrap_or(0)
    }
}

#[derive(Clone)]
pub(crate) struct Token {
    pub kind: TokenKind,
    pub offset: u32,
    pub len: u32,
    pub byte_offset: u32,
    pub byte_len: u32,
    pub parent: Option<Weak<Node>>,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = f.debug_struct("Token");
        out.field("kind", &self.kind)
            .field("offset", &self.offset)
            .field("len", &self.len)
            .field("byte_offset", &self.byte_offset)
            .field("byte_len", &self.byte_len);
        if let Some(parent) = &self.parent {
            out.field_with("parent", |f| {
                f.debug_struct("Node")
                    .field("kind", &parent.upgrade().unwrap().kind)
                    .field("offset", &parent.upgrade().unwrap().offset)
                    .finish()
            });
        } else {
            out.field("parent", &self.parent);
        }

        out.finish()
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.offset == other.offset && self.len == other.len
    }
}

impl Eq for Token {}

impl Token {
    pub fn new(kind: TokenKind, offset: u32, len: u32, byte_offset: u32, byte_len: u32) -> Self {
        Self { kind, offset, len, byte_offset, byte_len, parent: None }
    }

    pub fn parent(&self) -> Option<Rc<Node>> {
        Some(self.parent.as_ref()?.upgrade().unwrap())
    }

    pub fn text<'a>(&self, string: &'a str) -> &'a str {
        unsafe {
            std::str::from_utf8_unchecked(
                &string.as_bytes()
                    [self.byte_offset as usize..(self.byte_offset + self.byte_len) as usize],
            )
        }
    }

    pub fn end(&self) -> u32 {
        self.offset + self.len
    }
}

macro_rules! tokens {
    ($(($group:ident) $($($lit:literal =>)? $ident:ident $(($display:literal))?,)*)*) => {
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
                    blocktypes!() => Some(Self::BlockType),
                    "true" | "false" => Some(Self::Bool),
                    _ => None,
                }
            }
        }

        impl ::std::fmt::Display for TokenKind {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                match self {
                    $($(
                        $(Self::$ident => f.write_str($lit),)?
                        $(Self::$ident => f.write_str($display),)?
                    )*)*
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

// TODO
macro_rules! blocktypes {
    () => {
        "menumode" | "gamemode"
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NodeKind {
    Script,
    BeginStmt,
    FnDeclStmt,
    VarDeclStmt,
    ExprStmt,
    ForStmt,
    IfStmt,
    ReturnStmt,
    BreakStmt,
    ContinueStmt,
    WhileStmt,
    BlockStmt,
    AssignmentExpr,
    TernaryExpr,
    BinaryExpr,
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

    ArgList,
    ParamList,
    VarDecl,
    Name,
    NameRef,
    Literal,

    Error,
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
    BlockType("block type"),
    "name" => Name,
    "continue" => Continue,
    "break" => Break,

    (is_type)
    "int" => IntType,
    "double" => DoubleType,
    "ref" => RefType,
    "string" => StringType,
    "array" => ArrayType,

    (is_unary_and_bin_op)
    "+" => Plus,
    "-" => Minus,
    "*" => Star,
    "&" => BitwiseAnd,

    (is_unary_only_op)
    "++" => PlusPlus,
    "--" => MinusMinus,
    "$" => Dollar,
    "#" => Pound,
    "!" => Bang,
    "~" => Tilde,

    (is_bin_only_op)
    "+=" => PlusEq,
    "-=" => MinusEq,
    "*=" => StarEq,
    "/" => Slash,
    "/=" => SlashEq,
    "%" => Mod,
    "%=" => ModEq,
    "^" => Pow,
    "^=" => PowEq,
    "|=" => BitwiseOrEq,
    "&=" => BitwiseAndEq,
    "=" => Eq,
    "==" => EqEq,
    "<" => Less,
    ">" => Greater,
    "<=" => LessEq,
    ">=" => GreaterEq,
    "!=" => BangEq,
    "||" => LogicOr,
    "&&" => LogicAnd,
    "<<" => Lt2,
    ">>" => Gt2,
    "|" => BitwiseOr,
    ":" => Colon,
    "::" => Colon2,
    "." => Dot,


    (is_brace)
    "{" => LeftBrace,
    "}" => RightBrace,
    "[" => LeftBracket,
    "]" => RightBracket,
    "(" => LeftParen,
    ")" => RightParen,

    (is_literal)
    String("string"),
    Number("number"),
    Bool("boolean"),

    (is_misc)
    Identifier("identifier"),
    "," => Comma,
    ";" => Semicolon,
    "?" => Ternary,
    Whitespace("whitespace"),
    Comment("comment"),
    Eof("end of file"),
    Error("error"),
}

impl TokenKind {
    pub fn is_op(&self) -> bool {
        self.is_unary_only_op()
            || self.is_bin_only_op()
            || self.is_unary_and_bin_op()
            || *self == TokenKind::Ternary
    }

    pub fn is_unary_op(&self) -> bool {
        self.is_unary_only_op() || self.is_unary_and_bin_op()
    }

    pub fn is_bin_op(&self) -> bool {
        self.is_bin_only_op() || self.is_unary_and_bin_op()
    }

    pub fn to_semantic(self) -> Option<SemanticTokenType> {
        Some(if self.is_keyword() {
            SemanticTokenType::KEYWORD
        } else if self.is_type() {
            SemanticTokenType::TYPE
        } else if self.is_op() {
            SemanticTokenType::OPERATOR
        } else if self.is_brace() {
            SemanticTokenType::new("punctuation")
        } else {
            match self {
                Self::String => SemanticTokenType::STRING,
                Self::Number => SemanticTokenType::NUMBER,
                Self::Bool => SemanticTokenType::new("boolean"),
                Self::Identifier => SemanticTokenType::VARIABLE,
                Self::Comment => SemanticTokenType::COMMENT,
                _ => return None,
            }
        })
    }
}
