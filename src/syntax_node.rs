use std::{
    fmt::Debug,
    rc::{Rc, Weak},
};

use tower_lsp::lsp_types::SemanticTokenType;

use crate::semantic_tokens::SemanticTokenTypeCustom;

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
            Self::Node(x) => unsafe { &mut Rc::get_mut_unchecked(x).parent },
            Self::Token(x) => unsafe { &mut Rc::get_mut_unchecked(x).parent },
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum NodeOrTokenRef<'a> {
    Node(&'a Node),
    Token(&'a Token),
}

impl<'a> NodeOrTokenRef<'a> {
    pub fn node(&self) -> Option<&'a Node> {
        match self {
            NodeOrTokenRef::Node(x) => Some(x),
            NodeOrTokenRef::Token(_) => None,
        }
    }

    pub fn token(&self) -> Option<&'a Token> {
        match self {
            NodeOrTokenRef::Node(_) => None,
            NodeOrTokenRef::Token(x) => Some(x),
        }
    }

    pub fn offset(&self) -> u32 {
        match self {
            NodeOrTokenRef::Node(x) => x.offset,
            NodeOrTokenRef::Token(x) => x.offset,
        }
    }

    pub fn end(&self) -> u32 {
        match self {
            NodeOrTokenRef::Node(x) => x.end(),
            NodeOrTokenRef::Token(x) => x.end(),
        }
    }

    pub fn parent(&self) -> Option<&'a Node> {
        match self {
            NodeOrTokenRef::Node(x) => unsafe { Some(Rc::as_ptr(&x.parent()?).as_ref_unchecked()) },
            NodeOrTokenRef::Token(x) => unsafe {
                Some(Rc::as_ptr(&x.parent()?).as_ref_unchecked())
            },
        }
    }
}

impl<'a> From<&'a NodeOrToken> for NodeOrTokenRef<'a> {
    fn from(value: &'a NodeOrToken) -> Self {
        match value {
            NodeOrToken::Node(x) => x.as_ref().into(),
            NodeOrToken::Token(x) => x.as_ref().into(),
        }
    }
}

impl<'a> From<&'a Node> for NodeOrTokenRef<'a> {
    fn from(value: &'a Node) -> Self {
        Self::Node(value)
    }
}

impl<'a> From<&'a Token> for NodeOrTokenRef<'a> {
    fn from(value: &'a Token) -> Self {
        Self::Token(value)
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

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.offset == other.offset
    }
}

impl Eq for Node {}

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
        #[allow(dead_code, non_camel_case_types, clippy::upper_case_acronyms)]
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
                    blocktypes!() => Some(Self::BLOCK_TYPE),
                    "true" | "false" => Some(Self::BOOL),
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
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
pub(crate) enum NodeKind {
    SCRIPT,
    FN_DECL_ITEM,
    BLOCK_TYPE_ITEM,

    VAR_DECL_STMT,
    EXPR_STMT,
    FOR_STMT,
    FOR_RANGE_STMT,
    IF_STMT,
    RETURN_STMT,
    BREAK_STMT,
    CONTINUE_STMT,
    WHILE_STMT,
    BLOCK_STMT,

    EMPTY_EXPR,
    TERNARY_EXPR,
    BIN_EXPR,
    UNARY_EXPR,
    POSTFIX_EXPR,
    FIELD_EXPR,
    SUBSCRIPT_EXPR,
    CALL_EXPR,
    STR_EXPR,
    PAREN_EXPR,
    LAMBDA_EXPR,

    ARG_LIST,
    PARAM_LIST,
    ARR_PAT,
    VAR_DECL,
    NAME,
    NAME_REF,
    STR_SHARD_LITERAL,
    STR_SHARD_EXPR,
    LITERAL,

    ERROR,
    TOMBSTONE,
}

tokens! {
    (is_keyword)
    "if" => IF_KW,
    "else" => ELSE_KW,
    "while" => WHILE_KW,
    "fn" => FN_KW,
    "return" => RETURN_KW,
    "for" => FOR_KW,
    "in" => IN_KW,
    "export" => EXPORT_KW,
    BLOCK_TYPE("block type"),
    "name" => NAME_KW,
    "continue" => CONTINUE_KW,
    "break" => BREAK_KW,

    (is_type)
    "int" => INT_TY,
    "double" => DOUBLE_TY,
    "float" => FLOAT_TY,
    "ref" => REF_TY,
    "string" => STRING_TY,
    "array" => ARRAY_TY,

    (is_unary_and_bin_op)
    "+" => PLUS,
    "-" => MINUS,
    "*" => ASTERISK,
    "&" => AMPERSAND,

    (is_unary_only_op)
    "++" => PLUS_2,
    "--" => MINUS_2,
    "$" => DOLLAR,
    "#" => NUM_SIGN,
    "!" => EXCLAMATION,
    "~" => TILDE,

    (is_bin_only_op)
    "+=" => PLUS_EQ,
    "-=" => MINUS_EQ,
    "*=" => ASTERISK_EQ,
    "/" => SLASH,
    "/=" => SLASH_EQ,
    "%" => PERCENT,
    "%=" => PERCENT_EQ,
    "^" => CIRCUMFLEX,
    "^=" => CIRCUMFLEX_EQ,
    "|=" => VBAR_EQ,
    "&=" => AMPERSAND_EQ,
    "=" => EQ,
    "==" => EQ_2,
    "<" => LT,
    ">" => GT,
    "<=" => LT_EQ,
    ">=" => GT_EQ,
    "!=" => EXCLAMATION_EQ,
    "||" => VBAR_2,
    "&&" => AMPERSAND_2,
    "<<" => LT_2,
    ">>" => GT_2,
    "|" => VBAR,
    ":" => COLON,
    "::" => COLON_2,


    (is_paired)
    "{" => LBRACK,
    "}" => RBRACK,
    "[" => LSQ_BRACK,
    "]" => RSQ_BRACK,
    "(" => LPAREN,
    ")" => RPAREN,

    (is_literal)
    NUMBER("number"),
    BOOL("boolean"),

    (is_misc)
    IDENT("identifier"),
    "${" => DOLLAR_LBRACK,
    "\"" => QUOTE_DOUBLE,
    "," => COMMA,
    "." => DOT,
    ";" => SEMICOLON,
    "?" => QUESTION_MARK,
    STR_SHARD("string"),
    WHITESPACE("whitespace"),
    COMMENT("comment"),
    EOF("end of file"),
    ERROR("error"),
}

impl TokenKind {
    pub fn is_op(&self) -> bool {
        self.is_unary_only_op()
            || self.is_bin_only_op()
            || self.is_unary_and_bin_op()
            || *self == TokenKind::QUESTION_MARK
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
        } else if self.is_paired() {
            SemanticTokenTypeCustom::PUNCTUATION
        } else {
            match self {
                Self::DOLLAR_LBRACK => SemanticTokenTypeCustom::ESCAPE_SEQUENCE,
                Self::STR_SHARD | Self::QUOTE_DOUBLE => SemanticTokenType::STRING,
                Self::NUMBER => SemanticTokenType::NUMBER,
                Self::BOOL => SemanticTokenTypeCustom::BOOLEAN,
                Self::IDENT => SemanticTokenType::VARIABLE,
                Self::COMMENT => SemanticTokenType::COMMENT,
                _ => return None,
            }
        })
    }
}
