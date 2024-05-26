use tower_lsp::lsp_types::SemanticTokenType;

pub(crate) struct Lexer<'a> {
    input: &'a str,
    indices: Vec<(usize, char)>,
    pos: usize,
    byte_pos: usize,

    token_start_pos: usize,
    token_start_byte_pos: usize,

    done: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            indices: input.char_indices().collect(),
            pos: 0,
            byte_pos: 0,

            token_start_pos: 0,
            token_start_byte_pos: 0,

            done: false,
        }
    }

    fn next_token(&mut self) -> Option<Token<'a>> {
        if let Some(c) = self.peek(0) {
            if c.is_whitespace() {
                self.next_char();
                while let Some(c) = self.peek(0)
                    && c.is_whitespace()
                {
                    self.next_char();
                }
                return Some(self.finish_token(TokenKind::Whitespace));
            }

            if c.is_ascii_digit() {
                return Some(self.number());
            }

            if c.is_alphabetic() || c == '_' {
                return Some(self.ident());
            }

            if c == '"' {
                return Some(self.string());
            }

            return Some(self.op_misc());
        }
        if self.done {
            None
        } else {
            self.done = true;
            Some(self.finish_token(TokenKind::Eof))
        }
    }

    fn number(&mut self) -> Token<'a> {
        self.next_char();
        let mut has_dot = false;
        while let Some(c) = self.peek(0) {
            if c.is_ascii_digit() {
            } else if c == '.' && !has_dot && self.peek(1).is_some_and(|x| x.is_ascii_digit()) {
                has_dot = true;
            } else {
                break;
            }
            self.next_char();
        }

        self.finish_token(TokenKind::Number)
    }

    fn ident(&mut self) -> Token<'a> {
        self.next_char();
        while let Some(c) = self.peek(0)
            && c.is_alphanumeric()
        {
            self.next_char();
        }

        self.finish_token(
            TokenKind::from_str(&self.token_text().to_lowercase()).unwrap_or(TokenKind::Identifier),
        )
    }

    fn string(&mut self) -> Token<'a> {
        self.next_char();
        while let Some(c) = self.peek(0)
            && (c != '"' || self.peek(-1).is_some_and(|x| x == '\\'))
        {
            self.next_char();
        }
        self.next_char();

        self.finish_token(TokenKind::String)
    }

    fn op_misc(&mut self) -> Token<'a> {
        let c = self.next_char().unwrap();

        if let Some(next_c) = self.peek(0) {
            if matches!(c, '+' | '-' | '*' | '/' | '=' | '!' | '<' | '>') && next_c == '='
                || matches!(c, '+' | '-' | '|' | '&') && next_c == c
            {
                self.next_char();
            }
        }

        let text = self.token_text();
        self.finish_token(TokenKind::from_str(text).unwrap_or(TokenKind::Error))
    }

    fn finish_token(&mut self, kind: TokenKind) -> Token<'a> {
        let text = self.token_text();
        let t = Token::new(kind, self.token_start_pos, text);

        self.token_start_pos = self.pos;
        self.token_start_byte_pos = self.byte_pos;

        t
    }

    fn token_text(&self) -> &'a str {
        unsafe {
            std::str::from_utf8_unchecked(
                &self.input.as_bytes()[self.token_start_byte_pos..self.byte_pos],
            )
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let next = self.indices.get(self.pos)?;
        self.pos += 1;
        self.byte_pos += next.1.len_utf8();
        Some(next.1)
    }

    fn peek(&self, n: isize) -> Option<char> {
        Some(self.indices.get(self.pos.wrapping_add_signed(n))?.1)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Token<'a> {
    pub kind: TokenKind,
    pub offset: usize,
    pub text: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, offset: usize, text: &'a str) -> Self {
        Self { kind, offset, text }
    }
}

impl Token<'_> {
    pub fn len(&self) -> usize {
        self.text.len()
    }
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

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn number() {
        fn test(string: &str) {
            let mut lexer = Lexer::new(string);
            assert_eq!(
                Token::new(TokenKind::Number, 0, string),
                lexer.next().unwrap(),
            );
        }

        test("1");
        test("123");
        test("1.2");
        test("12.3");
        test("1.23");
        test("1.23");

        let mut lexer = Lexer::new(".1");
        assert_eq!(Token::new(TokenKind::Number, 1, "1"), lexer.nth(1).unwrap());

        let mut lexer = Lexer::new("1.");
        assert_eq!(Token::new(TokenKind::Number, 0, "1"), lexer.next().unwrap());
    }

    #[test]
    fn ident() {
        fn test(string: &str) {
            let mut lexer = Lexer::new(string);
            assert_eq!(
                Token::new(
                    TokenKind::from_str(string).unwrap_or(TokenKind::Identifier),
                    0,
                    string
                ),
                lexer.next().unwrap(),
            );
        }

        test("if");
        test("else");
        test("while");
        test("fn");
        test("return");
        test("for");
        test("name");
        test("begin");

        test("int");
        test("double");
        test("ref");
        test("string");
        test("array");

        test("foo");
        test("bar1");

        let mut lexer = Lexer::new("1a");
        assert_eq!(Token::new(TokenKind::Number, 0, "1"), lexer.next().unwrap());
        assert_eq!(
            Token::new(TokenKind::Identifier, 1, "a"),
            lexer.next().unwrap()
        );
    }

    #[test]
    fn string() {
        fn test(string: &str) {
            let string = string.trim();
            let mut lexer = Lexer::new(string);
            assert_eq!(
                Token::new(TokenKind::String, 0, string),
                lexer.next().unwrap(),
            );
        }

        test(r#"   ""   "#);
        test(r#"   "1"   "#);
        test(r#"   "123"   "#);
        test(r#"   "\""   "#);
        test(r#"   "\"\""   "#);
        test(r#"   "\"123\""   "#);
        test(r#"   "\"123\"   "#);
    }

    #[test]
    fn ops_misc() {
        fn test(string: &str) {
            let mut lexer = Lexer::new(string);
            assert_eq!(
                Token::new(TokenKind::from_str(string).unwrap(), 0, string),
                lexer.next().unwrap(),
            );
        }

        test("+");
        test("++");
        test("+=");
        test("-");
        test("--");
        test("-=");
        test("*");
        test("*=");
        test("/");
        test("/=");
        test("=");
        test("==");
        test("!");
        test("!=");
        test("<");
        test("<=");
        test(">");
        test(">=");
        test("|");
        test("||");
        test("&");
        test("&&");
        test("~");
        test("$");
        test("{");
        test("[");
        test("(");
        test("}");
        test("]");
        test(")");
        test(",");
        test(";");
        test("?");
        test(":");
        test(".");
    }
}
