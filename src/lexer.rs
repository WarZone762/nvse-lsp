use crate::node::{Token, TokenKind};

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

    fn next_token(&mut self) -> Option<Token> {
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

            if c == '/' && self.peek(1) == Some('/') {
                return Some(self.comment());
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

    fn comment(&mut self) -> Token {
        self.next_char();
        self.next_char();
        while let Some(c) = self.peek(0) {
            if c == '\n' {
                self.next_char();
                break;
            }
            self.next_char();
        }

        self.finish_token(TokenKind::Comment)
    }

    fn number(&mut self) -> Token {
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

    fn ident(&mut self) -> Token {
        self.next_char();
        while let Some(c) = self.peek(0)
            && (c.is_alphanumeric() || c == '_')
        {
            self.next_char();
        }

        self.finish_token(
            TokenKind::from_str(&self.token_text().to_lowercase()).unwrap_or(TokenKind::Identifier),
        )
    }

    fn string(&mut self) -> Token {
        self.next_char();
        while let Some(c) = self.peek(0)
            && (c != '"' || self.peek(-1).is_some_and(|x| x == '\\'))
        {
            self.next_char();
        }
        self.next_char();

        self.finish_token(TokenKind::String)
    }

    fn op_misc(&mut self) -> Token {
        let c = self.next_char().unwrap();

        if let Some(next_c) = self.peek(0) {
            if matches!(c, '+' | '-' | '*' | '/' | '%' | '^' | '=' | '!' | '<' | '>')
                && next_c == '='
                || matches!(c, '+' | '-' | '|' | '&') && next_c == c
            {
                self.next_char();
            }
        }

        let text = self.token_text();
        self.finish_token(TokenKind::from_str(text).unwrap_or(TokenKind::Error))
    }

    fn finish_token(&mut self, kind: TokenKind) -> Token {
        let t = Token::new(
            kind,
            self.token_start_pos,
            self.pos - self.token_start_pos,
            self.token_start_byte_pos,
            self.byte_pos - self.token_start_byte_pos,
        );

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

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn token_from_str(kind: TokenKind, string: &str, offset: usize) -> Token {
        let byte_offset = string.char_indices().nth(offset).map(|x| x.0).unwrap_or(0);
        Token::new(kind, offset, string.len(), byte_offset, string.bytes().len())
    }

    fn test_str(string: &str, kind_fn: impl FnOnce() -> TokenKind) {
        let mut lexer = Lexer::new(string);
        assert_eq!(token_from_str(kind_fn(), string, 0), lexer.next().unwrap(), "{string:?}",);
    }

    #[test]
    fn comment() {
        fn test(string: &str) {
            test_str(string, || TokenKind::Comment);
        }

        test("//");
        test("//1");
        test("//123");
        test("// ");
        test("// 1");
        test("// 123");
        test("// 123 ");
        test("//\n");
        test("//1\n");
        test("//123\n");
        test("// \n");
        test("// 1\n");
        test("// 123\n");
        test("// 123 \n");
    }

    #[test]
    fn number() {
        fn test(string: &str) {
            test_str(string, || TokenKind::Number);
        }

        test("1");
        test("123");
        test("1.2");
        test("12.3");
        test("1.23");
        test("1.23");

        let mut lexer = Lexer::new(".1");
        assert_eq!(token_from_str(TokenKind::Number, "1", 1), lexer.nth(1).unwrap());

        let mut lexer = Lexer::new("1.");
        assert_eq!(token_from_str(TokenKind::Number, "1", 0), lexer.next().unwrap());
    }

    #[test]
    fn ident() {
        fn test(string: &str) {
            test_str(string, || {
                TokenKind::from_str(&string.to_lowercase()).unwrap_or(TokenKind::Identifier)
            });
        }

        test("if");
        test("else");
        test("while");
        test("fn");
        test("return");
        test("break");
        test("continue");
        test("for");
        test("name");

        test("MenuMode");
        test("GameMode");

        test("int");
        test("double");
        test("ref");
        test("string");
        test("array");

        test("foo");
        test("bar1");

        let mut lexer = Lexer::new("1a");
        assert_eq!(token_from_str(TokenKind::Number, "1", 0), lexer.next().unwrap());
        assert_eq!(token_from_str(TokenKind::Identifier, "a", 1), lexer.next().unwrap());
    }

    #[test]
    fn string() {
        fn test(string: &str) {
            test_str(string.trim(), || TokenKind::String);
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
            test_str(string, || TokenKind::from_str(string).unwrap());
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
        test("%");
        test("%=");
        test("^");
        test("^=");
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
        test("#");
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
