use std::{cell::Cell, mem};

use tower_lsp::lsp_types::DiagnosticSeverity;

use self::other::script;
use crate::syntax_node::{NodeKind, TokenKind};

mod expression;
mod other;
mod statement;

use expression::*;
use other::*;
use statement::*;

pub(crate) fn parse(input: Input) -> Vec<Event> {
    let mut p = Parser::new(input);
    script(&mut p);

    p.finish()
}

#[derive(Debug)]
pub(crate) struct Parser {
    input: Input,
    pos: usize,
    events: Vec<Event>,
    steps: Cell<usize>,
}

impl Parser {
    const STEP_LIMIT: usize = 15_000_000;

    pub fn new(input: Input) -> Self {
        Self { input, pos: 0, events: vec![], steps: 0.into() }
    }

    pub fn finish(self) -> Vec<Event> {
        self.events
    }

    pub fn cur(&self) -> TokenKind {
        self.nth(0)
    }

    pub fn nth(&self, n: usize) -> TokenKind {
        assert!(
            self.steps.get() < Self::STEP_LIMIT,
            "parser step limit exceeded at {:?}@{}",
            self.input.kind(self.pos + n),
            self.pos,
        );
        self.steps.set(self.steps.get() + 1);
        self.input.kind(self.pos + n)
    }

    pub fn more(&self) -> bool {
        !self.at(TokenKind::Eof)
    }

    pub fn at(&self, kind: TokenKind) -> bool {
        self.nth_at(0, kind)
    }

    pub fn nth_at(&self, n: usize, kind: TokenKind) -> bool {
        self.input.kind(self.pos + n) == kind
    }

    pub fn opt(&mut self, kind: TokenKind) -> bool {
        if !self.at(kind) {
            return false;
        }

        self.do_next(kind);
        true
    }

    pub fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.push_event(Event::Start(NodeKind::Tombstone, None));
        Marker::new(pos)
    }

    /// Assert the current token is `kind` and advance
    pub fn next(&mut self, kind: TokenKind) {
        if !self.opt(kind) {
            panic!("unexpected token: expected '{kind}', got '{}'", self.cur())
        }
    }

    pub fn next_any(&mut self) {
        let kind = self.cur();
        if kind == TokenKind::Eof {
            return;
        }
        self.do_next(kind);
    }

    pub fn warn_and_next(&mut self, msg: &str) {
        self.warn(msg);
        self.next_any();
    }

    pub fn warn(&mut self, msg: &str) {
        self.push_event(Event::Diagnostic(msg.into(), DiagnosticSeverity::WARNING));
    }

    pub fn err(&mut self, msg: &str) {
        self.push_event(Event::Diagnostic(
            format!("parsing error: {msg}"),
            DiagnosticSeverity::ERROR,
        ));
    }

    /// Advance and return `true` if the current token is `kind`, otherwise emit
    /// an error and return `false`
    pub fn expect(&mut self, kind: TokenKind) -> bool {
        if self.opt(kind) {
            return true;
        }
        self.err(&format!("expected '{kind}'"));
        false
    }

    pub fn err_and_next(&mut self, msg: &str) {
        self.err(msg);
        self.next_any();
    }

    fn do_next(&mut self, kind: TokenKind) {
        self.pos += 1;
        self.steps = 0.into();
        self.push_event(Event::Token(kind));
    }

    pub fn push_event(&mut self, event: Event) {
        self.events.push(event);
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct CompletedMarker {
    pos: usize,
    kind: NodeKind,
}

impl CompletedMarker {
    pub fn new(pos: usize, kind: NodeKind) -> Self {
        Self { pos, kind }
    }

    /// Create a [`Marker`] before `self`
    pub fn precede(&self, p: &mut Parser) -> Marker {
        let new_pos = p.start();

        let Event::Start(_, forward_parent) = &mut p.events[self.pos] else {
            panic!("tried to precede a non-start Marker")
        };
        *forward_parent = Some(new_pos.pos - self.pos);
        new_pos
    }

    /// Extend `self` to the left until `m`
    pub fn extend_to(self, p: &mut Parser, m: Marker) -> CompletedMarker {
        let Event::Start(_, forward_parent) = &mut p.events[self.pos] else {
            panic!("tried to extend_to a non-start Marker")
        };
        *forward_parent = Some(self.pos - m.pos);
        self
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Marker {
    pos: usize,
}

impl Marker {
    pub fn new(pos: usize) -> Self {
        Self { pos }
    }

    pub fn complete(self, p: &mut Parser, kind: NodeKind) -> CompletedMarker {
        let event = &mut p.events[self.pos];
        let Event::Start(node_kind, _) = event else {
            panic!("tried to complete a non-start Marker");
        };
        *node_kind = kind;
        p.push_event(Event::Finish);

        CompletedMarker::new(self.pos, kind)
    }

    pub fn abandon(self, p: &mut Parser) {
        if self.pos == p.events.len() - 1 {
            let Event::Start(NodeKind::Tombstone, None) = p.events.pop().unwrap() else {
                panic!("tried to abandon a valid marker");
            };
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Event {
    Start(NodeKind, Option<usize>),
    Diagnostic(String, DiagnosticSeverity),
    Token(TokenKind),
    Finish,
}

impl Event {
    pub fn take(&mut self) -> Self {
        mem::replace(self, Event::Start(NodeKind::Tombstone, None))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Input {
    tokens: Vec<TokenKind>,
}

impl Input {
    pub fn kind(&self, n: usize) -> TokenKind {
        self.tokens[n]
    }
}

impl FromIterator<TokenKind> for Input {
    fn from_iter<T: IntoIterator<Item = TokenKind>>(iter: T) -> Self {
        Self {
            tokens: iter
                .into_iter()
                .filter(|x| !matches!(x, TokenKind::Whitespace | TokenKind::Comment))
                .collect(),
        }
    }
}

#[cfg(test)]
mod test {
    use std::{
        fs,
        path::{Path, PathBuf},
    };

    use crate::{tree_builder::parse_str, AstNode};

    fn case_paths() -> impl Iterator<Item = (PathBuf, PathBuf)> {
        let test_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("test_data");
        let ast_dir = test_dir.join("ast");
        let case_dir = test_dir.join("cases");
        fs::create_dir_all(&case_dir).unwrap();
        fs::create_dir_all(&ast_dir).unwrap();

        fs::read_dir(case_dir).unwrap().map(move |f| {
            let f = f.unwrap();
            let mut ast = f.path();
            ast.set_extension("ast");
            let ast = ast_dir.join(ast.file_name().unwrap());

            (f.path(), ast)
        })
    }

    fn strip_range(string: &str) -> String {
        let mut buf = String::new();
        let mut chars = string.chars();
        while let Some(c) = chars.next() {
            if c == '@' {
                buf.push(loop {
                    if let Some(c) = chars.next()
                        && (c == ' ' || c == '\n')
                    {
                        break c;
                    }
                });
            } else {
                buf.push(c);
            }
        }
        buf
    }

    macro_rules! test_from_file {
        ($name:ident, $file:literal) => {
            #[test]
            fn $name() {
                let test_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("test_data");
                let ast_dir = test_dir.join("ast");
                let case_dir = test_dir.join("cases");

                let case = case_dir.join(format!("{}.gek", $file));
                let ast = ast_dir.join(format!("{}.ast", $file));

                if !ast.exists() {
                    let text = fs::read_to_string(&case).unwrap();
                    let mut tree = parse_str(&text).0.syntax().tree_string(&text);
                    tree.push('\n');
                    fs::write(&ast, tree).unwrap();
                    println!("Generated {ast:?}");
                }

                let text = fs::read_to_string(&case)
                    .unwrap()
                    .replace("\\r\\n", "\\n")
                    .replace("\r\n", "\n");
                let mut tree = parse_str(&text).0.syntax().tree_string(&text);
                tree.push('\n');
                let must = fs::read_to_string(&ast)
                    .unwrap()
                    .replace("\\r\\n", "\\n")
                    .replace("\r\n", "\n");
                if tree != must {
                    panic!(
                        "{case:?}:\n\n{}",
                        similar_asserts::SimpleDiff::from_str(
                            &strip_range(&must),
                            &strip_range(&tree),
                            "old",
                            "new",
                        )
                    )
                }
            }
        };
    }

    test_from_file!(nvse_ternary_op, "nvse-ternary-operations");
    test_from_file!(nvse_bin_op, "nvse-binary-operations");
    test_from_file!(nvse_unary_op, "nvse-unary-operations");
    test_from_file!(nvse_comments, "nvse-comments");
}
