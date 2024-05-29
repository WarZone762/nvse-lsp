use std::rc::Rc;

use tower_lsp::lsp_types::DiagnosticSeverity;

use crate::{
    lexer::Lexer,
    parser::{parse, Event},
    syntax_node::{Node, NodeKind, NodeOrToken, Token, TokenKind},
};

pub(crate) fn parse_str(string: &str) -> (Rc<Node>, Vec<Diagnostic>) {
    let tokens = Lexer::new(string).collect::<Vec<_>>();

    output_to_tree(process_events(parse(tokens.iter().map(|x| x.kind).collect())), tokens)
}

pub(crate) fn output_to_tree(
    output: Vec<Event>,
    tokens: Vec<Token>,
) -> (Rc<Node>, Vec<Diagnostic>) {
    let builder = TriviaBuilder::new(tokens);
    builder.process(output)
}

pub(crate) fn process_events(mut events: Vec<Event>) -> Vec<Event> {
    let mut output = vec![];
    let mut forward_parents = vec![];

    for i in 0..events.len() {
        match events[i].take() {
            Event::Start(kind, forward_parent) => {
                forward_parents.push(kind);
                let mut idx = i;
                let mut fp = forward_parent;
                while let Some(fwd) = fp {
                    idx += fwd;
                    let Event::Start(kind, forward_parent) = events[idx].take() else {
                        unreachable!()
                    };
                    forward_parents.push(kind);
                    fp = forward_parent;
                }

                output.extend(forward_parents.drain(..).rev().filter_map(|x| match x {
                    NodeKind::Tombstone => None,
                    x => Some(Event::Start(x, None)),
                }));
            }
            x => output.push(x),
        }
    }

    output
}

#[derive(Debug, Default)]
pub(crate) struct TreeBuilder {
    parents: Vec<(Node, usize)>,
    children: Vec<NodeOrToken>,
}

impl TreeBuilder {
    pub fn token(&mut self, token: Token) {
        self.children.push(Rc::new(token).into());
    }

    pub fn start_node(&mut self, kind: NodeKind) {
        let node = Node::new(
            kind,
            self.children
                .last()
                .map(|x| x.end())
                .or_else(|| Some(self.parents.last()?.0.end()))
                .unwrap_or(0),
        );
        self.parents.push((node, self.children.len()));
    }

    pub fn finish_node(&mut self) {
        let (node, first_child) = self
            .parents
            .pop()
            .expect("called TreeBuilder::finish_node more times than TreeBuilder::start_node");
        let mut node = Rc::new(node);
        let n = self.children.len() - first_child;
        for _ in 0..n {
            let mut child = self.children.pop().unwrap();
            unsafe {
                *child.parent_mut() = Some(Rc::downgrade(&node));
                Rc::get_mut_unchecked(&mut node).children.insert(0, child);
            }
        }
        self.children.push(node.into());
    }

    pub fn finish(mut self) -> Rc<Node> {
        let Some(root) = self.children.pop() else { panic!("TreeBuilder has no root node") };
        let NodeOrToken::Node(root) = root else { panic!("TreeBuilder root is a token") };
        root
    }
}

#[derive(Debug)]
pub(crate) struct TriviaBuilder {
    tokens: Vec<Token>,
    token_pos: usize,
    diagnostics: Vec<Diagnostic>,
    tree_builder: TreeBuilder,
}

impl TriviaBuilder {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, token_pos: 0, diagnostics: vec![], tree_builder: TreeBuilder::default() }
    }

    pub fn process(mut self, events: Vec<Event>) -> (Rc<Node>, Vec<Diagnostic>) {
        for e in events {
            match e {
                Event::Start(kind, _) => self.start_node(kind),
                Event::Diagnostic(msg, severity) => self.diagnostic(msg, severity),
                Event::Token(_) => self.token(),
                Event::Finish => self.finish_node(false),
            }
        }
        self.attach_trivia();

        self.finish()
    }

    fn token(&mut self) {
        self.attach_trivia();
        self.do_token();
    }

    fn start_node(&mut self, kind: NodeKind) {
        if !self.tree_builder.parents.is_empty() {
            self.attach_trivia();
        }
        self.tree_builder.start_node(kind);
    }

    fn finish_node(&mut self, is_last: bool) {
        if self.tree_builder.parents.len() > 1 || is_last {
            self.tree_builder.finish_node()
        }
    }

    fn diagnostic(&mut self, msg: String, severity: DiagnosticSeverity) {
        let offset = self
            .tokens
            .get(self.token_pos)
            .or_else(|| self.tokens.last())
            .map(|x| x.offset)
            .unwrap_or(0);

        self.diagnostics.push(Diagnostic { msg, severity, offset, len: 1 });
    }

    fn attach_trivia(&mut self) {
        while self.token_pos < self.tokens.len() {
            let kind = self.tokens[self.token_pos].kind;
            if !matches!(kind, TokenKind::Whitespace | TokenKind::Comment) {
                break;
            }
            self.do_token();
        }
    }

    fn do_token(&mut self) {
        self.tree_builder.token(self.tokens[self.token_pos].clone());
        self.token_pos += 1;
    }

    fn finish(mut self) -> (Rc<Node>, Vec<Diagnostic>) {
        self.finish_node(true);

        (self.tree_builder.finish(), self.diagnostics)
    }
}

#[derive(Debug)]
pub(crate) struct Diagnostic {
    pub msg: String,
    pub severity: DiagnosticSeverity,
    pub offset: u32,
    pub len: u32,
}
