use std::{marker::PhantomData, rc::Rc};

use crate::syntax_node::{Node, NodeKind, NodeOrToken, NodeOrTokenRef, Token, TokenKind};

#[allow(dead_code)]
mod nodes;
pub(crate) use nodes::*;

pub(crate) trait AstNode {
    fn can_cast(kind: NodeKind) -> bool
    where
        Self: Sized;
    fn cast(syntax_node: Rc<Node>) -> Option<Self>
    where
        Self: Sized;
    fn syntax(&self) -> &Rc<Node>;
}

pub(crate) trait AstToken {
    fn can_cast(kind: TokenKind) -> bool;
    fn cast(syntax_node: Token) -> Option<Self>
    where
        Self: Sized;
    fn syntax(&self) -> &Token;
}

pub(crate) trait AsSyntax {
    fn as_syntax(&self) -> NodeOrTokenRef<'_>;
}

impl<T: AstNode> AsSyntax for &T {
    fn as_syntax(&self) -> NodeOrTokenRef<'_> {
        NodeOrTokenRef::Node(self.syntax())
    }
}

impl AsSyntax for &Token {
    fn as_syntax(&self) -> NodeOrTokenRef<'_> {
        NodeOrTokenRef::Token(self)
    }
}

#[derive(Debug)]
pub(crate) struct AstChildren<'a, N> {
    inner: std::slice::Iter<'a, NodeOrToken>,
    _phantom: PhantomData<N>,
}

impl<'a, N> AstChildren<'a, N> {
    pub fn new(node: &'a Rc<Node>) -> Self {
        Self { inner: node.children.iter(), _phantom: PhantomData }
    }
}

impl<N: AstNode> Iterator for AstChildren<'_, N> {
    type Item = N;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.find_map(|x| match x {
            NodeOrToken::Node(x) => N::cast(x.clone()),
            NodeOrToken::Token(_) => None,
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self
            .inner
            .clone()
            .filter(|x| match x {
                NodeOrToken::Node(x) => N::can_cast(x.kind),
                NodeOrToken::Token(_) => false,
            })
            .count();
        (len, Some(len))
    }
}

impl<N: AstNode> ExactSizeIterator for AstChildren<'_, N> {}
