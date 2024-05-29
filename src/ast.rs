use std::{marker::PhantomData, rc::Rc};

use crate::syntax_node::{Node, NodeKind, NodeOrToken, Token, TokenKind};

#[allow(dead_code)]
mod nodes;
pub(crate) use nodes::*;

pub(crate) trait AstNode {
    fn can_cast(kind: NodeKind) -> bool;
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
}
