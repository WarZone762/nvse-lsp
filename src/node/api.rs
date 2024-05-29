use std::rc::Rc;

use super::{Node, NodeOrToken, Token};

impl NodeOrToken {
    pub fn dfs(&self) -> Iter<'_> {
        Iter::new(self)
    }
}

#[derive(Debug)]
pub(crate) struct Iter<'a> {
    stack: Vec<&'a NodeOrToken>,
}

impl<'a> Iter<'a> {
    pub fn new(root: &'a NodeOrToken) -> Self {
        Self { stack: vec![root] }
    }
}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a NodeOrToken;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(top) = self.stack.pop() {
            if let NodeOrToken::Node(x) = top {
                self.stack.extend(x.children.iter().rev());
            }
            Some(top)
        } else {
            None
        }
    }
}
