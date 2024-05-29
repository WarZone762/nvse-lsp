use std::rc::Rc;

use super::{NodeOrToken, Token};

impl NodeOrToken {
    pub fn dfs(&self) -> Iter<'_> {
        Iter::new(self)
    }

    pub fn leafs(&self) -> impl Iterator<Item = &Rc<Token>> {
        self.dfs().filter_map(|x| x.token())
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
