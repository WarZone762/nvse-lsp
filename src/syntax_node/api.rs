use super::{Node, NodeOrTokenRef, Token};

impl Node {
    pub fn dfs(&self) -> Iter<'_> {
        Iter::new(self)
    }

    pub fn leafs(&self) -> impl Iterator<Item = &Token> {
        self.dfs().filter_map(|x| x.token())
    }

    pub fn token_at_offset(&self, offset: u32) -> Option<&Token> {
        self.leafs().find(|x| x.offset <= offset && offset < x.offset + x.len)
    }
}

#[derive(Debug)]
pub(crate) struct Iter<'a> {
    stack: Vec<NodeOrTokenRef<'a>>,
}

impl<'a> Iter<'a> {
    pub fn new(root: &'a Node) -> Self {
        Self { stack: vec![root.into()] }
    }
}

impl<'a> Iterator for Iter<'a> {
    type Item = NodeOrTokenRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let top = self.stack.pop()?;
        if let Some(top) = top.node() {
            self.stack.extend(top.children.iter().rev().map(|x| x.into()));
        }
        Some(top)
    }
}
