use std::{iter, rc::Rc};

use super::{Node, NodeOrToken, NodeOrTokenRef, Token};

impl<'a> NodeOrTokenRef<'a> {
    pub fn siblings(&self) -> Option<impl Iterator<Item = &'a NodeOrToken> + 'a> {
        Some(self.parent()?.children.iter())
    }

    pub fn prev_sibling(&self) -> Option<&'a NodeOrToken> {
        let parent = self.parent()?;
        let idx = parent.children.iter().position(|x| NodeOrTokenRef::from(x) == *self)?;
        Some(&parent.children[idx.checked_sub(1)?])
    }

    pub fn next_sibling(&self) -> Option<&'a NodeOrToken> {
        let parent = self.parent()?;
        let idx = parent.children.iter().position(|x| NodeOrTokenRef::from(x) == *self)?;
        parent.children.get(idx.checked_add(1)?)
    }
}

impl Node {
    pub fn dfs(&self) -> Iter<'_> {
        Iter::new(self)
    }

    pub fn ancestors(self: Rc<Self>) -> impl Iterator<Item = Rc<Node>> {
        iter::successors(self.parent(), |x| x.parent())
    }

    pub fn first_token(&self) -> Option<&Token> {
        let mut parent = self;
        loop {
            match parent.children.first()? {
                NodeOrToken::Node(x) => parent = x,
                NodeOrToken::Token(x) => return Some(x),
            }
        }
    }

    pub fn last_token(&self) -> Option<&Token> {
        let mut parent = self;
        loop {
            match parent.children.last()? {
                NodeOrToken::Node(x) => parent = x,
                NodeOrToken::Token(x) => return Some(x),
            }
        }
    }

    pub fn leafs(&self) -> impl Iterator<Item = &Token> {
        self.dfs().filter_map(|x| x.token())
    }

    pub fn tokens_within_range(&self, start: u32, end: u32) -> impl Iterator<Item = &Token> {
        self.leafs().skip_while(move |x| x.offset < start).take_while(move |x| x.end() <= end)
    }

    pub fn token_at_offset(&self, offset: u32) -> Option<&Token> {
        self.leafs().find(|x| x.offset <= offset && offset < x.offset + x.len)
    }

    pub fn nearest_token(&self, offset: u32) -> Option<&Token> {
        self.leafs().take_while(|x| x.offset <= offset).last()
    }
}

impl Token {
    pub fn ancestors(&self) -> impl Iterator<Item = Rc<Node>> {
        iter::successors(self.parent(), |x| x.parent())
    }

    pub fn next_token(&self) -> Option<&Token> {
        let mut r = NodeOrTokenRef::from(self);
        loop {
            if let Some(next) = r.next_sibling() {
                match next {
                    NodeOrToken::Node(x) => {
                        if let Some(next_token) = x.first_token() {
                            return Some(next_token);
                        } else {
                            r = next.into();
                        }
                    }
                    NodeOrToken::Token(x) => return Some(x),
                }
            } else {
                r = r.parent()?.into();
            }
        }
    }

    pub fn prev_token(&self) -> Option<&Token> {
        let mut r = NodeOrTokenRef::from(self);
        loop {
            if let Some(prev) = r.prev_sibling() {
                match prev {
                    NodeOrToken::Node(x) => {
                        if let Some(prev_token) = x.last_token() {
                            return Some(prev_token);
                        } else {
                            r = prev.into();
                        }
                    }
                    NodeOrToken::Token(x) => return Some(x),
                }
            } else {
                r = r.parent()?.into();
            }
        }
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
