// use std::rc::{Rc, Weak};
//
// use crate::ast;
//
// #[derive(Debug)]
// pub(crate) struct Workspace {
//     pub scripts: Vec<Script>,
//     pub global_symbols: Vec<Symbol>,
// }
//
// #[derive(Debug)]
// pub(crate) struct Script {
//     pub name: Rc<Name>,
//     pub items: Vec<Item>,
// }
//
// #[derive(Debug)]
// pub(crate) enum Item {
//     FnDecl(FnDecl),
//     BlockType(BlockType),
// }
//
// #[derive(Debug)]
// pub(crate) struct FnDecl {
//     name: Name,
// }
//
// #[derive(Debug)]
// pub(crate) struct Name {
//     pub name: String,
//     pub type_: Type,
// }
//
// #[derive(Debug)]
// pub(crate) enum Symbol {
//     Name(Rc<Name>),
// }
//
// #[derive(Debug)]
// pub(crate) enum Type {
//     Bool,
//     Number,
//     Ref,
//     String,
//     Array,
// }
