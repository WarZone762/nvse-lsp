use crate::node::{NodeKind, TokenKind};

use super::Parser;

pub(crate) fn script(p: &mut Parser) {
    let m = p.start();

    p.expect(TokenKind::Name);
    p.expect(TokenKind::Identifier);
    p.expect(TokenKind::Semicolon);

    m.complete(p, NodeKind::Script);
}

#[cfg(test)]
mod test {
    use crate::{
        lexer::Lexer,
        parser::{parse, Event},
    };

    use super::*;

    fn lex(string: &str) -> Vec<Event> {
        parse(Lexer::new(string).map(|x| x.kind).collect())
    }

    #[test]
    fn a() {
        let e = lex("name foo;");
        println!("{e:#?}");

        let e = lex("name foo");
        println!("{e:#?}");
    }
}
