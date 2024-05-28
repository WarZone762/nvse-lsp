use super::*;
use crate::node::{NodeKind, TokenKind};

pub(crate) fn script(p: &mut Parser) {
    let m = p.start();

    // Name
    p.expect(TokenKind::Name);
    name(p);
    p.expect(TokenKind::Semicolon);

    // Variable declarations
    // TODO: TokenSet
    while p.more() && !p.at(TokenKind::BlockType) && !p.at(TokenKind::Fn) {
        stmt_var_decl(p);
    }

    match p.cur() {
        TokenKind::BlockType => begin(p),
        TokenKind::Fn => fn_decl(p),
        _ => p.err_and_next("expected block type (GameMode, MenuMode, ...) or 'fn'"),
    }

    while p.more() {
        p.err("cannot have multiple blocks in one script");
        match p.cur() {
            TokenKind::BlockType => begin(p),
            TokenKind::Fn => fn_decl(p),
            _ => p.err_and_next("expected block type (GameMode, MenuMode, ...) or 'fn'"),
        }
    }

    m.complete(p, NodeKind::Script);
}

pub(crate) fn begin(p: &mut Parser) {
    let m = p.start();
    p.next(TokenKind::BlockType);
    if p.opt(TokenKind::Colon) {
        expr_primary(p);
    }
    stmt_block(p);

    m.complete(p, NodeKind::BeginStmt);
}

pub(crate) fn fn_decl(p: &mut Parser) {
    let m = p.start();

    p.next(TokenKind::Fn);
    param_list(p);
    stmt_block(p);

    m.complete(p, NodeKind::FnDeclStmt);
}

pub(crate) fn arg_list(p: &mut Parser) {
    let m = p.start();

    p.next(TokenKind::LeftParen);
    while p.more() && !p.at(TokenKind::RightParen) {
        expr(p);
        if p.at(TokenKind::RightParen) || !p.expect(TokenKind::Comma) {
            break;
        }
    }
    p.expect(TokenKind::RightParen);

    m.complete(p, NodeKind::ArgList);
}

pub(crate) fn param_list(p: &mut Parser) {
    let m = p.start();

    p.next(TokenKind::LeftParen);
    while p.more() && !p.at(TokenKind::RightParen) {
        var_decl(p);
        if p.at(TokenKind::RightParen) || !p.expect(TokenKind::Comma) {
            break;
        }
    }
    p.expect(TokenKind::RightParen);

    m.complete(p, NodeKind::ParamList);
}

pub(crate) fn var_decl(p: &mut Parser) {
    let m = p.start();
    if !p.cur().is_type() {
        p.err_and_next("expected type");
    }
    p.next_any();
    name(p);
    if p.opt(TokenKind::Eq) {
        expr(p);
    }

    m.complete(p, NodeKind::VarDecl);
}

pub(crate) fn name(p: &mut Parser) {
    let m = p.start();
    if p.at(TokenKind::Identifier) {
        p.next(TokenKind::Identifier);
        m.complete(p, NodeKind::Name);
    } else {
        p.err_and_next("expected an identifier");
        m.complete(p, NodeKind::Error);
    }
}

pub(crate) fn name_ref(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    if p.at(TokenKind::Identifier) {
        p.next(TokenKind::Identifier);
        m.complete(p, NodeKind::NameRef)
    } else {
        p.err_and_next("expected an identifier");
        m.complete(p, NodeKind::Error)
    }
}
