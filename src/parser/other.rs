use super::*;
use crate::syntax_node::{NodeKind, TokenKind};

pub(crate) fn script(p: &mut Parser) {
    let m = p.start();

    p.expect(TokenKind::NAME_KW);
    name(p);
    p.expect(TokenKind::SEMICOLON);

    while p.more() {
        match p.cur() {
            TokenKind::BLOCK_TYPE => begin(p),
            TokenKind::FN_KW => fn_decl(p),
            x if x.is_type() => stmt_var_decl(p),
            _ => p.err_and_next("expected block type (GameMode, MenuMode, ...) or 'fn'"),
        }
    }

    m.complete(p, NodeKind::SCRIPT);
}

pub(crate) fn begin(p: &mut Parser) {
    let m = p.start();
    p.next(TokenKind::BLOCK_TYPE);
    if p.opt(TokenKind::COLON) {
        expr_primary(p);
    }
    stmt_block(p);

    m.complete(p, NodeKind::BLOCK_TYPE_ITEM);
}

pub(crate) fn fn_decl(p: &mut Parser) {
    let m = p.start();

    p.next(TokenKind::FN_KW);
    if p.at(TokenKind::IDENT) {
        name(p);
    }
    param_list(p);
    stmt_block(p);

    m.complete(p, NodeKind::FN_DECL_ITEM);
}

pub(crate) fn arg_list(p: &mut Parser) {
    let m = p.start();

    p.next(TokenKind::LPAREN);
    while p.more() && !p.at(TokenKind::RPAREN) {
        expr(p);
        if p.at(TokenKind::RPAREN) || !p.expect(TokenKind::COMMA) {
            break;
        }
    }
    p.expect(TokenKind::RPAREN);

    m.complete(p, NodeKind::ARG_LIST);
}

pub(crate) fn param_list(p: &mut Parser) {
    let m = p.start();

    p.expect(TokenKind::LPAREN);
    while p.more() && !p.at(TokenKind::RPAREN) {
        var_decl(p);
        if p.at(TokenKind::RPAREN) || !p.expect(TokenKind::COMMA) {
            break;
        }
    }
    p.expect(TokenKind::RPAREN);

    m.complete(p, NodeKind::PARAM_LIST);
}

pub(crate) fn pat(p: &mut Parser) {
    match p.cur() {
        x if x.is_type() => var_decl(p),
        TokenKind::LSQ_BRACK => pat_arr(p),
        _ => p.err_and_next("expected a type or '['"),
    }
}

pub(crate) fn pat_arr(p: &mut Parser) {
    let m = p.start();

    p.next(TokenKind::LSQ_BRACK);
    while p.more() && !p.at(TokenKind::RSQ_BRACK) {
        pat(p);
        if p.at(TokenKind::RSQ_BRACK) || !p.expect(TokenKind::COMMA) {
            break;
        }
    }
    p.expect(TokenKind::RSQ_BRACK);

    m.complete(p, NodeKind::ARR_PAT);
}

pub(crate) fn var_decl(p: &mut Parser) {
    let m = p.start();
    if !p.cur().is_type() {
        p.err_and_next("expected type");
    }
    p.next_any();
    name(p);
    if p.opt(TokenKind::EQ) {
        expr(p);
    }

    m.complete(p, NodeKind::VAR_DECL);
}

pub(crate) fn name(p: &mut Parser) {
    let m = p.start();
    if p.at(TokenKind::IDENT) {
        p.next(TokenKind::IDENT);
        m.complete(p, NodeKind::NAME);
    } else {
        p.err_and_next("expected an identifier");
        m.complete(p, NodeKind::ERROR);
    }
}

pub(crate) fn name_ref(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    if p.at(TokenKind::IDENT) {
        p.next(TokenKind::IDENT);
        m.complete(p, NodeKind::NAME_REF)
    } else {
        p.err_and_next("expected an identifier");
        m.complete(p, NodeKind::ERROR)
    }
}
