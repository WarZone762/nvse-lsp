use super::*;
use crate::syntax_node::{NodeKind, TokenKind};

pub(crate) fn stmt(p: &mut Parser) {
    match p.cur() {
        TokenKind::FOR_KW => stmt_for(p),
        TokenKind::IF_KW => stmt_if(p),
        TokenKind::RETURN_KW => stmt_return(p),
        TokenKind::BREAK_KW => {
            let m = p.start();
            p.next_any();
            p.expect(TokenKind::SEMICOLON);
            m.complete(p, NodeKind::BREAK_STMT);
        }
        TokenKind::CONTINUE_KW => {
            let m = p.start();
            p.next_any();
            p.expect(TokenKind::SEMICOLON);
            m.complete(p, NodeKind::CONTINUE_STMT);
        }
        TokenKind::WHILE_KW => stmt_while(p),
        TokenKind::LBRACK => stmt_block(p),
        TokenKind::SEMICOLON => {
            let m = p.start();
            p.next_any();
            m.complete(p, NodeKind::EMPTY_EXPR);
        }
        x if x.is_type() || p.at(TokenKind::EXPORT_KW) => stmt_var_decl(p),
        _ => stmt_expr(p),
    }
}

pub(crate) fn stmt_for(p: &mut Parser) {
    let m = p.start();

    p.next(TokenKind::FOR_KW);
    p.expect(TokenKind::LPAREN);
    if !p.at(TokenKind::SEMICOLON) && !p.at(TokenKind::RPAREN) {
        // range-based for
        if (p.cur().is_type() && p.nth_at(2, TokenKind::IN_KW)) || p.at(TokenKind::LSQ_BRACK) {
            pat(p);
            p.expect(TokenKind::IN_KW);
            expr(p);
            p.expect(TokenKind::RPAREN);
            stmt_block(p);

            m.complete(p, NodeKind::FOR_RANGE_STMT);
            return;
        } else if p.cur().is_type() {
            var_decl(p);
        } else {
            expr(p);
        }
    }
    p.expect(TokenKind::SEMICOLON);

    if !p.at(TokenKind::SEMICOLON) && !p.at(TokenKind::RPAREN) {
        expr(p);
    }
    p.expect(TokenKind::SEMICOLON);

    if !p.at(TokenKind::RPAREN) {
        expr(p);
    }
    p.expect(TokenKind::RPAREN);
    stmt_block(p);

    m.complete(p, NodeKind::FOR_STMT);
}

pub(crate) fn stmt_if(p: &mut Parser) {
    let m = p.start();

    p.next(TokenKind::IF_KW);
    p.expect(TokenKind::LPAREN);
    expr(p);
    p.expect(TokenKind::RPAREN);
    stmt_block(p);
    if p.opt(TokenKind::ELSE_KW) {
        if p.at(TokenKind::IF_KW) {
            stmt_if(p);
        } else {
            stmt_block(p);
        }
    }

    m.complete(p, NodeKind::IF_STMT);
}
pub(crate) fn stmt_return(p: &mut Parser) {
    let m = p.start();

    p.next(TokenKind::RETURN_KW);
    if !p.at(TokenKind::SEMICOLON) {
        expr(p);
    }
    p.expect(TokenKind::SEMICOLON);

    m.complete(p, NodeKind::RETURN_STMT);
}

pub(crate) fn stmt_while(p: &mut Parser) {
    let m = p.start();

    p.next(TokenKind::WHILE_KW);
    p.expect(TokenKind::LPAREN);
    expr(p);
    p.expect(TokenKind::RPAREN);
    stmt_block(p);

    m.complete(p, NodeKind::WHILE_STMT);
}

pub(crate) fn stmt_expr(p: &mut Parser) {
    let m = p.start();

    if p.opt(TokenKind::SEMICOLON) {
        m.complete(p, NodeKind::EXPR_STMT);
        return;
    }
    expr(p);
    p.expect(TokenKind::SEMICOLON);

    m.complete(p, NodeKind::EXPR_STMT);
}

pub(crate) fn stmt_var_decl(p: &mut Parser) {
    let m = p.start();

    p.opt(TokenKind::EXPORT_KW);
    let var_decl_m = p.start();
    if !p.cur().is_type() {
        p.err_and_next("expected type");
    }
    p.next_any();
    name(p);
    while p.more() && p.opt(TokenKind::COMMA) {
        name(p);
    }
    if p.opt(TokenKind::EQ) {
        expr(p);
    }

    var_decl_m.complete(p, NodeKind::VAR_DECL);

    p.expect(TokenKind::SEMICOLON);
    m.complete(p, NodeKind::VAR_DECL_STMT);
}

pub(crate) fn stmt_block(p: &mut Parser) {
    let m = p.start();

    p.expect(TokenKind::LBRACK);
    while p.more() && !p.at(TokenKind::RBRACK) {
        stmt(p);
    }
    p.expect(TokenKind::RBRACK);

    m.complete(p, NodeKind::BLOCK_STMT);
}
