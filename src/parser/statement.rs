use super::*;
use crate::node::{NodeKind, TokenKind};

pub(crate) fn stmt(p: &mut Parser) {
    match p.cur() {
        TokenKind::For => stmt_for(p),
        TokenKind::If => stmt_if(p),
        TokenKind::Return => stmt_return(p),
        TokenKind::Break => {
            let m = p.start();
            p.next_any();
            p.expect(TokenKind::Semicolon);
            m.complete(p, NodeKind::BreakStmt);
        }
        TokenKind::Continue => {
            let m = p.start();
            p.next_any();
            p.expect(TokenKind::Semicolon);
            m.complete(p, NodeKind::ContinueStmt);
        }
        TokenKind::While => stmt_while(p),
        TokenKind::LeftBrace => stmt_block(p),
        x if x.is_type() => stmt_var_decl(p),
        _ => stmt_expr(p),
    }
}

pub(crate) fn stmt_for(p: &mut Parser) {
    let m = p.start();

    p.next(TokenKind::For);
    p.expect(TokenKind::LeftParen);
    if !p.at(TokenKind::Semicolon) && !p.at(TokenKind::RightParen) {
        if p.cur().is_type() {
            var_decl(p);
        } else {
            expr(p);
        }
        p.expect(TokenKind::Semicolon);
    }

    if !p.at(TokenKind::Semicolon) && !p.at(TokenKind::RightParen) {
        expr(p);
        p.expect(TokenKind::Semicolon);
    }

    if !p.at(TokenKind::RightParen) {
        expr(p);
    }
    p.expect(TokenKind::RightParen);
    stmt_block(p);

    m.complete(p, NodeKind::ForStmt);
}

pub(crate) fn stmt_if(p: &mut Parser) {
    let m = p.start();

    p.next(TokenKind::If);
    p.expect(TokenKind::LeftParen);
    expr(p);
    p.expect(TokenKind::RightParen);
    stmt_block(p);
    if p.opt(TokenKind::Else) {
        stmt_block(p);
    }

    m.complete(p, NodeKind::IfStmt);
}
pub(crate) fn stmt_return(p: &mut Parser) {
    let m = p.start();

    p.next(TokenKind::Return);
    if !p.at(TokenKind::Semicolon) {
        expr(p);
    }
    p.expect(TokenKind::Semicolon);

    m.complete(p, NodeKind::ReturnStmt);
}

pub(crate) fn stmt_while(p: &mut Parser) {
    let m = p.start();

    p.next(TokenKind::While);
    p.expect(TokenKind::LeftParen);
    expr(p);
    p.expect(TokenKind::RightParen);
    stmt_block(p);

    m.complete(p, NodeKind::WhileStmt);
}

pub(crate) fn stmt_expr(p: &mut Parser) {
    let m = p.start();

    if p.opt(TokenKind::Semicolon) {
        m.complete(p, NodeKind::ExprStmt);
        return;
    }
    expr(p);
    p.expect(TokenKind::Semicolon);

    m.complete(p, NodeKind::ExprStmt);
}

pub(crate) fn stmt_var_decl(p: &mut Parser) {
    let m = p.start();
    var_decl(p);
    p.expect(TokenKind::Semicolon);
    m.complete(p, NodeKind::VarDeclStmt);
}

pub(crate) fn stmt_block(p: &mut Parser) {
    let m = p.start();

    p.expect(TokenKind::LeftBrace);
    while p.more() && !p.at(TokenKind::RightBrace) {
        stmt(p);
    }
    p.expect(TokenKind::RightBrace);

    m.complete(p, NodeKind::BlockStmt);
}
