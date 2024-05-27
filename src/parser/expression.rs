use super::*;

pub(crate) fn expr(p: &mut Parser) -> CompletedMarker {
    expr_assignment(p)
}

pub(crate) fn expr_assignment(p: &mut Parser) -> CompletedMarker {
    let lhs = expr_ternary(p);

    if p.at(TokenKind::Eq) {
        let m = lhs.precede(p);
        p.next_any();
        expr_assignment(p);
        return m.complete(p, NodeKind::AssignmentExpr);
    }

    lhs
}

pub(crate) fn expr_ternary(p: &mut Parser) -> CompletedMarker {
    let mut cond = expr_logical_or(p);

    while p.at(TokenKind::Ternary) {
        let m = cond.precede(p);
        p.next_any();

        if p.opt(TokenKind::Colon) {
            expr_logical_or(p);
            p.expect(TokenKind::Colon);
        }
        expr_logical_or(p);
        cond = m.complete(p, NodeKind::TernaryExpr);
    }

    cond
}

pub(crate) fn expr_logical_or(p: &mut Parser) -> CompletedMarker {
    let mut lhs = expr_logical_and(p);

    while p.at(TokenKind::LogicOr) {
        let m = lhs.precede(p);
        p.next_any();
        expr_logical_and(p);
        lhs = m.complete(p, NodeKind::BinayExpr);
    }

    lhs
}

pub(crate) fn expr_logical_and(p: &mut Parser) -> CompletedMarker {
    let mut lhs = expr_equality(p);

    while p.at(TokenKind::LogicAnd) {
        let m = lhs.precede(p);
        p.next_any();
        expr_equality(p);
        lhs = m.complete(p, NodeKind::BinayExpr);
    }

    lhs
}

pub(crate) fn expr_equality(p: &mut Parser) -> CompletedMarker {
    let mut lhs = expr_comparison(p);

    while p.at(TokenKind::EqEq) || p.at(TokenKind::BangEq) {
        let m = lhs.precede(p);
        p.next_any();
        expr_comparison(p);
        lhs = m.complete(p, NodeKind::BinayExpr);
    }

    lhs
}

pub(crate) fn expr_comparison(p: &mut Parser) -> CompletedMarker {
    let mut lhs = expr_term(p);

    while p.at(TokenKind::Less)
        || p.at(TokenKind::LessEq)
        || p.at(TokenKind::Greater)
        || p.at(TokenKind::GreaterEq)
    {
        let m = lhs.precede(p);
        p.next_any();
        expr_term(p);
        lhs = m.complete(p, NodeKind::BinayExpr);
    }

    lhs
}

pub(crate) fn expr_term(p: &mut Parser) -> CompletedMarker {
    let mut lhs = expr_factor(p);

    while p.at(TokenKind::Plus) || p.at(TokenKind::Minus) {
        let m = lhs.precede(p);
        p.next_any();
        expr_factor(p);
        lhs = m.complete(p, NodeKind::BinayExpr);
    }

    lhs
}

pub(crate) fn expr_factor(p: &mut Parser) -> CompletedMarker {
    let mut lhs = expr_unary(p);

    while p.at(TokenKind::Star) || p.at(TokenKind::Slash) {
        let m = lhs.precede(p);
        p.next_any();
        expr_unary(p);
        lhs = m.complete(p, NodeKind::BinayExpr);
    }

    lhs
}

pub(crate) fn expr_unary(p: &mut Parser) -> CompletedMarker {
    if p.at(TokenKind::Bang) || p.at(TokenKind::Minus) || p.at(TokenKind::Dollar) {
        let m = p.start();
        p.next_any();
        expr_unary(p);
        return m.complete(p, NodeKind::UnaryExpr);
    }

    expr_postfix(p)
}

pub(crate) fn expr_postfix(p: &mut Parser) -> CompletedMarker {
    let lhs = expr_call(p);

    if p.at(TokenKind::LeftBracket) {
        let m = lhs.precede(p);
        p.next_any();
        expr(p);
        p.expect(TokenKind::RightBracket);
        return m.complete(p, NodeKind::SubscriptExpr);
    }

    if p.at(TokenKind::PlusPlus) || p.at(TokenKind::MinusMinus) {
        let m = lhs.precede(p);
        p.next_any();
        return m.complete(p, NodeKind::UnaryExpr);
    }

    lhs
}

pub(crate) fn expr_call(p: &mut Parser) -> CompletedMarker {
    let mut lhs = expr_primary(p);

    while p.at(TokenKind::Dot) || p.at(TokenKind::LeftParen) {
        let m = lhs.precede(p);
        if p.at(TokenKind::Dot) {
            p.next_any();
            name_ref(p);
        } else {
            p.next_any();
            while p.more() && !p.at(TokenKind::RightParen) {
                expr(p);
                if p.at(TokenKind::RightParen) || !p.expect(TokenKind::Comma) {
                    break;
                }
            }
            p.expect(TokenKind::RightParen);
        }
        lhs = m.complete(p, NodeKind::CallExpr);
    }

    lhs
}

pub(crate) fn expr_primary(p: &mut Parser) -> CompletedMarker {
    match p.cur() {
        x if x.is_literal() => {
            let m = p.start();
            p.next_any();
            m.complete(p, NodeKind::Literal)
        }
        TokenKind::Identifier => name_ref(p),
        TokenKind::LeftParen => {
            p.next_any();
            let m = expr(p);
            p.expect(TokenKind::RightParen);
            m
        }
        TokenKind::Fn => expr_fn(p),
        _ => {
            let m = p.start();
            p.err_and_next("expected expression");
            m.complete(p, NodeKind::Error)
        }
    }
}

pub(crate) fn expr_fn(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.next(TokenKind::Fn);
    arg_list(p);
    stmt_block(p);
    m.complete(p, NodeKind::LambdaExpr)
}
