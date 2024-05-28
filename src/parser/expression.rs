use super::*;

pub(crate) fn expr(p: &mut Parser) -> CompletedMarker {
    expr_bp(p, 1)
}

pub(crate) fn expr_bp(p: &mut Parser, min_bp: u8) -> CompletedMarker {
    let mut lhs = expr_lhs(p);

    loop {
        let bp = bin_op_bp(p.cur()) * 2;
        if bp < min_bp {
            break;
        }
        let m = lhs.precede(p);

        p.next_any();
        expr_bp(p, bp + 1);

        lhs = m.complete(p, NodeKind::BinayExpr);
    }

    lhs
}

pub(crate) fn expr_lhs(p: &mut Parser) -> CompletedMarker {
    if p.at(TokenKind::Bang) | p.at(TokenKind::Minus) | p.at(TokenKind::Dollar) {
        let m = p.start();
        p.next_any();
        expr(p);
        m.complete(p, NodeKind::UnaryExpr)
    } else {
        let lhs = expr_primary(p);
        expr_postfix(p, lhs)
    }
}

pub(crate) fn expr_postfix(p: &mut Parser, mut lhs: CompletedMarker) -> CompletedMarker {
    loop {
        lhs = match p.cur() {
            TokenKind::LeftParen => expr_call(p, lhs),
            TokenKind::LeftBracket => {
                let m = lhs.precede(p);
                p.next_any();
                expr(p);
                p.expect(TokenKind::RightBracket);
                m.complete(p, NodeKind::SubscriptExpr)
            }
            TokenKind::PlusPlus | TokenKind::MinusMinus => {
                let m = lhs.precede(p);
                p.next_any();
                m.complete(p, NodeKind::UnaryExpr)
            }
            TokenKind::Ternary => {
                let m = lhs.precede(p);
                p.next_any();
                expr(p);
                p.expect(TokenKind::Colon);
                expr(p);
                m.complete(p, NodeKind::TernaryExpr)
            }
            _ => break,
        };
    }
    lhs
}

pub(crate) fn expr_call(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    let m = lhs.precede(p);
    arg_list(p);
    m.complete(p, NodeKind::CallExpr)
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
    param_list(p);
    stmt_block(p);
    m.complete(p, NodeKind::LambdaExpr)
}

pub(crate) fn bin_op_bp(token: TokenKind) -> u8 {
    match token {
        TokenKind::Dot => 7,
        TokenKind::Star | TokenKind::Slash | TokenKind::Mod => 6,
        TokenKind::Plus | TokenKind::Minus => 5,
        TokenKind::Less | TokenKind::LessEq | TokenKind::Greater | TokenKind::GreaterEq => 5,
        TokenKind::EqEq | TokenKind::BangEq => 4,
        TokenKind::LogicAnd => 3,
        TokenKind::LogicOr => 2,
        TokenKind::Eq
        | TokenKind::PlusEq
        | TokenKind::MinusEq
        | TokenKind::StarEq
        | TokenKind::SlashEq
        | TokenKind::ModEq
        | TokenKind::PowEq => 1,
        _ => 0,
    }
}
