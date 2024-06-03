use super::*;

pub(crate) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_bp(p, 1)
}

pub(crate) fn expr_bp(p: &mut Parser, min_bp: u8) -> Option<CompletedMarker> {
    let mut lhs = expr_lhs(p)?;

    loop {
        let bp = bin_op_bp(p.cur()) * 2;
        if bp < min_bp {
            break;
        }
        let m = lhs.precede(p);

        p.next_any();
        expr_bp(p, bp + 1);

        lhs = m.complete(p, NodeKind::BinaryExpr);
    }

    Some(lhs)
}

pub(crate) fn expr_lhs(p: &mut Parser) -> Option<CompletedMarker> {
    Some(if p.cur().is_unary_op() {
        let m = p.start();
        p.next_any();
        expr(p);
        m.complete(p, NodeKind::UnaryExpr)
    } else {
        let lhs = expr_primary(p)?;
        expr_postfix(p, lhs)
    })
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
                expr_bp(p, bin_op_bp(TokenKind::Colon) * 2 + 1);
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

pub(crate) fn expr_primary(p: &mut Parser) -> Option<CompletedMarker> {
    Some(match p.cur() {
        x if x.is_literal() => {
            let m = p.start();
            p.next_any();
            m.complete(p, NodeKind::Literal)
        }
        TokenKind::QuoteDouble => expr_str(p),
        TokenKind::Identifier => name_ref(p),
        TokenKind::LeftParen => {
            p.next_any();
            let m = expr(p)?;
            p.expect(TokenKind::RightParen);
            m
        }
        TokenKind::Fn => expr_fn(p),
        _ => {
            let m = p.start();
            p.err("expected expression");
            if p.at(TokenKind::RightBrace) {
                m.abandon(p);
                return None;
            }
            p.next_any();
            m.complete(p, NodeKind::Error)
        }
    })
}

pub(crate) fn expr_fn(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.next(TokenKind::Fn);
    param_list(p);
    stmt_block(p);
    m.complete(p, NodeKind::LambdaExpr)
}

pub(crate) fn expr_str(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.next(TokenKind::QuoteDouble);
    while p.more() && !p.at(TokenKind::QuoteDouble) {
        match p.cur() {
            TokenKind::DollarLeftBrace => {
                let shard_m = p.start();
                p.next_any();
                expr(p);
                p.expect(TokenKind::RightBrace);
                shard_m.complete(p, NodeKind::StringShardExpr);
            }
            TokenKind::StringShard => {
                let shard_m = p.start();
                p.next_any();
                shard_m.complete(p, NodeKind::StringShardLiteral);
            }
            _ => {
                let m = p.start();
                p.err_and_next("expected a string or '}'");
                return m.complete(p, NodeKind::Error);
            }
        }
    }
    p.expect(TokenKind::QuoteDouble);
    m.complete(p, NodeKind::StrExpr)
}

pub(crate) fn bin_op_bp(token: TokenKind) -> u8 {
    match token {
        TokenKind::Dot => 13,
        TokenKind::Star | TokenKind::Slash | TokenKind::Mod | TokenKind::Pow => 12,
        TokenKind::Plus | TokenKind::Minus => 11,
        TokenKind::Lt2 | TokenKind::Gt2 => 10,
        TokenKind::BitwiseAnd => 9,
        TokenKind::BitwiseOr => 8,
        TokenKind::Less | TokenKind::LessEq | TokenKind::Greater | TokenKind::GreaterEq => 7,
        TokenKind::EqEq | TokenKind::BangEq => 6,
        TokenKind::LogicAnd => 5,
        TokenKind::LogicOr => 4,
        TokenKind::Colon2 => 3,
        TokenKind::Colon => 2,
        TokenKind::Eq
        | TokenKind::PlusEq
        | TokenKind::MinusEq
        | TokenKind::StarEq
        | TokenKind::SlashEq
        | TokenKind::ModEq
        | TokenKind::PowEq
        | TokenKind::BitwiseOrEq
        | TokenKind::BitwiseAndEq => 1,
        x if x.is_bin_op() => {
            panic!("encountered binary operator with no binding power assigned: '{x}'")
        }
        _ => 0,
    }
}
