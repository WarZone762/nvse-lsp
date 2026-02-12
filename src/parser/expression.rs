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

        if let TokenKind::NOT_KW = p.cur() {
            let next = p.nth(1);
            if next == TokenKind::IN_KW {
                p.next_any();
                p.next_any();
            } else {
                p.err_and_next(&format!("expected 'in', got {next}"));
            }
        } else {
            p.next_any();
        }
        expr_bp(p, bp + 1);

        lhs = m.complete(p, NodeKind::BIN_EXPR);
    }

    Some(lhs)
}

pub(crate) fn expr_lhs(p: &mut Parser) -> Option<CompletedMarker> {
    Some(if p.cur().is_unary_op() {
        let m = p.start();
        p.next_any();
        expr(p);
        m.complete(p, NodeKind::UNARY_EXPR)
    } else {
        let lhs = expr_primary(p)?;
        expr_postfix(p, lhs)
    })
}

pub(crate) fn expr_postfix(p: &mut Parser, mut lhs: CompletedMarker) -> CompletedMarker {
    loop {
        lhs = match p.cur() {
            TokenKind::LPAREN => expr_call(p, lhs),
            TokenKind::LSQ_BRACK => {
                let m = lhs.precede(p);
                p.next_any();
                expr(p);
                p.expect(TokenKind::RSQ_BRACK);
                m.complete(p, NodeKind::SUBSCRIPT_EXPR)
            }
            TokenKind::PLUS_2 | TokenKind::MINUS_2 => {
                let m = lhs.precede(p);
                p.next_any();
                m.complete(p, NodeKind::POSTFIX_EXPR)
            }
            TokenKind::QUESTION_MARK => {
                let m = lhs.precede(p);
                p.next_any();
                expr_bp(p, bin_op_bp(TokenKind::COLON) * 2 + 1);
                p.expect(TokenKind::COLON);
                expr(p);
                m.complete(p, NodeKind::TERNARY_EXPR)
            }
            TokenKind::DOT => {
                let m = lhs.precede(p);
                p.next_any();
                name_ref(p);
                m.complete(p, NodeKind::FIELD_EXPR)
            }
            _ => break,
        };
    }
    lhs
}

pub(crate) fn expr_call(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    let m = lhs.precede(p);
    arg_list(p);
    m.complete(p, NodeKind::CALL_EXPR)
}

pub(crate) fn expr_primary(p: &mut Parser) -> Option<CompletedMarker> {
    Some(match p.cur() {
        x if x.is_literal() => {
            let m = p.start();
            p.next_any();
            m.complete(p, NodeKind::LITERAL)
        }
        TokenKind::LSQ_BRACK => lit_arr(p),
        TokenKind::LBRACK => lit_map(p),
        TokenKind::QUOTE_DOUBLE => expr_str(p),
        TokenKind::IDENT => name_ref(p),
        TokenKind::LPAREN => {
            let m = p.start();
            p.next_any();
            expr(p);
            p.expect(TokenKind::RPAREN);
            m.complete(p, NodeKind::PAREN_EXPR)
        }
        TokenKind::FN_KW => expr_lambda(p),
        _ => {
            let m = p.start();
            p.err("expected expression");
            // if p.at(TokenKind::RBRACK) {
            //     m.abandon(p);
            //     return None;
            // }
            p.next_any();
            m.complete(p, NodeKind::ERROR)
        }
    })
}

pub(crate) fn expr_lambda(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.next(TokenKind::FN_KW);
    param_list(p);

    if p.at(TokenKind::LBRACK) {
        stmt_block(p);
    } else {
        p.expect(TokenKind::RARROW);
        expr(p);
    }
    m.complete(p, NodeKind::LAMBDA_EXPR)
}

pub(crate) fn expr_str(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.next(TokenKind::QUOTE_DOUBLE);
    while p.more() && !p.at(TokenKind::QUOTE_DOUBLE) {
        match p.cur() {
            TokenKind::DOLLAR_LBRACK => {
                let shard_m = p.start();
                p.next_any();
                expr(p);
                p.expect(TokenKind::RBRACK);
                shard_m.complete(p, NodeKind::STR_SHARD_EXPR);
            }
            TokenKind::STR_SHARD => {
                let shard_m = p.start();
                p.next_any();
                shard_m.complete(p, NodeKind::STR_SHARD_LITERAL);
            }
            _ => {
                let m = p.start();
                p.err_and_next("expected a string or '}'");
                return m.complete(p, NodeKind::ERROR);
            }
        }
    }
    p.expect(TokenKind::QUOTE_DOUBLE);
    m.complete(p, NodeKind::STR_EXPR)
}

pub(crate) fn lit_arr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.next(TokenKind::LSQ_BRACK);

    while p.more() && !p.at(TokenKind::RSQ_BRACK) {
        expr(p);
        p.opt(TokenKind::COMMA);
    }

    p.expect(TokenKind::RSQ_BRACK);

    m.complete(p, NodeKind::LIT_ARR)
}

pub(crate) fn lit_map(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.next(TokenKind::LBRACK);

    while p.more() && !p.at(TokenKind::RBRACK) {
        kv_pair(p);
        p.opt(TokenKind::COMMA);
    }

    p.expect(TokenKind::RBRACK);

    m.complete(p, NodeKind::LIT_MAP)
}

pub(crate) fn kv_pair(p: &mut Parser) {
    let m = p.start();

    expr_primary(p);
    p.expect(TokenKind::COLON_2);
    expr_primary(p);

    m.complete(p, NodeKind::KV_PAIR);
}

pub(crate) fn bin_op_bp(token: TokenKind) -> u8 {
    match token {
        TokenKind::ASTERISK | TokenKind::SLASH | TokenKind::PERCENT | TokenKind::CIRCUMFLEX => 13,
        TokenKind::PLUS | TokenKind::MINUS => 12,
        TokenKind::LT_2 | TokenKind::GT_2 => 11,
        TokenKind::AMPERSAND => 10,
        TokenKind::VBAR => 9,
        TokenKind::LT | TokenKind::LT_EQ | TokenKind::GT | TokenKind::GT_EQ => 8,
        TokenKind::EQ_2 | TokenKind::EXCLAMATION_EQ => 7,
        TokenKind::AMPERSAND_2 => 6,
        TokenKind::VBAR_2 => 5,
        TokenKind::COLON_2 => 4,
        TokenKind::IN_KW | TokenKind::NOT_KW => 3,
        TokenKind::COLON => 2,
        TokenKind::EQ
        | TokenKind::PLUS_EQ
        | TokenKind::MINUS_EQ
        | TokenKind::ASTERISK_EQ
        | TokenKind::SLASH_EQ
        | TokenKind::PERCENT_EQ
        | TokenKind::CIRCUMFLEX_EQ
        | TokenKind::VBAR_EQ
        | TokenKind::AMPERSAND_EQ => 1,
        x if x.is_bin_op() => {
            panic!("encountered binary operator with no binding power assigned: '{x}'")
        }
        _ => 0,
    }
}
