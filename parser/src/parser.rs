use common::span::{Span, Spanned};

use crate::{
    literal::Literal,
    node::{Node, Op},
};

peg::parser!(
    pub grammar parser(src_id: usize) for str {
        use peg::ParseLiteral;

        rule spanned<T>(inner: rule<T>) -> Spanned<T>
        = start:position!() n:inner() end:position!() { (n, Span(start, end, src_id)) }

        rule TERM()       = ("\r"? "\n") / (";")
        rule COMMENT()    = "//" (!TERM() [_])*
        rule TERMINATOR() = COMMENT()? TERM()
        rule SIMPLEWHITESPACE() = [' ' | '\t' | '\u{09}' | '\u{0B}' | '\u{0C}' | '\u{20}' | '\u{A0}' ]
        rule WHITESPACE() = SIMPLEWHITESPACE() / "\\" TERM()

        rule _  = quiet!{ (WHITESPACE() / TERMINATOR() / COMMENT())* }
        rule __ = quiet!{ (WHITESPACE() /  COMMENT())+ }
        rule ws() = quiet!{ SIMPLEWHITESPACE()* }

        rule COMMASEP<T> (x: rule<T>) -> Vec<T> = _ v:(( _ y:x() _ {y}) ** ",") ","? _ {v}
        rule COMMASEPP<T>(x: rule<T>) -> Vec<T> = _ v:(( _ y:x() _ {y}) ++ ",") ","? _ {v}

        rule KW(id: &str) = ##parse_string_literal(id) !['a'..='z' | 'A'..='Z' | '0'..='9' | '_'] { () }

        rule IDENT() -> String
        = quiet!{ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) {?
            if matches!(n, "true" | "false" | "none" | "let" | "fn" | "include" | "with" | "as" | "while" | "for" | "in" | "return" | "switch" | "default" | "case") {
                return Err("Invalid identifier");
            }

            Ok(n.to_owned())
        }}
        / expected!("identifier")

        rule INT() -> i64
        = quiet!{ "0b" i:$(['0'..='1']+) { i64::from_str_radix(i, 2).unwrap_or(0) } }
        / quiet!{ "0x" i:$(['0'..='9' | 'a'..='f' | 'A'..='F']+) { i64::from_str_radix(i, 16).unwrap_or(0) } }
        / quiet!{ "0o" i:$(['0'..='7']+) { i64::from_str_radix(i, 8).unwrap_or(0) } }
        / quiet!{ i:$("-"?['0'..='9']+) { i.parse().unwrap_or(0) } }
        / expected!("integer")

        rule INT32() -> i32
        = quiet!{ "0b" i:$(['0'..='1']+) { i32::from_str_radix(i, 2).unwrap_or(0) } }
        / quiet!{ "0x" i:$(['0'..='9' | 'a'..='f' | 'A'..='F']+) { i32::from_str_radix(i, 16).unwrap_or(0) } }
        / quiet!{ "0o" i:$(['0'..='7']+) { i32::from_str_radix(i, 8).unwrap_or(0) } }
        / quiet!{ i:$("-"?['0'..='9']+) { i.parse().unwrap_or(0) } }
        / expected!("integer")

        rule FLOAT() -> f64
        = quiet!{  i:$("-"?['0'..='9']+ "." !"." ['0'..='9']*)  { i.parse().unwrap_or(0.0) } }
        / expected!("float")

        rule HEX() -> [u8; 4]
        = "#" s:$(['0'..='9' | 'a'..='f' | 'A'..='F']+) {?
            if let Some(c) = hex_to_rgba(s) {
                Ok(c)
            } else {
                Err("Invalid hex color")
            }
        }

        rule STRING() -> String
        = quiet!{ "\"" s:(doubleQuotedCharacter()*) "\"" { s.into_iter().collect() }}
        / expected!("string")

        rule doubleQuotedCharacter() -> char
        = !("\"") c:([_]) { c }
        / "\\u{" value:$(['0'..='9' | 'a'..='f' | 'A'..='F']+) "}" { char::from_u32(u32::from_str_radix(value, 16).unwrap_or(0)).unwrap_or(' ') }
        / expected!("valid escape sequence")

        pub rule literal() -> Literal
        = KW("true") { Literal::Bool(true) }
        / KW("false") { Literal::Bool(false) }
        / KW("none") { Literal::None }
        / h:HEX() { Literal::Color(h) }
        / f:FLOAT() { Literal::Float(f) }
        / s:INT32() _ ":" _ e:INT32() { Literal::Range(s, e) }
        / i:INT() { Literal::Int(i) }
        / s:STRING() { Literal::String(s) }
        / "(" _ h:literal() _ "," _ t:literal() _ ")" { Literal::Pair(Box::new(h), Box::new(t)) }
        / "[" _ e:COMMASEP(<literal()>) _ "]" { Literal::Array(e) }

        rule _elif() -> Vec<Spanned<Node>>
        = s:position!() _ code:if_condition()  _ e:position!() { vec![(code, Span(s, e, src_id) )] }

        rule else_elif() -> Vec<Spanned<Node>>
        = KW("else") _ res:(block(false) / _elif()) { res }

        rule if_condition() -> Node
        = _ KW("if") _ condition:node() _  then:block(false) _ otherwise:(else_elif())? _ {
            Node::Conditional {
                condition: Box::new(condition),
                then,
                otherwise,
            }
        }

        rule case() -> (Vec<Spanned<Node>>, Vec<Spanned<Node>>)
        = _ KW("case") _ value:(node() ++ (_ "," _))  _ code:block(false) _ { (value, code) }

        rule default_case() -> Vec<Spanned<Node>>
        = _ KW("default") _ code:block(false) _ { code }

        rule switch_case() -> Node
        = _ KW("switch") _ value:node() _ "{" _ cases:(case() ** _) _ default:default_case()? _ "}" _ {
            let mut fcases = Vec::with_capacity(cases.len());

            for (values, code) in cases {
                for value in values {
                    fcases.push((value, code.clone()));
                }
            }

            Node::SwitchCase {
                value: Box::new(value),
                cases: fcases,
                default
            }
        }

        rule op() -> Op
        = "+" { Op::Add }
        / "-" { Op::Sub }
        / "*" { Op::Mul }
        / "/" { Op::Div }
        / "%" { Op::Mod }
        / "**" { Op::Pow }
        / "&&" { Op::And }
        / "||" { Op::Or }
        / "&" { Op::BitwiseAnd }
        / "|" { Op::BitwiseOr }
        / "^" { Op::BitwiseXor }
        / "<<" { Op::BitwiseLeftShift }
        / ">>" { Op::BitwiseRightShift }
        / ".." { Op::Join }

        rule node() -> Spanned<Node>
        = precedence! {
            start:position!() e:(@) end:position!() { (e, Span(start, end, src_id)) }
            --
            KW("return") __? e:node()? { Node::Return(e.map(Box::new)) }
            --
            KW("let") _ i:spanned(<IDENT()>) _ op:op()? "=" _ n:@ {
                if let Some(op) = op {
                    let symbol = (Node::Identifier(i.0.clone()), i.1);
                    let span = n.1;
                    let op = (Node::Binary(op, Box::new(symbol), Box::new(n)), span);
                    Node::Declaration(i, Box::new(op))
                } else {
                    Node::Declaration(i, Box::new(n))
                }
            }
            KW("let") _  i:COMMASEPP(<spanned(<IDENT()>)>)  _ "=" _ e:@  { Node::MultiDeclaration(i, Box::new(e)) }
            i:spanned(<IDENT()>) _  op:op()? "=" _ e:@  {
                if let Some(op) = op {
                    let symbol = (Node::Identifier(i.0.clone()), i.1);
                    let span = e.1;
                    let op = (Node::Binary(op, Box::new(symbol), Box::new(e)), span);
                    Node::Assignment(i, Box::new(op))
                } else {
                    Node::Assignment(i, Box::new(e))
                }
            }
            // i:spanned(<IDENT()>) _ "+=" _ e:@  {
            //     let symbol = (Node::Identifier(i.0.clone()), i.1);
            //     let span = e.1;
            //     let op = (Node::Binary(Op::Add, Box::new(symbol), Box::new(e)), span);
            //     Node::Assignment(i,  Box::new(op))
            // }



            // e:@ _ "[" _ i:node() _ "]" _ "=" _ v:node() { Node::IndexSet(Box::new(e), Box::new(i), Box::new(v)) }
            --
            KW("fn") _ name:spanned(<IDENT()>)? _ "(" _ args:COMMASEP(<spanned(<IDENT()>)>) _ ")" _ body:block(true) {
                if let Some(name) = name {
                    Node::Function {
                        name,
                        args,
                        body,
                    }
                } else {
                    Node::Lambda(args, body)
                }
            }
            --
            conditional:if_condition() { conditional }
            switch:switch_case() { switch }
            --
            KW("while") _ condition:node() _ body:block(false) {
                Node::WhileLoop(
                    Box::new(condition),
                    body,
                )
            }
            KW("for") _ variable:spanned(<IDENT()>) _ KW("in") _ iterable:node() _ body:block(false) {
                Node::ForLoop{
                    variable,
                    iterable: Box::new(iterable),
                    body,
                }
            }
            KW("for") _ variables:COMMASEPP(<spanned(<IDENT()>)>) _ KW("in") _ iterable:node() _ body:block(false) {
                Node::MultiForLoop{
                    variables,
                    iterable: Box::new(iterable),
                    body,
                }
            }
            --
            KW("with") _ name:("@" _ name:spanned(<IDENT()>) {name} / expected!("builtin") ) _ "(" _ args:COMMASEP(<node()>) _ ")" variable:(_ KW("as") _ n:spanned(<IDENT()>) {n})? _ body:block(false) {
                Node::ContextWrapped { name, args, variable, body }
            }
            --
            KW("include") _ s:spanned(<STRING()>) { Node::Include(s, None) }
            --
            s:INT32() _ ":" _ e:INT32() { Node::Literal(Literal::Range(s, e)) }
            x:(@) _ ":" _ y:@ { Node::Range(Box::new(x), Box::new(y)) }
            --
            x:(@) _ "&&" _ y:@ { Node::Binary(Op::And, Box::new(x), Box::new(y)) }
            x:(@) _ "||" _ y:@ { Node::Binary(Op::Or, Box::new(x), Box::new(y)) }
            --
            x:(@) _ "==" _ y:@ { Node::Binary(Op::Eq, Box::new(x), Box::new(y)) }
            x:(@) _ "!=" _ y:@ { Node::Binary(Op::Neq, Box::new(x), Box::new(y)) }
            x:(@) _ ">=" _ y:@ { Node::Binary(Op::Gte, Box::new(x), Box::new(y)) }
            x:(@) _ "<=" _ y:@ { Node::Binary(Op::Lte, Box::new(x), Box::new(y)) }
            x:(@) _ ">" _ y:@ { Node::Binary(Op::Gt, Box::new(x), Box::new(y)) }
            x:(@) _ "<" _ y:@ { Node::Binary(Op::Lt, Box::new(x), Box::new(y)) }
            --
            x:(@) _ "&" _ y:@ { Node::Binary(Op::BitwiseAnd, Box::new(x), Box::new(y)) }
            x:(@) _ "|" _ y:@ { Node::Binary(Op::BitwiseOr, Box::new(x), Box::new(y)) }
            x:(@) _ "^" _ y:@ { Node::Binary(Op::BitwiseXor, Box::new(x), Box::new(y)) }
            x:(@) _ "<<" _ y:@ { Node::Binary(Op::BitwiseLeftShift, Box::new(x), Box::new(y)) }
            x:(@) _ ">>" _ y:@ { Node::Binary(Op::BitwiseRightShift, Box::new(x), Box::new(y)) }
            x:(@) _ "~" _ y:@ { Node::Binary(Op::BitwiseNot, Box::new(x), Box::new(y)) }
            --
            x:(@) _ ".." _ y:@ { Node::Binary(Op::Join, Box::new(x), Box::new(y)) }
            --
            x:(@) _ "+" _ y:@ { Node::Binary(Op::Add, Box::new(x), Box::new(y)) }
            x:(@) _ "-" _ y:@ { Node::Binary(Op::Sub, Box::new(x), Box::new(y)) }
            --
            x:(@) _ "*" _ y:@ { Node::Binary(Op::Mul, Box::new(x), Box::new(y)) }
            x:(@) _ "/" _ y:@ { Node::Binary(Op::Div, Box::new(x), Box::new(y)) }
            x:(@) _ "%" _ y:@ { Node::Binary(Op::Mod, Box::new(x), Box::new(y)) }
            --
            x:(@) _ "**" _ y:@ { Node::Binary(Op::Pow, Box::new(x), Box::new(y)) }
            --
            "!" _ e:@ { Node::Unary(Op::Not, Box::new(e)) }
            "-" _ e:@ { Node::Unary(Op::Neg, Box::new(e)) }
            "~" _ e:@ { Node::Unary(Op::BitwiseNot, Box::new(e)) }
            --
            l:literal() { Node::Literal(l) }
            // "..." _ e:@ { Node::Unary(Op::Spread, Box::new(e)) }
            "[" _ e:COMMASEP(<node()>) _ "]" { Node::Array(e) }
            "(" _ l:node() _ "," _ r:node() _ ")" { Node::Pair(Box::new(l), Box::new(r)) }
            "(" _ l:literal() _ "," _ r:literal() _ ")" { Node::Literal(Literal::Pair(Box::new(l), Box::new(r))) }
            "@" i:spanned(<IDENT()>) ws() "(" _ args:COMMASEP(<node()>) _ ")" { Node::BuiltinCall(i, args) }
            // i:spanned(<IDENT()>) _ "(" _ args:COMMASEP(<node()>) _ ")" { Node::Call(i, args) }
            i:@ ws() "(" _ args:COMMASEP(<node()>) _ ")" { Node::Call(Box::new(i), args) }
            e:@  _ "[" _ i:node() _ "]" { Node::IndexGet(Box::new(e), Box::new(i)) }
            i:IDENT() { Node::Identifier(i) }
            "(" _ e:node() _ ")" { e.0 }
            b:block(false) { Node::Block(b) }
        }

        rule block(is_function: bool) -> Vec<Spanned<Node>>
        = s:position!() "{" _ code:(line:node() ** (";"/_)) _ "}" e:position!() {
            let mut code = code;
            add_implicit_none((&mut code, Span(s, e, src_id)), is_function);
            code
        }

        pub rule code() -> Vec<Spanned<Node>>
        = _ start:position!()  code:(line:node() ** (";"/_))?  end:position!() _ {
            let mut code = code.unwrap_or_default();
            add_implicit_none((&mut code, Span(start, end, src_id)), false);
            code
        }
    }
);

/// Helper function which converts a hex string to an RGBA color.
fn hex_to_rgba(hex: &str) -> Option<[u8; 4]> {
    let hex = hex.trim_start_matches('#');

    if hex.len() == 3 {
        let r = u8::from_str_radix(&hex[0..1], 16).unwrap_or(0);
        let g = u8::from_str_radix(&hex[1..2], 16).unwrap_or(0);
        let b = u8::from_str_radix(&hex[2..3], 16).unwrap_or(0);
        return Some([r * 17, g * 17, b * 17, 255]);
    } else if hex.len() < 6 || hex.len() > 8 {
        return None;
    }

    let r = u8::from_str_radix(&hex[0..2], 16).unwrap_or(0);
    let g = u8::from_str_radix(&hex[2..4], 16).unwrap_or(0);
    let b = u8::from_str_radix(&hex[4..6], 16).unwrap_or(0);
    let a = if hex.len() == 8 {
        u8::from_str_radix(&hex[6..8], 16).unwrap_or(255)
    } else {
        255
    };

    Some([r, g, b, a])
}

pub fn add_implicit_none((body, span): Spanned<&mut Vec<Spanned<Node>>>, is_function: bool) {
    if body.is_empty() {
        if is_function {
            body.push((Node::Return(None), span));
        } else {
            body.push((Node::Literal(Literal::None), span));
        }
    } else {
        let last = body.last_mut().expect("body is not empty");
        let span = last.1;
        let last = &mut last.0;

        if let Node::Return(_) = last {
            return;
        }

        if last.is_expression() {
            match last {
                Node::Block(b) => add_implicit_none((b, span), false),
                Node::Conditional {
                    then, otherwise, ..
                } => {
                    add_implicit_none((then, span), false);
                    if let Some(otherwise) = otherwise {
                        add_implicit_none((otherwise, span), false);
                    }
                }
                Node::ForLoop { body, .. } => {
                    add_implicit_none((body, span), false);
                }
                Node::MultiForLoop { body, .. } => {
                    add_implicit_none((body, span), false);
                }
                Node::WhileLoop(_, body) => {
                    add_implicit_none((body, span), false);
                }
                Node::ContextWrapped { body, .. } => {
                    add_implicit_none((body, span), false);
                }
                _ => {}
            }

            if is_function {
                let rep = std::mem::replace(last, Node::Literal(Literal::None));
                *last = Node::Return(Some(Box::new((rep, span))));
            }
        } else {
            if is_function {
                body.push((Node::Return(None), span));
            } else {
                body.push((Node::Literal(Literal::None), span));
            }
        }
    }
}

#[test]
fn test_hex_to_rgba() {
    assert_eq!(hex_to_rgba("#fff"), Some([255, 255, 255, 255]));
    assert_eq!(hex_to_rgba("#ffffff"), Some([255, 255, 255, 255]));
    assert_eq!(hex_to_rgba("#000"), Some([0, 0, 0, 255]));
    assert_eq!(hex_to_rgba("#000000"), Some([0, 0, 0, 255]));
    assert_eq!(hex_to_rgba("#f00"), Some([255, 0, 0, 255]));
    assert_eq!(hex_to_rgba("#ff0000"), Some([255, 0, 0, 255]));
    assert_eq!(hex_to_rgba("#0f0"), Some([0, 255, 0, 255]));
    assert_eq!(hex_to_rgba("#00ff00"), Some([0, 255, 0, 255]));
    assert_eq!(hex_to_rgba("#00f"), Some([0, 0, 255, 255]));
    assert_eq!(hex_to_rgba("#0000ff"), Some([0, 0, 255, 255]));
    assert_eq!(hex_to_rgba("#f0f"), Some([255, 0, 255, 255]));
    assert_eq!(hex_to_rgba("#ff00ff"), Some([255, 0, 255, 255]));
    assert_eq!(hex_to_rgba("#ffffff00"), Some([255, 255, 255, 0]));
    assert_eq!(hex_to_rgba("#fff0"), None);
    assert_eq!(hex_to_rgba("#fffff"), None);
    assert_eq!(hex_to_rgba("fff"), Some([255, 255, 255, 255]));
}
