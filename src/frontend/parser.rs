use crate::shared::{Spanned, Value};

/// A type that represents an expression in Quilt
/// ### Variants
/// * `Literal`: A literal value, holds a [`Value`]
/// * `List`: A list of expressions, holds a [`Vec`] of [`Spanned`] expressions
/// * `Ident`: An identifier, holds the name of the identifier as a [`String`]
/// * `Yoink`: Similar to an Ident except it removes the variable from the frame.
/// * `Declaration`: A declaration, holds the name of the variable as a [`String`] and the value as a [`Spanned`] expression
/// * `MultiDeclaration`: A declaration of multiple variables, holds a [`Vec`] of variable names as [`String`]s and the value as a [`Spanned`] expression
/// * `Assignment`: An assignment, holds the name of the variable as a [`String`] and the value as a [`Spanned`] expression
/// * `Binary`: A binary operation, holds the operator as an [`Op`], the left hand side as a [`Spanned`] expression, and the right hand side as a [`Spanned`] expression
/// * `Unary`: A unary operation, holds the operator as an [`Op`] and the expression as a [`Spanned`] expression
/// * `Conditional`: A conditional, holds the condition as a [`Spanned`] expression, the body as a [`Vec`] of [`Spanned`] expressions, and the else body as an [`Option`] of a [`Vec`] of [`Spanned`] expressions
/// * `Function`: A function, holds the name of the function as a [`String`], the arguments as a [`Vec`] of [`String`]s, and the body as a [`Vec`] of [`Spanned`] expressions
/// * `Call`: A function call, holds the name of the function as a [`Spanned`] [`String`] and the arguments as a [`Vec`] of [`Spanned`] expressions
/// * `BuiltinCall`: A builtin function call, holds the name of the builtin function as a [`Spanned`] [`String`] and the arguments as a [`Vec`] of [`Spanned`] expressions
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Value),
    List(Vec<Spanned<Expr>>),
    Pair(Box<Spanned<Expr>>, Box<Spanned<Expr>>),

    Ident(String),
    Yoink(String),
    Declaration(String, Box<Spanned<Expr>>),
    MultiDeclaration(Vec<String>, Box<Spanned<Expr>>),
    Assignment(Spanned<String>, Box<Spanned<Expr>>),

    Binary(Op, Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Unary(Op, Box<Spanned<Expr>>),

    Conditional(
        Box<Spanned<Expr>>,
        Vec<Spanned<Expr>>,
        Option<Vec<Spanned<Expr>>>,
    ),

    Function(String, Vec<String>, Vec<Spanned<Expr>>),
    Call(Spanned<String>, Vec<Spanned<Expr>>),
    BuiltinCall(Spanned<String>, Vec<Spanned<Expr>>),

    ForLoop(
        Spanned<String>,
        Box<Spanned<Expr>>,
        Box<Spanned<Expr>>,
        Vec<Spanned<Expr>>,
    ),
}

/// A type that represents an operator in Quilt
/// 
/// ### Variants
/// * `Add`: Addition
/// * `Sub`: Subtraction
/// * `Mul`: Multiplication
/// * `Div`: Division
/// * `Mod`: Modulo
/// * `Pow`: Power
/// * `Eq`: Equality
/// * `Neq`: Inequality
/// * `Lt`: Less than
/// * `Gt`: Greater than
/// * `Lte`: Less than or equal to
/// * `Gte`: Greater than or equal to
/// * `And`: Logical and
/// * `Or`: Logical or
/// * `Not`: Logical not
/// * `Neg`: Negation
/// * `Join`: Join
#[rustfmt::skip]
#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add, Sub, Mul, Div, Mod, 
    Pow, Eq,  Neq, Lt,  Gt,  
    Lte, Gte, And, Or, Not, 
    Neg, Join,
}

peg::parser!(
    pub grammar parser() for str {
        use peg::ParseLiteral;

        rule spanned<T>(innter: rule<T>) -> Spanned<T>
        = start:position!() v:innter() end:position!() { (v, start..end) }

        rule TERM() = "\r"? "\n"
        rule COMMENT() = "//" (!TERM() [_])*
        rule TERMINATOR() = COMMENT()? TERM()
        rule WHITESPACE()
            = [' ' | '\t' | '\u{09}' | '\u{0B}' | '\u{0C}' | '\u{20}' | '\u{A0}' ]
                / "\\" TERM()
        rule _  = quiet!{ (WHITESPACE() / TERMINATOR() / COMMENT() )* }
        rule __ = quiet!{ (WHITESPACE() / TERMINATOR() / COMMENT() )+ }

        rule COMMASEP<T>(x: rule<T>) -> Vec<T> = _ v:(( _ y:x() _ {y}) ** ",") ","? _ {v}

        rule KW(id: &str) = ##parse_string_literal(id) !['0'..='9' | 'a'..='z' | 'A'..='Z' | '_']

        rule IDENT() -> String
        = quiet!{ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) {
            n.to_owned()
        } }
        / expected!("identifier")

        rule INT() -> i32
        = quiet!{ _ i:$("-"?['0'..='9']+) _ { i.parse().unwrap() } }
        / expected!("integer")

        rule FLOAT() -> f32
            = quiet!{ _ i:$("-"?['0'..='9']+ "." !"." ['0'..='9']*) _ { i.parse().unwrap() } }
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
        / "\\u{" value:$(['0'..='9' | 'a'..='f' | 'A'..='F']+) "}" { char::from_u32(u32::from_str_radix(value, 16).unwrap()).unwrap() }
        / expected!("valid escape sequence")

        rule literal() -> Value
        = KW("true") { Value::Bool(true) }
        / KW("false") { Value::Bool(false) }
        / KW("none") { Value::None }
        / h:HEX() { Value::Color(h) }
        / f:FLOAT() { Value::Float(f) }
        / i:INT() { Value::Int(i) }
        / s:STRING() { Value::Str(s) }


        rule expr() -> Spanned<Expr>
        = precedence! {
            start:position!() e:(@) end:position!() { (e, start..end) }
            --
            KW("let") _ i:IDENT() _ "=" _ e:@  { Expr::Declaration(i, Box::new(e)) }
            KW("let") _ "[" _ i:COMMASEP(<IDENT()>) _ "]" _ "=" _ e:@  { Expr::MultiDeclaration(i, Box::new(e)) }
            KW("let") _ "(" _ left:IDENT() _ "," _ right:IDENT() _ ")" _ "=" _ e:@  { Expr::MultiDeclaration(vec![left, right], Box::new(e)) }
            i:spanned(<IDENT()>) _ "=" _ e:@  { Expr::Assignment(i, Box::new(e)) }
            --
            KW("fn") _ i:IDENT() _ "(" _ args:COMMASEP(<IDENT()>) _ ")" _ body:block() { Expr::Function(i, args, body) }
            --
            KW("if")  _ cond:expr() _ body:block() _ then:(KW("else") _ then:block() {then})? { Expr::Conditional(Box::new(cond), body, then) }
            --
            KW("for") _ i:spanned(<IDENT()>) _ "in" _ start:expr() _ ":" _ end:expr() _ body:block() { Expr::ForLoop(i, Box::new(start), Box::new(end), body) }
            --
            x:(@) _ "&&" _ y:@ { Expr::Binary(Op::And, Box::new(x), Box::new(y)) }
            x:(@) _ "||" _ y:@ { Expr::Binary(Op::Or, Box::new(x), Box::new(y)) }
            --
            x:(@) _ "==" _ y:@ { Expr::Binary(Op::Eq, Box::new(x), Box::new(y)) }
            x:(@) _ "!=" _ y:@ { Expr::Binary(Op::Neq, Box::new(x), Box::new(y)) }
            x:(@) _ ">=" _ y:@ { Expr::Binary(Op::Gte, Box::new(x), Box::new(y)) }
            x:(@) _ "<=" _ y:@ { Expr::Binary(Op::Lte, Box::new(x), Box::new(y)) }
            x:(@) _ ">" _ y:@ { Expr::Binary(Op::Gt, Box::new(x), Box::new(y)) }
            x:(@) _ "<" _ y:@ { Expr::Binary(Op::Lt, Box::new(x), Box::new(y)) }
            --
            x:(@) _ ".." _ y:@ { Expr::Binary(Op::Join, Box::new(x), Box::new(y)) }
            --
            x:(@) _ "+" _ y:@ { Expr::Binary(Op::Add, Box::new(x), Box::new(y)) }
            x:(@) _ "-" _ y:@ { Expr::Binary(Op::Sub, Box::new(x), Box::new(y)) }
            --
            x:(@) _ "*" _ y:@ { Expr::Binary(Op::Mul, Box::new(x), Box::new(y)) }
            x:(@) _ "/" _ y:@ { Expr::Binary(Op::Div, Box::new(x), Box::new(y)) }
            x:(@) _ "%" _ y:@ { Expr::Binary(Op::Mod, Box::new(x), Box::new(y)) }
            --
            x:(@) _ "**" _ y:@ { Expr::Binary(Op::Pow, Box::new(x), Box::new(y)) }
            --
            "!" _ e:@ { Expr::Unary(Op::Not, Box::new(e)) }
            "-" _ e:@ { Expr::Unary(Op::Neg, Box::new(e)) }
            --
            "[" _ e:COMMASEP(<expr()>) _ "]" { Expr::List(e) }
            "[" _ e:COMMASEP(<literal()>) _ "]" { Expr::Literal(Value::List(e)) }
            "(" _ l:expr() _ "," _ r:expr() _ ")" { Expr::Pair(Box::new(l), Box::new(r)) }
            "(" _ l:literal() _ "," _ r:literal() _ ")" { Expr::Literal(Value::Pair(Box::new(l), Box::new(r))) }
            "@" i:spanned(<IDENT()>) _ "(" _ args:COMMASEP(<expr()>) _ ")" { Expr::BuiltinCall(i, args) }
            i:spanned(<IDENT()>) _ "(" _ args:COMMASEP(<expr()>) _ ")" { Expr::Call(i, args) }
            l:literal() { Expr::Literal(l) }
            "$" i:IDENT() { Expr::Yoink(i) }
            i:IDENT() { Expr::Ident(i) }
            "(" _ e:expr() _ ")" { e.0 }
        }

        rule block() -> Vec<Spanned<Expr>>
        = "{" _ code:(x:parse() ** (";"/"\n"/_)) _ "}" {code}

        rule parse() -> Spanned<Expr>
        = _ e:expr() _ { e }

        pub rule parse_code() -> Vec<Spanned<Expr>>
        = _ code:(x:parse() ** (";"/"\n"/_))? _ {code.unwrap_or_default()}
    }
);

/// Helper function which converts a hex string to an RGBA color.
fn hex_to_rgba(hex: &str) -> Option<[u8; 4]> {
    let hex = hex.trim_start_matches('#');

    if hex.len() < 6 || hex.len() > 8 {
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
