use common::span::Spanned;

use crate::literal::Literal;

pub type NodeS = Spanned<Node>;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Literal(Literal),
    Array(Vec<NodeS>),
    Pair(Box<NodeS>, Box<NodeS>),
    Range(Box<NodeS>, Box<NodeS>),

    Identifier(String),
    Declaration(Var, Box<NodeS>),
    Assignment(Spanned<String>, Box<NodeS>),
    Function {
        name: Spanned<String>,
        args: Vec<Spanned<String>>,
        body: Vec<NodeS>,
    },

    Lambda(Vec<Spanned<String>>, Vec<NodeS>),
    Block(Vec<NodeS>),
    Conditional {
        condition: Box<NodeS>,
        then: Vec<NodeS>,
        otherwise: Option<Vec<NodeS>>,
    },
    SwitchCase {
        value: Box<NodeS>,
        cases: Vec<(NodeS, Vec<NodeS>)>,
        default: Option<Vec<NodeS>>,
    },
    ForLoop {
        variable: Spanned<String>,
        iterable: Box<NodeS>,
        body: Vec<NodeS>,
    },
    MultiForLoop {
        variables: Vec<Spanned<String>>,
        iterable: Box<NodeS>,
        body: Vec<NodeS>,
    },
    WhileLoop(Box<NodeS>, Vec<NodeS>),

    Binary(Op, Box<NodeS>, Box<NodeS>),
    Unary(Op, Box<NodeS>),
    Call(Box<NodeS>, Vec<NodeS>),
    BuiltinCall(Spanned<String>, Vec<NodeS>),
    ContextWrapped {
        name: Spanned<String>,
        args: Vec<NodeS>,
        variable: Option<Var>,
        body: Vec<NodeS>,
    },

    IndexGet(Box<NodeS>, Box<NodeS>),
    // IndexSet(Box<NodeS>, Box<NodeS>, Box<NodeS>),
    Include(Spanned<String>, Option<Vec<NodeS>>),
    Return(Option<Box<NodeS>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Var {
    Declaration(Spanned<String>),
    MultiDeclaration(Vec<Spanned<String>>),
}

#[rustfmt::skip]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Add, Sub, Mul, Div, Mod, 
    Pow, Eq,  Neq, Lt,  Gt,  
    Lte, Gte, And, Or, Not, 
    Neg, Join, 
    
    BitwiseAnd, BitwiseOr, BitwiseXor, BitwiseNot, BitwiseLeftShift, BitwiseRightShift,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeType {
    Expression,
    Statement,
}

impl Node {
    pub fn ntype(&self) -> NodeType {
        match self {
            Node::Declaration(_, _) => NodeType::Statement,
            Node::Function { .. } => NodeType::Statement,
            Node::Include(_, _) => NodeType::Statement,
            Node::Return(_) => NodeType::Statement,

            _ => NodeType::Expression,
        }
    }

    pub fn is_expression(&self) -> bool {
        self.ntype() == NodeType::Expression
    }

    pub fn is_statement(&self) -> bool {
        self.ntype() == NodeType::Statement
    }
}
