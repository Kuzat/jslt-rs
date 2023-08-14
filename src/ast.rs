use std::collections::HashMap;

use serde::{Serialize, Serializer};

// #[derive(Debug, PartialEq, Clone)]
// pub enum ASTNode {
//     Expression(Expression),
//     Statement(Statement),
// }
    
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    StringLiteral(LiteralNode<String>),
    NumberLiteral(LiteralNode<i64>),
    BooleanLiteral(LiteralNode<bool>),
    NullLiteral,
    Identifier(IdentifierNode),
    Object(ObjectNode),
    Array(ArrayNode),
    Function(FunctionNode),
    // Operator(OperatorNode),
}

impl Serialize for Expression {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
            
            match self {
                Expression::StringLiteral(node) => node.value.serialize(serializer),
                Expression::NumberLiteral(node) => node.value.serialize(serializer),
                Expression::BooleanLiteral(node) => node.value.serialize(serializer),
                Expression::NullLiteral => serializer.serialize_none(),
                Expression::Identifier(node) => node.name.serialize(serializer),
                Expression::Object(node) => node.properties.serialize(serializer),
                Expression::Array(node) => node.elements.serialize(serializer),
                Expression::Function(node) => node.arguments.serialize(serializer),
            }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LiteralNode<T> {
    pub value: T,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierNode {
    pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ObjectNode {
    pub properties: HashMap<String, Box<Expression>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayNode {
    pub elements: Vec<Box<Expression>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionNode {
    pub arguments: Vec<Box<Expression>>,
    pub body: Box<Expression>
}

// pub struct OperatorNode {
//     pub operator: String,
//     pub left: Box<Expression>,
//     pub right: Box<Expression>,
// }

