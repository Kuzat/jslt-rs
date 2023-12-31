use std::collections::HashMap;

use crate::ast::Expression;

pub struct Variables {
    pub variables: HashMap<String, Box<Expression>>,
}

pub struct Runtime {
    environments: Vec<Variables>,
}

impl Runtime {
    pub fn new() -> Runtime {
        Runtime {
            environments: vec![Variables {
                variables: HashMap::new(),
            }],
        }
    }

    pub fn get_variable(&self, name: &str) -> Option<&Box<Expression>> {
        for environment in self.environments.iter().rev() {
            let value = environment.variables.get(name);
            match value {
                Some(value) => return Some(value),
                None => continue,
            }
        }

        None
    }

    pub fn set_variable(&mut self, name: &str, value: Box<Expression>) {
        let environment = self.environments.last_mut().unwrap();
        environment.variables.insert(name.to_string(), value);
    }

    pub fn get_object_property<'a>(
        &'a self,
        object: &'a Expression,
        property: &'a str,
    ) -> Option<&Box<Expression>> {
        match object {
            Expression::Object(node) => node.properties.get(property),
            Expression::ObjectPropertyAccess(node) => {
                let object = self.get_object_property(&node.object, &node.property);
                match object {
                    Some(object) => self.get_object_property(object, property),
                    None => None,
                }
            }
            Expression::RootObject => {
                let root = self.get_variable("root").unwrap();
                self.get_object_property(root, property)
            }
            _ => return None,
        }
    }

    pub fn push_environment(&mut self) {
        self.environments.push(Variables {
            variables: HashMap::new(),
        });
    }

    pub fn pop_environment(&mut self) {
        self.environments.pop();
    }
}
