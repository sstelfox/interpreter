use crate::tokens;

pub enum Expression {
    Binary(Box<Expression>, tokens::Token, Box<Expression>),
    Grouping(Box<Expression>),
    Literal(tokens::Literal),
    // I could potentially split this in prefix and postfix operations but I don't think that's
    // necessary yet...
    Unary(tokens::Token, Box<Expression>),
}

pub trait Visitor<T> {
    fn visit_expression(&mut self, expr: &Expression) -> T;
}

#[derive(Debug)]
pub struct AstPrinter {
    result: String,
}

impl AstPrinter {
    pub fn new() -> Self {
        AstPrinter { result: String::new() }
    }

    pub fn print(&mut self, expr: &Expression) {
        self.visit_expression(expr);
        println!("{}", self.result);
    }
}

impl Visitor<()> for AstPrinter {
    fn visit_expression(&mut self, expr: &Expression) -> () {
        match expr {
            Expression::Binary(left, op, right) => {
                self.result.push_str("(");
                self.visit_expression(left);
                self.result.push_str(&format!(" {} ", op));
                self.visit_expression(right);
                self.result.push_str(")");
            },
            Expression::Grouping(left) => {
                self.result.push_str("(");
                self.visit_expression(left);
                self.result.push_str(")");
            },
            Expression::Literal(value) => {
                self.result.push_str(&format!("{}", value));
            },
            Expression::Unary(op, right) => {
                self.result.push_str(&format!("{}", op));
                self.visit_expression(right);
            },
        }
    }
}
