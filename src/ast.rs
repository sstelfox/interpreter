use crate::parser::*;

pub struct AstPrinter;

impl AstPrinter {
    pub fn new() -> Self {
        Self { }
    }

    pub fn print(&self, expr: Box<&dyn Expression>) {
        expr.accept(Box::new(self));
        println!("");
    }
}

impl ExpressionVisitor for AstPrinter {
    fn visit_binary_expression(&self, expr: &BinaryExpression) {
        print!("({} ", expr.operator);
        expr.left.accept(Box::new(self));
        print!(" ");
        expr.right.accept(Box::new(self));
        print!(")");
    }

    fn visit_grouping_expression(&self, expr: &GroupingExpression) {
        print!("(group ");
        expr.expr.accept(Box::new(self));
        print!(")");
    }

    fn visit_literal_expression(&self, expr: &LiteralExpression) {
        print!("{}", expr.value);
    }

    fn visit_unary_expression(&self, expr: &UnaryExpression) {
        print!("({} ", expr.operator);
        expr.right.accept(Box::new(self));
        print!(")");
    }
}
