use crate::tokens::{Literal, Token};
use std::fmt::Debug;

pub trait Expression: Debug {
    fn accept(&self, visitor: Box<&dyn ExpressionVisitor>);
}

macro_rules! define_expression {
    ($type_name:ident($($field_name:ident: $field_type:ty),*).$visit_name:ident) => {
        #[derive(Debug)]
        pub struct $type_name {
            $(
                pub $field_name: $field_type,
            )*
        }

        impl $type_name {
            pub fn new($($field_name: $field_type),+) -> Self {
                Self {
                    $(
                        $field_name: $field_name,
                    )*
                }
            }
        }

        impl Expression for $type_name {
            fn accept(&self, visitor: Box<&dyn ExpressionVisitor>) {
                visitor.$visit_name(self);
            }
        }
    };
}

pub trait ExpressionVisitor {
    fn visit_binary_expression(&self, _expr: &BinaryExpression) {}
    fn visit_grouping_expression(&self, _expr: &GroupingExpression) {}
    fn visit_literal_expression(&self, _expr: &LiteralExpression) {}
    fn visit_unary_expression(&self, _expr: &UnaryExpression) {}
}

define_expression!(BinaryExpression(left: Box<dyn Expression>, operator: Token, right: Box<dyn Expression>).visit_binary_expression);
define_expression!(GroupingExpression(expr: Box<dyn Expression>).visit_grouping_expression);
define_expression!(LiteralExpression(value: Literal).visit_literal_expression);
define_expression!(UnaryExpression(operator: Token, right: Box<dyn Expression>).visit_unary_expression);

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
