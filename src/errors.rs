use std::error::Error;
use std::fmt::{self, Display};

use crate::tokens::Token;

#[derive(Debug, PartialEq)]
pub enum RIError {
    // This should be a LexicalError and I should ditch the Illegal token
    InvalidCharacter(Token),
}

impl Display for RIError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::RIError::*;

        match self {
            InvalidCharacter(tok) => write!(f, "encountered an invalid character: {:?}", tok),
        }
    }
}

impl Error for RIError {
    fn description(&self) -> &str {
        use self::RIError::*;

        match *self {
            InvalidCharacter(_) => {
                "encountered an invalid character while tokenizing the input"
            }
        }
    }
}
