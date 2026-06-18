/// An XSD validation error.
#[derive(Debug, Clone, PartialEq)]
pub struct XSDValidationError {
    /// Human-readable description of the validation failure.
    pub message: String,
    /// Line number in the instance document, if available.
    pub line: Option<usize>,
    /// Column number in the instance document, if available.
    pub column: Option<usize>,
}

impl std::fmt::Display for XSDValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match (self.line, self.column) {
            (Some(l), Some(c)) => write!(f, "{}:{}: {}", l, c, self.message),
            _ => write!(f, "{}", self.message),
        }
    }
}

impl std::error::Error for XSDValidationError {}
