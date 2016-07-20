use compileerror::{Span};


#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ModuleName
{
    pub parts: Vec<String>,
    pub span: Span,
}

impl ModuleName
{
    pub fn new(parts: Vec<String>, span: Span) -> ModuleName {
        ModuleName{
            parts: parts,
            span: span,
        }
    }
    pub fn to_string(&self) -> String
    {
        self.parts.join("::")
    }
}
