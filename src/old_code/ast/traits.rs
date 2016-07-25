use ast::{FunctionSignature, Struct, TreePrinter, prefix};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Trait
{
    pub name: String,
    pub public: bool,
    pub functions: Vec<FunctionSignature>,
    pub span: Span,
}

impl Trait
{
    pub fn new(name: String, public: bool, functions: Vec<FunctionSignature>, span: Span) -> Trait
    {
        Trait{
            name: name,
            public: public,
            functions: functions,
            span: span,
        }
    }

    fn is_match(left: &FunctionSignature, right: &FunctionSignature) -> bool
    {
        if left.name.split("::").last() != right.name.split("::").last() {
            return false;
        }

        if left.args.len() != right.args.len() || left.return_type != right.return_type {
            return false;
        }

        for (l, r) in left.args.iter().zip(right.args.iter()) {
            if l.name == "self" && r.name == "self" {
                continue;
            } else if l != r {
                return false;
            }
        }
        true
    }

    // Does a struct implements the trait
    pub fn is_implemented_by(&self, s: &Struct) -> bool
    {
        for trait_func in &self.functions
        {
            let mut found = false;
            for struct_func in &s.functions
            {
                if Trait::is_match(trait_func, &struct_func.sig)
                {
                    found = true;
                    break;
                }
            }

            if !found {
                return false;
            }
        }

        true
    }
}

impl TreePrinter for Trait
{
    fn print(&self, level: usize)
    {
        println!("{}trait {} (span: {})", prefix(level), self.name, self.span);
        for f in &self.functions {
            f.print(level + 1);
        }
    }
}
