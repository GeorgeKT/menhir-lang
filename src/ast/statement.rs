use ast::{Expression, Import, Variable, Function, ExternalFunction, While, If, Return, Struct,
    Union, Match, Trait, TreePrinter};


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement
{
    Import(Import),
    Variable(Vec<Variable>),
    Function(Function),
    ExternalFunction(ExternalFunction),
    While(While),
    If(If),
    Return(Return),
    Struct(Struct),
    Union(Union),
    Match(Match),
    Expression(Expression),
    Trait(Trait),
}

impl TreePrinter for Statement
{

    fn print(&self, level: usize)
    {
        match *self
        {
            Statement::Import(ref i) => i.print(level),
            Statement::Variable(ref vars) => {
                for v in vars {
                    v.print(level);
                }
            },
            Statement::Function(ref fun) => fun.print(level),
            Statement::ExternalFunction(ref fun) => fun.print(level),
            Statement::While(ref w) => w.print(level),
            Statement::If(ref i) => i.print(level),
            Statement::Return(ref r) => r.print(level),
            Statement::Struct(ref s) => s.print(level),
            Statement::Union(ref u) => u.print(level),
            Statement::Match(ref m) => m.print(level),
            Statement::Expression(ref e) => e.print(level),
            Statement::Trait(ref t) => t.print(level),
        }
    }
}
