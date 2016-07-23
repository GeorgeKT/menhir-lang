use std::collections::HashMap;
use ast::{Function, FunctionSignature, Type, GenericType, Argument, Block, Statement, While, If, ElsePart,
    Struct, Union, UnionCase, Match, MatchCase, Variable, ObjectConstruction, Expression, NameRef};

#[allow(dead_code)]
pub struct GenericInstantiator
{
    mapping: HashMap<String, String>,
}

#[allow(dead_code)]
impl GenericInstantiator
{
    pub fn new() -> GenericInstantiator
    {
        GenericInstantiator{
            mapping: HashMap::new(),
        }
    }

    pub fn add_generic_type(&mut self, generic_name: &str, actual_name: &str)
    {
        self.mapping.insert(generic_name.into(), actual_name.into());
    }

    fn subst(&self, name: &str) -> String
    {
        self.mapping.get(name)
            .map(|s| s.as_str())
            .unwrap_or(name).into()
    }

    pub fn gen_function(&self, f: &Function) -> Function
    {
        Function::new(
            self.gen_function_signature(&f.sig),
            f.public,
            self.gen_block(&f.block),
            f.span
        )
    }

    fn gen_function_signature(&self, sig: &FunctionSignature) -> FunctionSignature
    {
        let name_addendum = format!("<{}>", self.mapping.values().map(|v| v.clone()).collect::<Vec<_>>().join(","));
        FunctionSignature{
            name: format!("{}{}", sig.name, name_addendum),
            return_type: self.gen_type(&sig.return_type),
            args: sig.args.iter().map(|arg| self.gen_argument(arg)).collect(),
            generic_args: Vec::new(),
            span: sig.span,
        }
    }

    fn gen_type(&self, typ: &Type) -> Type
    {
        match *typ
        {
            Type::Void => Type::Void,
            Type::Unknown => Type::Unknown,
            Type::Primitive(ref t) => Type::Primitive(t.clone()),
            Type::Complex(ref s) => Type::Complex(self.subst(s)),
            Type::Pointer(ref st) => Type::Pointer(Box::new(self.gen_type(st))),
            Type::Array(ref at, count) => Type::Array(Box::new(self.gen_type(at)), count),
            Type::Slice(ref at) => Type::Slice(Box::new(self.gen_type(at))),
            Type::Generic(ref g) => {
                Type::Generic(GenericType::new(
                    self.subst(&g.name),
                    g.generic_args.iter().map(|t| self.gen_type(t)).collect()
                ))
            }
        }
    }

    fn gen_argument(&self, arg: &Argument) -> Argument
    {
        Argument{
            name: arg.name.clone(),
            typ: self.gen_type(&arg.typ),
            constant: arg.constant,
            span: arg.span,
        }
    }

    fn gen_block(&self, b: &Block) -> Block
    {
        let mut stmts = Vec::new();
        for s in &b.statements
        {
            stmts.push(self.gen_statement(s));
        }

        Block::new(stmts)
    }

    pub fn gen_statement(&self, s: &Statement) -> Statement
    {
        match *s
        {
            Statement::Function(ref fun) => Statement::Function(self.gen_function(fun)),
            Statement::Variable(ref vars) => Statement::Variable(vars.iter().map(|v| self.gen_variable(v)).collect()),
            Statement::While(ref w) => Statement::While(self.gen_while(w)),
            Statement::If(ref i) => Statement::If(self.gen_if(i)),
            Statement::Struct(ref s) => Statement::Struct(self.gen_struct(s)),
            Statement::Union(ref u) => Statement::Union(self.gen_union(u)),
            Statement::Match(ref m) => Statement::Match(self.gen_match(m)),
            Statement::Expression(ref e) => Statement::Expression(self.gen_expression(e)),
            _ => s.clone(),
        }
    }

    fn gen_expression(&self, e: &Expression) -> Expression
    {
        match *e
        {
            Expression::ObjectConstruction(ref oc) => Expression::ObjectConstruction(self.gen_object_construction(oc)),
            _ => e.clone(),
        }
    }

    fn gen_object_construction(&self, oc: &ObjectConstruction) -> ObjectConstruction
    {
        ObjectConstruction::new(
            NameRef::new(self.subst(&oc.object_type.name), oc.object_type.span),
            oc.args.clone(),
            oc.span)
    }

    fn gen_variable(&self, v: &Variable) -> Variable
    {
        Variable::new(
            v.name.clone(),
            self.gen_type(&v.typ),
            v.is_const,
            v.public,
            self.gen_expression(&v.init),
            v.span)
    }

    fn gen_while(&self, w: &While) -> While
    {
        While::new(self.gen_expression(&w.cond), self.gen_block(&w.block), w.span)
    }

    fn gen_if(&self, i: &If) -> If
    {
        If::new(self.gen_expression(&i.cond), self.gen_block(&i.if_block), self.gen_else_part(&i.else_part), i.span)
    }

    fn gen_else_part(&self, e: &ElsePart) -> ElsePart
    {
        match *e
        {
            ElsePart::Empty => ElsePart::Empty,
            ElsePart::Block(ref b) => ElsePart::Block(self.gen_block(b)),
            ElsePart::If(ref i) => ElsePart::If(Box::new(self.gen_if(i))),
        }
    }

    fn gen_struct(&self, s: &Struct) -> Struct
    {
        let mut new_s = Struct::new(s.name.clone(), s.public, s.span);
        new_s.impls = s.impls.clone();
        new_s.variables = s.variables.iter().map(|v| self.gen_variable(v)).collect();
        new_s.functions = s.functions.iter().map(|f| self.gen_function(f)).collect();
        new_s
    }

    fn gen_union(&self, u: &Union) -> Union
    {
        let mut new_u = Union::new(u.name.clone(), u.public, u.span);
        new_u.cases = u.cases.iter().map(|c| self.gen_union_case(c)).collect();
        new_u.functions = u.functions.iter().map(|f| self.gen_function(f)).collect();
        new_u
    }

    fn gen_union_case(&self, uc: &UnionCase) -> UnionCase
    {
        let mut new_uc = UnionCase::new(uc.name.clone(), uc.span);
        new_uc.vars = uc.vars.iter().map(|v| self.gen_argument(v)).collect();
        new_uc
    }

    fn gen_match(&self, m: &Match) -> Match
    {
        let mut new_m = Match::new(m.expr.clone(), m.span);
        new_m.cases = m.cases.iter().map(|c| self.gen_match_case(c)).collect();
        new_m
    }

    fn gen_match_case(&self, m: &MatchCase) -> MatchCase
    {
        MatchCase::new(m.name.clone(), m.bindings.clone(), self.gen_block(&m.block), m.span)
    }
}

#[cfg(test)]
mod tests
{
    use super::GenericInstantiator;
    use parser::{th_statement, sig, name_ref2, name_ref};
    use ast::{Function, Block, Return, Argument, Type, Call, Member, Statement, TreePrinter,
        Variable, member_access, object_construction};
    use compileerror::{span};

    #[test]
    fn test_function_instantiation()
    {
        let stmt = th_statement(r#"
func sum<T: Sum>(x: T) -> int:
    return x.sum()
        ""#);

        let mut gi: GenericInstantiator = GenericInstantiator::new();
        gi.add_generic_type("T", "Bar");

        let modified_stmt = gi.gen_statement(&stmt);
        modified_stmt.print(0);
        assert!(modified_stmt == Statement::Function(
            Function::new(
                sig(
                    "sum<Bar>",
                    Type::Primitive("int".into()),
                    vec![
                        Argument::new("x".into(), Type::Complex("Bar".into()), true, span(2, 18, 2, 21))
                    ],
                    span(2, 1, 2, 29),
                ),
                false,
                Block::new(vec![
                    Statement::Return(
                        Return::new(
                            member_access(
                                name_ref2("x", span(3, 12, 3, 12)),
                                Member::Call(Call::new(
                                    name_ref("sum", span(3, 14, 3, 16)),
                                    Vec::new(),
                                    span(3, 14, 3, 18),
                                )),
                                span(3, 12, 3, 18)
                            ),
                            span(3, 5, 3, 18),
                        )
                    )
                ]),
                span(2, 1, 4, 9))
            )
        );
    }

    #[test]
    fn test_var_instantiation()
    {
        let stmt = th_statement(r#"
var foo: T = T{}
        ""#);

        let mut gi: GenericInstantiator = GenericInstantiator::new();
        gi.add_generic_type("T", "Bar");

        let modified_stmt = gi.gen_statement(&stmt);
        modified_stmt.print(0);
        assert!(modified_stmt == Statement::Variable(
            vec![
                Variable::new(
                    "foo".into(),
                    Type::Complex("Bar".into()),
                    false,
                    false,
                    object_construction(
                        name_ref2("Bar", span(2, 14, 2, 14)),
                        Vec::new(),
                        span(2, 14, 2, 16),
                    ),
                    span(2, 5, 2, 16)
                )
            ]
        ));
    }
}
