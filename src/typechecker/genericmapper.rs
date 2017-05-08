use ast::*;
use compileerror::{CompileResult, type_error_result};
use span::Span;
use super::typecheckercontext::TypeCheckerContext;
use super::instantiategenerics::make_concrete;

pub fn add(mapping: &mut GenericMapping, from: &Type, to: &Type, span: &Span) -> CompileResult<()>
{
    if let Some(prev_arg_type) = mapping.insert(from.clone(), to.clone()) {
        if prev_arg_type != *to {
            return type_error_result(span, format!("Generic argument {} mismatch, expecting type {}, not {}", from, prev_arg_type, to));
        }
    }

    Ok(())
}

pub fn fill_in_generics(ctx: &TypeCheckerContext, actual: &Type, generic: &Type, known_types: &mut GenericMapping, span: &Span) -> CompileResult<Type>
{
    if *actual == *generic {
        return Ok(actual.clone());
    }

    let new_generic = make_concrete(ctx, known_types, generic, span)?;
    if !new_generic.is_generic() {
        return Ok(new_generic);
    }

    let map_err = || {
        type_error_result(span, format!("Cannot map argument type {} on type {}", actual, new_generic))
    };

    match (&new_generic, actual)
    {
        (&Type::Unknown, _) => {
            Ok(actual.clone())
        },

        (&Type::Generic(_), _) => {
            add(known_types, &new_generic, actual, span)?;
            Ok(actual.clone())
        },

        (&Type::Slice(ref generic_st), &Type::Slice(ref actual_st)) => {
            add(known_types, &generic_st.element_type, &actual_st.element_type, span)?;
            let new_el_type = fill_in_generics(ctx, &actual_st.element_type, &generic_st.element_type, known_types, span)?;
            Ok(slice_type(new_el_type))
        },

        (&Type::Array(ref generic_at), &Type::Array(ref actual_at)) => {
            add(known_types, &generic_at.element_type, &actual_at.element_type, span)?;
            let new_el_type = fill_in_generics(ctx, &actual_at.element_type, &generic_at.element_type, known_types, span)?;
            Ok(array_type(new_el_type, actual_at.len))
        },

        (&Type::Func(ref generic_ft), &Type::Func(ref actual_ft)) => {
            if generic_ft.args.len() != actual_ft.args.len() {
                return map_err();
            }

            let mut new_args = Vec::with_capacity(generic_ft.args.len());
            for (ga, aa) in generic_ft.args.iter().zip(actual_ft.args.iter()) {
                let na = fill_in_generics(ctx, aa, ga, known_types, span)?;
                new_args.push(na);
            }

            let nr = fill_in_generics(ctx, &actual_ft.return_type, &generic_ft.return_type, known_types, span)?;
            Ok(func_type(new_args, nr))
        },

        (&Type::Struct(ref generic_st), &Type::Struct(ref actual_st))  => {
            if generic_st.members.len() != actual_st.members.len() {
                return map_err();
            }

            let mut new_members = Vec::with_capacity(generic_st.members.len());
            for (ga, aa) in generic_st.members.iter().zip(actual_st.members.iter()) {
                if aa.name != ga.name {
                    return map_err();
                }

                let nt = fill_in_generics(ctx, &aa.typ, &ga.typ, known_types, span)?;
                new_members.push(struct_member(&aa.name, nt));
            }

            Ok(struct_type(&actual_st.name, new_members))
        },

        (&Type::Sum(ref generic_st), &Type::Sum(ref actual_st)) => {
            if generic_st.cases.len() != actual_st.cases.len() {
                return map_err();
            }

            let mut new_cases = Vec::with_capacity(actual_st.cases.len());
            for (ga, aa) in generic_st.cases.iter().zip(actual_st.cases.iter()) {
                if aa.name != ga.name {
                    return map_err();
                }

                let nt = fill_in_generics(ctx, &aa.typ, &ga.typ, known_types, span)?;
                new_cases.push(sum_type_case(&aa.name, nt));
            }

            Ok(sum_type(&actual_st.name, new_cases))
        },

        (&Type::Pointer(ref generic_inner), &Type::Pointer(ref actual_inner)) => {
            let inner = fill_in_generics(ctx, actual_inner, generic_inner, known_types, span)?;
            Ok(ptr_type(inner))
        },

        (&Type::Optional(ref generic_inner), &Type::Optional(ref actual_inner)) => {
            let inner = fill_in_generics(ctx, actual_inner, generic_inner, known_types, span)?;
            Ok(optional_type(inner))
        }

        _ => map_err(),
    }
}

#[cfg(test)]
mod tests
{
    use super::*;
    use ast::{Type, GenericMapping, array_type, slice_type, func_type, string_type, ptr_type, generic_type};
    use typechecker::instantiategenerics::make_concrete;
    use span::Span;

    #[test]
    fn test_simple()
    {
        let ctx = TypeCheckerContext::new();
        let mut tm = GenericMapping::new();
        let ga = generic_type("a");
        assert!(fill_in_generics(&ctx, &Type::Int(IntSize::I32), &ga, &mut tm, &Span::default()) == Ok(Type::Int(IntSize::I32)));
        assert!(make_concrete(&ctx, &tm, &ga, &Span::default()).unwrap() == Type::Int(IntSize::I32));
    }

    #[test]
    fn test_slice()
    {
        let ctx = TypeCheckerContext::new();
        let mut tm = GenericMapping::new();
        let ga = slice_type(generic_type("a"));
        let r = fill_in_generics(&ctx, &slice_type(Type::Int(IntSize::I32)), &ga, &mut tm, &Span::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(slice_type(Type::Int(IntSize::I32))));
        assert!(make_concrete(&ctx, &tm, &generic_type("a"), &Span::default()).unwrap() == Type::Int(IntSize::I32));
    }

    #[test]
    fn test_array()
    {
        let ctx = TypeCheckerContext::new();
        let mut tm = GenericMapping::new();
        let ga = array_type(generic_type("a"), 10);
        let r = fill_in_generics(&ctx, &array_type(Type::Int(IntSize::I32), 10), &ga, &mut tm, &Span::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(array_type(Type::Int(IntSize::I32), 10)));
        assert!(make_concrete(&ctx, &tm, &generic_type("a"), &Span::default()).unwrap() == Type::Int(IntSize::I32));
    }

    #[test]
    fn test_pointer()
    {
        let ctx = TypeCheckerContext::new();
        let mut tm = GenericMapping::new();
        let gptr = ptr_type(generic_type("a"));
        let aptr = ptr_type(Type::Int(IntSize::I32));
        let r = fill_in_generics(&ctx, &aptr, &gptr, &mut tm, &Span::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(ptr_type(Type::Int(IntSize::I32))));
        assert!(make_concrete(&ctx, &tm, &generic_type("a"), &Span::default()).unwrap() == Type::Int(IntSize::I32));
    }

    #[test]
    fn test_func()
    {
        let ctx = TypeCheckerContext::new();
        let mut tm = GenericMapping::new();
        let ga = func_type(vec![generic_type("a"), generic_type("b"), generic_type("c")], generic_type("d"));
        let aa = func_type(vec![Type::Int(IntSize::I32), Type::Float(FloatSize::F64), Type::Bool], string_type());
        let r = fill_in_generics(&ctx, &aa, &ga, &mut tm, &Span::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(aa));
        assert!(make_concrete(&ctx, &tm, &generic_type("a"), &Span::default()).unwrap() == Type::Int(IntSize::I32));
        assert!(make_concrete(&ctx, &tm, &generic_type("b"), &Span::default()).unwrap() == Type::Float(FloatSize::F64));
        assert!(make_concrete(&ctx, &tm, &generic_type("c"), &Span::default()).unwrap() == Type::Bool);
        assert!(make_concrete(&ctx, &tm, &generic_type("d"), &Span::default()).unwrap() == string_type());
    }

    #[test]
    fn test_func_wrong_args()
    {
        let ctx = TypeCheckerContext::new();
        let mut tm = GenericMapping::new();
        let ga = func_type(vec![generic_type("a"), generic_type("b"), generic_type("c")], generic_type("d"));
        let aa = func_type(vec![Type::Int(IntSize::I32), Type::Float(FloatSize::F64), Type::Bool, Type::Int(IntSize::I32)], string_type());
        let r = fill_in_generics(&ctx, &aa, &ga, &mut tm, &Span::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r.is_err());
    }

    #[test]
    fn test_mixed_func()
    {
        let ctx = TypeCheckerContext::new();
        let mut tm = GenericMapping::new();
        let ga = func_type(vec![generic_type("a"), generic_type("b"), generic_type("c"), Type::Int(IntSize::I32)], generic_type("d"));
        let aa = func_type(vec![Type::Int(IntSize::I32), Type::Float(FloatSize::F64), Type::Bool, Type::Int(IntSize::I32)], string_type());
        let r = fill_in_generics(&ctx, &aa, &ga, &mut tm, &Span::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(aa));
        assert!(make_concrete(&ctx, &tm, &generic_type("a"), &Span::default()).unwrap() == Type::Int(IntSize::I32));
        assert!(make_concrete(&ctx, &tm, &generic_type("b"), &Span::default()).unwrap() == Type::Float(FloatSize::F64));
        assert!(make_concrete(&ctx, &tm, &generic_type("c"), &Span::default()).unwrap() == Type::Bool);
        assert!(make_concrete(&ctx, &tm, &generic_type("d"), &Span::default()).unwrap() == string_type());
    }

    #[test]
    fn test_simple_complex_mix()
    {
        let ctx = TypeCheckerContext::new();
        let mut tm = GenericMapping::new();
        let ga = generic_type("a");
        let r = fill_in_generics(&ctx, &array_type(Type::Int(IntSize::I32), 10), &ga, &mut tm, &Span::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(array_type(Type::Int(IntSize::I32), 10)));
        assert!(make_concrete(&ctx, &tm, &ga, &Span::default()).unwrap() == array_type(Type::Int(IntSize::I32), 10));
    }

    #[test]
    fn test_with_already_filled_in_map()
    {
        let ctx = TypeCheckerContext::new();
        let mut tm = GenericMapping::new();
        add(&mut tm, &generic_type("a"), &Type::Int(IntSize::I32), &Span::default()).unwrap();
        add(&mut tm, &generic_type("b"), &Type::Float(FloatSize::F64), &Span::default()).unwrap();
        add(&mut tm, &generic_type("c"), &Type::Bool, &Span::default()).unwrap();

        let ga = func_type(vec![generic_type("a"), generic_type("b")], generic_type("c"));
        let aa = func_type(vec![Type::Unknown, Type::Unknown], Type::Unknown);
        let r = fill_in_generics(&ctx, &aa, &ga, &mut tm, &Span::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(func_type(vec![Type::Int(IntSize::I32), Type::Float(FloatSize::F64)], Type::Bool)));
    }
}
