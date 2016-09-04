use std::collections::HashMap;
use ast::{slice_type, func_type, array_type, struct_type, struct_member, sum_type, sum_type_case, Type};
use compileerror::{Pos, CompileResult, ErrorCode, err};


fn add_generic_mapping(known_types: &mut HashMap<Type, Type>, from: &Type, to: &Type, pos: Pos) -> CompileResult<()>
{
    if let Some(prev_arg_type) = known_types.insert(from.clone(), to.clone()) {
        if prev_arg_type != *to {
            return err(pos, ErrorCode::GenericTypeSubstitutionError,
                format!("Generic argument {} mismatch, expecting type {}, not {}", from, prev_arg_type, to));
        }
    }

    Ok(())
}

pub fn substitute_types(generic: &Type, known_types: &HashMap<Type, Type>) -> Type
{
    if let Some(t) = known_types.get(generic) {
        return t.clone();
    }

    match *generic
    {
        Type::Array(ref at) => {
            array_type(substitute_types(&at.element_type, known_types), at.length)
        },
        Type::Slice(ref st) => {
            slice_type(substitute_types(&st.element_type, known_types))
        },
        Type::Func(ref ft) => {
            func_type(
                ft.args.iter().map(|t| substitute_types(t, known_types)).collect(),
                substitute_types(&ft.return_type, known_types))
        },
        Type::Struct(ref st) => {
            struct_type(
                st.members.iter()
                    .map(|m| struct_member(&m.name, substitute_types(&m.typ, known_types), m.span))
                    .collect()
            )
        },
        Type::Sum(ref st) => {
            sum_type(
                st.cases.iter()
                    .map(|c| sum_type_case(&c.name, substitute_types(&c.typ, known_types)))
                    .collect(),
                st.index
            )
        },
        _ => generic.clone(),
    }
}

pub fn fill_in_generics(actual: &Type, generic: &Type, known_types: &mut HashMap<Type, Type>, pos: Pos) -> CompileResult<Type>
{
    if *actual == *generic {
        return Ok(actual.clone());
    }

    let new_generic = substitute_types(generic, known_types);
    if !new_generic.is_generic() {
        return Ok(new_generic);
    }

    let map_err = || {
        err(pos, ErrorCode::GenericTypeSubstitutionError, format!("Cannot map argument type {} on type {}", actual, new_generic))
    };
    match new_generic
    {
        Type::Unknown => {
            Ok(actual.clone())
        },
        Type::Generic(_) => {
            try!(add_generic_mapping(known_types, &new_generic, actual, pos));
            Ok(actual.clone())
        },
        Type::Slice(ref generic_st) => {
            match *actual
            {
                Type::Slice(ref actual_st) => {
                    try!(add_generic_mapping(known_types, &generic_st.element_type, &actual_st.element_type, pos));
                    let new_el_type = try!(fill_in_generics(&actual_st.element_type, &generic_st.element_type, known_types, pos));
                    Ok(slice_type(new_el_type))
                },
                _ =>  map_err(),
            }
        },
        Type::Array(ref generic_at) => {
            match *actual
            {
                Type::Array(ref actual_at) => {
                    if generic_at.length != actual_at.length {
                        return map_err();
                    }
                    try!(add_generic_mapping(known_types, &generic_at.element_type, &actual_at.element_type, pos));
                    let new_el_type = try!(fill_in_generics(&actual_at.element_type, &generic_at.element_type, known_types, pos));
                    Ok(array_type(new_el_type, generic_at.length))
                },
                _ => map_err(),
            }
        },
        Type::Func(ref generic_ft) => {
            match *actual {
                Type::Func(ref actual_ft) => {
                    if generic_ft.args.len() != actual_ft.args.len() {
                        return map_err();
                    }

                    let mut new_args = Vec::with_capacity(generic_ft.args.len());
                    for (ga, aa) in generic_ft.args.iter().zip(actual_ft.args.iter()) {
                        let na = try!(fill_in_generics(aa, ga, known_types, pos));
                        new_args.push(na);
                    }

                    let nr = try!(fill_in_generics(&actual_ft.return_type, &generic_ft.return_type, known_types, pos));
                    Ok(func_type(new_args, nr))
                },
                _ => map_err(),
            }
        },
        Type::Struct(ref generic_st) => {
            match *actual {
                Type::Struct(ref actual_st) => {
                    if generic_st.members.len() != actual_st.members.len() {
                        return map_err();
                    }

                    let mut new_members = Vec::with_capacity(generic_st.members.len());
                    for (ga, aa) in generic_st.members.iter().zip(actual_st.members.iter()) {
                        if aa.name != ga.name {
                            return map_err();
                        }

                        let nt = try!(fill_in_generics(&aa.typ, &ga.typ, known_types, pos));
                        new_members.push(struct_member(&aa.name, nt, aa.span));
                    }

                    Ok(struct_type(new_members))
                },
                _ => map_err(),
            }
        },
        Type::Sum(ref _st) => {
            panic!("NYI")
        },
        _ => map_err(),
    }
}

#[cfg(test)]
mod tests
{
    use super::*;
    use std::collections::HashMap;
    use ast::{Type, slice_type, array_type, func_type};
    use compileerror::Pos;

    fn gen_type(name: &str) -> Type
    {
        Type::Generic(name.into())
    }

    #[test]
    fn test_simple()
    {
        let mut tm = HashMap::new();
        let ga = gen_type("a");
        assert!(fill_in_generics(&Type::Int, &ga, &mut tm, Pos::default()) == Ok(Type::Int));
        assert!(*tm.get(&ga).unwrap() == Type::Int);
    }

    #[test]
    fn test_slice()
    {
        let mut tm = HashMap::new();
        let ga = slice_type(gen_type("a"));
        let r = fill_in_generics(&slice_type(Type::Int), &ga, &mut tm, Pos::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(slice_type(Type::Int)));
        assert!(*tm.get(&gen_type("a")).unwrap() == Type::Int);
    }

    #[test]
    fn test_array()
    {
        let mut tm = HashMap::new();
        let ga = array_type(gen_type("a"), 3);
        let r = fill_in_generics(&array_type(Type::Int, 3), &ga, &mut tm, Pos::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(array_type(Type::Int, 3)));
        assert!(*tm.get(&gen_type("a")).unwrap() == Type::Int);
    }

    #[test]
    fn test_array_wrong_length()
    {
        let mut tm = HashMap::new();
        let ga = array_type(gen_type("a"), 6);
        let r = fill_in_generics(&array_type(Type::Int, 3), &ga, &mut tm, Pos::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r.is_err());
    }

    #[test]
    fn test_func()
    {
        let mut tm = HashMap::new();
        let ga = func_type(vec![gen_type("a"), gen_type("b"), gen_type("c")], gen_type("d"));
        let aa = func_type(vec![Type::Int, Type::Float, Type::Bool], Type::String);
        let r = fill_in_generics(&aa, &ga, &mut tm, Pos::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(aa));
        assert!(*tm.get(&gen_type("a")).unwrap() == Type::Int);
        assert!(*tm.get(&gen_type("b")).unwrap() == Type::Float);
        assert!(*tm.get(&gen_type("c")).unwrap() == Type::Bool);
        assert!(*tm.get(&gen_type("d")).unwrap() == Type::String);
    }

    #[test]
    fn test_func_wrong_args()
    {
        let mut tm = HashMap::new();
        let ga = func_type(vec![gen_type("a"), gen_type("b"), gen_type("c")], gen_type("d"));
        let aa = func_type(vec![Type::Int, Type::Float, Type::Bool, Type::Int], Type::String);
        let r = fill_in_generics(&aa, &ga, &mut tm, Pos::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r.is_err());
    }

    #[test]
    fn test_mixed_func()
    {
        let mut tm = HashMap::new();
        let ga = func_type(vec![gen_type("a"), gen_type("b"), gen_type("c"), Type::Int], gen_type("d"));
        let aa = func_type(vec![Type::Int, Type::Float, Type::Bool, Type::Int], Type::String);
        let r = fill_in_generics(&aa, &ga, &mut tm, Pos::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(aa));
        assert!(*tm.get(&gen_type("a")).unwrap() == Type::Int);
        assert!(*tm.get(&gen_type("b")).unwrap() == Type::Float);
        assert!(*tm.get(&gen_type("c")).unwrap() == Type::Bool);
        assert!(*tm.get(&gen_type("d")).unwrap() == Type::String);
    }

    #[test]
    fn test_simple_complex_mix()
    {
        let mut tm = HashMap::new();
        let ga = gen_type("a");
        let r = fill_in_generics(&slice_type(Type::Int), &ga, &mut tm, Pos::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(slice_type(Type::Int)));
        assert!(*tm.get(&ga).unwrap() == slice_type(Type::Int));
    }

    #[test]
    fn test_with_already_filled_in_map()
    {
        let mut tm = HashMap::new();
        tm.insert(gen_type("a"), Type::Int);
        tm.insert(gen_type("b"), Type::Float);
        tm.insert(gen_type("c"), Type::Bool);

        let ga = func_type(vec![gen_type("a"), gen_type("b")], gen_type("c"));
        let aa = func_type(vec![Type::Unknown, Type::Unknown], Type::Unknown);
        let r = fill_in_generics(&aa, &ga, &mut tm, Pos::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(func_type(vec![Type::Int, Type::Float], Type::Bool)));
    }
}
