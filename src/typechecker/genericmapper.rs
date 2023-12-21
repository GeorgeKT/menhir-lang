use super::instantiate::make_concrete;
use super::typecheckercontext::TypeCheckerContext;
use crate::ast::*;
use crate::compileerror::{type_error_result, CompileResult};
use crate::span::Span;

pub fn add(mapping: &mut GenericMapping, from: &Type, to: &Type, span: &Span) -> CompileResult<()> {
    if let Some(prev_arg_type) = mapping.insert(from.clone(), to.clone()) {
        if prev_arg_type != *to {
            return type_error_result(
                span,
                format!(
                    "Generic argument {} mismatch, expecting type {}, not {}",
                    from, prev_arg_type, to
                ),
            );
        }
    }

    Ok(())
}

pub fn fill_in_generics(
    ctx: &TypeCheckerContext,
    actual: &Type,
    generic: &Type,
    known_types: &mut GenericMapping,
    span: &Span,
) -> CompileResult<Type> {
    if *actual == *generic {
        return Ok(actual.clone());
    }

    let new_generic = make_concrete(ctx, known_types, generic, span)?;
    if !new_generic.is_generic() {
        return Ok(new_generic);
    }

    let map_err = || {
        type_error_result(
            span,
            format!("Cannot map argument type {} on type {}", actual, new_generic),
        )
    };

    match (&new_generic, actual) {
        (&Type::Unknown, _) => Ok(actual.clone()),

        (&Type::Generic(_), _) => {
            add(known_types, &new_generic, actual, span)?;
            Ok(actual.clone())
        }

        (Type::Slice(generic_st), Type::Slice(actual_st)) => {
            add(known_types, &generic_st.element_type, &actual_st.element_type, span)?;
            let new_el_type = fill_in_generics(
                ctx,
                &actual_st.element_type,
                &generic_st.element_type,
                known_types,
                span,
            )?;
            Ok(slice_type(new_el_type))
        }

        (Type::Array(generic_at), Type::Array(actual_at)) => {
            add(known_types, &generic_at.element_type, &actual_at.element_type, span)?;
            let new_el_type = fill_in_generics(
                ctx,
                &actual_at.element_type,
                &generic_at.element_type,
                known_types,
                span,
            )?;
            Ok(array_type(new_el_type, actual_at.len))
        }

        (Type::Slice(generic_at), Type::Array(actual_at)) => {
            // We support automatic conversion from array to slice
            add(known_types, &generic_at.element_type, &actual_at.element_type, span)?;
            let new_el_type = fill_in_generics(
                ctx,
                &actual_at.element_type,
                &generic_at.element_type,
                known_types,
                span,
            )?;
            Ok(array_type(new_el_type, actual_at.len))
        }

        (Type::Func(generic_ft), Type::Func(actual_ft)) => {
            if generic_ft.args.len() != actual_ft.args.len() {
                return map_err();
            }

            let mut new_args = Vec::with_capacity(generic_ft.args.len());
            for (ga, aa) in generic_ft.args.iter().zip(actual_ft.args.iter()) {
                let na = fill_in_generics(ctx, &aa.typ, &ga.typ, known_types, span)?;
                new_args.push(FuncArg {
                    typ: na,
                    mutable: ga.mutable,
                });
            }

            let nr = fill_in_generics(ctx, &actual_ft.return_type, &generic_ft.return_type, known_types, span)?;
            Ok(func_type(new_args, nr))
        }

        (Type::Struct(generic_st), Type::Struct(actual_st)) => {
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
        }

        (Type::Sum(generic_st), Type::Sum(actual_st)) => {
            if generic_st.cases.len() != actual_st.cases.len() {
                return map_err();
            }

            let mut new_cases = Vec::with_capacity(actual_st.cases.len());
            for (ga, aa) in generic_st.cases.iter().zip(actual_st.cases.iter()) {
                if aa.name != ga.name {
                    return map_err();
                }

                if let (Some(aat), Some(gat)) = (&aa.typ, &ga.typ) {
                    let nt = fill_in_generics(ctx, aat, gat, known_types, span)?;
                    new_cases.push(sum_type_case(&aa.name, Some(nt)));
                }
            }

            Ok(sum_type(&actual_st.name, new_cases))
        }

        (Type::Pointer(generic_inner), Type::Pointer(actual_inner)) => {
            let inner = fill_in_generics(ctx, actual_inner, generic_inner, known_types, span)?;
            Ok(ptr_type(inner))
        }

        (Type::Optional(generic_inner), Type::Optional(actual_inner)) => {
            let inner = fill_in_generics(ctx, actual_inner, generic_inner, known_types, span)?;
            Ok(optional_type(inner))
        }

        (Type::Result(generic_rt), Type::Result(actual_rt)) => {
            let ok_typ = if generic_rt.ok_typ.is_generic() {
                fill_in_generics(ctx, &actual_rt.ok_typ, &generic_rt.ok_typ, known_types, span)?
            } else {
                generic_rt.ok_typ.clone()
            };

            let err_typ = if generic_rt.err_typ.is_generic() {
                fill_in_generics(ctx, &actual_rt.err_typ, &generic_rt.err_typ, known_types, span)?
            } else {
                generic_rt.err_typ.clone()
            };

            Ok(result_type(ok_typ, err_typ))
        }

        _ => map_err(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        array_type, func_arg, func_type, generic_type, ptr_type, slice_type, GenericMapping, IntSize, Type,
    };
    use crate::span::Span;
    use crate::typechecker::instantiate::make_concrete;
    use crate::typechecker::typecheckercontext::ImportSymbolResolver;

    #[test]
    fn test_simple() {
        let imports = ImportMap::new();
        let ctx = TypeCheckerContext::new(ImportSymbolResolver::ImportMap(&imports));
        let mut tm = GenericMapping::new();
        let ga = generic_type("a");
        assert!(
            fill_in_generics(&ctx, &Type::Int(IntSize::I32), &ga, &mut tm, &Span::default())
                == Ok(Type::Int(IntSize::I32))
        );
        assert!(make_concrete(&ctx, &tm, &ga, &Span::default()).unwrap() == Type::Int(IntSize::I32));
    }

    #[test]
    fn test_slice() {
        let imports = ImportMap::new();
        let ctx = TypeCheckerContext::new(ImportSymbolResolver::ImportMap(&imports));
        let mut tm = GenericMapping::new();
        let ga = slice_type(generic_type("a"));
        let r = fill_in_generics(
            &ctx,
            &slice_type(Type::Int(IntSize::I32)),
            &ga,
            &mut tm,
            &Span::default(),
        );
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(slice_type(Type::Int(IntSize::I32))));
        assert!(make_concrete(&ctx, &tm, &generic_type("a"), &Span::default()).unwrap() == Type::Int(IntSize::I32));
    }

    #[test]
    fn test_array() {
        let imports = ImportMap::new();
        let ctx = TypeCheckerContext::new(ImportSymbolResolver::ImportMap(&imports));
        let mut tm = GenericMapping::new();
        let ga = array_type(generic_type("a"), 10);
        let r = fill_in_generics(
            &ctx,
            &array_type(Type::Int(IntSize::I32), 10),
            &ga,
            &mut tm,
            &Span::default(),
        );
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(array_type(Type::Int(IntSize::I32), 10)));
        assert!(make_concrete(&ctx, &tm, &generic_type("a"), &Span::default()).unwrap() == Type::Int(IntSize::I32));
    }

    #[test]
    fn test_pointer() {
        let imports = ImportMap::new();
        let ctx = TypeCheckerContext::new(ImportSymbolResolver::ImportMap(&imports));
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
    fn test_func() {
        let imports = ImportMap::new();
        let ctx = TypeCheckerContext::new(ImportSymbolResolver::ImportMap(&imports));
        let mut tm = GenericMapping::new();
        let ga = func_type(
            vec![
                func_arg(generic_type("a"), false),
                func_arg(generic_type("b"), false),
                func_arg(generic_type("c"), false),
            ],
            generic_type("d"),
        );
        let aa = func_type(
            vec![
                func_arg(Type::Int(IntSize::I32), false),
                func_arg(Type::Float(FloatSize::F64), false),
                func_arg(Type::Bool, false),
            ],
            Type::String,
        );
        let r = fill_in_generics(&ctx, &aa, &ga, &mut tm, &Span::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(aa));
        assert!(make_concrete(&ctx, &tm, &generic_type("a"), &Span::default()).unwrap() == Type::Int(IntSize::I32));
        assert!(make_concrete(&ctx, &tm, &generic_type("b"), &Span::default()).unwrap() == Type::Float(FloatSize::F64));
        assert!(make_concrete(&ctx, &tm, &generic_type("c"), &Span::default()).unwrap() == Type::Bool);
        assert!(make_concrete(&ctx, &tm, &generic_type("d"), &Span::default()).unwrap() == Type::String);
    }

    #[test]
    fn test_func_wrong_args() {
        let imports = ImportMap::new();
        let ctx = TypeCheckerContext::new(ImportSymbolResolver::ImportMap(&imports));
        let mut tm = GenericMapping::new();
        let ga = func_type(
            vec![
                func_arg(generic_type("a"), false),
                func_arg(generic_type("b"), false),
                func_arg(generic_type("c"), false),
            ],
            generic_type("d"),
        );
        let aa = func_type(
            vec![
                func_arg(Type::Int(IntSize::I32), false),
                func_arg(Type::Float(FloatSize::F64), false),
                func_arg(Type::Bool, false),
                func_arg(Type::Int(IntSize::I32), false),
            ],
            Type::String,
        );
        let r = fill_in_generics(&ctx, &aa, &ga, &mut tm, &Span::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r.is_err());
    }

    #[test]
    fn test_mixed_func() {
        let imports = ImportMap::new();
        let ctx = TypeCheckerContext::new(ImportSymbolResolver::ImportMap(&imports));
        let mut tm = GenericMapping::new();
        let ga = func_type(
            vec![
                func_arg(generic_type("a"), false),
                func_arg(generic_type("b"), false),
                func_arg(generic_type("c"), false),
                func_arg(Type::Int(IntSize::I32), false),
            ],
            generic_type("d"),
        );
        let aa = func_type(
            vec![
                func_arg(Type::Int(IntSize::I32), false),
                func_arg(Type::Float(FloatSize::F64), false),
                func_arg(Type::Bool, false),
                func_arg(Type::Int(IntSize::I32), false),
            ],
            Type::String,
        );
        let r = fill_in_generics(&ctx, &aa, &ga, &mut tm, &Span::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(aa));
        assert!(make_concrete(&ctx, &tm, &generic_type("a"), &Span::default()).unwrap() == Type::Int(IntSize::I32));
        assert!(make_concrete(&ctx, &tm, &generic_type("b"), &Span::default()).unwrap() == Type::Float(FloatSize::F64));
        assert!(make_concrete(&ctx, &tm, &generic_type("c"), &Span::default()).unwrap() == Type::Bool);
        assert!(make_concrete(&ctx, &tm, &generic_type("d"), &Span::default()).unwrap() == Type::String);
    }

    #[test]
    fn test_simple_complex_mix() {
        let imports = ImportMap::new();
        let ctx = TypeCheckerContext::new(ImportSymbolResolver::ImportMap(&imports));
        let mut tm = GenericMapping::new();
        let ga = generic_type("a");
        let r = fill_in_generics(
            &ctx,
            &array_type(Type::Int(IntSize::I32), 10),
            &ga,
            &mut tm,
            &Span::default(),
        );
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(r == Ok(array_type(Type::Int(IntSize::I32), 10)));
        assert!(make_concrete(&ctx, &tm, &ga, &Span::default()).unwrap() == array_type(Type::Int(IntSize::I32), 10));
    }

    #[test]
    fn test_with_already_filled_in_map() {
        let imports = ImportMap::new();
        let ctx = TypeCheckerContext::new(ImportSymbolResolver::ImportMap(&imports));
        let mut tm = GenericMapping::new();
        add(&mut tm, &generic_type("a"), &Type::Int(IntSize::I32), &Span::default()).unwrap();
        add(
            &mut tm,
            &generic_type("b"),
            &Type::Float(FloatSize::F64),
            &Span::default(),
        )
        .unwrap();
        add(&mut tm, &generic_type("c"), &Type::Bool, &Span::default()).unwrap();

        let ga = func_type(
            vec![func_arg(generic_type("a"), false), func_arg(generic_type("b"), false)],
            generic_type("c"),
        );
        let aa = func_type(
            vec![func_arg(Type::Unknown, false), func_arg(Type::Unknown, false)],
            Type::Unknown,
        );
        let r = fill_in_generics(&ctx, &aa, &ga, &mut tm, &Span::default());
        println!("tm: {:?}", tm);
        println!("r: {:?}", r);
        assert!(
            r == Ok(func_type(
                vec![
                    func_arg(Type::Int(IntSize::I32), false),
                    func_arg(Type::Float(FloatSize::F64), false)
                ],
                Type::Bool
            ))
        );
    }
}
