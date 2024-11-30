use super::typecheckercontext::TypeCheckerContext;
use crate::ast::*;
use crate::compileerror::{type_error_result, unknown_name_result, CompileResult};
use crate::span::Span;
use std::collections::HashSet;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Eq, PartialEq, Debug)]
pub enum TypeResolved {
    Yes,
    No,
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum ResolveMode {
    Lazy,
    Forced,
}

fn resolve_type_helper(ctx: &TypeCheckerContext, typ: &Type) -> (Option<Type>, TypeResolved) {
    match typ {
        Type::Unresolved(ut) => {
            if let Some(r) = ctx.resolve(&ut.name) {
                (Some(r.typ.clone()), TypeResolved::Yes)
            } else {
                (None, TypeResolved::No)
            }
        }

        Type::Pointer(inner) => {
            let r = resolve_type_helper(ctx, inner);
            if let (Some(typ), TypeResolved::Yes) = r {
                (Some(ptr_type(typ)), TypeResolved::Yes)
            } else {
                r
            }
        }

        Type::Optional(inner) => {
            let r = resolve_type_helper(ctx, inner);
            if let (Some(typ), TypeResolved::Yes) = r {
                (Some(optional_type(typ)), TypeResolved::Yes)
            } else {
                r
            }
        }

        Type::Result(rt) => {
            let mut dst = ResultType {
                ok_typ: rt.ok_typ.clone(),
                err_typ: rt.err_typ.clone(),
            };
            let ok_resolved = resolve_type(ctx, &mut dst.ok_typ);
            let err_resolved = resolve_type(ctx, &mut dst.err_typ);

            if ok_resolved == TypeResolved::Yes && err_resolved == TypeResolved::Yes {
                (Some(Type::Result(Rc::new(dst))), TypeResolved::Yes)
            } else {
                (None, TypeResolved::No)
            }
        }

        Type::Generic(ref gt) => match *gt.deref() {
            GenericType::Any(ref name) => ctx
                .resolve(name)
                .map(|r| {
                    if let Type::Interface(_) = r.typ {
                        (
                            Some(generic_type_with_constraints(vec![r.typ.clone()])),
                            TypeResolved::Yes,
                        )
                    } else {
                        (None, TypeResolved::Yes)
                    }
                })
                .unwrap_or((None, TypeResolved::Yes)),

            GenericType::Restricted(ref interfaces) => {
                let mut new_interfaces = Vec::new();
                for interface in interfaces {
                    let r = resolve_type_helper(ctx, interface);
                    if let (Some(typ), TypeResolved::Yes) = r {
                        new_interfaces.push(typ);
                    } else {
                        return (None, TypeResolved::No);
                    }
                }

                (Some(generic_type_with_constraints(new_interfaces)), TypeResolved::Yes)
            }
        },

        _ => (None, TypeResolved::Yes),
    }
}

pub fn resolve_type(ctx: &TypeCheckerContext, typ: &mut Type) -> TypeResolved {
    match resolve_type_helper(ctx, typ) {
        (Some(resolved_typ), TypeResolved::Yes) => {
            *typ = resolved_typ;
            TypeResolved::Yes
        }
        (_, result) => result,
    }
}

fn resolve_function_args_and_ret_type(
    ctx: &mut TypeCheckerContext,
    sig: &mut FunctionSignature,
    mode: ResolveMode,
) -> CompileResult<TypeResolved> {
    if sig.typ != Type::Unknown {
        return Ok(TypeResolved::Yes);
    }

    if resolve_type(ctx, &mut sig.return_type) == TypeResolved::No {
        return unknown_name_result(&sig.span, format!("Unknown function return type {}", sig.return_type));
    }

    if !sig.return_type.is_valid() {
        return type_error_result(&sig.span, format!("The type {} is not a valid type", sig.return_type));
    }

    let mut args = Vec::with_capacity(sig.args.len());
    for ref mut arg in &mut sig.args {
        if resolve_type(ctx, &mut arg.typ) == TypeResolved::No {
            if mode == ResolveMode::Lazy {
                return Ok(TypeResolved::No);
            } else {
                return unknown_name_result(&arg.span, format!("Unknown function argument type {}", arg.typ));
            }
        }

        if !arg.typ.is_valid_member_or_arg_type() {
            return type_error_result(
                &arg.span,
                format!("The type {} is not a valid function argument type", arg.typ),
            );
        }

        args.push(func_arg(arg.typ.clone(), arg.mutable));
    }

    sig.typ = func_type(args, sig.return_type.clone());
    Ok(TypeResolved::Yes)
}

fn resolve_struct_member_types(
    ctx: &mut TypeCheckerContext,
    sd: &mut StructDeclaration,
    mode: ResolveMode,
) -> CompileResult<TypeResolved> {
    if sd.typ != Type::Unknown {
        return Ok(TypeResolved::Yes);
    }

    let mut member_types = Vec::with_capacity(sd.members.len());
    for m in &mut sd.members {
        if resolve_type(ctx, &mut m.typ) == TypeResolved::No {
            if mode == ResolveMode::Lazy {
                return Ok(TypeResolved::No);
            } else {
                return unknown_name_result(&m.span, format!("Unknown struct member type {}", m.typ));
            }
        }

        if !m.typ.is_valid_member_or_arg_type() {
            return type_error_result(&m.span, format!("The type {} is not a valid struct member type", m.typ));
        }
        member_types.push(struct_member(m.name.clone(), m.typ.clone()));
    }

    if resolve_implements_list(ctx, &mut sd.implements, mode)? == TypeResolved::No {
        return Ok(TypeResolved::No);
    }

    let implements = sd.implements.iter().map(|(t, _)| t.clone()).collect();
    sd.typ = struct_type(Some(sd.name.clone()), member_types, implements);
    Ok(TypeResolved::Yes)
}

fn resolve_implements_list(
    ctx: &mut TypeCheckerContext,
    il: &mut Vec<(Type, Span)>,
    mode: ResolveMode,
) -> CompileResult<TypeResolved> {
    for (typ, span) in il {
        if resolve_type(ctx, typ) == TypeResolved::No {
            if mode == ResolveMode::Lazy {
                return Ok(TypeResolved::No);
            } else {
                return unknown_name_result(span, format!("Unknown interface {}", typ));
            }
        } else if !matches!(typ, Type::Interface(_)) {
            return type_error_result(span, format!("Type {typ} is not an interface"));
        }
    }

    Ok(TypeResolved::Yes)
}

fn resolve_sum_case_types(
    ctx: &mut TypeCheckerContext,
    st: &mut SumTypeDeclaration,
    mode: ResolveMode,
) -> CompileResult<TypeResolved> {
    if st.typ != Type::Unknown {
        return Ok(TypeResolved::Yes);
    }

    let mut case_types = Vec::with_capacity(st.cases.len());
    for c in &mut st.cases {
        if let Some(sd) = &mut c.data {
            sd.name = c.name.clone();
            if resolve_struct_member_types(ctx, sd, mode)? == TypeResolved::No {
                return Ok(TypeResolved::No);
            } else {
                case_types.push(sum_type_case(&c.name, Some(sd.typ.clone())));
            }
        } else {
            case_types.push(sum_type_case(&c.name, None));
        }
    }

    if resolve_implements_list(ctx, &mut st.implements, mode)? == TypeResolved::No && mode == ResolveMode::Lazy {
        return Ok(TypeResolved::No);
    }

    let implements = st.implements.iter().map(|(t, _)| t.clone()).collect();
    if case_types.iter().all(|ct| ct.typ.is_none()) {
        let case_names: Vec<String> = st.cases.iter().map(|c| c.name.clone()).collect();
        st.typ = enum_type(&st.name, case_names, implements);
    } else {
        st.typ = sum_type(&st.name, case_types, implements);
    }

    Ok(TypeResolved::Yes)
}

fn resolve_interface_types(
    ctx: &mut TypeCheckerContext,
    i: &mut Interface,
    mode: ResolveMode,
) -> CompileResult<TypeResolved> {
    if i.typ != Type::Unknown {
        return Ok(TypeResolved::Yes);
    }

    let mut generic_args = HashSet::new();
    let mut functions = Vec::new();
    for func in &mut i.functions {
        if resolve_function_args_and_ret_type(ctx, func, mode)? == TypeResolved::No {
            return Ok(TypeResolved::No);
        }

        if func.return_type.is_generic() {
            generic_args.insert(func.return_type.clone());
        }

        for arg in &func.args {
            if arg.typ.is_generic() {
                generic_args.insert(arg.typ.clone());
            }
        }

        functions.push(func.clone());
    }

    i.typ = interface_type(&i.name, generic_args.into_iter().collect(), functions);
    Ok(TypeResolved::Yes)
}

const MAX_SUM_CASES: usize = 16384;

fn resolve_all_types(ctx: &mut TypeCheckerContext, module: &mut Module, mode: ResolveMode) -> CompileResult<usize> {
    let mut num_resolved = 0;
    for typ in module.types.values_mut() {
        match *typ {
            TypeDeclaration::Interface(ref mut i) => {
                if resolve_interface_types(ctx, i, mode)? == TypeResolved::Yes {
                    ctx.add(Symbol::new(&i.name, &i.typ, false, &i.span, SymbolType::Normal))?;
                    num_resolved += 1;
                }
            }

            TypeDeclaration::Struct(ref mut s) => {
                if resolve_struct_member_types(ctx, s, mode)? == TypeResolved::Yes {
                    ctx.add(Symbol::new(&s.name, &s.typ, false, &s.span, SymbolType::Normal))?;
                    num_resolved += 1;
                }
            }

            TypeDeclaration::Sum(ref mut s) => {
                if s.cases.len() > MAX_SUM_CASES {
                    return type_error_result(
                        &s.span,
                        format!("Enum type has too many cases, the maximum is {MAX_SUM_CASES}"),
                    );
                }

                if resolve_sum_case_types(ctx, s, mode)? == TypeResolved::Yes {
                    ctx.add(Symbol::new(&s.name, &s.typ, false, &s.span, SymbolType::Normal))?;
                    match s.typ {
                        Type::Enum(ref et) => {
                            for c in &et.cases {
                                ctx.add(Symbol::new(c, &s.typ, false, &s.span, SymbolType::Normal))?;
                            }
                        }
                        Type::Sum(ref st) => {
                            for c in &st.cases {
                                ctx.add(Symbol::new(&c.name, &s.typ, false, &s.span, SymbolType::Normal))?;
                            }
                        }
                        _ => {}
                    }

                    num_resolved += 1;
                }
            } /*TypeDeclaration::Alias(ref mut _a) => {
                  panic!("NYI");
              }*/
        }
    }

    Ok(num_resolved)
}

pub fn resolve_types(ctx: &mut TypeCheckerContext, module: &mut Module) -> CompileResult<()> {
    let mut num_resolved = 0;
    loop {
        let already_resolved = num_resolved;
        num_resolved = resolve_all_types(ctx, module, ResolveMode::Lazy)?;

        if num_resolved == module.types.len() {
            break;
        } else if already_resolved == num_resolved {
            // We weren't able to resolve any in this pass, so something is missing
            resolve_all_types(ctx, module, ResolveMode::Forced)?;
            break;
        }
    }

    for f in module.functions.values_mut() {
        resolve_function_args_and_ret_type(ctx, &mut f.sig, ResolveMode::Forced)?;
        ctx.add(Symbol::new(
            &f.sig.name,
            &f.sig.typ,
            false,
            &f.sig.span,
            SymbolType::Normal,
        ))?;
    }

    for ext in module.externals.values_mut() {
        match ext {
            External::Function { sig, .. } => {
                resolve_function_args_and_ret_type(ctx, sig, ResolveMode::Forced)?;
                ctx.add(Symbol::new(&sig.name, &sig.typ, false, &sig.span, SymbolType::Normal))?;
            }
            External::Variable {
                typ,
                name,
                mutable,
                span,
                ..
            } => {
                if resolve_type(ctx, typ) == TypeResolved::No {
                    return type_error_result(span, format!("Cannot resolve type {} of external {}", typ, name));
                }
                ctx.add(Symbol::new(name, typ, *mutable, span, SymbolType::External))?;
            }
        }
    }

    Ok(())
}
