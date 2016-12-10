use ast::*;
use typechecker::TypeCheckerContext;
use compileerror::{CompileResult, unknown_name};

#[derive(Eq, PartialEq, Debug)]
enum TypeResolved
{
    Yes,
    No,
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum ResolveMode
{
    Lazy,
    Forced,
}

fn resolve_type(ctx: &TypeCheckerContext, typ: &mut Type) -> TypeResolved
{
    let resolved = if let Type::Unresolved(ref ut) = *typ {
        if let Some(r) = ctx.resolve_type(&ut.name) {
            r
        } else {
            return TypeResolved::No;
        }
    } else {
        return TypeResolved::Yes;
    };

    *typ = resolved.typ;
    TypeResolved::Yes
}

fn resolve_function_args_and_ret_type(ctx: &mut TypeCheckerContext, sig: &mut FunctionSignature) -> CompileResult<()>
{
    if sig.typ != Type::Unknown {
        return Ok(());
    }

    if resolve_type(ctx, &mut sig.return_type) == TypeResolved::No {
        return Err(unknown_name(&sig.span, &format!("{}", sig.return_type)));
    }

    let mut args = Vec::with_capacity(sig.args.len());
    for ref mut arg in &mut sig.args {
        if resolve_type(ctx, &mut arg.typ) == TypeResolved::No {
            return Err(unknown_name(&arg.span, &format!("{}", arg.typ)));
        }

        args.push(arg.typ.clone());
    }

    sig.typ = func_type(args, sig.return_type.clone());
    Ok(())
}

fn resolve_struct_member_types(ctx: &mut TypeCheckerContext, sd: &mut StructDeclaration, mode: ResolveMode) -> CompileResult<TypeResolved>
{
    if sd.typ != Type::Unknown {
        return Ok(TypeResolved::Yes);
    }

    let mut member_types = Vec::with_capacity(sd.members.len());
    for m in &mut sd.members
    {
        if resolve_type(ctx, &mut m.typ) == TypeResolved::No {
            if mode == ResolveMode::Lazy {
                return Ok(TypeResolved::No);
            } else {
                return Err(unknown_name(&m.span, &format!("{}", m.typ)));
            }
        }

        member_types.push(struct_member(&m.name, m.typ.clone()));
    }

    sd.typ = struct_type(&sd.name, member_types);
    Ok(TypeResolved::Yes)
}

fn resolve_sum_case_types(ctx: &mut TypeCheckerContext, st: &mut SumTypeDeclaration, mode: ResolveMode) -> CompileResult<TypeResolved>
{
    if st.typ != Type::Unknown {
        return Ok(TypeResolved::Yes);
    }

    let mut case_types = Vec::with_capacity(st.cases.len());
    for c in st.cases.iter_mut()
    {
        if let Some(ref mut sd) = c.data
        {
            if resolve_struct_member_types(ctx, sd, mode)? == TypeResolved::No
            {
                return Ok(TypeResolved::No);
            }
            else
            {
                case_types.push(sum_type_case(&c.name, sd.typ.clone()));
            }
        }
        else
        {
            case_types.push(sum_type_case(&c.name, Type::Int)); // Use integer type for cases without structs
        }
    }

    if case_types.iter().all(|ct| ct.typ == Type::Int)
    {
        let case_names: Vec<String> = st.cases.iter().map(|c| c.name.clone()).collect();
        st.typ = enum_type(&st.name, case_names);
    }
    else
    {
        st.typ = sum_type(&st.name, case_types);
    }

    Ok(TypeResolved::Yes)
}

fn resolve_all_types(ctx: &mut TypeCheckerContext, module: &mut Module, mode: ResolveMode) -> CompileResult<usize>
{
    let mut num_resolved = 0;
    for typ in module.types.values_mut()
    {
        match *typ
        {
            TypeDeclaration::Struct(ref mut s) => {
                if resolve_struct_member_types(ctx, s, mode)? == TypeResolved::Yes
                {
                    ctx.add(&s.name, s.typ.clone(), &s.span)?;
                    num_resolved += 1;
                }
            },
            TypeDeclaration::Sum(ref mut s) => {
                if resolve_sum_case_types(ctx, s, mode)? == TypeResolved::Yes
                {
                    ctx.add(&s.name, s.typ.clone(), &s.span)?;
                    match s.typ
                    {
                        Type::Enum(ref et) => {
                            for c in et.cases.iter()
                            {
                                ctx.add(c, s.typ.clone(), &s.span)?;
                            }
                        },
                        Type::Sum(ref st) => {
                            for c in st.cases.iter()
                            {
                                ctx.add(&c.name, s.typ.clone(), &s.span)?;
                            }
                        },
                        _ => {},
                    }

                    num_resolved += 1;
                }
            },
            TypeDeclaration::Alias(ref mut _a) => {
                panic!("NYI");
            }
        }
    }

    Ok(num_resolved)
}

pub fn resolve_types(ctx: &mut TypeCheckerContext, module: &mut Module) -> CompileResult<()>
{
    let mut num_resolved = 0;
    loop
    {
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


    for ref mut f in module.functions.values_mut() {
        resolve_function_args_and_ret_type(ctx, &mut f.sig)?;
        ctx.add(&f.sig.name, f.sig.typ.clone(), &f.sig.span)?;
    }

    for ref mut f in module.externals.values_mut() {
        resolve_function_args_and_ret_type(ctx, &mut f.sig)?;
        ctx.add(&f.sig.name, f.sig.typ.clone(), &f.sig.span)?;
    }

    Ok(())
}
