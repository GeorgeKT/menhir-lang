use super::instantiate::instantiate;
use super::typecheck::type_check_function;
use super::typecheckercontext::{ImportSymbolResolver, TypeCheckerContext};
use crate::ast::*;
use crate::compileerror::{unknown_name_result, CompileResult};
use std::collections::HashMap;
use crate::target::Target;

type FunctionMap = HashMap<String, Function>;

fn do_instantiation(
    ctx: &mut TypeCheckerContext,
    new_functions: &mut FunctionMap,
    func: &Function,
    module: &Module,
    call: &Call,
    target: &Target,
) -> CompileResult<()> {
    let name = new_func_name(&func.sig.name, &call.generic_args);
    if !new_functions.contains_key(&name) && !module.functions.contains_key(&name) {
        let mut new_func = instantiate(ctx, func, &call.generic_args)?;
        type_check_function(ctx, &mut new_func, target)?;
        new_functions.insert(name, new_func);
    }

    Ok(())
}

fn resolve_generic_call(
    ctx: &mut TypeCheckerContext,
    new_functions: &mut FunctionMap,
    imports: &ImportMap,
    module: &Module,
    call: &Call,
    target: &Target,
) -> CompileResult<()> {
    if let Some(func) = module.functions.get(&call.callee.name) {
        return do_instantiation(ctx, new_functions, func, module, call, target);
    }

    for import in imports.values() {
        if let Some(func) = import.generics.get(&call.callee.name) {
            let mut ctx = TypeCheckerContext::new(ImportSymbolResolver::ExternalImport(import));
            return do_instantiation(&mut ctx, new_functions, func, module, call, target);
        }
    }

    unknown_name_result(&call.span, format!("Unknown function {}", call.callee.name))
}

fn resolve_generics(
    ctx: &mut TypeCheckerContext,
    new_functions: &mut FunctionMap,
    imports: &ImportMap,
    module: &Module,
    e: &Expression,
    target: &Target,
) -> CompileResult<()> {
    let mut rg = |e: &Expression| {
        if let Expression::Call(ref c) = *e {
            if !c.generic_args.is_empty() {
                resolve_generic_call(ctx, new_functions, imports, module, c, target)?;
            }
        }
        Ok(())
    };
    e.visit(&mut rg)
}

fn replace_generic_calls(e: &mut Expression) -> CompileResult<()> {
    let mut replace_calls = |e: &mut Expression| {
        if let Expression::Call(ref mut call) = *e {
            if !call.generic_args.is_empty() {
                call.callee.name = new_func_name(&call.callee.name, &call.generic_args);
            }
        }
        Ok(())
    };

    e.visit_mut(&mut replace_calls)
}

/*
    Instantiate all generics
*/
pub fn instantiate_generics(
    module: &mut Module,
    ctx: &mut TypeCheckerContext,
    imports: &ImportMap,
    target: &Target,
) -> CompileResult<()> {
    let mut new_functions = FunctionMap::new();
    for f in module.functions.values() {
        if !f.generics_resolved && !f.is_generic() {
            resolve_generics(ctx, &mut new_functions, imports, module, &f.expression, target)?;
        }
    }

    for f in module.functions.values_mut() {
        if !f.generics_resolved && !f.is_generic() {
            replace_generic_calls(&mut f.expression)?;
            f.generics_resolved = true;
        }
    }

    module.functions.extend(new_functions.into_iter());
    Ok(())
}
