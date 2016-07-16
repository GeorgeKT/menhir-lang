use std::rc::Rc;
use llvm::core::*;
use llvm::prelude::*;
use ast::*;
use compileerror::Span;
use codegen::{type_name, cstr};
use codegen::context::Context;
use codegen::symbols::{StructType, StructMemberVar};

pub unsafe fn get_slice_type_name(element_type: LLVMTypeRef) -> String
{
    format!("slice@{}", type_name(element_type))
}

pub unsafe fn get_slice_type(ctx: &mut Context, typ: &Type) -> Option<Rc<StructType>>
{
    if let Some(element_type) = ctx.resolve_type(typ)
    {
        let name = get_slice_type_name(element_type);
        if let Some(st) = ctx.get_complex_type(&name) {
            return Some(st);
        }

        let st = LLVMStructCreateNamed(ctx.context, cstr(&name));
        // A slice is a struct containing a length and a pointer
        let mut element_types = vec![LLVMInt64TypeInContext(ctx.context), LLVMPointerType(element_type, 0)];
        LLVMStructSetBody(st, element_types.as_mut_ptr(), 2, 0);

        let t = Rc::new(StructType{
            name: name.clone(),
            typ: st,
            members: vec![
                Rc::new(StructMemberVar{
                    name: "len".into(),
                    typ: Type::Primitive("int".into()),
                    llvm_typ: LLVMPointerType(element_type, 0),
                    constant: true,
                    public: true,
                    init: Expression::IntLiteral(Span::zero(), 0),
                })
            ],
            public: true,
        });

        ctx.add_builtin_type(t.clone());
        Some(t)
    }
    else
    {
        None
    }
}
