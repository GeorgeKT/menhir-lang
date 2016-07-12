use llvm::prelude::*;
use llvm::core::*;

use codegen::symbols::*;


pub struct StackFrame
{
    function: LLVMValueRef,
    pub symbols: SymbolTable,
    current_bb: LLVMBasicBlockRef,
}

impl StackFrame
{
    pub fn new(fun: LLVMValueRef, bb: LLVMBasicBlockRef) -> StackFrame
    {
        StackFrame{
            function: fun,
            symbols: SymbolTable::new(),
            current_bb: bb,
        }
    }

    pub unsafe fn return_type(&self) -> LLVMTypeRef
    {
        LLVMGetReturnType(LLVMGetElementType(LLVMTypeOf(self.function)))
    }

    pub fn set_current_bb(&mut self, bb: LLVMBasicBlockRef)
    {
        self.current_bb = bb;
    }

    pub fn get_current_bb(&self) -> LLVMBasicBlockRef
    {
        self.current_bb
    }

    pub fn get_current_function(&self) -> LLVMValueRef
    {
        self.function
    }
}
