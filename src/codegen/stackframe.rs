use llvm::prelude::*;
use codegen::symbols::*;


pub struct StackFrame
{
    function: LLVMValueRef,
    pub symbols: SymbolTable,
}

impl StackFrame
{
    pub fn new(fun: LLVMValueRef) -> StackFrame
    {
        StackFrame{
            function: fun,
            symbols: SymbolTable::new(),
        }
    }

    pub fn get_current_function(&self) -> LLVMValueRef
    {
        self.function
    }
}
