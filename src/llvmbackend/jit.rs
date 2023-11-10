use libc;
use llvm_sys::orc::{
    LLVMOrcAddEagerlyCompiledIR, LLVMOrcCreateInstance, LLVMOrcDisposeInstance, LLVMOrcGetSymbolAddress,
    LLVMOrcJITStackRef,
};
use llvm_sys::prelude::LLVMModuleRef;
use llvm_sys::support::LLVMSearchForAddressOfSymbol;
use llvmbackend::target::TargetMachine;
use std::ffi::CStr;
use std::mem;
use std::ops::Drop;
use std::ptr;

extern "C" fn resolve_symbol(name: *const libc::c_char, jit_stack: *mut libc::c_void) -> u64 {
    unsafe {
        let js = jit_stack as LLVMOrcJITStackRef;
        let symbol = LLVMOrcGetSymbolAddress(js, name);
        if symbol != 0 {
            return symbol;
        }

        let addr = LLVMSearchForAddressOfSymbol(name);
        if addr != ptr::null_mut() {
            return addr as u64;
        }

        match CStr::from_ptr(name).to_str().unwrap() {
            "memcpy" => libc::memcpy as u64,
            _ => 0,
        }
    }
}

pub struct JIT {
    jit_stack: LLVMOrcJITStackRef,
    target_machine: TargetMachine,
}

impl JIT {
    pub unsafe fn new() -> Result<JIT, String> {
        let target_machine = TargetMachine::new()?;
        let jit_stack = LLVMOrcCreateInstance(target_machine.target_machine);
        if jit_stack == ptr::null_mut() {
            return Err(format!("Failed to create ORC JIT instance"));
        }

        Ok(JIT {
            jit_stack,
            target_machine,
        })
    }

    pub unsafe fn run(&self, module: LLVMModuleRef) -> Result<i64, String> {
        LLVMOrcAddEagerlyCompiledIR(
            self.jit_stack,
            module,
            resolve_symbol,
            self.jit_stack as *mut libc::c_void,
        );
        let main_func = LLVMOrcGetSymbolAddress(self.jit_stack, cstr!("main"));
        println!("main_func {:x}", main_func);
        if main_func == 0 {
            return Err(format!("Cannot execute module, no main function found"));
        }

        let func: extern "C" fn() -> i64 = mem::transmute(main_func);
        let ret = func();
        Ok(ret)
    }
}

impl Drop for JIT {
    fn drop(&mut self) {
        unsafe {
            LLVMOrcDisposeInstance(self.jit_stack);
        }
    }
}
