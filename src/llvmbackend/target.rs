use std::ptr;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;

use llvm::prelude::*;
use llvm::core::*;
use llvm::target_machine::*;
use llvm::target::*;

pub struct TargetMachine
{
    pub target_machine: LLVMTargetMachineRef,
    pub target_data: LLVMTargetDataRef,
}

impl TargetMachine
{
    pub unsafe fn new() -> Result<TargetMachine, String>
    {
        let target_triple = CStr::from_ptr(LLVMGetDefaultTargetTriple());
        let target_triple_str = target_triple.to_str().expect("Invalid target triple");
        println!("Compiling for {}", target_triple_str);

        let mut target: LLVMTargetRef = ptr::null_mut();
        let mut error_message: *mut c_char = ptr::null_mut();
        if LLVMGetTargetFromTriple(target_triple.as_ptr(), &mut target, &mut error_message) != 0 {
            let msg = CStr::from_ptr(error_message).to_str().expect("Invalid C string");
            let e = format!("Unable to get an LLVM target reference for {}: {}", target_triple_str, msg);
            LLVMDisposeMessage(error_message);
            return Err(e);
        }

        let target_machine = LLVMCreateTargetMachine(
            target,
            target_triple.as_ptr(),
            cstr!(""),
            cstr!(""),
            LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            LLVMRelocMode::LLVMRelocPIC,
            LLVMCodeModel::LLVMCodeModelDefault,
        );
        if target_machine == ptr::null_mut() {
            let e = format!("Unable to get a LLVM target machine for {}", target_triple_str);
            return Err(e);
        }

        Ok(TargetMachine{
            target_machine: target_machine,
            target_data: LLVMGetTargetMachineData(target_machine),
        })
    }

    pub unsafe fn size_of_type(&self, typ: LLVMTypeRef) -> usize
    {
        LLVMStoreSizeOfType(self.target_data, typ) as usize
    }

    // Get the native integer size in bytes
    pub unsafe fn native_int_size(&self) -> usize
    {
        LLVMPointerSize(self.target_data) as usize
    }

    pub unsafe fn emit_to_file(&self, module: LLVMModuleRef, obj_file_name: &str) -> Result<(), String>
    {
        let mut error_message: *mut c_char = ptr::null_mut();
        let obj_file_name = CString::new(obj_file_name).expect("Invalid String");
        if LLVMTargetMachineEmitToFile(self.target_machine, module, obj_file_name.into_raw(), LLVMCodeGenFileType::LLVMObjectFile, &mut error_message) != 0 {
            let msg = CStr::from_ptr(error_message).to_str().expect("Invalid C string");
            let e = format!("Unable to create object file: {}", msg);
            LLVMDisposeMessage(error_message);
            return Err(e);
        }
        Ok(())
    }
}

impl Drop for TargetMachine
{
    fn drop(&mut self)
    {
        unsafe {
            LLVMDisposeTargetMachine(self.target_machine);
        }
    }
}
