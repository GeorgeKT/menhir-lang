use std::ptr;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;

use llvm::prelude::*;
use llvm::core::*;
use llvm::target_machine::*;
use llvm::target::*;

use compileerror::{CompileResult, ErrorCode, err};
use span::Span;

pub struct TargetMachine
{
    pub target_machine: LLVMTargetMachineRef,
    pub target_data: LLVMTargetDataRef,
}

impl TargetMachine
{
    pub unsafe fn new() -> CompileResult<TargetMachine>
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
            return err(&Span::default(), ErrorCode::CodegenError, e);
        }

        let target_machine = LLVMCreateTargetMachine(
            target,
            target_triple.as_ptr(),
            cstr!(""),
            cstr!(""),
            LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            LLVMRelocMode::LLVMRelocDefault,
            LLVMCodeModel::LLVMCodeModelDefault,
        );
        if target_machine == ptr::null_mut() {
            let e = format!("Unable to get a LLVM target machine for {}", target_triple_str);
            return err(&Span::default(), ErrorCode::CodegenError, e);
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

    pub unsafe fn emit_to_file(&self, module: LLVMModuleRef, obj_file_name: &str) -> CompileResult<()>
    {
        let mut error_message: *mut c_char = ptr::null_mut();
        let obj_file_name = CString::new(obj_file_name).expect("Invalid String");
        if LLVMTargetMachineEmitToFile(self.target_machine, module, obj_file_name.into_raw(), LLVMCodeGenFileType::LLVMObjectFile, &mut error_message) != 0 {
            let msg = CStr::from_ptr(error_message).to_str().expect("Invalid C string");
            let e = format!("Unable to create object file: {}", msg);
            LLVMDisposeMessage(error_message);
            return err(&Span::default(), ErrorCode::CodegenError, e);
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
