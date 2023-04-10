use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::ptr;

use crate::ast::IntSize;
use crate::target::Target;
use llvm::core::*;
use llvm::prelude::*;
use llvm::target::*;
use llvm::target_machine::*;

unsafe fn create_target_machine() -> Result<(String, LLVMTargetMachineRef), String> {
    let target_triple = LLVMGetDefaultTargetTriple();
    let target_triple_str = CStr::from_ptr(target_triple)
        .to_str()
        .expect("Invalid target triple")
        .to_owned();

    let mut target: LLVMTargetRef = ptr::null_mut();
    let mut error_message: *mut c_char = ptr::null_mut();
    if LLVMGetTargetFromTriple(target_triple, &mut target, &mut error_message) != 0 {
        let msg = CStr::from_ptr(error_message)
            .to_str()
            .expect("Invalid C string");
        let e = format!(
            "Unable to get an LLVM target reference for {}: {}",
            target_triple_str, msg
        );
        LLVMDisposeMessage(error_message);
        LLVMDisposeMessage(target_triple);
        return Err(e);
    }

    let target_machine = LLVMCreateTargetMachine(
        target,
        target_triple,
        cstr!(""),
        cstr!(""),
        LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
        LLVMRelocMode::LLVMRelocPIC,
        LLVMCodeModel::LLVMCodeModelDefault,
    );

    LLVMDisposeMessage(target_triple);
    if target_machine.is_null() {
        let e = format!("Unable to get a LLVM target machine for {}", target_triple_str);
        return Err(e);
    }

    Ok((target_triple_str, target_machine))
}

pub struct TargetMachine {
    pub target_machine: LLVMTargetMachineRef,
    pub target_data: LLVMTargetDataRef,
    pub target: Target,
}

impl TargetMachine {
    pub unsafe fn new() -> Result<TargetMachine, String> {
        let (target_triplet, target_machine) = create_target_machine()?;
        let target_data = LLVMCreateTargetDataLayout(target_machine);
        let int_size = match LLVMPointerSize(target_data) {
            1 => IntSize::I8,
            2 => IntSize::I16,
            4 => IntSize::I32,
            8 => IntSize::I64,
            v => panic!("Not supported native integer size: {}", v),
        };

        Ok(TargetMachine {
            target_machine,
            target_data,
            target: Target::new(int_size, target_triplet),
        })
    }

    pub unsafe fn size_of_type(&self, typ: LLVMTypeRef) -> usize {
        LLVMStoreSizeOfType(self.target_data, typ) as usize
    }

    pub unsafe fn emit_to_file(&self, module: LLVMModuleRef, obj_file_name: &str) -> Result<(), String> {
        let mut error_message: *mut c_char = ptr::null_mut();
        let obj_file_name = CString::new(obj_file_name).expect("Invalid String");
        if LLVMTargetMachineEmitToFile(
            self.target_machine,
            module,
            obj_file_name.into_raw(),
            LLVMCodeGenFileType::LLVMObjectFile,
            &mut error_message,
        ) != 0
        {
            let msg = CStr::from_ptr(error_message)
                .to_str()
                .expect("Invalid C string");
            let e = format!("Unable to create object file: {}", msg);
            LLVMDisposeMessage(error_message);
            return Err(e);
        }
        Ok(())
    }
}

impl Drop for TargetMachine {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeTargetMachine(self.target_machine);
            LLVMDisposeTargetData(self.target_data);
        }
    }
}
