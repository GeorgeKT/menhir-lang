section .text
bits 64

; Compiler will generate this
extern main

%define SYSCALL_EXIT 60

global _start, sys_exit
_start:
    call main
    mov rdi, rax
    call sys_exit
    ret

sys_exit:
    ; rdi already contains the exit code
    mov rax, SYSCALL_EXIT
    syscall
    ret
