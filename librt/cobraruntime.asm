section .text
bits 64

; Compiler will generate this
extern main

global _start, exit

_start:
    call main
    mov rdi, rax
    call exit
    ret

%define SYSCALL_EXIT 60

exit:
    ; rdi already contains the exit code
    mov rax, SYSCALL_EXIT
    syscall
    ret
