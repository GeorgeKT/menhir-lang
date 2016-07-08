section .text
bits 64

; Compiler will generate this
extern main

global _start, exit, read, write

_start:
    call main
    mov rdi, rax
    call exit
    ret

%define SYSCALL_EXIT 		60
%define SYSCALL_READ 		0
%define SYSCALL_WRITE 		1


exit:
    mov rax, SYSCALL_EXIT
    syscall
    ret

write: 
	mov rax, SYSCALL_WRITE
	syscall 
	ret

read: 
	mov rax, SYSCALL_READ
	syscall
	ret