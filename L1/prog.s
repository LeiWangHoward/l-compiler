	.text
	.globl go
	.type  go, @function
go:
movl $5, %esi
movl $5, %edi
cmpl %edi, %esi
setle %dl
movzbl %dl, %edx
movl $15, %edi
movl $27, %ebx
jmp _label1
_label1:
pushl $_newlab16
pushl %ebp
movl %esp, %ebp
jmp _funfun
_newlab16:
jmp _label3
_label2:
pushl $_newlab17
pushl %ebp
movl %esp, %ebp
jmp _funfun2
_newlab17:
_label3:
pushl %ebx
call print
addl $4, %esp
ret
_funfun:
movl $11, %ebx
movl %ebp, %esp
popl %ebp
ret
_funfun2:
addl %edi, %ebx
movl %ebp, %esp
popl %ebp
ret
	.size  go, .-go
	.section	.note.GNU-stack,"",@progbits
