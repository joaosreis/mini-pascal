.text
	.globl	main
main:
	subq $16, %rsp
	leaq 0(%rsp), %rbp
	movq $32, %rdi
	pushq %rdi
	movq $1, %rdi
	popq %rsi
	imulq %rsi, %rdi
	pushq %rdi
	movq $2, %rdi
	popq %rsi
	subq %rsi, %rdi
	pushq %rdi
	movq $5, %rdi
	popq %rsi
	imulq %rsi, %rdi
	pushq %rdi
	movq $1, %rdi
	popq %rsi
	addq %rsi, %rdi
	call print_int
	addq $16, %rsp
	movq $0, %rax
	ret
print_int:
	movq %rdi, %rsi
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	ret
print_float:
	movq %rdi, %rsi
	movq $.Sprint_float, %rdi
	movq $1, %rax
	call printf
	ret
	
.data
.Sprint_int:
	.string "%d\n"
.Sprint_float:
	.string "%f\n"
