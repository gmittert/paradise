.section .text

# Allocates eax bytes of memory. Pointer to allocated memory returned in eax.
.global __alloc
__alloc:
  push %rax
# sys_brk system call
  movq $12, %rax
# 0 bytes - query current brk
  movq $0, %rdi
  syscall

  pop %rdi
# move brk ahead by number of bytes requested
  add %rax, %rdi
# sys_brk system call
  movq $12, %rax
  syscall
  cmp $0, %rax
  jne ok
  movq $1, %rax
  call __exit_fail
ok:
  ret

# Debugging exit: ends the process, returning the value of
# rax as the exit code.
.global __exit_fail
__exit_fail:
  mov %rax, %rbx
	# sys_exit system call
  movq $60, %rax
  syscall

.section .data
