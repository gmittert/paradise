.section .text

# Allocates rax bytes of memory. Pointer to allocated memory returned in rax.
.global __alloc
__alloc:
  push %rax
  # First, we need to get the current system break. To do this, we call
  # sys_brk with 0

# sys_brk system call
  movq $12, %rax
# 0 bytes - query current brk
  movq $0, %rdi
  syscall
# Program break is now %rax
  popq %rdi
  pushq %rax
# move brk ahead by number of bytes requested
  addq %rax, %rdi
# sys_brk system call
  movq $12, %rax
  syscall
  cmp $0, %rax
  jne ok
  movq $13, %rax
  call __exit_fail
ok:
  # Put the old program break into the return register
  pop %rax
  ret

# Debugging exit: ends the process, returning the value of
# rax as the exit code.
.global __exit_fail
__exit_fail:
  mov %rax, %rdi
	# sys_exit system call
  movq $60, %rax
  syscall

.section .data
