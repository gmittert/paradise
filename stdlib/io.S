stdlib_io_printc:
# Our character is in RDI
  push %rdi
# Call sys_write
  movq $1, %rax
# stdout
  movq $1, %rdi
# The address of the character to print
  movq %rsp, %rsi
# number of characters to print
  movq $1, %rdx
  syscall
  addq $8, %rsp
  ret
