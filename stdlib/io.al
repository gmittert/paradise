module io

int print(char[] c) {
    int x = 0;
    while (x != #c) {
           printc(c[x]);
    }
}

asm int printc(char c) {
`
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
`
}

int printi(int i) {
    if (i == 0) {
        printc('0');
        return 0;
    }
    if (i == 1) {
        printc('1');
        return 0;
    }
    if (i == 2) {
        printc('2');
        return 0;
    }
    if (i == 3) {
        printc('3');
        return 0;
    }
    if (i == 4) {
        printc('4');
        return 0;
    }
    if (i == 5) {
        printc('5');
        return 0;
    }
    if (i == 6) {
        printc('6');
        return 0;
    }
    if (i == 7) {
        printc('7');
        return 0;
    }
    if (i == 8) {
        printc('8');
        return 0;
    }
    if (i == 9) {
        printc('9');
        return 0;
    }

    printi(i/10);
    printi(i - ((i/10) * 10));
    return 0;
}
