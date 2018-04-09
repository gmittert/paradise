module io

i64 print(char[] c) {
    u64 x = 0:u64;
    while (x != #c) {
           printc(c[x]);
           x = x + 1:u64;
    }
    return 0;
}

C i64 printc(char c) {
`
  void stdlib_io_printc(char c) {
      putc(c, stdout);
  }
`
}

C i64 printi(i64 i) {
`
void stdlib_io_printi(long i) {
    printf("%ld", i);
}
`
}
