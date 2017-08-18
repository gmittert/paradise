# Jcc

A simple compiler for C. Compiles to AT&T x86-64 which is assembled by GAS.

Todo:

- [x] Integers
- [ ] Strings/Printing
- [x] Scope/blocks
- [x] If statements
- [x] While Loops
- [ ] For Loops
- [ ] Functions
- [x] Type checking
- [ ] Any sort of optimization

# Building

Requires Stack

```
stack install
stack test
```

# Running
```
# Compile
jcc myfile.c > myfile.S
# Assemble
gcc myfile.S -o myfile
./myfile
```
