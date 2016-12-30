# Jlang

A simple compiler for a simple language. Compiles to AT&T x86-64 which is assembled by GAS.

Todo:

- [x] Integers
- [x] Strings/Printing
- [ ] Scope/blocks
- [ ] If statements
- [ ] Loops
- [ ] Functions
- [ ] Type checking
- [ ] Any sort of optimization


# Examples

Source code: hello\_world.jl
```
int main() {
    string s = "Hello World\n";
    print(s);
    return 0;
}
```
Compile:
```
$ jlc hello_world.jl
```
Run!
```
$ ./hello_world.jl.elf
Hello World!
$
```

