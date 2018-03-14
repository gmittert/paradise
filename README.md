# Jcc

A simple compiler for C. Compiles to AT&T x86-64 which is assembled/linked by GAS/LD.

# Sample programs

Factorial
```
module main
i64 fact(i64 x) {
    if (x <= 0) {
        return 1;
    }
    return x * fact(x-1);
}

i64 main() {
    return fact(5);
}
```


# Building

Requires Stack

```
stack install
stack test
```

# Running
```
# Compile
jcc myfile.al
# Run
./a.out
```
