# Paradise

A programming language with compiles to LLVM with OpenCL

# Sample programs

Squaring and summing an array:
```
module main
I64 sumSqrs() {
    let data1 = [1,2,3,4,5,6,7,8,9,10];
    let sqrs = [0,0,0,0,0,0,0,0,0,0];
    [|sqrs = data1 .* data1 |];

    let sum = 0;
    for i in sqrs {
        sum = sum + i;
    }
    return sum;
}
```
This creates the OpenCL Kernel
```
__kernel void MYPROG (__global long* t2_sqrs,__global long* t0_data1,__global long* t1_data2) { 
    const int i = get_global_id(0);
    t2_sqrs[i] = t0_data1[i] * t1_data2[i];
}
```
which is used to execute the array multiplication.

Check out the `samples` directory for even more samples.
# Building

Requires Stack, llvm, and OpenCL

```
stack install
stack test
```

# Running
```
# Compile
parac myfile.para
# Run
./a.out
```

# Editor Support
If you use Emacs, you can add 
```
(setq para-highlights
        '(("\\<module\\>\\|\\<return\\>\\|\\<while\\>\\|\\<if\\>\\|\\<for\\>\\|\\<in\\>\\|\\<case\\>\\|\\<of\\>" . font-lock-keyword-face)
          ("\\/\\/.*" . font-lock-comment-face)
          ("\\<[A-Z][_A-Za-z0-9]*\\>" . font-lock-type-face)
          ("\"[^\"]*\"" . font-lock-type-face)
          ))
	(define-derived-mode para-mode fundamental-mode "para"
    "major mode for editing para"
    (setq font-lock-defaults '(para-highlights))
		)
```
to your .emacs to get basic syntax highlighting.
