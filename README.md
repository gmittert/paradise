# Paradise

A programming language with compiles to C with OpenCL

# Sample programs

Multiplying and summing two arrays
```
module main
i64 sumSqrs() {
    i64[] data1 = {1,2,3,4,5,6,7,8,9,10};

    i64[] sqrs = {0,0,0,0,0,0,0,0,0,0};
    [|sqrs = data1 .* data1 |];

    i64 sum = 0;
    for i  in s um {
        sum = sum +  i;
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
# Building

Requires Stack, clang, clang-format, and OpenCL

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
