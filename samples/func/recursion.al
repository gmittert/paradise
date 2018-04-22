module main
i64 fact(i64 x) {
    i64 ret = 1;
    if (x > 1) {
        ret = x * fact(x-1);
    }
    return ret;
}

i64 main() {
    return fact(5);
}
