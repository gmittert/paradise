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
