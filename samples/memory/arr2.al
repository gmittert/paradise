module main
i64 main() {
    u64[] arr = {u64.0, u64.1, u64.2, u64.3, u64.4, u64.5, u64.6, u64.7, u64.8, u64.9};
    u64 x = u64.0;
    while (x < #arr) {
        if (arr[x] != x) {
            return 1;
        }
        x = x + u64.1;
    }
    return 3;
}
