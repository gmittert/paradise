module main
i64 main() {
    i64[] arr = {0,1,2,3,4,5,6,7,8,9};
    i64 x = 0;
    while (x < 10) {
        if (arr[x] != x) {
            return 1;
        }
        x = x + 1;
    }
    return 3;
}
