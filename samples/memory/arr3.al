module main
i64 main() {
    char[] word = "abcd";
    char[] word2 = "efgh";

    word[0] = 'e';
    word[1] = 'f';
    word[2] = 'g';
    word[3] = 'h';
    u64 x = 0:u64;
    i64 ret = 3;
    while (x < #word) {
        if (word[x] != word2[x]) {
            ret =1;
        }
        x = x + 1:u64;
    }
    return ret;
}
