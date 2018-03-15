module main
i64 main() {
    char[] word = "abcd";
    char[] word2 = "efgh";

    word[0] = 'e';
    word[1] = 'f';
    word[2] = 'g';
    word[3] = 'h';
    u64 x = u64.0;
    while (x < #word) {
        if (word[x] != word2[x]) {
            return 1;
        }
        x = x + u64.1;
    }
    return 3;
}
