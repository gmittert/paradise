module main
i64 main() {
    char[] word = "abcd";
    char[] word2 = "efgh";

    word[0] = 'e';
    word[1] = 'f';
    word[2] = 'g';
    word[3] = 'h';
    i64 x = 0;
    while (x < 4) {
        if (word[x] != word2[x]) {
            return 1;
        }
        x = x + 1;
    }
    return 3;
}
