module main

i64 main() {
    char[] word = {'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\n'};
    i64 x = 0;
    while (x < 13) {
        printc(word[x]);
        x = x + 1;
    }

    return 0;
}
