module main

i64 main() {
    char[] word = "Hello World!\n";
    i64 x = 0;
    while (x < 13) {
        printc(word[x]);
        x = x + 1;
    }

    return 0;
}
