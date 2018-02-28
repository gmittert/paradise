module main

int main() {
    char word[13] = {'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\n'};
    int x = 0;
    while (x < 13) {
        printc(word[x]);
        x = x + 1;
    }

    return 0;
}
