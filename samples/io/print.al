module main

int main() {
    int word[13] = {72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33, 10};
    int x = 0;
    while (x < 13) {
        printc(word[x]);
        x = x + 1;
    }

    return 0;
}
