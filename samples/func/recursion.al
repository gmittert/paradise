module main
int fact(int x) {
    if (x <= 0) {
        return 1;
    }
    return x * fact(x-1);
}

int main() {
    int x = fact(5);
    printc(x + 65);
    return 0;
}
