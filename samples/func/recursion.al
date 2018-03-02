module main
int fact(int x) {
    if (x <= 0) {
        return 1;
    }
    return x * fact(x-1);
}

int main() {
    printi(345);
    printc('\n');
    int x = fact(1);
    printi(x);
    printc('\n');
    return 0;
}
