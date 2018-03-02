module main
int fact(int x) {
    if (x <= 0) {
        return 1;
    }
    return x * fact(x-1);
}

int main() {
    return fact(5);
}
