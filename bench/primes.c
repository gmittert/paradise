int main() {
  int num_primes = 1000000;
  int primes[1000000];
  int i = 0;
  while (i < num_primes) {
    primes[i] = 1;
    i = i + 1;
  }

  i = 2;
  while (i <= 1000) {
    if (primes[i]) {
      int j = i*2;
      while (j < num_primes) {
        primes[j] = 0;
        j = j + i;
      }
    }
    i = i + 1;
  }

  i = 2;
  int total = 0;
  while (i < num_primes) {
    if (primes[i]) {
      total = total + 1;
    }
    i = i + 1;
  }
  return total;
}
