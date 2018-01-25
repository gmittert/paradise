module main
int main() {
  int arr[4] = {0,0,0,0};
  arr[0] = 1;
  arr[1] = 2;
  arr[2] = 3;
  arr[3] = 4;
  int w = arr[0];
  int x = arr[1];
  int y = arr[2];
  int z = arr[3];
  return ((z - y) + x) * w;
}
