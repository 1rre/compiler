int main() {
  int a = 5;
  int b = 10;
  int c = 15;
  int* y[3] = {&a,&b,&c};
  *y[0] = 20;
  *y[1] = 25;
  *y[2] = 30;
  return **y;
}
