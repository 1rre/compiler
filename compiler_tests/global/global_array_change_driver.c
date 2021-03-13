extern int array[5];

int f(int);

int main() {
  array[2] = 39;
  return !(f(2) == 39);
}
