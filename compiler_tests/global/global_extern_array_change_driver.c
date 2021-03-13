extern int array[5];

int main() {
  array[2] = 39;
  return !(array[2] == 39 && array[1] == 2);
}
