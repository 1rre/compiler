void f(int*);

int main() {
  int x = 76;
  f((&x)-1);
  return !(x == 15);
}

