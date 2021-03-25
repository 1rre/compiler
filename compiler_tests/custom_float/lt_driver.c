#include <stdio.h>
int f(float,float);

int main() {
  int a = f(12.,15.);
  int b = f(18.,15.);
  printf("%d\n%d\n",a,b);
    return !(a == 1);
}
