#include <stdio.h>
int f(float,float);

int main() {
  int a = f(12.,15.);
  int b = f(12.,12.);
  printf("%d\n%d\n",a,b);
    return !(b == 1);
}
