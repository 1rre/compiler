#include <stdio.h>
int f();

int main()
{
  printf("%d\n",f());
    return !(f()==5678);
}
