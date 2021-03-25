#include<stdio.h>
float f(float x, int n);

int main()
{
  printf("%f\n",f(5.0f,3));
    return !(f(5.0f,3)==125.0f);
}
