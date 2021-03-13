#include<stdio.h>
extern int array[3][3];

int main() {
  //printf("0/0: %d\t0/1: %d\t0/2: %d\n1/0: %d\t1/1: %d\t1/2: %d\n2/0: %d\t2/1: %d\t2/2: %d\n",array[0][0],array[0][1],array[0][2],array[1][0],array[1][1],array[1][2],array[2][0],array[2][1],array[2][2]);
  return !(array[1][1] == 5);
}
