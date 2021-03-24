#include <string.h>
#include <stdio.h>

int ok;

void fakeputs(char *x)
{
    printf("%s\n",x);
    ok=!strcmp(x,"wibble");
}

int g();

int main()
{
    ok=0;
    g();
    return !(ok==1);
}
