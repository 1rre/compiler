#include <string.h>

int ok;

void fakeputs(char *x)
{
    ok=!strcmp(x,"wibble");
}

int g();

int main()
{
    ok=0;
    g();
    return !(ok==1);
}
