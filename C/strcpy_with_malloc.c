#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    char *p;
    char *str = "ABC";
    p = malloc(strlen(str) + 1);
    strcpy(p, str);
    printf("p=%s\n", p);
}
