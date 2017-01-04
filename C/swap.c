#include <stdio.h>

void swap(int *a, int *b)
{
    int tmp = *a;
    *a = *b;
    *b = tmp;
}

int main()
{
    int a = 1;
    int b = 2;
    swap(&a, &b);
    printf("a=%d, b=%d\n", a, b);
}
