#include <stdio.h>

void func(int a)
{
    int local[10];
}

int main()
{
    printf("func=%p\n", func);
    printf("main=%p\n", main);

    int func_size = ((int)main) - ((int)func);
    printf("func_size=%dbyte\n", func_size);

    return 0;
}
