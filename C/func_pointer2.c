#include <stdio.h>

void func(int a)
{
    printf("func called %d\n", a);
}

int main()
{
    void (*func_p)(int);
    func_p = func;
    printf("func=%p\n", func);
    printf("func_p=%p\n", func_p);
    return 0;
}
