#include <stdio.h>

void swap_str(char **a, char **b)
{
    char *tmp = *a;
    *a = *b;
    *b = tmp;
}

int main(void)
{
    char *str_a = "ABC";
    char *str_b = "123";
    swap_str(&str_a, &str_b);
    printf("str_a=%s, str_b=%s\n", str_a, str_b);
    return 0;
}
