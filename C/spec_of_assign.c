#include <stdio.h>

// C言語では代入演算の結果として代入結果が返される
int main(void)
{
    int i;
    printf("%d\n", i = 1); // 1
    printf("%d\n", i = 2); // 2
    return 0;
}
