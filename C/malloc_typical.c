#include <stdio.h>
#include <stdlib.h>

// 典型的なmalloc処理
int main(void)
{
    char *p = NULL;
    
    p = malloc(256);
    if (p == NULL) {
        fprintf(stderr, "メモリ不足\n");
    }

    strcpy(p, "ABCDEFG");

    free(p);
}
