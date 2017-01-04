#include <stdio.h>

struct point {
    int x;
    int y;
};

typedef struct {
    int x;
    int y;
} Point;

int main(void)
{
    struct point pt;
    pt.x = 1;
    pt.y = 2;
    printf("(%d, %d)\n", pt.x, pt.y);

    Point p;
    p.x = 10;
    p.y = 20;
    printf("(%d, %d)\n", p.x, p.y);

    return 0;
}
