#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

void onsig(int sig)
{
    printf("\nContrl+C (%d)\n", sig);
    exit(0);
}

int main(void)
{
    signal(SIGINT, onsig);
    while (1) {
        ;
    }
    return 0;
}


