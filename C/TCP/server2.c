#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h>
#include <arpa/inet.h>

int
main(void)
{
    int sock0;
    struct sockaddr_in serv;
    struct sockaddr_in client;
    socklen_t len;
    int sock1;
    char serv_name[16];
    int n;

    sock0 = socket(AF_INET, SOCK_STREAM, 0);

    n = listen(sock0, 5);
    if (n != 0) {
        perror("listen");
    }

    memset(&serv, 0, sizeof(serv));
    len = sizeof(serv);

    n = getsockname(sock0, (struct sockaddr *)&serv, &len);
    if (n != 0) {
        perror("listen");
    }

    memset(serv_name, 0, sizeof(serv_name));
    inet_ntop(serv.sin_family, &serv.sin_addr, serv_name, sizeof(serv_name));
    printf("ipaddr=%s, port=%d\n", serv_name, ntohs(serv.sin_port));

    len = sizeof(client);
    sock1 = accept(sock0, (struct sockaddr *)&client, &len);

    write(sock1, "HELLO", 6);

    close(sock1);

    close(sock0);

    return 0;
}