#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

int
main(void)
{
    int sock0;
    struct sockaddr_in addr;
    struct sockaddr_in client;
    socklen_t len;
    int sock1, sock2;

    // ソケットの作成
    sock0 = socket(AF_INET, SOCK_STREAM, 0);

    /*** ソケットの設定 ***/

    // IPv4を利用
    addr.sin_family = AF_INET;

    // ポート番号11111
    addr.sin_port = htons(11111);

    // 任意のインターフェースを指定
    addr.sin_addr.s_addr = INADDR_ANY;

    // 構造体のサイズを指定（Linuxでは不要）
    addr.sin_len = sizeof(addr);

    // ソケットをインターフェースとポートに関連付け
    bind(sock0, (struct sockaddr *)&addr, sizeof(addr));

    // TCPクライアントからの接続要求を待てる状態にする
    listen(sock0, 5);

    // TCPクライアントからの接続要求を受け付ける（1回目）
    len = sizeof(client);
    sock1 = accept(sock0, (struct sockaddr *)&client, &len);

    // 6文字送信
    write(sock1, "HELLO", 6);

    // TCPセッション1の終了
    close(sock1);

    
    // TCPクライアントからの接続要求を受け付ける（2回目）
    len = sizeof(client);
    sock2 = accept(sock0, (struct sockaddr *)&client, &len);

    // 5文字送信
    write(sock2, "HOGE", 5);

    // TCPセッション2の終了
    close(sock2);


    // listenするソケットの終了
    close(sock0);

    return 0;
}