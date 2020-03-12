# rust-server

# Docker

## Build

```bash
$ docker-compose build
```

## Run

```bash
$ docker-compose up -d
```

```bash
$ curl localhost:5000/index.html
<!DOCTYPE html>
<html lang="ja">
<head>
<title>Hello, rust-server.</title>
<link rel="stylesheet" type="text/css" href="css/base.css">
</head>
<body>
<h1>Hello, rust-server.</h1>
<p>これはRustで作られた簡易HTTPサーバです。<br>本ページはindex.htmlの内容が表示されています。</p>
</body>
</html>
```

## Trouble?

`/etc/hosts/`:

```
127.0.0.1       localhost
255.255.255.255 broadcasthost
#::1             localhost # <--- Please try comment-out
```
