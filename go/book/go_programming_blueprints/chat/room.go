package main

import (
	"log"
	"net/http"

	"github.com/YusukeHosonuma/study-go/book/go_programming_blueprints/trace"
	"github.com/gorilla/websocket"
)

type room struct {
	forward chan []byte
	join    chan *client
	leave   chan *client
	clients map[*client]bool
	tracer  trace.Tracer
}

const (
	socketBufferSize  = 1024
	messageBufferSize = 256
)

var upgrader = &websocket.Upgrader{ReadBufferSize: socketBufferSize, WriteBufferSize: socketBufferSize}

func newRoom() *room {
	return &room{
		forward: make(chan []byte),
		join:    make(chan *client),
		leave:   make(chan *client),
		clients: make(map[*client]bool),
		tracer:  trace.Off(),
	}
}

func (r *room) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	socket, err := upgrader.Upgrade(w, req, nil)
	if err != nil {
		log.Fatal("ServeHTTP: ", err)
	}
	client := &client{
		socket: socket,
		send:   make(chan []byte, messageBufferSize),
		room:   r,
	}
	r.join <- client
	defer func() { r.leave <- client }()
	go client.write()
	client.read()
}

func (r *room) run() {
	for {
		select {
		case client := <-r.join:
			r.clients[client] = true
			r.tracer.Trace("新しいクライアントが参加しました。")
		case client := <-r.leave:
			delete(r.clients, client)
			close(client.send)
			r.tracer.Trace("クライアントが退室しました。")
		case msg := <-r.forward:
			for client := range r.clients {
				select {
				case client.send <- msg:
					r.tracer.Trace(" -- クライアントに送信されました。")
				default:
					delete(r.clients, client)
					close(client.send)
					r.tracer.Trace(" -- 送信に失敗しました。クライアントをクリーンアップします。")
				}
			}
		}
	}
}
