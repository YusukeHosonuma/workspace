package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
)

type session struct {
	Title    string `json:"title"`
	Category string `json:"category"`
}

func main() {
	res, _ := http.Get("https://us-central1-droidkaigi-index.cloudfunctions.net/fetchData?id=67107")
	byteArray, _ := ioutil.ReadAll(res.Body)
	defer res.Body.Close()

	var session session
	if err := json.Unmarshal(byteArray, &session); err != nil {
		fmt.Printf("Error (%v)\n", err)
	}

	fmt.Printf("title: %s\n", session.Title)
	fmt.Printf("category: %s\n", session.Category)
}
