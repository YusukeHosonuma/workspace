package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	bytes, err := ioutil.ReadFile("./readfile.go")
	if err != nil {
		fmt.Printf("Error: %v", err)
	}
	fmt.Println(string(bytes))
}
