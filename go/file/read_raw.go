package main

import (
	"fmt"
	"os"
)

const bufsize = 1024

func main() {
	file, err := os.Open("./file/read_raw.go")
	if err != nil {
		fmt.Printf("Error: %s\n", err)
	}
	defer file.Close()

	buf := make([]byte, bufsize)
	for {
		n, err := file.Read(buf)
		if n == 0 {
			break
		}
		if err != nil {
			fmt.Printf("Error: %s\n", err)
			break
		}
		fmt.Print(string(buf))
	}
}
