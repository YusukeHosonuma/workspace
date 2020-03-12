package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	files, err := ioutil.ReadDir("./")
	if err != nil {
		fmt.Printf("Error %v\n", err)
	}

	for i, file := range files {
		var prefix string
		if file.IsDir() {
			prefix = "[d]"
		} else {
			prefix = "[f]"
		}
		fmt.Printf("%v. %v %v\n", i+1, prefix, file.Name())
	}
}
