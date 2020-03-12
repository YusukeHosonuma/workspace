package main

import (
	"fmt"
	"os"
)

func main() {
	fmt.Println("好きな数字を入力してください。")

	var i int

	if _, err := fmt.Scan(&i); err != nil {
		fmt.Printf("Error: %v\n", err)
		os.Exit(-1)
	}
	fmt.Println("--------")

	switch i {
	case 7:
		fmt.Println("ラッキーセブンです！")
	default:
		fmt.Println("ふつーだね。")
	}
}
