package main

import "fmt"
import "./helper" // ローカルパッケージをインポート

func main() {
	var fruits []string
	fruits = append(fruits, "Apple")
	fruits = append(fruits, "Orange")
	fruits = append(fruits, "Banana")

	s := helper.ToArrayLiteral(fruits)
	fmt.Println(s)
}
