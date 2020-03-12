package main

import (
	"fmt"
	"strings"
)

type Point struct {
	X int
	Y int
}

type Point2 struct {
	X, Y int
}

type Person struct {
	Age  int    `json:"age"`
	Name string `json:"name"`
}

func Add(x, y int) int {
	return x + y
}

func Div(x, y int) (int, error) {
	if y == 0 {
		return 0, fmt.Errorf("")
	}
	return x / y, nil
}

func main() {

	strings.Map(func(r rune) rune {
		return r + 1
	}, "hello")

	n, err := Div(6, 2)
	if err != nil {
		panic("error!")
	}
	print(n)

	// var s string

	// xs := [4]int{1, 2, 3, 4}
	// ys := xs[1:3] // [2 3]

	p := Point{X: 1, Y: 2}

	// x := map[string]int{
	// 	"one": 1,
	// 	"two": 2,
	// }

	fmt.Printf("Hello, world: %v\n", p)
}
