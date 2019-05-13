package main

import (
	"fmt"
)

func main() {

	var x uint8 = 1
	var y uint8 = 2

	fmt.Printf("x = %#v\n", x)
	fmt.Printf("y = %#v\n", y)
	fmt.Printf("x^y = %#v\n", x^y)
	fmt.Printf("uint32(x^y) = %#v\n", uint32(x^y))
	fmt.Printf("uint32(x^y) - 1 = %#v\n", uint32(x^y)-1)
	fmt.Printf("(uint32(x^y) - 1) >> 31 = %#v\n", (uint32(x^y)-1)>>31)
	fmt.Printf("int((uint32(x^y) - 1) >> 31) = %#v\n", int((uint32(x^y)-1)>>31))
}
