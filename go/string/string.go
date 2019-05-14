package main

import "fmt"

func main() {

	// いずれも "ÿ" を出力
	fmt.Println("U+00FF: ÿ")
	fmt.Println("U+00FF: \u00FF")     // \u だと4桁
	fmt.Println("U+00FF: \U000000FF") // \U だと8桁
	fmt.Println("U+00FF: \xc3\xbf")   // U+00FF を UTF-8 エンコードした値

	// Unicodeポイントが5桁以上の場合は \U を使用して 8byte で表記
	fmt.Println("U+1F247: \U0001F427")         // 🐧
	fmt.Printf("U+1F247: %#v\n", "\U0001F427") // => "\U0004adf5"

	// 不正なコードポイントの場合、 `%#v`だとコードポイントがそのまま出力される
	fmt.Printf("U+4adf5: %v\n", "\U0004adf5")  // => □
	fmt.Printf("U+4adf5: %#v\n", "\U0004adf5") // => "\U0004adf5"
}
