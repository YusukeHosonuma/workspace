package quick_test

import (
	"fmt"
	"math/rand"
	"reflect"
	"testing"
	"testing/quick"
)

func TestValue(t *testing.T) {

	//
	// reflect.TypeOf で Type型 が取得できる
	//
	rType := reflect.TypeOf("")
	fmt.Printf("rType: %v <%T>\n", rType, rType) // => string <*reflect.rtype>

	// なお、Type型の実態はインターフェース
	//
	// ````
	// type Type interface {
	// 	   Align() int
	// 	   FieldAlign() int
	// 	   Method(int) Method
	// ...
	// ````

	// `%#v`すると中身の値が分かる
	fmt.Printf("rType: %#v\n", rType)
	// => &reflect.rtype{size:0x10, ptrdata:0x8, hash:0xe0ff5cb4, tflag:0x7, align:0x8, fieldAlign:0x8, kind:0x18, alg:(*reflect.typeAlg)(0x1228910), gcdata:(*uint8)(0x11568bc), str:8688, ptrToThis:75328}

	//
	// Value関数でランダムな値を取得できる
	// ここでは乱数のシードは`42`で固定している
	//
	rValue, ok := quick.Value(rType, rand.New(rand.NewSource(42)))
	fmt.Printf("ok: %v\n", ok) // => true

	// すべて不正なユニコードポイントなので、`%v`で出力すると □ になる
	fmt.Printf("rValue: %v <%T>\n", rValue, rValue) // => 𻓊񯜐򉏐񽠳򧵃 <reflect.Value>

	// `%#v`で出力するとコードポイントが出力され、たしかに不正であることが分かる
	fmt.Printf("rValue: %#v <%T>\n", rValue, rValue) // => "\U0003b4ca\U0006f710\U000893d0\U0007d833\U000a7d43" <reflect.Value>
}
