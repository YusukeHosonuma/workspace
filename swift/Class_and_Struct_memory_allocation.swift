// Class and Struct memory allocation in Swift 3.0

func adr<T>(_ title: String, _ x: inout T) {
    withUnsafePointer(to: &x) {
        print(title, $0)
    }
}

class C0 {}
class C1 {
    var x: Int
    init(x: Int) { self.x = x }
}
class C2 {
    var x: Int
    var y: Int
    init(x: Int, y: Int) { self.x = x; self.y = y }
}

struct S0 {}
struct S1 {
    var x: Int
}
struct S2 {
    var x: Int
    var y: Int
}

var x: Int = 1
var c0: C0 = C0()
var c1: C1 = C1(x: 1)
var c2: C2 = C2(x: 1, y: 2)
var s0 = S0()
var s1 = S1(x: 1)
var s2 = S2(x: 1, y: 2)
var y: Int = 2
adr("x    : ", &x)
adr("c0   : ", &c0)
adr("c1   : ", &c1)
adr("c1.x : ", &c1.x)
adr("c2   : ", &c2)
adr("c2.x : ", &c2.x)
adr("c2.y : ", &c2.y)
adr("s0   : ", &s0)
adr("s1   : ", &s1)
adr("s1.x : ", &s1.x)
adr("s2   : ", &s2)
adr("s2.x : ", &s2.x)
adr("s2.y : ", &s2.y)
adr("y    : ", &y)

/*:
 # Environment
 ```
 $ swiftc --version
 Apple Swift version 3.0 (swiftlang-800.0.46.2 clang-800.0.38)
 Target: x86_64-apple-macosx10.9
 ```

 # Output by compiled binary; use `swiftc` (no optimized)
 ```
 $ time ./no-optimize
 x    :  0x0000000101e1fd98  +0
 c0   :  0x0000000101e1fda0  +8
 c1   :  0x0000000101e1fda8  +16
 c1.x :  0x00007f8d2940cd50  <heap +0>
 c2   :  0x0000000101e1fdb0  +24
 c2.x :  0x00007f8d2940ce80  <heap +130>
 c2.y :  0x00007f8d2940ce88  <heap +138>
 s0   :  0x0000000000000007  <data segment ?>
 s1   :  0x0000000101e1fdb8  +32
 s1.x :  0x0000000101e1fdb8  +32
 s2   :  0x0000000101e1fdc0  +40
 s2.x :  0x0000000101e1fdc0  +40
 s2.y :  0x0000000101e1fdc8  +48
 y    :  0x0000000101e1fdd0  +56
 ./no-optimize  0.00s user 0.01s system 59% cpu 0.017 total
 ```

 # Output by compiled binary; use `swiftc -O` (optimized)

 ```
 time ./optimize
 x    :  0x0000000106d07dd0  +0
 c0   :  0x0000000106d07dd8  +8
 c1   :  0x0000000106d07de0  +16
 c1.x :  0x00007f8fc9c0cd00  <heap +0>
 c2   :  0x0000000106d07de8  +24
 c2.x :  0x00007f8fc9c0cdf0  <heap +240>
 c2.y :  0x00007f8fc9c0cdf8  <heap +248>
 s0   :  0x00007f8fc9c0cde0  <heap> (Why heap allocation ?)
 s1   :  0x0000000106d07df0  +32
 s1.x :  0x0000000106d07df0  +32
 s2   :  0x0000000106d07e00  +48 (Why +16 than s1.x ?)
 s2.x :  0x0000000106d07e00  +48
 s2.y :  0x0000000106d07e08  +56
 y    :  0x0000000106d07e10  +64
 ./optimize  0.00s user 0.00s system 51% cpu 0.014 total
 ```

 # Output by Playground (Xcode 8.0）

 ```console
 x    :  0x000000011b014f80    +0
 c0   :  0x000000011b014f88    +8
 c1   :  0x000000011b014f90   +16
 c1.x :  0x000061800002abd0   <heap +0>
 c2   :  0x000000011b014f98   +24
 c2.x :  0x000061800002ae10   <heap +0>
 c2.y :  0x000061800002ae18   <heap +8>
 s0   :  0x0000000000000000   <erased ?>
 s1   :  0x000000011b014fa0   +32
 s1.x :  0x000000011b014fa0   +32（== s1 address）
 s2   :  0x000000011b014fa8   +40
 s2.x :  0x000000011b014fa8   +40（== s2 address）
 s2.y :  0x000000011b014fb0   +48
 y    :  0x000000011b014fb8   +56
 ```

 # 今回のサンプルコードおよび実行環境での結果（環境によって変化する可能性あり）：

 - ポインタは8byte（64bit）で表現される。
 - ポインタはスタックに確保され、中身の要素はヒープに確保される。
 - 構造体はスタックに確保され、中身の要素も同様にスタックに確保される。
 （構造体のアドレスは、構造体で最初に定義された要素のアドレスと同じ）
 */
