class Outer {

    val inner = Inner()

    // `inner`をつけることで（外側のクラスへの参照を持つ）内部クラスを作成できる
    inner class Inner {

        // `this@Outer`でエンクロージングインスタンスへの参照を取得できる
        fun getOuterReference() = this@Outer
    }

    // デフォルトでは`static`な内部クラス
    class StaticInner {}
}

fun main(args: Array<String>) {

    val outer = Outer()
    println("outer: ${outer.javaClass}")
    println("inner: ${outer.inner.javaClass}")
    println("outer: ${outer.inner.getOuterReference().javaClass}")

    val staticInner = Outer.StaticInner()
    println("staticInner: ${staticInner}")
}