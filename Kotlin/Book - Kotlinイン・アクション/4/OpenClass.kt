interface Clickable {
    fun click()
    fun showOff() = println("I'm clickable!")
}

// デフォルトで`final`なので、継承を許可するには`open`をつける
open class RichButton : Clickable {

    // `open`が付いていないのでサブクラスでオーバーライド不可
    fun disable() {}

    // `open`がついているのでオーバーライド可
    open fun animate() {}

    // `open`な関数をオーバーライドした場合は、`open`が引き継がれオーバーライド可となる
    // オーバーライド不可に更新する場合は`final`をつける
    override fun click()
}
