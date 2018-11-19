
interface Focusable {
    fun setFocus(b: Boolean) = println("I ${if (b) "got" else "lost"} focus.")
    fun showOff() = println("I'm focusable!")
}

// `internal`はモジュール内から見える
internal open class TalkativeButton : Focusable {

    // デフォルトの可視性は`public`
    private fun yell() = println("Hey!")

    // `protected`は同クラスかサブクラスからのみ見える
    protected fun whisper() = println("Let's talk!")
}

fun TalkativeButton.giveSpeech() {

    // 以下はアクセス出来ないのでコンパイルエラー
    // yell()
    // whisper()
}
