// インターフェース
interface Clickable {

    // メソッドの宣言
    fun click()

    // デフォルト実装を持てる
    fun showOff() = println("I'm clickable!")
}

interface Focusable {
    fun setFocus(b: Boolean) = println("I ${if (b) "got" else "lost"} focus.")
    fun showOff() = println("I'm focusable!")
}

class Button : Clickable, Focusable {

    // interfaceのメソッドを実装する場合でも`override`は必須
    override fun click() = println("I was clicked")

    // 同名のメソッドが実装した複数のインターフェースに含まれる場合はオーバーライド必須
    override fun showOff() {

        // super<親クラス名>.メソッド() で明示的に呼び出す
        super<Clickable>.showOff()
        super<Focusable>.showOff()
    }
}

fun main(args: Array<String>) {
    val button = Button()
    button.showOff()
    button.setFocus(true)
    button.click()
}