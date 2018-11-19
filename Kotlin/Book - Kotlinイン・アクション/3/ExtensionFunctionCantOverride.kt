open class View {
    open fun onClick() = println("View clicked")
}

class Button: View() {
    override fun onClick() = println("Button clicked")
}

fun View.showOff() = println("I'm a View!")
fun Button.showOff() = println("I'm a Button!")

fun Button.onClick() = println("Button clicked at extension")

fun main(args: Array<String>) {

    val view: View = Button()

    // 通常のメソッドは実行時の型によってオーバーライドされたメソッドが呼ばれる
    view.onClick() // => Button clicked

    // 拡張メソッドはコンパイル時の型によって静的に確定される
    // （Javaのstaticなメソッドに変換されることからも分かる）
    view.showOff() // => I'm a View!

    // 同名のメソッドがメンバ関数と拡張関数に存在した場合は、常にメンバ関数が優先される
    val button = Button()
    button.onClick() // => Button clicked
}
