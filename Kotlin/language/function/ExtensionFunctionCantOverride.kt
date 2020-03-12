package language.function

open class View {
    open fun onClick() = println("View clicked")
}

class Button: View() {
    override fun onClick() = println("Button clicked")
}

/*
 ExtensionFunctionCantOverrideKt.showOff(View instance) に変換される。
 */
fun View.showOff()   = println("I'm a View!")
fun Button.showOff() = println("I'm a Button!")

fun Button.onClick() = println("Button clicked at extension")

fun main(args: Array<String>) {

    val view: View = Button()

    /*
     通常のメソッドは*実行時*の型によって、呼び出されるメソッドが動的に解決される
     */
    view.onClick() // => Button clicked

    /*
     拡張関数は*コンパイル時の宣言型*（ここでは`View`）によって、静的に解決される
     （拡張関数はJavaのstaticメソッドに変換されることからもわかる）
     */
    view.showOff() // => I'm a View!

    /*
     同シグネチャのメソッドと拡張関数の2つが存在した場合、常にメソッドが優先される
     */
    val button = Button()
    button.onClick() // => Button clicked
}
