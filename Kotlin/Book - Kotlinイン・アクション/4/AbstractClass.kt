// 抽象クラスはデフォルトで`open`
abstract class Animated {

    // サブクラスでオーバーライド必須
    abstract fun animate()

    // 非抽象メソッドはデフォルトでは`open`ではないが、`open`をつけることが出来る
    open fun stopAnimating() {
    }

    // `open`が付いていないのでオーバーライド不可
    fun animateTwice() {
    }
}
