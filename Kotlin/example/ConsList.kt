//
// KotlinでConsList
//

package example

fun main(args: Array<String>) {

    // Cons: 1 -> 2 -> 3 -> nil
    val cons = Cons(1, Cons(2, Cons(3, Nil())))

    // toString
    println(cons) // => 1, 2, 3, nil

    // count
    println(cons.count()) // => 3

    // take
    println(cons.take(0)) // => nil
    println(cons.take(1)) // => 1, nil
    println(cons.take(2)) // => 1, 2, nil
    println(cons.take(3)) // => 1, 2, 3, nil
    println(cons.take(4)) // => 1, 2, 3, nil

    // head
    println(cons.head()) // => 1

    // tail
    println(cons.tail()) // => 2, 3, nil

    // for-in (iterator)
    for (n in cons) {
        print("$n, ") // 1, 2, 3,
    }
}

/**
 * Consリスト
 */
sealed class ConsList<T> {

    /**
     * 文字列表現を返す
     */
    override fun toString(): String = when (this) {
        is Nil  -> "nil"
        is Cons -> "$value, $next"
    }

    /**
     * for-in で走査可能な Iterator を返す
     */
    operator fun iterator() = Iterator(this)

    /**
     * Iterator
     */
    class Iterator<T>(val consList: ConsList<T>) {

        var current = consList

        operator fun hasNext(): Boolean = when (current) {
            is Nil  -> false
            is Cons -> true
        }

        operator fun next(): T = when (current) {
            is Nil  -> throw IllegalStateException()
            is Cons -> {
                val current = (this.current as Cons<T>)
                val result = current.value
                this.current = current.next
                result
            }
        }
    }
}

/**
 * Cons
 */
data class Cons<T>(val value: T, val next: ConsList<T>) : ConsList<T>()

/**
 * Nil
 */
class Nil<T>: ConsList<T>() // TODO: objectにしたいけど、ジェネリクスが使えないのでNG？

/**
 * count
 */
fun <T> ConsList<T>.count(): Int = when (this) {
    is Nil  -> 0
    is Cons -> 1 + next.count()
}

/**
 * take
 */
fun <T> ConsList<T>.take(n: Int): ConsList<T> = when (this) {
    is Nil  -> Nil()
    is Cons -> when (n) {
        0    -> Nil()
        else -> copy(next = next.take(n - 1))
    }
}

/**
 * head
 */
fun <T> ConsList<T>.head(): T = when (this) {
    is Nil  -> throw IllegalStateException("list is empty")
    is Cons -> value
}

/**
 * tail
 */
fun <T> ConsList<T>.tail(): ConsList<T> = when (this) {
    is Nil  -> throw IllegalStateException("list is empty")
    is Cons -> next
}
