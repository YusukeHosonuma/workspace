class User(val id: Int, val name: String, val address: String)

// 拡張関数に移したコード：
//
fun User.validateBeforeSave() {

    fun validate(value: String,
                 fieldName: String) {
        if (value.isEmpty()) {
            throw IllegalArgumentException(
                "Can't save user ${id}: empty ${fieldName}")    
        }
    }

    validate(name, "Name")
    validate(address, "Address")
}

fun saveUser(user: User) {

    // 最初のコード：
    //
    // if (user.name.isEmpty()) {
    //     throw IllegalArgumentException(
    //         "Can't save user ${user.id}: empty Name")
    // }
    // if (user.address.isEmpty()) {
    //     throw IllegalArgumentException(
    //         "Can't save user ${user.id}: empty Address")
    // }

    // ローカル関数に移したコード：
    //
    // fun validate(value: String,
    //              fieldName: String) {
    //     if (value.isEmpty()) {
    //         throw IllegalArgumentException(
    //             "Can't save user ${user.id}: empty ${fieldName}")    
    //     }
    // }

    // validate(user.name, "Name")
    // validate(user.address, "Address")

    // このように既存のオブジェクトにAPIを生やすようにする実装を「Pimp My Library」パターンと呼ぶ
    user.validateBeforeSave()

    // Save user to the database
}

fun main(args: Array<String>) {
    val user = User(1, "Bob", "")
    saveUser(user)
}
