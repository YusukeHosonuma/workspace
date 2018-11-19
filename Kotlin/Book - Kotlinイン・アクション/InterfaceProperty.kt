interface User {
    val nickname: String
}

// 通常のプロパティとしてオーバーライド
class PrivateUser(override val nickname: String) : User

// カスタムgetterによるオーバーライド
class SubscribingUser(val email: String) : User {
    override val nickname: String
        get() = email.substringBefore('@')
}

// 初期化時に、値を取得して格納
class FacebookUser(val accountId: Int) : User {
    override val nickname = getFacebookName(accountId)
}

interface MailUser {
    val email: String

    // カスタムgetterをinterfaceに持つことが出来る
    val nickname: String
        get() = email.substringBefore('@')
}