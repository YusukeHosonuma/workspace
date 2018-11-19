// 省略しないコンストラクタの書き方：
//
// プライマリコンストラクタに修飾子やアノテーションがつかなければ、`constructor`は省略できる
class User constructor(_nickname: String) {
    val nickname: String
    init {
        nickname = _nickname
    }
}

// 省略するコンストラクタの書き方：
//
// `nickname`プロパティの宣言と代入が同時に行われるイメージ
class User2(val nickname: String)

open class SuperUser(val nickname: String)

// 継承するときは親クラスのコンストラクタを呼び出す必要がある
class TwitterUser(nickname: String) : SuperUser(nickname)

// コンストラクタに`private`などの修飾子をつける場合
class Secretive private constructor() {}