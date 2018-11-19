// プライマリコンストラクタが無く、セカンダリコンストラクタが複数宣言されている例
open class View {
    constructor(ctx: Context) {}
    constructor(ctx: Context, attr: AttributeSet) {}
}

// サブクラスで同名のコンストラクタを宣言できる
class Button : View {

    // 自分のクラス内に定義された別コンストラクタを呼び出す
    constructor(ctx, Context): this(ctx, MY_STYLE) {}

    // 親クラスのコンストラクタを呼び出す
    constructor(ctx: Context, attr: AttributeSet): super(ctx, attr) {}
}
