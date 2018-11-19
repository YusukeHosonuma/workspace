fun main(args: Array<String>) {
    val kotlinLogo = """| // 
                       .|//
                       .|/ \ """

    // `trimMergin`することでインデントを無効化している
    println(kotlinLogo.trimMargin("."))
}