// テーブル定義
object CountryTable : IdTable() {
    val name = varchar("name", 250).uniqueIndex()
    val iso = varchar("iso", 2).uniqueIndex()
}

// テーブルに対応するEntityの定義
class Country(id: EntityID) : Entity(id) {
    var name: String by CountryTable.name
    var iso: String by CountryTable.iso
}

// クエリ実行
var russia = Coutry.find {
    CountryTable.iso.eq("ru")
}.first()

println(russia.name)
