fun renderPersonList(persons: Collection<Person>) =
    createHTML().table {
        for (person in persons) { // 標準のforループ
            tr {
                td { +person.name } // HTMLに対応するDSL
                td { +person.age }
            }
        }
    }