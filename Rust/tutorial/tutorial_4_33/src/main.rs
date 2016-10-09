// 4.33. Deref による型強制

fn main() {

    // Deref による型強制
    {
        use std::ops::Deref;
        use std::rc::Rc;

        struct DerefExample<T> {
            value: T,
        }

        impl<T> Deref for DerefExample<T> {
            type Target = T;
            fn deref(&self) -> &T { // *value としたときにこれが呼び出される
                &self.value
            }
        }

        fn foo(s: &str) {
            println!("Called foo: {}", s);
        }

        let x = DerefExample { value: 'a' };
        assert_eq!('a', *x); // 参照はずし

        // impl Deref<Target=str> for String
        let owned: String = "Hello".to_string();
        foo(&owned); // String -> str へ型強制がされるので、この呼出しはOK

        let owned = "Hello".to_string();
        let counted = Rc::new(owned);
        foo(&counted); // Rc<String> -> String -> str と2段階で変換（Rustは必要なだけ行う）
    }

    // Deref とメソッド呼び出し
    {
        struct Foo;
        impl Foo {
            fn foo(&self) { println!("Foo"); }
        }
        let f = &&Foo;
        f.foo(); // コンパイラが自動的に必要な数だけ*演算牛を補う
    }
}
