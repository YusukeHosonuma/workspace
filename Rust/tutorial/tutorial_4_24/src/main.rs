// 4.24. 共通の関数呼び出し構文

fn main() {

    // 共通の関数呼び出し構文
    {
        trait Foo {
            fn f(&self);
        }

        trait Bar {
            fn f(&self);
        }

        struct Baz;

        impl Foo for Baz {
            fn f(&self) { println!("Baz's impl of Foo"); }
        }

        impl Bar for Baz {
            fn f(&self) { println!("Baz's impl of Bar"); }
        }

        let b = Baz;

        // error[E0034]: multiple applicable items in scope
        // b.f();

        // 「共通の関数呼び出し構文」を使って、曖昧性を排除する必要がある
        Foo::f(&b);
        Bar::f(&b);

        // b.f() => f() が &self を引数に取る場合、自動的に b を借用する
    }

    // 山括弧形式
    {
        trait Foo {
            fn foo() -> i32;
        }

        struct Bar;

        impl Bar {
            fn foo() -> i32 {
                20
            }
        }

        impl Foo for Bar {
            fn foo() -> i32 {
                10
            }
        }

        // クラスメソッドを呼び出しているイメージ（&selfを受け取らないので）

        // <Type as Trait>::
        assert_eq!(10, <Bar as Foo>::foo());

        // Type::
        assert_eq!(20, Bar::foo());
    }
}
