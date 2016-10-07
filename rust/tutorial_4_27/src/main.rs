// 4.27. アトリビュート

// 独自のアトリビュートを作成することは出来ない

#[foo] // 直後に適用
struct Foo;

fn main() {
    #![bar] // 囲んでいるアイテムに適用
}

#[test]
fn check() {
    assert_eq!(2, 1 + 1);
}

#[inline(always)]
fn super_fast_fn() {
}

#[cfg(target_os = "macos")]
mod macos_only {
}
