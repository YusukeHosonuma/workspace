//
// 関数自体をエクスポートする例
//

// 1から数え上げるカウンター
let n = 0;
function counter() {
    n = n + 1
    return n;
}

// 関数としてエクスポート
module.exports = counter;
