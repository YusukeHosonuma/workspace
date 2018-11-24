//
// `module.exports`を利用してエクスポートする方法
//

function add(a, b) {
    return a + b;
};

function sub(a, b) {
    return a - b;
};

module.exports = {
    add: add,
    sub: sub
};