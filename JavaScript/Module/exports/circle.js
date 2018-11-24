var PI = Math.PI;

// Note:
// ES6 では `export` というキーワードでエクスポートするらしい。

// `exports`はエイリアスで、実際には`module.exports`らしい
exports.area = function area(r) {
    return PI * r * r;
};
