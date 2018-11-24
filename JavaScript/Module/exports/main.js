// `rquire`すると`exports`オブジェクトが入手できるイメージ
const circle = require('./circle.js');

// で、`オブジェクト.メソッド`みたいに呼び出せる
console.log(`半径2の円の面積：${circle.area(2)}`);
