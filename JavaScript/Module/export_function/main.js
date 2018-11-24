// `require`して代入した`counter`の実体は関数（クロージャ）
const counter = require('./counter.js');

console.log(counter());
console.log(counter());
console.log(counter());
