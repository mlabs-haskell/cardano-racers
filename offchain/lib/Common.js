const bigInt = require('big-integer');

exports.fromJsBigInt = (jsBigInt) => bigInt(jsBigInt);
exports.toJsBigInt = (bigInt) => bigInt.value;
