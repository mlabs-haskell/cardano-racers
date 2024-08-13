const bigInt = require('big-integer');

export const fromJsBigInt = (jsBigInt) => bigInt(jsBigInt);
export const toJsBigInt = (bigInt) => BigInt(bigInt.toString());
