export const fromInt = BigInt;
export const toInt = Number;
export const mkBigNat = BigInt;
export const bnBin = x => "0b" + x.toString(2);
export const bnDec = x => x.toString(10);
export const bnHex = x => "0x" + x.toString(16);
export const bnOct = x => "0o" + x.toString(8);
export const bnQua = x => "0q" + x.toString(4);
export const bnZro = 0n;
export const bnOne = 1n;
export const bnCEQ = x => y => x==y;
export const bnCLT = x => y => x<y;
export const bnAdd = x => y => x+y;
export const bnSub = x => y => y>x?0:x-y;
export const bnMul = x => y => x*y;
export const bnDiv = x => y => y==0n?0:x/y;
export const bnMod = x => y => y==0n?0:x%y;
export const bnPow = x => y => x**y;
export const bnShL = x => y => x<<y;
export const bnShR = x => y => x>>y;
export const bnUni = x => y => x|y;
export const bnInt = x => y => x&y;
export const bnSym = x => y => x^y;
export const bnCount = x => BigInt(bitCountBigInt(x));
export const bnWidth = x => BigInt(bitWidthBigInt(x));

export function bnUnpack(n) {
  n = BigInt(n);
  let bit = 0n;
  let members = [];
  while (n !== 0n) {
    if (!(n & 0xffffffffn)) {
      n >>= 32n;
      bit += 32n;
    } else {
      if (n & 1n) {
        members.push(bit);
      }
      n >>= 1n;
      bit += 1n;
    }
  }
  return members;
}

// https://stackoverflow.com/questions/54758130/how-to-obtain-the-amount-of-bits-of-a-bigint/76616288#76616288
const testersCoeff = [];
const testersBigCoeff = [];
const testers = [];
let testersN = 0;

function bitWidthBigInt(x) {
  // find upper bound
  let k = 0
  while (true) {
    if (testersN === k) {
      testersCoeff.push(32 << testersN);
      testersBigCoeff.push(BigInt(testersCoeff[testersN]));
      testers.push(1n << testersBigCoeff[testersN]);
      testersN++;
    }
    if (x < testers[k]) break;
    k++;
  }

  if (!k)
    return 32 - Math.clz32(Number(x));

  // determine length by bisection
  k--;
  let i = testersCoeff[k];
  let a = x >> testersBigCoeff[k];
  while (k--) {
    let b = a >> testersBigCoeff[k];
    if (b)
      (i += testersCoeff[k], a = b);
  }

  return i + 32 - Math.clz32(Number(a));
}

// https://stackoverflow.com/questions/43122082/efficiently-count-the-number-of-bits-in-an-integer-in-javascript#57631591
function bitCountBigInt (n) {
  n = BigInt(n);
  let bits = 0n;
  while (n !== 0n) {
    bits += count1s32(n & ((1n << 32n) - 1n));
    n >>= 32n;
  }
  return bits;
}
function count1s32(i) {
  i = i - ((i >> 1n) & 0x55555555n);
  i = (i & 0x33333333n) + ((i >> 2n) & 0x33333333n);
  i = (i + (i >> 4n)) & 0x0f0f0f0fn;
  i = i + (i >> 8n);
  i = i + (i >> 16n);
  return i & 0x3fn;
}
