const fs = require('node:fs');

const THIS = __filename.replace('.node.js', '');
const wasmBuffer = fs.readFileSync(THIS+'.wasm');
let wasmInstance, ex;

var defaults = {
  "fuel": 10000,
};

const assert = (c, msg='Assertion failed',...info) => {
  if (!c) {
    if (info.length) console.error(msg, ...info);
    throw new Error(msg, ...info);
  }
};
const asserteq = (l, r, msg='Were not equal', ...info) => {
  assert(l === r, msg, l, r, ...info);
};

const u64 = (value) => ((value + (1n<<64n)) % (1n<<64n));

// Convert quaternary to binary
const q2b = i => {
  const digit = x => {const v = parseInt(x, 4); return isFinite(v) ? v : '`IKS'.indexOf(x)};
  return Array.from(i, x => digit(x).toString(2).padStart(2, 0)).join("");
};
// Binary to quaternary
const b2q = i => {
  return i.padStart(roundUp(i.length, 2), 0).replace(/[01][01]/g, q => parseInt(q, 2).toString(4));
};
// Separate every 64 bits with `_`, 32 bits with `…`, and 16 bits with `.`
const prettyCrumbs = i => {
  let place = 0;
  const markers = ".…._.…";
  return i.replace(/[0123]{8}(?!$)/g, m => m+markers[place++%markers.length]);
};
// Round `l` up to the next multiple of `amt`
const roundUp = (l, amt=64) => l + (amt - l%amt)%amt;


const getOutput = (startOrLen=undefined, len=undefined) => {
  var start = 0;
  if (len === undefined) {
    len = startOrLen;
  } else {
    start = startOrLen || 0;
  }
  let output = new BigUint64Array(ex.memory.buffer, ex.output_words.value + start, len && roundUp(len*2, 64) / 64);
  let outputCrumbs = b2q(Array.from(output, bigint => bigint.toString(2).padStart(64, 0)).join("")).substring(0, len);
  return outputCrumbs;
}

function assertOutput(crumbs) {
  if (typeof crumbs === 'function') return crumbs(getOutput);
  let outputCrumbs = getOutput(crumbs.length);
  assert(outputCrumbs === crumbs, "Output did not match expected", prettyCrumbs(outputCrumbs), "vs", prettyCrumbs(crumbs));
}


// Helper to repeat characters
const repeat = (n, fill="0") => [...Array(n)].map((_,i) => typeof fill === 'function' ? fill(i) : fill).join("");
// Random quaternary word of the day
const randomWord = [...Array(32)].map(_ => Math.max(0,Math.floor(6*Math.random() - 2))).join("");
const balanced = "00200033000210000331132000323102";


let tests = 0;

// Test the performance of a function
const perform = (label, f, ...args) => {
  tests = 0;
  const start = performance.now();
  const r = f(...args);
  const end = performance.now();
  if (true) console.debug(`${label}: ${(end - start).toFixed(2)}ms x${tests}`);
  return r;
};
// Test the performance of an object of functions, passing the
// same arguments to each.
const Perform = (obj, ...args) => {
  const r = {};
  for (const k in obj) r[k] = perform(k, obj[k], ...args);
  return r;
};

function reachesZero(expected, input) {
  return ex.reachesZero(BigInt(expected), BigInt("0b"+q2b(input).padEnd(64, 0)));
}
function reachesZero_impl(expected, input) {
  expected = Number(expected);
  let i = 0;
  for (const c of input) if (!(i++, expected += 2*!+c - 1)) return (2*i);
  return 0;
}
function deltaExpecting(input) {
  return ex.deltaExpecting(BigInt("0b"+q2b(input).padEnd(64, 0)));
}
function deltaExpecting_impl(input) {
  let expected = 0;
  for (const c of input) expected += 2*!+c - 1;
  return expected;
}
function redexes(leading, input) {
  if (input === undefined) {input=leading;leading=0}
  // convert crumbs to bits, shove it at the start of a word, calculate redexes, back to bits, place it at the end of the word, convert to crumbs, and then trim to the input length again
  return b2q(ex.redexes(BigInt(leading), BigInt("0b"+q2b(input).padEnd(64, 0))).toString(2).padStart(64, 0)).substring(0, input.length);
}


const testIO = ({ input: i, output: o, inputs: extra, test: func }) => {
  // Zero out the memory
  // for (const k in ex) {
  //   if (ex[k] instanceof WebAssembly.Memory)
  //     new Uint8Array(ex[k].buffer).fill(0);
  // }
  // Set the input string (if set)
  if (i != null) {
    let bits = q2b(i);
    bits = bits.padEnd(roundUp(bits.length, 64), 0);
    let input = new BigUint64Array(ex.memory.buffer, ex.input_words.value, roundUp(bits.length, 64) / 64);
    input.forEach((_, i) => {
      input[i] = BigInt("0b" + bits.substring(64*i, 64*i+64));
    });
    // console.log(i, bits, Array.from(input, x => x.toString(2).padStart(64, 0)));
  }
  // Set global variables (ugh)
  extra = {...defaults, ...(extra || {})};
  for (const k in extra) {
    if (extra[k] == undefined) continue;
    ({
      "fuel": (value) => {
        new BigUint64Array(ex.memory.buffer, ex.fuel, 1)[0] = BigInt(value);
      },
    })[k]?.(extra[k]);
  }
  // Run the test function
  const r = func();
  tests++;
  // Assert the output string (if set)
  if (o != undefined) {
    assertOutput(o);
  }
  // Assert values of global variables
  // if (extraMore) {
  //   assertGlobals(extraMore);
  // }
  return r;
}



const combinators = {
  "S": "3", "K": "2", "I": "1", "P": "0",
  "*":     "((S(KS))K)", // B
  "+":     "((S(KS))(S(K((S(KS))K))))", // S(KS)D
  "ι":     "((((S(K((S((S(K((S(KS))K)))S))(KK))))((S(K(S((SK)K))))K))S)K)",
  "Θ":     "((((SS)K)((S(K((SS)(S((SS)K)))))K)(S((SK)K))))",
  "W**":   "(S(K(S(K((S(K(S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))))K)))))",
  "C**":   "(S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))",
  "R**":   "(S(K((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK)))))))",
  "F**":   "(S(K((S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK))))))))",
  "V**":   "(S(K((S(K((S((S(K((S(KS))K)))S))(KK))))((S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK)))))))))",
  "I*":    "(S(SK))",
  "W*":    "(S(K((S(K(S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))))K)))",
  "C*":    "(S(K((S((S(K((S(KS))K)))S))(KK))))",
  "R*":    "((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK)))))",
  "F*":    "((S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK))))))",
  "V*":    "((S(K((S((S(K((S(KS))K)))S))(KK))))((S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK)))))))",
  "Ê":     "((S(K((S(K((S(KS))K)))((S(KS))K))))(S(K((S(K((S(KS))K)))((S(KS))K)))))",
  "B1":    "((S(K((S(KS))K)))((S(KS))K))",
  "B2":    "((S(K((S(K((S(KS))K)))((S(KS))K))))((S(KS))K))",
  "B3":    "((S(K((S(K((S(KS))K)))((S(KS))K))))((S(KS))K))",
  "D1":    "(S(K(S(K((S(KS))K)))))",
  "D2":    "((S(K((S(KS))K)))(S(K((S(KS))K))))",
  "M2":    "(S(K((S((SK)K))((SK)K))))",
  "Q1":    "((S(K((S((S(K((S(KS))K)))S))(KK))))((S(KS))K))",
  "Q2":    "((S(K(S((S(K((S((S(K((S(KS))K)))S))(KK))))((S(KS))K)))))K)",
  "Q3":    "(S(K((S(K(S((SK)K))))K)))",
  "Q4":    "((S(K((S((S(K((S(KS))K)))S))(KK))))((S(K(S((S(K((S((S(K((S(KS))K)))S))(KK))))((S(KS))K)))))K))",
  "W1":    "((S(K(S((S(K(S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))))K))))K)",
  "B":     "((S(KS))K)",
  "C":     "((S((S(K((S(KS))K)))S))(KK))",
  "D":     "(S(K((S(KS))K)))",
  "E":     "(S(K((S(K((S(KS))K)))((S(KS))K))))",
  "F":     "((S(K((S((SK)K))(K((S(K(S((SK)K))))K)))))((S(K((S(K((S(KS))K)))((S(KS))K))))((S(K(S((SK)K))))K)))",
  "G":     "((S(K((S(KS))K)))((S((S(K((S(KS))K)))S))(KK)))",
  "H":     "((S(K((S(K(S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))))K)))(S(K((S((S(K((S(KS))K)))S))(KK)))))",
  "I":     "((SK)K)",
  "J":     "((S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))((S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))(K((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S(K((S(KS))K)))((S(KS))K))))))))",
  "L":     "((S((S(KS))K))(K((S((SK)K))((SK)K))))",
  "M":     "((S((SK)K))((SK)K))",
  "O":     "(S((SK)K))",
  "Q":     "((S(K(S((S(KS))K))))K)",
  "R":     "((S(K((S(KS))K)))((S(K(S((SK)K))))K))",
  "T":     "((S(K(S((SK)K))))K)",
  "U":     "((S(K(S((SK)K))))((S((SK)K))((SK)K)))",
  "V":     "((S(K((S((S(K((S(KS))K)))S))(KK))))((S(K(S((SK)K))))K))",
  "ω":     "((S(K(S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))))K)",
  "Y":     "(((SS)K)((S(K((SS)(S((SS)K)))))K)",
}
for (const k in combinators) {
  if (k === "P") continue;
  combinators[k] = combinators[k]
    .replaceAll('(', '0')
    .replaceAll('I', '1')
    .replaceAll('K', '2')
    .replaceAll('S', '3')
    .replaceAll(')', '');
  assert(deltaExpecting_impl(combinators[k]) === -1, k, deltaExpecting_impl(combinators[k]));
  assert(reachesZero_impl(1, combinators[k]) === combinators[k].length*2, k, reachesZero_impl(1, combinators[k]));
}
function isatomic(s) {
  return reachesZero_impl(1, s) === (s.length * 2);
}

function sugar(more) {
  if (typeof more !== 'object' && more != undefined) return sugar({})(more);
  const vars = {...combinators};
  for (const k in more||{}) {
    vars[k] = sugar(undefined)(more[k]);
    assert(isatomic(vars[k]), k + ' was not atomic', more[k], vars[k]);
  }
  return s => {
    if (typeof s === 'number') s = '#'+s;
    s = s.replaceAll(/#[0-9]+/g, x => repeat(Number(x.substring(1)), '00SB')+'0SK');
    for (const k in vars) {
      s = s.replaceAll(k, vars[k]);
    }
    return s.replaceAll(/\(| |\)/g, '');
  };
}

// https://crypto.stanford.edu/~blynn/lambda/sk.html
const factorialfn = 'PPPSPKPPSPPSKKPPSKKPPSPKPPSSPKPPSPPSKKPPSKKKPPSPKPPSPKPSPPSPPSPPSPPSKKPKPKPSKPKKPSKPSPPSPKSKPPSPKPPSSPKPPSPKPPSPKPSPKPPSSPKPSKPSPKPPSPKPPSSPKKKPPSPKPPSSPKPPSPKPPSPKPSPKPPSPKPSPPSKKKPSPPSKKKKK';
const factorial = n => 'P' + factorialfn + '#' + n;
const factorial_calc = n => {let r=1; while(n) r*=n--; return r;};


function toCombinators(crumbs, pedantic=true) {
  crumbs = sugar(crumbs);
  assert(isatomic(crumbs), "Needs to be atomic to convert to combinators", crumbs, reachesZero_impl(1, crumbs), crumbs.length);
  let i = 0; let r = '';
  let take1 = () => {
    let l = reachesZero_impl(1, crumbs.substring(i))/2;
    assert(l, "Overran", crumbs.substring(i));
    return [l, toCombinators(crumbs.substring(i, i+l), pedantic)];
  };
  if (crumbs[i] === '0') {
    i++;
    let [j,f] = take1();
    i+=j;
    let [k,x] = take1();
    i+=k;
    r = `(${f}${x})`;
  } else {
    r = ({
      '1': 'I',
      '2': 'K',
      '3': 'S',
    })[crumbs[i]];
    assert(r, "Unknown crumb", crumbs[i]);
    i++;
  }
  assert(i === crumbs.length, "Stopped at correct point", i, crumbs.length);
  return r;
}

function slowest_impl(crumbs, fuel=1000, strategy=undefined) {
  if (strategy === undefined) strategy = 0;
  crumbs = String(sugar(crumbs));
  let take1 = (i) => {
    let l = reachesZero_impl(1, crumbs.substring(i))/2;
    return crumbs.substring(i, i+l);
  };
  let reverted = 0;
  while (fuel--) {
    var I = crumbs.indexOf('01', reverted);
    var K = crumbs.indexOf('002', reverted);
    var S = crumbs.indexOf('0003', reverted);
    if (I + K + S === -3) break;
    reverted = 0;
    if (I > -1 && (K === -1 || I < K) && (S === -1 || I < S)) {
      let x = take1(I + 2);
      if (x) {
        crumbs = crumbs.substring(0, I) + x + crumbs.substring(I + 2 + x.length);
      } else {
        reverted = I+2;
      }
    } else if (K > -1 && (S === -1 || K < S)) {
      let x = take1(K + 3);
      let y = take1(K + 3 + x.length);
      if (x && y) {
        crumbs = crumbs.substring(0, K) + x + crumbs.substring(K + 3 + x.length + y.length);
      } else {
        reverted = K+3;
      }
    } else {
      let x = take1(S + 4);
      let y = take1(S + 4 + x.length);
      let z = take1(S + 4 + x.length + y.length);
      if (x && y && z) {
        crumbs = crumbs.substring(0, S) + '00' + x + z + '0' + y + z + crumbs.substring(S + 4 + x.length + y.length + z.length);
      } else {
        reverted = S+4;
      }
    }
  }
  return crumbs;
}


function test_eval() {
  function testcase(input, output, fuel=100, debug=false) {
    fuel += 2;
    fuel *= 2;
    debugging && console.log();
    let o, r;
    input = sugar(input);
    output = sugar(output);
    let by_impl = slowest_impl(input, fuel);
    if (output !== by_impl) {
      console.error('Discrepancy', {input,output,by_impl,fuel});
    }
    try {
      trydebug(() => {
        r = testIO({
          input, output, inputs: {fuel, debug},
          test: () => o = ex.eval(BigInt(input.length)),
          outputs: {
            optr: 8 * (output.length>>5),
            obit: (2*output.length) % 64,
          },
        });
        assert(o === BigInt(output.length));
      });
    } catch(e) {
      if (o !== undefined) {
        console.error(o+':', prettyCrumbs(getOutput(Number(o))));
        console.error('vs'.padEnd((o+':').length, ' '), prettyCrumbs(output));
      }
      throw e;
    }
    return r;
  }
  function reductions(...reductions) {
    if (reductions.length === 1) {
      testcase(reductions[0], reductions[0], 10);
      return;
    }
    let prev;
    for (const next of reductions) {
      if (prev) {
        testcase(prev, next, 1);
      }
      prev = next;
    }
    testcase(reductions[0], reductions.at(-1), reductions.length - 1);
    testcase(reductions[0], reductions.at(-1), reductions.length + 1);
  }

  {
    try {
      testcase("00231", "3", 100);
      // testcase("0100231", "00231", 1);
      testcase("3", "3", 1);
      testcase("033", "033", 1);
      // testcase("0100231", "00231", 1);
      testcase("00231", "3", 2);
      testcase("3", "3", 2);
      testcase("0100231", "3", 4);
      testcase("0100231", "3", 30);
      testcase('000022330030232', '3', 25);
      // testcase('03000022330030232', '033', 25);
      // testcase('0030030232000022330030232', '00300302323', 25);
      // reductions("000302113", "000213013", "01013", "013", "3");
      // testcase('010000030030200302323022330030232', '0000030030200302323022330030232', 1);
      // testcase('010000030030200302323022330030232', '00300302323', 25);
    } catch(e) {
      console.error("Locals", {});
      throw e;
    }
  }
  tinyFactorial:{
    // break tinyFactorial;
    // input = sugar(factorial(1));
    // console.log(isatomic(input));
    // output = problem;//sugar(1);
    // testIO({
    //   input,
    //   output,
    //   inputs: {fuel: 197}, // 198: memory access OOB with 16*1024 *and* 1024
    //   test: () => {
    //     o = ex.slowest(BigInt(input.length));
    //     console.log('max_output', new Uint32Array(ex.memory.buffer, ex.max_output, 1)[0]);
    //     console.log(o+':', getOutput(Number(o)));
    //   },
    // });

    // input = sugar(factorial(1));
    // output = sugar(1);
    // (() => testIO({
    //   input,
    //   output,
    //   inputs: {fuel: 428},
    //   test: () => {
    //     o = ex.slowest(BigInt(input.length));
    //     console.log('max_output', new Uint32Array(ex.memory.buffer, ex.max_output, 1)[0]);
    //     console.log(o+':', getOutput(Number(o)));
    //   },
    // }))();

    // input = sugar(`PP${factorial(1)}21`);
    // output = `021`;
    // (() => testIO({
    //   input,
    //   output,
    //   inputs: {fuel: 120},
    //   test: () => {
    //     o = ex.slowest(BigInt(input.length));
    //     console.log('max_output', new Uint32Array(ex.memory.buffer, ex.max_output, 1)[0]);
    //     console.log(o+':', getOutput(Number(o)));
    //   },
    // }))();
  }
  // smallFactorial:{
  //   break smallFactorial;
  //   input = sugar(factorial(2));
  //   output = sugar(2);
  //   testIO({
  //     input,
  //     output,
  //     inputs: {fuel: 197}, // 198: memory access OOB with 16*1024 *and* 1024
  //     test: () => o = ex.slowest(BigInt(input.length)),
  //   });
  // }
  // factorialTest:{
  //   break factorialTest;
  //   //ex.input.grow(5000)
  //   //ex.output.grow(5000)
  //   console.log(ex.input);
  //   input = sugar(factorial(4));
  //   output = sugar(repeat(24, '00SB')+'0SK');
  //   testIO({
  //     input,
  //     output,
  //     inputs: {fuel: 1000, fuel_copy: 3000, fuel_scan: 3000},
  //     test: () => o = ex.slowest(BigInt(input.length)),
  //     outputs: {
  //       optr: 8 * (output.length>>5),
  //       obit: (2*output.length) % 64,
  //     },
  //   });
  // }
  for (const m of ["3"])
  for (const n of ["3", "2", "032"])
  for (const o of ["3", "2", "1", "032", "021", "B"])
  {
    let test, desugared;
    try {
      const mno = sugar({m,n,o});
      const t = (s, fuel=30, debug=false) => {
        test = s; desugared = mno(s);
        let [lhs, rhs] = mno(s).split("=");
        testcase(lhs, rhs, fuel, debug);
        // testcase(lhs, rhs, fuel+1, debug);
      };
      t('000Bmno = 0m0no', 4)
      t('000302mno = 0m0no', 2)
      t('000Cmno = 00mon', 10)
      t('000030203m2no = 00mon', 4)
      t('0003m02no = 00mon', 2)
      t('000302m02no = 0mn', 3)
      // these bump it up over word size for some cases
      t('010000030030200302323022mno = 00mon', 11)
      t('01010000030030200302323022mno = 00mon', 12)
      t('002(0030232)(00000300302(B)3022mno) = 0030232', 1)
      t('002(o)(0YY) = o', 100)
      t('00(00SB(00SB(0SK)))mn = 0m0mn', 13)
      t('00(#1)mn = 0mn', 13)
      t('00(#2)mn = 0m0mn', 13)
      t('00(00SBI)mn = 0m0mn', 13)
      t('0000*(00SBI)(#1)mn = 0m0mn', 25)
      // mul 2 2 = 4
      t('0000*(00SBI)(00SBI)mn = 0m0m0m0mn', 25)
      t('0000+(00SBI)(00SBI)mn = 0m0m0m0mn', 23)
      t('0000*(#2)(00SBI)mn = 0m0m0m0mn', 30)
      t('0000+(#2)(00SBI)mn = 0m0m0m0mn', 28)
    } catch(e) {
      console.error("Locals", {test,desugared,m,n,o});
      throw e;
    }
  }

  for (const m of ["3"])
  for (const n of ["3", "2", "032"])
  for (const x of [0, 1, 2, 3, 4, 6])
  for (const y of [0, 1, 2, 3, 4, 6])
  {
    let test, desugared;
    try {
      const mno = sugar({m,n,x,y});
      const t = (s, fuel=30, debug=false) => {
        test = s; desugared = mno(s);
        let [lhs, rhs] = mno(s).split("=");
        testcase(lhs, rhs, fuel, debug);
        testcase(lhs, rhs, fuel+1, debug);
      };
      if (x * y <= 12) t(`0000*xymn = ${repeat(x*y, '0m')}n`, 150)
      t(`0000+xymn = ${repeat(x+y, '0m')}n`, 100)

      // if (!x && y <= 2) t(`00${sugar(factorial(y))}mn = ${repeat(factorial_calc(y), '0m')}n`, 5000)
    } catch(e) {
      console.error("Locals", {test,desugared,m,n,x,y});
      throw e;
    }
  }
}

function test_factorial(fixed_x=undefined) {
  function testcase(input, output, fuel=100, debug=false) {
    debugging && console.log();
    let o, r;
    input = sugar(input);
    output = sugar(output);
    // let by_impl = slowest_impl(input, fuel);
    // if (output !== by_impl) {
    //   console.error('Discrepancy', {input,output,by_impl,fuel});
    // }
    try {
      trydebug(() => {
        r = testIO({
          input, output, inputs: {fuel, debug},
          test: () => o = ex.eval(BigInt(input.length)),
          outputs: {
            optr: 8 * (output.length>>5),
            obit: (2*output.length) % 64,
          },
        });
        assert(o === BigInt(output.length));
      });
    } catch(e) {
      if (o !== undefined) {
        console.error(o+':', prettyCrumbs(getOutput(Number(o))));
        console.error('vs'.padEnd((o+':').length, ' '), prettyCrumbs(output));
      }
      throw e;
    }
    return r;
  }

  for (const x of fixed_x !== undefined ? [fixed_x] : [0, 1, 2, 3])
  for (const [fx, succs] of [[sugar(factorial(x)), repeat(factorial_calc(x), '0n')]])
  for (const n of ["3", "2", "032"])
  for (const o of ["2", "3", "1", "032", "021", "B"])
  {
    let test, desugared;
    try {
      const mno = sugar({n,o});
      const t = (s, fuel=30, debug=false) => {
        test = s; desugared = mno(s);
        let [lhs, rhs] = mno(s).split("=");
        testcase(lhs, rhs, fuel, debug);
      };
      t(`00${fx}no = ${succs}o`, [1000,1000,10000,10000,100000,1000000,10000000,100000000][x])
      // if (x === 6) console.error(mno(`00${fx}no = ${succs}o`));
      // Only do one test of x=6
      if (x === 6) return;
    } catch(e) {
      console.error("Locals", {test,desugared,x,n,o});
      throw e;
    }
  }
}

var c_str = str_ptr => {
  var len = 0;
  var buf = new Uint8Array(ex.memory.buffer, str_ptr);
  while (buf[len]) len++;
  return new TextDecoder().decode(new Uint8Array(ex.memory.buffer, str_ptr, len));
};
var indentation = 0;
var debugging = false;
var debugfn = fn => {
  if (debugging) {
    if (Array.isArray(debugging)) {
      debugging.push(fn);
    } else {
      fn();
    }
  }
};
var tracing = {by_ptr: {}, flat: {by_event: [], all_events: []}};
var trace_n = 0;
var trydebug = inner => {
  var previous = debugging;
  var here = debugging = [];
  tracing = {by_ptr: {}, flat: {by_event: [], all_events: []}};
  trace_n = 0;
  let r;
  try {
    r = inner();
  } catch(e) {
    console.log("Caught", e.message);
    fs.writeFileSync(THIS + '.trace.json',
      JSON.stringify(tracing, (_, v) => typeof v === 'bigint' ? v.toString() : v, 2)
        .replaceAll(/\n        (  )*/g, ' ')
        .replaceAll(/\n      (  )*\]/g, ' ]')
        .replaceAll(/    "_/g, '    "')
    );
    if (Array.isArray(here) && here.length < 10000)
      for (var debugged of here)
        debugged();
    debugging = previous;
    throw e;
  }
  debugging = previous;
  return r;
};

var prettytrace = v => typeof v === 'bigint' ? (u64(v) > 0x10000n ? [u64(v), u64(v).toString(4).padStart(32, 0)] : Number(u64(v))) : v;

WebAssembly.instantiate(wasmBuffer, {
  env: {
    dbg: (x,y) => debugfn(() => console.log(
      "0x"+((x + (2**32)) % (2**32)).toString(16).padStart(8, 0),
      "0x"+((y + (1n<<64n)) % (1n<<64n)).toString(16).padStart(16, 0),
      ((y + (1n<<64n)) % (1n<<64n)).toString(4).padStart(32, 0),
    )),
    print64: (str_ptr, value) => {
      tracing.flat.all_events.push([ -trace_n, indentation, c_str(str_ptr), prettytrace(value)]);
      debugfn(() => console.log(
        repeat(indentation, '  ') + c_str(str_ptr).padEnd(22 + 10 - 2*indentation),
        "0x"+((value + (1n<<64n)) % (1n<<64n)).toString(16).padStart(16, 0),
        ((value + (1n<<64n)) % (1n<<64n)).toString(4).padStart(32, 0),
        ((value + (1n<<64n)) % (1n<<64n)),
      ));
    },
    dent: value => {
      indentation += value;
      debugfn(() => { indentation += value; })
    },
    trace: (ptr, c_str1, c_str2, ...arg) => {
      let event = [c_str(c_str1), c_str(c_str2)].concat(arg.map(prettytrace));
      if (event[1] === "" && event.length === 2) event = [event[0]];
      ++trace_n;
      tracing.flat.by_event.push([trace_n, ptr, ...event]);
      tracing.flat.all_events.push([trace_n, ptr, ...event]);
      (tracing.by_ptr["_"+ptr] || (tracing.by_ptr["_"+ptr]=[])).push([trace_n, ...event]);
    },
    epoch: (c_str1, ...arg) => {
      let event = [c_str(c_str1)].concat(arg.map(prettytrace));
      ++trace_n;
      tracing.flat.by_event.push([trace_n, ...event]);
      tracing.flat.all_events.push([trace_n, ...event]);
      for (let ptr in tracing.by_ptr) {
        let evs = tracing.by_ptr[ptr];
        if (evs.at(-1)[1] === event[0] && evs.at(-2)[1] === event[0]) {
          evs[evs.length-1] = [trace_n, ...event];
        } else {
          evs.push([trace_n, ...event]);
        }
      }
    },
  },
}).then(wasmModule => {
  wasmInstance = wasmModule.instance;
  ex = wasmInstance.exports;
  // console.log(wasmModule, ex);

  asserteq(deltaExpecting(balanced), 0);
  asserteq(reachesZero(1, balanced), 0);
  asserteq(deltaExpecting_impl(balanced), 0);
  asserteq(reachesZero_impl(1, balanced), 0);
  asserteq(reachesZero(1, "0"), 0);
  asserteq(reachesZero(1, "32"), 2);
  asserteq(reachesZero(1, "032"), 6);
  asserteq(reachesZero(1, "00132"),10);

  Perform({test_eval});
  console.log('max_output:', new Uint32Array(ex.memory.buffer, ex.max_output, 1)[0]);
  Perform({
    test_factorial0: () => test_factorial(0),
    test_factorial1: () => test_factorial(1),
    test_factorial2: () => test_factorial(2),
    test_factorial3: () => test_factorial(3),
    test_factorial4: () => test_factorial(4),
    test_factorial5: () => test_factorial(5),
    test_factorial6: () => test_factorial(6),
    // test_factorial7: () => test_factorial(7),
    // test_factorial8: () => test_factorial(8),
  });

  // console.log(sugar()('#1'));
  console.log('max_output:', new Uint32Array(ex.memory.buffer, ex.max_output, 1)[0]);
});
