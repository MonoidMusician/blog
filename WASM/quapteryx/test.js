const wasmInstance = new WebAssembly.Instance(wasmModule, {
  imports: {
    dbg: (x,y) => console.log(
      "0x"+((x + (2**32)) % (2**32)).toString(16).padStart(8, 0),
      "0x"+((y + (1n<<64n)) % (1n<<64n)).toString(16).padStart(16, 0),
      ((y + (1n<<64n)) % (1n<<64n)).toString(4).padStart(32, 0),
    ),
  },
});
const ex = wasmInstance.exports;

// Capture the default values of globals
const defaults = {};
for (const k in ex) {
  if (ex[k] instanceof WebAssembly.Global)
    defaults[k] = ex[k].value;
}

// Helpers (not so applicable to quapteryx...)
const mem = (i,l) => new Uint8Array(ex.output.buffer, i, l);
const hex = x=>x.toString(16).padStart(2,0);
const h0x = x=>"0x"+hex(x);
const dbg = mem => Array.from(mem, h0x).join(" ");
const dbgmem = (i,l) => dbg(mem(i,l));

const assert = (c, msg='Assertion failed',...info) => {
  if (!c) {
    if (info.length) console.error(...info);
    throw new Error(msg, ...info);
  }
};

//console.log(((ex.redexes(0n, 0b1111001100000011000010000101001011110011000000110000100001010010n)+(1n<<64n)) % (1n<<64n)).toString(4)); // 3002010000000000300201000
//console.log(ex.deltaExpecting(0x5FFFFFFFFn)); // 4
//console.log(ex.reachesZero(1n,0b001100110100n << 52n)); // 8

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
  var start;
  if (len === undefined) {
    len = startOrLen;
  } else {
    start = startOrLen;
  }
  let output = new BigUint64Array(ex.output.buffer, start, len && roundUp(len*2, 64) / 64);
  let outputCrumbs = b2q(Array.from(output, bigint => bigint.toString(2).padStart(64, 0)).join("")).substring(0, len);
  return outputCrumbs;
}

function assertOutput(crumbs) {
  if (typeof crumbs === 'function') return crumbs(getOutput);
  let outputCrumbs = getOutput(crumbs.length);
  assert(outputCrumbs === crumbs, "Output did not match expected", prettyCrumbs(outputCrumbs), "vs", prettyCrumbs(crumbs));
}
function assertGlobals(extraMore) {
  if (extraMore === console.log) {
    return console.log(Object.fromEntries(Object.keys(defaults).map(k => ([k, ex[k].value]))));
  }
  const got = {};
  const wanted = {};
  let matched = true;
  for (const k in extraMore) {
    got[k] = ex[k].value;
    wanted[k] = extraMore[k];
    if (typeof ex[k].value === 'bigint' && typeof wanted[k] !== 'function')
      wanted[k] = BigInt(wanted[k]);
    if (typeof wanted[k] === 'function' ? !wanted[k](got[k]) : got[k] !== wanted[k])
      matched = false;
  }
  assert(matched, "Globals did not match their expected values", got, "vs", wanted);
}

const testIO = ({ input: i, output: o, inputs: extra, test: func, outputs: extraMore=undefined }) => {
  // Zero out the memory
  for (const k in ex) {
    if (ex[k] instanceof WebAssembly.Memory)
      new Uint8Array(ex[k].buffer).fill(0);
  }
  let stop = {};
  // Set the input string (if set)
  if (i != null) {
    let bits = q2b(i);
    bits = bits.padEnd(roundUp(bits.length, 64), 0);
    let input = new BigUint64Array(ex.input.buffer, 0, roundUp(bits.length, 64) / 64);
    input.forEach((_, i) => {
      input[i] = BigInt("0b" + bits.substring(64*i, 64*i+64));
    });
    stop = { sptr: 8*(i.length>>5), sbit: 2 * (i.length % 32) };
    // console.log(i, bits, Array.from(input, x => x.toString(2).padStart(64, 0)));
  }
  // Set global variables
  extra = {...defaults, ...stop, ...(extra || {})};
  for (const k in extra) {
    ex[k].value = typeof ex[k].value === 'bigint' ? BigInt(extra[k]) : extra[k];
  }
  // Run the test function
  const r = func();
  tests++;
  // Assert the output string (if set)
  if (o != undefined) {
    assertOutput(o);
  }
  // Assert values of global variables
  if (extraMore) {
    assertGlobals(extraMore);
  }
  return r;
}

console.clear();
//const input = new Uint8Array(ex.input.buffer, 0, 30);
//input[0] = 0b00010100;
//console.log(new Uint16Array(ex.input.buffer, 0, 1)[0]);
//console.log(new Uint32Array(ex.input.buffer, 0, 1)[0]);

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
  for (const c of input) if (!(i++, expected += 2*!+c - 1)) return BigInt(2*i);
  return 0n;
}
function deltaExpecting(input) {
  return ex.deltaExpecting(BigInt("0b"+q2b(input).padEnd(64, 0)));
}
function deltaExpecting_impl(input) {
  let expected = 0;
  for (const c of input) expected += 2*!+c - 1;
  return BigInt(expected);
}
function redexes(leading, input) {
  if (input === undefined) {input=leading;leading=0}
  // convert crumbs to bits, shove it at the start of a word, calculate redexes, back to bits, place it at the end of the word, convert to crumbs, and then trim to the input length again
  return b2q(ex.redexes(BigInt(leading), BigInt("0b"+q2b(input).padEnd(64, 0))).toString(2).padStart(64, 0)).substring(0, input.length);
}

assert(deltaExpecting(balanced) === 0n);
assert(reachesZero(1, balanced) === 0n);
assert(deltaExpecting_impl(balanced) === 0n);
assert(reachesZero_impl(1, balanced) === 0n);

assert(reachesZero(1, "0") === 0n);
assert(reachesZero(1, "32") === 2n);
assert(reachesZero(1, "032") === 6n);
assert(reachesZero(1, "00132") === 10n);

function test_slowCopySubWord() {
  for (const lens of [[32], [16,16], [12], [6], [44], [26,24]])
  for (const len of [lens.reduce((x,y)=>x+y)])
  for (const iptr of [0])
  for (const ibit of [0, 4, 20, 32, 38, 50])
  for (const optr of [0, 8])
  for (const obit of [0, 2, 8, 18, 28, 40])
  if (len + ibit <= 64)
  {
    try {
      testIO({
        input: randomWord,
        inputs: {iptr, ibit, optr, obit},
        test: () => {
          for (const l of lens) ex.slowCopySubWord(BigInt(l));
        },
        output: repeat(4*optr + obit/2)+randomWord.substring(ibit/2, ibit/2 + len/2),
        outputs: {
          iptr: iptr + 8*((ibit + len)>>6),
          ibit: (ibit + len) % 64,
          optr: optr + 8*((obit + len)>>6),
          obit: (obit + len) % 64,
        },
      });
    } catch(e) {
      console.error("Locals", {len, iptr, ibit, optr, obit, randomWord});
      throw e;
    }
  }
}

function test_slowCopy1() {
  {
    try {
      testIO({
        input: "002311",
        output: "00231",
        inputs: {},
        test: () => {
          ex.slowCopy1();
        },
        outputs: {
          iptr: 0,
          ibit: 10,
          optr: 0,
          obit: 10,
          sptr: 0,
          sbit: 12,
        },
      });
      testIO({
        input: balanced+"002311",
        output: balanced+"00231",
        inputs: {},
        test: () => {
          ex.slowCopy1();
        },
        outputs: {
          iptr: 8,
          ibit: 10,
          optr: 8,
          obit: 10,
        },
      });
    } catch(e) {
      console.error("Locals", {randomWord});
      throw e;
    }
  }
}

Perform({test_slowCopySubWord});
Perform({test_slowCopy1});

//console.log(reachesZero(1, "23221021000000001020100300223303"));

//console.log(ex.redexes(64n, 0x10b5000000000000n).toString(16));

//console.log(redexes("002311"));
//console.log(redexes("010023110000"), BigInt("0b"+q2b("01002311").padEnd(64, 0)).toString(16));
//console.log(ex.redex(redexes("002311")));
//console.log(reachesZero(1, "00231")); // 10
//console.log(reachesZero(4, "11231")); // 8
//console.log(reachesZero(5, "33331")); // 10

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
  assert(deltaExpecting_impl(combinators[k]) === -1n, k, deltaExpecting_impl(combinators[k]));
  assert(reachesZero_impl(1, combinators[k]) === BigInt(combinators[k].length*2), k, reachesZero_impl(1, combinators[k]));
}
function isatomic(s) {
  return reachesZero_impl(1, s) === BigInt(s.length * 2);
}

function sugar(more) {
  const vars = {...combinators};
  for (const k in more||{}) {
    vars[k] = sugar(undefined)(more[k]);
    assert(isatomic(vars[k]), k + ' was not atomic', more[k], vars[k]);
  }
  return s => {
    s = s.replaceAll(/#[0-9]+/g, x => repeat(Number(x.substring(1)), '00SB')+'0SK');
    for (const k in vars) {
      s = s.replaceAll(k, vars[k]);
    }
    return s.replaceAll(/\(|\)/g, '');
  };
}

const _factorial = 'PPPPBMPPSPKPPSSPKMKPPSPKPPSPKPSPPSPPSPOPKPKPSKPKKPSKPSBPPSPKPPSSPKPPSPKPPSPKPSPKPPSSPKPSKPSPKPPSPKPPSSPKKKPPSPKPPSSPKPPSPKPPSPKPBTOKKKPPSBPPSBPPSBI';
console.log(sugar({})(_factorial));

function test_slowest() {
  function testcase(input, output, fuel=100, debug=false) {
    let o;
    input = sugar({})(input);
    output = sugar({})(output);
    const r = testIO({
      input, output, inputs: {fuel, debug},
      test: () => o = ex.slowest(BigInt(input.length)),
      outputs: {
        optr: 8 * (output.length>>5),
        obit: (2*output.length) % 64,
      },
    });
    assert(o === BigInt(output.length));
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
      testcase("0100231", "00231", 1);
      testcase("00231", "3", 1);
      testcase("3", "3", 2);
      testcase("0100231", "3", 3);
      testcase("0100231", "3", 30);
      reductions("000302113", "000213013", "01013", "013", "3");
      testcase('010000030030200302323022330030232', '0000030030200302323022330030232', 1);
      testcase('010000030030200302323022330030232', '00300302323', 11);
    } catch(e) {
      console.error("Locals", {});
      throw e;
    }
  }
  factorialTest:{
    break factorialTest;
    //ex.input.grow(5000)
    //ex.output.grow(5000)
    console.log(ex.input);
    input = sugar({})(_factorial);
    output = sugar({})(repeat(24, '00SB')+'0SK');
    testIO({
      input,
      output,
      inputs: {fuel: 1000, fuel_copy: 3000, fuel_scan: 3000},
      test: () => o = ex.slowest(BigInt(input.length)),
      outputs: {
        optr: 8 * (output.length>>5),
        obit: (2*output.length) % 64,
      },
    });
  }
  for (const m of ["3"])
  for (const n of ["3", "2", "032"])
  for (const o of ["3", "2", "1", "032", "021", "B"])
  {
    let test;
    try {
      const mno = sugar({m,n,o});
      const t = (s, fuel=30, debug=false) => {
        test = s;
        let [lhs, rhs] = mno(s).split(" = ");
        testcase(lhs, rhs, fuel, debug);
        testcase(lhs, rhs, fuel+1, debug);
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
      // mul 2 2 = 4
      t('0000*(00SBI)(00SBI)mn = 0m0m0m0mn', 25);
      // t('0000+(00SBI)(00SBI)mn = 0m0m0m0mn', 25);
    } catch(e) {
      console.error("Locals", {test,m,n,o});
      throw e;
    }
  }
}

Perform({test_slowest});

console.log(sugar()('#1'));
