const fs = await import('node:fs');
const {
  assert,
  asserteq,
  u64,
  repeat,
  roundUp,
  perform,
  Perform,
  performed1,
} = await import('./base.mjs');
const {
  q2b,
  b2q,
  prettyCrumbs,
  reachesZero_impl,
  deltaExpecting_impl,
  combinators,
  isatomic,
  sugar,
  factorialfn,
  factorial,
  factorial_calc,
  asNat,
  toCombinators,
  eval_impl,
} = await import('./combinators.mjs');

const THIS = import.meta.filename.replace('.node.mjs', '');
const wasmBuffer = fs.readFileSync(THIS+'.wasm');
let wasmInstance, ex;

var defaults = {
  "fuel": 10000,
};



const getOutput = (startOrLen=undefined, len=undefined) => {
  var start = 0;
  if (len === undefined) {
    len = startOrLen;
  } else {
    start = startOrLen || 0;
  }
  let output = new BigUint64Array(ex.memory.buffer, Number(ex.output_words.value) + start, len && roundUp(len*2, 64) / 64);
  let outputCrumbs = b2q(Array.from(output, bigint => bigint.toString(2).padStart(64, 0)).join("")).substring(0, len);
  return outputCrumbs;
}

function assertOutput(crumbs) {
  if (typeof crumbs === 'function') return crumbs(getOutput);
  let outputCrumbs = getOutput(crumbs.length);
  assert(outputCrumbs === crumbs, "Output did not match expected", prettyCrumbs(outputCrumbs), "vs", prettyCrumbs(crumbs));
}


// Random quaternary word of the day
const randomWord = [...Array(32)].map(_ => Math.max(0,Math.floor(6*Math.random() - 2))).join("");
const balanced = "00200033000210000331132000323102";


function reachesZero(expected, input) {
  return ex.reachesZero(BigInt(expected), BigInt("0b"+q2b(input).padEnd(64, 0)));
}
function deltaExpecting(input) {
  return ex.deltaExpecting(BigInt("0b"+q2b(input).padEnd(64, 0)));
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
    let input = new BigUint64Array(ex.memory.buffer, Number(ex.input_words.value), roundUp(bits.length, 64) / 64);
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
        new BigUint64Array(ex.memory.buffer, Number(ex.fuel), 1)[0] = BigInt(value);
      },
    })[k]?.(extra[k]);
  }
  // Run the test function
  const r = func();
  performed1();
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



function test_eval() {
  function testcase(input, output, fuel=100, debug=false) {
    debugging && console.log();
    let o, r;
    input = sugar(input);
    output = sugar(output);
    let by_impl = eval_impl(input, fuel);
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
        console.error(getOutput(Number(o)));
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
      testcase("00231", "3", 1);
      testcase("0100231", "3", 2);
      testcase("3", "3", 1);
      testcase("033", "033", 1);
      testcase("0100231", "00231", 1);
      testcase("00231", "3", 2);
      testcase("3", "3", 2);
      testcase("0100231", "3", 4);
      testcase("0100231", "3", 30);
      testcase('000022330030232', '3', 25);
      testcase('03000022330030232', '033', 25);
      testcase('0030030232000022330030232', '00300302323', 25);
      // reductions("000302113", "000213013", "01013", "013", "3");
      testcase('010000030030200302323022330030232', '0000030030200302323022330030232', 1);
      testcase('00210000030030200302323022330030232', '1', 1);
      testcase('010000030030200302323022330030232', '00300302323', 25);
    } catch(e) {
      console.error("Locals", {});
      throw e;
    }
  }

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
    // let by_impl = eval_impl(input, fuel);
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
        console.error(getOutput(Number(o)));
        console.error(o+':', prettyCrumbs(getOutput(Number(o))));
        console.error('vs'.padEnd((o+':').length, ' '), prettyCrumbs(output));
      }
      throw e;
    }
    return r;
  }


  for (const x of fixed_x !== undefined ? [fixed_x] : [0, 1, 2, 3])
  if (x <= 6)
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
      t(`00${fx}no = ${succs}o`, [1000,1000,10000,10000,100000,1000000,10000000,100000000,1000000000][x])
      // if (x === 8) console.error(mno(`00${fx}no = ${succs}o`));
      // Only do one test of x=6
      if (x >= 6) return;
    } catch(e) {
      console.error("Locals", {test,desugared,x,n,o});
      throw e;
    }
  }

  for (const x of fixed_x !== undefined ? [fixed_x] : [0, 1, 2, 3])
  {
    let test, desugared;
    try {
      const mno = sugar({});
      const t = (s, fuel=30, debug=false) => {
        test = s; desugared = mno(s);
        let [lhs, rhs] = mno(s).split("=");
        testcase(lhs, rhs, fuel, debug);
      };
      // console.error(x, sugar(`${asNat(factorial(x))} = ${sugar(+factorial_calc(x))}`));
      t(`${asNat(factorial(x))} = ${sugar(+factorial_calc(x))}`, 123248203829023423n)
    } catch(e) {
      console.error("Locals", {test,desugared,x});
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
  // console.log('max_output:', new Uint32Array(ex.memory.buffer, ex.max_output, 1)[0]);
  Perform({
    test_factorial0: () => test_factorial(0),
    test_factorial1: () => test_factorial(1),
    test_factorial2: () => test_factorial(2),
    test_factorial3: () => test_factorial(3),
    test_factorial4: () => test_factorial(4),
    test_factorial5: () => test_factorial(5),
    test_factorial6: () => test_factorial(6),
    test_factorial7: () => test_factorial(7),
    test_factorial8: () => test_factorial(8),
  });

  // console.log(sugar()('#1'));
  // console.log('max_output:', new Uint32Array(ex.memory.buffer, ex.max_output, 1)[0]);
});
