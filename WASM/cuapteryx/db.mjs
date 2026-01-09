import { isatomic, toatomic, deltaExpecting_impl, eval_impl, q2B, q2I, I2q, b2q, q2b, reachesZero_impl, B2q, q2W } from "./combinators.mjs";
import fs from 'node:fs/promises';
import { DatabaseSync } from 'node:sqlite';
const {
  assert,
  asserteq,
  u64,
  s64,
  repeat,
  roundUp,
  perform,
  Perform,
  performed1,
} = await import('./base.mjs');


const toDB = (q) => BigInt("0b" + q2b(toatomic(q, true)));

const toSearchable = crumbs => {
  if (typeof crumbs !== 'string') crumbs = crumbs.toString(4);
  let nonzeros = 0;
  for (const c of crumbs) if (c !== '0') nonzeros += 1;
  crumbs = crumbs.padStart(2 * nonzeros - 1, 0);
  return crumbs;
};
const re0 = /^0*[12](.*)$/;
const re1 = /(01|002|032|0003111|00003112|00003121)(.*)$/;
const re2 = /0+$/;
// The smallest *end* of a valid word, n*'0' + '3' + n*'1', with an extra '1' if needed
const seed = replacing => {
  if (!replacing) return '';
  if (replacing.length === 1) return '1';
  if (replacing.length === 2) return '11';
  const n = (replacing.length - 2 - replacing.length%2) / 2;
  return repeat(n + replacing.length % 2, '0') + '3' + repeat(n + 1, '1');
};
const bump = (/** @type string */ candidate) => {
  const original = candidate;
  const len = candidate.length;
  // Make sure it starts with 3
  candidate = candidate.replace(re0, (_match, tail) => {
    return toSearchable('3' + seed(tail));
  });
  if (candidate !== original) return candidate;
  // Bump up uninteresting substrings, to just keep with numeric order
  // Seed the rest of the string with the minimal viable substring
  candidate = candidate.replace(re1, (_match, /** @type string */ bumping, remaining) => {
    const bumped = { '01': '02', '002': '003', '032': '033', '0003111': '0003112', '00003112': '00003113', '00003121': '00003122' }[bumping];
    return bumped + seed(remaining);
  });
  // Replace trailing zeros with something more interesting
  // (Folded into above step)
  if (candidate.at(-1) === '0') {
    candidate = candidate.replace(re2, seed);
  }
  if (candidate.length !== len) throw new Error(`${original} -> ${candidate}`);
  if (candidate === original) return;
  while (candidate[0] === '0') candidate = candidate.substring(1);
  return toSearchable(candidate);
};
const nextSearchable = (ordinal=undefined, crumbs=undefined) => {
  if (crumbs === undefined) crumbs = toSearchable(ordinal);
  const bumped = bump(crumbs);
  if (bumped) return [false, undefined, bumped];
  if (ordinal === undefined) ordinal = BigInt("0b" + q2b(crumbs));
  const zeropoint = reachesZero_impl(1, crumbs);
  // Check if it is an atomic expression
  if (zeropoint !== (crumbs.length * 2)) {
    // console.log({ zeropoint, crumbs, ordinal });
    return [false, ordinal+1n, undefined];
  }
  // Needs to be reducible to evaluate
  if (!crumbs.includes('0003')) {
    return [false, ordinal+1n, undefined];
  }
  return [true, ordinal, crumbs];
};
// {
//   let ordinal = 1n, crumbs = undefined;
//   let dry = 0, maxDry = 8, maxDryAt = 0;
//   while (ordinal === undefined || ordinal < 50150669n) {
//     let next = nextSearchable(ordinal, crumbs);
//     if (!next) break;
//     // console.log(next[0] ? 'Y' : 'N', next[1] ?? BigInt("0b" + q2b(next[2])), next[2] ?? toSearchable(next[1]));
//     if (!next[0]) dry += 1; else {
//       console.log(next[1], next[2]);
//       if (dry > maxDry) {
//         maxDry = dry;
//         maxDryAt = next[1] ?? BigInt("0b" + q2b(next[2]));
//       }
//       dry = 0;
//     }
//     [, ordinal, crumbs] = next[0] ? [false, next[1]+1n, undefined] : next;
//   }
//   console.log({ maxDry, maxDryAt });
// }
// for (let desired = 5; desired < 25; desired += 2) {
//   const bound = 4**desired;
//   let i = 4**((desired-1)/2);
//   for (; i < bound; i++) {
//     const q = I2q(i).padStart(desired, 0);
//     if (isatomic(q)) { console.log(desired, q, i); break }
//   }
//   for (; i < bound; i++) {
//     const q = "03"+I2q(i).padStart(desired, 0);
//     if (isSearchable(q)) { console.log(desired, q, i); break }
//   }
// }
// for (let i=200; i < 300; i++) {
//   const crumbs = i.toString(4);
//   const search = toSearchable(crumbs);
//   console.log(isSearchable(search) && !isatomic(search), crumbs, search, isatomic(search));
// }

const STATUSES = {
  'timeout': -1,
  'out of fuel': -2,
  'unknown error': -3,
  'RangeError: Maximum call stack size exceeded': -4,
  'RuntimeError: memory access out of bounds': -5,
};

let db = new DatabaseSync([':memory:', 'db.test.sqlite', 'db.sqlite'][2]);

db.exec(`
  CREATE TABLE
  IF NOT EXISTS
    Evaluations
  (
    input INTEGER PRIMARY KEY NOT NULL,
    output_length INTEGER NOT NULL,
    output BLOB,
    fuel_used INTEGER,
    time REAL
  )
  STRICT
  ;
  PRAGMA journal_mode = WAL;
  PRAGMA synchronous = OFF;
`);

let _insert = db.prepare(`
  INSERT INTO
    Evaluations
    (input, output_length, output, fuel_used, time)
  VALUES
    (?, ?, ?, ?, ?)
`);
let insert = ({
  /** @type {BigInt} */ input,
  /** @type {BigInt} */ output_length,
  /** @type {ArrayBuffer | ArrayBufferView | null} */ output=null,
  /** @type {number | null} */ fuel_used=null,
  /** @type {number | null} */ time=null,
}) => {
  if (output instanceof ArrayBuffer) output = new DataView(output);
  input = toDB(input);
  if (input >= (1n << 63n)) input = s64(input);
  _insert.run(input, output_length, output, fuel_used, time);
};

let _query = db.prepare(`
  SELECT * FROM
    Evaluations
  where
    input = ?
`);
_query.setReadBigInts(true);
let query = (input) => {
  input = toDB(input);
  if (input >= (1n << 63n)) input = s64(input);
  return readback(_query.get(input));
};
let readback = (/** @type {{ input: BigInt, output_length: BigInt, output: Uint8Array | null, fuel_used: number | null, time: number | null } | undefined} */ queried) => {
  if (!queried) return queried;
  queried.input = u64(queried.input);
  queried.input_crumbs = I2q(queried.input);
  queried.output = queried.output && new BigUint64Array(queried.output.buffer);
  queried.output_crumbs = queried.output_length > 0 && queried.output ? B2q(queried.output_length, queried.output) : undefined;
  queried.fuel_used = queried.fuel_used && Number(queried.fuel_used);
  return queried;
};
let _getAll = db.prepare(`
  SELECT * FROM
    Evaluations
`);
_getAll.setReadBigInts(true);
let getAll = () => {
  return _getAll.all().map(readback);
};
let _getSuccesses = db.prepare(`
  SELECT * FROM
    Evaluations
  WHERE
    output_length > 0
`);
_getSuccesses.setReadBigInts(true);
let getSuccesses = () => {
  return _getSuccesses.all().map(readback);
};
let _getLast = db.prepare(`
  SELECT ( input ) FROM
    Evaluations
  ORDER BY
    input DESC
  LIMIT
    1
`);
_getLast.setReadBigInts(true);
let getLast = () => {
  return _getLast.get()?.input;
};

const littleEndian = true;

// console.log('last', getLast());
// insert({ input: q2I(toatomic(1n)), output_length: 1n, output: q2B(toatomic(1n)), fuel_used: 0, time: 0.1});
// insert({ input: q2I(toatomic(2n)), output_length: -3n });
// console.log('last', getLast());

// console.log('query1', query(1n));
// console.log('query2', query(2n));
// console.log('query3', query(3n));
// console.log('all', getAll());

// console.log(q2I(toatomic(2n))); // 9223372036854775808n
// console.log(toatomic(2n)); // 2n
// console.log(toDB("23")); // 11n

const wasmBuffer = await fs.readFile('quapteryx.wasm');

const wasmModule = await WebAssembly.compile(wasmBuffer);
let wasmInstance, ex, inputBuffer, fuelCell;
const reload = async () => {
  wasmInstance = await WebAssembly.instantiate(wasmModule, {
    env: {
      dbg: (x,y) => {},
      print64: (str_ptr, value) => {},
      dent: value => {},
      trace: (ptr, c_str1, c_str2, ...arg) => {},
      epoch: (c_str1, ...arg) => {},
    },
  });
  ex = wasmInstance.exports;
  inputBuffer = new BigUint64Array(ex.memory.buffer, Number(ex.input_words.value), 1);
  fuelCell = new BigUint64Array(ex.memory.buffer, Number(ex.fuel), 1);

  // force it to initialize
  const test = "0000003333111";
  inputBuffer[0] = q2W(test);
  ex.eval(BigInt(test.length));
};
await reload();
function reachesZero(expected, input) {
  return ex.reachesZero(BigInt(expected), BigInt("0b"+q2b(input).padEnd(64, 0)));
}
function deltaExpecting(input) {
  return ex.deltaExpecting(BigInt("0b"+q2b(input).padEnd(64, 0)));
}
const getOutput = (len) => {
  len = Number(len);
  let output = new BigUint64Array(ex.memory.buffer, Number(ex.output_words.value), roundUp(len, 32) / 32);
  let output_crumbs = B2q(len, output);
  return { output_crumbs, output };
};
const setInput = (wordcrumbs) => {
  while (wordcrumbs[0] === '0') {
    wordcrumbs = wordcrumbs.substring(1);
  }
  inputBuffer[0] = q2W(wordcrumbs);
  return wordcrumbs.length;
};
let maxFuelUsed = [0], maxTimeTaken = [0];
const evalWord = async (word, fuel=1_000_000) => {
  let wordcrumbs = toSearchable(word);
  // console.log(word, wordcrumbs, q2W(wordcrumbs), I2q(q2W(wordcrumbs)));
  const inputLen = setInput(wordcrumbs);
  if (!inputLen) return;
  fuelCell[0] = BigInt(fuel);
  let start = performance.now(), end, time;
  let outputLen;
  try {
    outputLen = Number(ex.eval(BigInt(inputLen)));
  } catch (e) {
    end = performance.now();
    time = end - start;
    const status = STATUSES[`${e.name}: ${e.message}`];
    await reload();
    if (status) return { output_length: status, time };
    console.error(word, e);
  }
  if (!end) {
    end = performance.now();
    time = end - start;
  }
  const fuelLeft = fuelCell[0];
  if (!fuelLeft) {
    await reload(); // ??
    return { output_length: STATUSES['out of fuel'], fuel_used: Number(fuel), time };
  }
  if (!outputLen) {
    console.log({ outputLen, word, ordinal: q2I(wordcrumbs), wordcrumbs, inputLen, fuelLeft, time, W: q2W(wordcrumbs), buf: inputBuffer[0], eval: ex.eval(BigInt(inputLen)) });
    return "no output??";
  }
  const output = getOutput(outputLen);
  const fuelUsed = Number(fuel) - Number(fuelLeft);
  if (fuelUsed > maxFuelUsed[0]) maxFuelUsed = [fuelUsed, wordcrumbs, output, time];
  if (time > maxTimeTaken[0]) maxTimeTaken = [time, wordcrumbs, output, fuelUsed];
  return { output_length: outputLen, ...output, fuel_used: fuelUsed, time };
};

{
const transactEnd = db.prepare(`COMMIT;`);
const transactStart = db.prepare(`BEGIN TRANSACTION;`);
transactStart.run();
let nth = 0, last;
let start = (getLast() ?? 0n) + 1n;
let bound = 150000n;
if (start > 150000n) bound = start * 18n;
if (bound > 2n**32n) bound = 2n**32n;
bound = 2n**34n;

let searchable, ordinal = start, crumbs = undefined;
while (ordinal === undefined || ordinal < bound) {
  if (searchable) { ordinal += 1n; crumbs = undefined; }
  let next = nextSearchable(ordinal, crumbs);

  [searchable, ordinal, crumbs] = next;

  if (searchable) {
    let out;
    try {
      out = await evalWord(crumbs, 90_000n);
      try {
        insert({ ...out, input: ordinal });
      } catch(e) {
        console.log(ordinal, crumbs, out, e);
        break;
      }
      if (!out?.output) continue;
    } catch (e) {
      console.log(ordinal, crumbs, e);
      out = e;
    }

    const chosen = !(nth++ % 500) || crumbs === '0000030333333';
    last = () => console.log((100 * Number(ordinal - start) / Number(bound - start)).toFixed(2), ordinal, crumbs, q2W(toatomic(ordinal)), out);
    if (chosen) {
      last();
      last = ()=>{};
      if (db.isTransaction) transactEnd.run();
      transactStart.run();
    }
  }
}
if (db.isTransaction) transactEnd.run();
last?.();
//const result = { all: getAll(), successes: getSuccesses() };
//console.log(result.all.length, result.successes.length);
//console.log(result.all.slice(-2), result.successes.slice(-5));
//console.log({ maxFuelUsed, maxTimeTaken });
}
