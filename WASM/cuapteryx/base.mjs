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

// Round `l` up to the next multiple of `amt`
const roundUp = (l, amt=64) => l + (amt - l%amt)%amt;

// Helper to repeat characters
const repeat = (n, fill="0") => n > 0 ? [...Array(n)].map((_,i) => typeof fill === 'function' ? fill(i) : fill).join("") : "";

// Test the performance of a function
const perform = (label, f, ...args) => {
  tests = 0;
  const start = performance.now();
  const r = f(...args);
  const end = performance.now();
  if (true) console.debug(`${label}: ${(end - start).toFixed(2)}ms x${tests}`);
  return r;
};
let tests = 0;
const performed1 = () => tests++;
// Test the performance of an object of functions, passing the
// same arguments to each.
const Perform = (obj, ...args) => {
  const r = {};
  for (const k in obj) r[k] = perform(k, obj[k], ...args);
  return r;
};


export {
  assert,
  asserteq,
  u64,
  repeat,
  roundUp,
  perform,
  performed1,
  Perform,
};
