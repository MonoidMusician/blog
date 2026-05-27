const as_f64 = new Float64Array(1);
const as_s64 = new BigInt64Array(as_f64.buffer);

export const doubleToBigInt = (/** @type number */ v) => {as_f64[0] = Number(v); return as_s64[0]};
export const bigIntToDouble = (/** @type number | BigInt */ v) => {as_s64[0] = BigInt(v); return as_f64[0]};

export const nudgeBy = (/** @type number */ v) => (/** @type number | BigInt */ amt) => {
  if (!amt) return v;
  amt = BigInt(amt);
  if (v === 0.0) v = amt > 0n ? 0.0 : -0.0;
  const n = doubleToBigInt(v);
  return bigIntToDouble(n + (n >= 0 ? amt : -amt));
};

const as_f32 = new Float32Array(1);
const as_s32 = new Int32Array(as_f32.buffer);

export const floatToInt = (/** @type number */ v) => {as_f32[0] = Number(v); return as_s32[0]};
export const intToFloat = (/** @type number */ v) => {as_s32[0] = Number(v); return as_f32[0]};

export const nudgeBy32 = (/** @type number */ v) => (/** @type number */ amt) => {
  if (!amt) return v;
  amt = Number(amt);
  if (v === 0.0) v = amt > 0n ? 0.0 : -0.0;
  const n = floatToInt(v);
  return intToFloat(n + (n >= 0 ? amt : -amt));
};
