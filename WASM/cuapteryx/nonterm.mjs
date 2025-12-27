import { repeat, s64, u64 } from "./base.mjs";
import { isatomic, toatomic, deltaExpecting_impl, eval_impl, q2B, q2I } from "./combinators.mjs";
import fs from 'node:fs/promises';

const smallest = '0000030333333';
const kernel = '00300303300300303333';
const m = '0300303300300303333';
const n = '000030330030030333303003033003003033330300303300300303333';
const f = '00303300300303333';

for (let last, e = '00'+f+'mn'; e != last; e = eval_impl(last = e, 1)) {
  console.log(e);
}
