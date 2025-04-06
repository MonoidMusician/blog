const wasmInstance = new WebAssembly.Instance(wasmModule, {});
const ex = wasmInstance.exports;
const { memory } = ex;

const mem = (i,l) => new Uint8Array(memory.buffer, i, l);
const hex = x=>x.toString(16).padStart(2,0);
const h0x = x=>"0x"+hex(x);
const dbg = mem => Array.from(mem, h0x).join(" ");
const dbgmem = (i,l) => dbg(mem(i,l));

const assert = (c, msg='Assertion failed') => {if (!c) throw new Error(msg)};

console.log(dbgmem(0,12));

const nat = ([alloc, limbs, ptr]) => {
  assert(alloc >= limbs, 'alloc >= limbs');
  const str = "0x00"+Array.from(mem(ptr, limbs*4), hex).reverse().join("");
  console.log(str, '@', [alloc, limbs, ptr]);
  return BigInt(str);
};
for (const k in ex) if (k.startsWith("nat_")) nat[k.slice(4)] = ex[k];
console.log('nat.one:', nat(nat.one()));
assert(nat(nat.one()) === 1n);

console.log(nat(nat.bump(0xFFFFFFFE, ...nat.one())));
console.log(nat(nat.bump(0xFFFEEFFF, 1, 1, 0)))
console.log(dbgmem(0,16));
