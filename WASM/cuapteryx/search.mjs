const { repeat } = await import("./base.mjs");
const { isatomic, deltaExpecting_impl, eval_impl } = await import("./combinators.mjs");
const fs = await import('node:fs/promises');

// console.log(
//   eval_impl('0000030333333', 1),
//   eval_impl('0000030333333', 2),
//   eval_impl('0000030333333', 3),
//   eval_impl('0000030333333', 10).length,
//   eval_impl('0000030333333', 100).length,
//   eval_impl('0000030333333', 120).length,
//   eval_impl('0000030333333', 140).length,
//   eval_impl('0000030333333', 160).length,
//   eval_impl('0000030333333', 180).length,
// );
// return;

const isSearchable = crumbs => {
  crumbs = String(crumbs);
  if (crumbs.length % 2 == 0) return false;
  // No trivial reductions
  if (crumbs.includes('01') || crumbs.includes('002')) {
    return false;
  }
  // But it needs to be reducible!
  if (!crumbs.includes('0003')) return false;
  // Optimizable strings
  if (crumbs.includes('032') || crumbs.includes('0003111') || crumbs.includes('00003112') || crumbs.includes('00003121')) {
    return false;
  }
  return isatomic(crumbs);
};

let searchable = 0 && [];
let i=1;

(await fs.open('search.state', 'a+')).close();
let state = await fs.open('search.state', 'r+');
{
let red = await state.readFile('utf-8');
if (red) [i, searchable] = JSON.parse(red);
console.log([i, searchable]);
}

for (i; i <= 2**44; i++) {
  let crumbs = i.toString(4);
  crumbs = repeat(-1-deltaExpecting_impl(crumbs)) + crumbs;
  if (isSearchable(crumbs)) {
    Array.isArray(searchable) ? searchable.push(crumbs) : searchable += 1;
    if ((i % (1 + 7*80)) == 0) {
      await state.write(JSON.stringify([i+1, searchable]), 0);
    }
  }
  if ((i & (i - 1)) == 0 && Math.log2(i) % 2 == 0) {
    console.log(
      Math.log2(i)+':',
      Array.isArray(searchable) ? searchable.length : searchable,
      '/', i,
      (Array.isArray(searchable) ? searchable.length : searchable) / i
    );
    if (i > 2**12 && i < 2**30 && Array.isArray(searchable)) {
      await fs.writeFile('search.json', JSON.stringify(searchable, undefined, 2), 'utf-8');
    }
  }
}
// console.log(searchable);
