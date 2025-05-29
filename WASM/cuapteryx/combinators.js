const {
  assert,
  roundUp,
  repeat,
} = require('./base.js');

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

function reachesZero_impl(expected, input) {
  expected = Number(expected);
  let i = 0;
  for (const c of input) if (!(i++, expected += 2*!+c - 1)) return (2*i);
  return 0;
}
function deltaExpecting_impl(input) {
  let expected = 0;
  for (const c of input) expected += 2*!+c - 1;
  return expected;
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
};


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
const factorialfn = 'PPPSPKPPSIIPPSPKPPSSPKPPSIIKPPSPKPPSPKPSPPSPPSPPSIPKPKPKIPKKPKIPSPPSPKSKPPSPKPPSSPKPPSPKPPSPKPSPKPPSSPKPKIPSPKPPSPKPPSSPKKKPPSPKPPSSPKPPSPKPPSPKPSPKPPSPKPSIKPSIKKK';
const factorial = n => '(' + 'P' + factorialfn + '(' + '#' + n + ')' + ')';
const factorial_calc = n => {let r=1; while(n) r*=n--; return r;};

const toNat = num => '00' + num + '0SB' + '0SK';


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



function eval_impl(crumbs, fuel=1000, strategy=undefined) {
  if (strategy === undefined) strategy = 0;
  crumbs = String(sugar(crumbs));
  let take1 = (i) => {
    let l = reachesZero_impl(1, crumbs.substring(i))/2;
    return crumbs.substring(i, i+l);
  };
  let seen = {};
  let reverted = 0;
  while (fuel--) {
    var I = crumbs.indexOf('01', reverted);
    var K = crumbs.indexOf('002', reverted);
    var S = crumbs.indexOf('0003', reverted);
    // console.log({I,K,S});
    if (I + K + S === -3) break;
    if (seen[crumbs]) {
      // console.log('loop', Object.keys(seen).length, Object.keys(seen));
      return crumbs;
    } else {
      seen[crumbs] = true;
    }
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

module.exports = {
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
  toNat,
  toCombinators,
  eval_impl,
};

