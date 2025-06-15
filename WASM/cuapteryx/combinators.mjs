import {
  assert,
  roundUp,
  repeat,
} from './base.mjs';

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

// Based on https://www.angelfire.com/tx4/cus/combinator/birds.html
const combinatorTable = {
  '_1': "Primitives",
  'S': {
    name: "Substitution",
    bird: "Starling",
    3: 'ac(bc)',
    value: '3', def: 'S',
  },
  'K': {
    name: "Konstant",
    bird: "Kestrel",
    2: 'a',
    value: '2', def: 'K',
  },
  'I': {
    name: "Identity",
    bird: "Ibis",
    1: 'a',
    value: '1', def: 'I',
  },
  'P': {
    name: "Prefix application",
    value: '0',
  },
  '`': {
    name: "Prefix application à\u00A0la Unlambda",
    value: '0',
  },

  '_2': "Basic combinators",
  'KI': {
    bird: "Kite",
    2: 'b',
    value: '(KI)', def: 'KI',
  },
  'B': {
    bird: "Bluebird",
    3: 'a(bc)',
    value: '((S(KS))K)',
  },
  'C': {
    name: "Converse",
    bird: "Cardinal",
    3: 'acb',
    def: 'S(BBS)(KK)',
    value: '((S((S(K((S(KS))K)))S))(KK))',
  },
  'D': {
    bird: "Dove",
    4: 'ab(cd)',
    def: 'BB',
    value: '(S(K((S(KS))K)))',
  },
  'E': {
    bird: 'Eagle',
    5: 'ab(cde)',
    def: 'B(BBB)',
    value: '(S(K((S(K((S(KS))K)))((S(KS))K))))',
  },
  'F': {
    bird: "Finch",
    3: 'cba',
    def: 'ETTET',
    value: '((S(K((S(K(S((S(K((S(KS))(SI))))K))))K)))K)',
  },
  'G': {
    bird: "Goldfinch",
    4: 'ad(bc)',
    def: 'BBC',
    value: '((S(K((S(KS))K)))((S((S(K((S(KS))K)))S))(KK)))',
  },
  'H': {
    bird: "Hummingbird",
    3: 'abcb',
    def: 'BW(BC)',
    value: '((S(K((SS)(KK))))(S(KS)))',
  },
  'J': {
    bird: "Jay",
    4: 'ab(adc)',
    def: 'B(BC)(W(BC(B(BBB))))',
    value: '((S((S(KS))(S(K((S(K((S(K((S(KS))K)))S)))K)))))((S(KK))((S(K((S(K((SS)(KK))))K)))S)))',
  },
  'L': {
    bird: "Lark",
    2: 'a(bb)',
    def: 'CBM',
    value: '(S(K((SS)(K((SI)I)))))',
  },
  'M': {
    bird: "Mockingbird",
    1: 'aa',
    def: 'SII',
    value: '((SI)I)',
  },
  'O': {
    bird: "Owl",
    2: 'b(ab)',
    def: 'SI',
    value: '(SI)',
  },
  'Q': {
    bird: "Queer Bird",
    3: 'b(ac)',
    def: 'CB',
    value: '((S(K(S((S(KS))K))))K)',
  },
  'R': {
    bird: "Robin",
    3: 'bca',
    def: 'BBT',
    value: '((S(K((S(KS))K)))((S(K(SI)))K))',
  },
  'T': {
    bird: "Thrush",
    2: 'ba',
    def: 'CI',
    value: '((S(K(SI)))K)',
  },
  'U': {
    bird: "Turing",
    2: 'b(aab)',
    def: 'LO',
    value: '((S(K(SI)))((SI)I))',
  },
  'V': {
    bird: "Vireo",
    3: 'cab',
    def: 'BCT',
    value: '((S(K((S(K((S(K((S(K((SS)(KK))))K)))S)))(SI))))K)',
    // ((S((SI)(Ka)))(Kb))
  },
  'W': {
    bird: "Warbler",
    2: 'abb',
    def: 'C(BMR)',
    value: '((SS)(SK))',
  },

  '_3': "Church encoding of numbers",
  '$succ': {
    name: "Church numeral successor",
    3: 'b(abc)',
    value: '(S((S(KS))K))', def: 'SB',
  },
  '#0': {
    name: "Canonical Church numeral 0",
    2: 'b',
    value: '(KI)', def: 'KI',
  },
  '#1': {
    name: "Canonical Church numeral 1",
    2: 'ab',
    value: '((S((S(KS))K))(KI))', def: 'SB(#0)',
  },
  '#2': {
    name: "Canonical Church numeral 2",
    2: 'a(ab)',
    value: '((S((S(KS))K))((S((S(KS))K))(KI)))', def: 'SB(#1)',
  },
  '*': {
    name: "Church multiplication",
    3: 'a(bc)',
    value: '((S(KS))K)', def: 'B',
  },
  '+': {
    name: "Church addition",
    4: 'ac(bcd)',
    value: '((S(KS))(S(K((S(KS))K))))', def: 'S(KS)D',
  },
  "$isZero": {
    value: '((S((SI)(K(K(KI)))))(KK))',
    def: 'V(K⊥)⊤', // 'V(K(KI))K'
  },
  "$factorial": {
    name: "Factorial of a Church number",
    def: '',
    value: '(((S(K((SI)I)))((S(K((SS)(K((SI)I)))))K))((S(K((S(K(S((S((S((SI)(K(K(KI)))))(KK)))(KI)))))(S((S(KS))K)))))((S(K((SS)(K((S(K((S(K(S(K((SS)(K(KI)))))))(S(K((S(K((SS)(KK))))K))))))((S(K((SS)(K((S(K((S(K(S(K((S(K(SI)))K)))))(SI))))K)))))K))))))K)))',
  },
  "$asNat": {
    name: "Canonicalize a Church numeral",
    def: 'V($succ)(#0)',
    value: '((S((SI)(K(S((S(KS))K)))))(K(KI)))',
  },

  '_4': 'Church encoding of Booleans',
  '⊤': {
    name: "True",
    2: 'a',
    value: 'K', def: 'K',
  },
  '⊥': {
    name: "False",
    2: 'b',
    value: '(KI)', def: 'KI',
  },
  '$if': {
    name: "Conditional operator",
    3: 'abc',
    value: 'I',
  },
  '$not': {
    name: "Boolean negation",
    3: 'acb',
    value: '((S((S(K((S(KS))K)))S))(KK))', def: 'C',
  },
  '&&': {
    name: "And",
    2: 'aab',
    value: '((SI)I)', def: 'SII',
  },
  '||': {
    name: "Or",
    2: 'aba',
    value: '((SS)K)', def: 'SSK',
  },
  '$asBool': {
    name: "Canonicalize a Church Boolean",
    1: 'a⊤⊥',
    def: 'V⊤⊥',
    value: '((S((SI)(KK)))(K(KI)))'
  },

  '_5': "Extended combinators",
  'Ê': {
    bird: "Bald Eagle",
    7: 'a(bcd)(efg)',
    def: 'B(BBB)(B(BBB))',
    value: '((S(K((S(K((S(KS))K)))((S(KS))K))))(S(K((S(K((S(KS))K)))((S(KS))K)))))',
  },
  'B1': {
    bird: "Blackbird",
    4: 'a(bcd)',
    def: 'BBB',
    value: '((S(K((S(KS))K)))((S(KS))K))',
  },
  'B2': {
    bird: "Bunting",
    5: 'a(bcde)',
    def: 'B(BBB)B',
    value: '((S(K((S(K((S(KS))K)))((S(KS))K))))((S(KS))K))',
  },
  'B3': {
    bird: "Becard",
    4: 'a(b(cd))',
    def: 'B(BB)B',
    value: '((S(K((S(K((S(KS))K)))((S(KS))K))))((S(KS))K))',
  },
  'D1': {
    bird: "Dickcissel",
    5: 'abc(de)',
    def: 'B(BB)',
    value: '(S(K(S(K((S(KS))K)))))',
  },
  'D2': {
    bird: "Dovekies",
    5: 'a(bc)(de)',
    def: 'BB(BB)',
    value: '((S(K((S(KS))K)))(S(K((S(KS))K))))',
  },
  'M2': {
    bird: "Double Mockingbird",
    2: 'ab(ab)',
    def: 'BM',
    value: '(S(K((SI)I)))',
  },
  'Q1': {
    bird: "Quixotic Bird",
    3: 'a(bc)',
    def: 'BCB',
    value: '((S(K((S((S(K((S(KS))K)))S))(KK))))((S(KS))K))',
  },
  'Q2': {
    bird: "Quizzical Bird",
    3: 'b(ca)',
    def: 'C(BCB)',
    value: '((S(K(S((S(K((S((S(K((S(KS))K)))S))(KK))))((S(KS))K)))))K)',
  },
  'Q3': {
    bird: "Quirky Bird",
    3: 'c(ab)',
    def: 'BT',
    value: '(S(K((S(K(SI)))K)))',
  },
  'Q4': {
    bird: "Quacky Bird",
    3: 'c(ba)',
    def: 'F*B',
    value: '((S(K((S(K(S(K((S(K(SI)))K)))))(SI))))K)',
  },
  'W1': {
    bird: "Converse Warbler",
    2: 'baa',
    def: 'CW',
    value: '((S((S(K((S(KS))(SI))))K))K)',
  },
  'I*': {
    bird: "Ibis Once Removed",
    2: 'ab',
    def: 'BI',
    value: '(S(KI))',
  },
  'W*': {
    bird: "Warbler Once Removed",
    3: 'abcc',
    def: 'BW',
    value: '(S(K((SS)(KI))))',
  },
  'C*': {
    bird: "Cardinal Once Removed",
    4: 'abdc',
    def: 'BC',
    value: '(S(K((S((S(K((S(KS))K)))S))(KK))))',
  },
  'R*': {
    bird: "Robin Once Removed",
    4: 'acdb',
    def: 'C*C*',
    value: '((S(K((S(K((S(K((SS)(K((S(KK))K)))))K)))S)))(S(KS)))',
  },
  'F*': {
    bird: "Finch Once Removed",
    4: 'adcb',
    def: 'BC*R*',
    value: '((S(K((S(K((S(K((S(K((SS)(K((S(KK))K)))))K)))S)))(S(KS)))))((S(K((S(K((SS)(KK))))K)))S))',
  },
  'V*': {
    bird: "Vireo Once Removed",
    4: 'acbd',
    def: 'C*F*',
    value: '((S(K((S(K((SS)(KK))))K)))S)',
  },
  'I**': {
    bird: "Ibis Twice Removed",
    3: 'abc',
    def: 'B(BI)',
    value: '(S(K(S(KI))))',
  },
  'W**': {
    bird: "Warbler Twice Removed",
    4: 'abcdd',
    def: 'B(BW)',
    value: '(S(K(S(K((SS)(KI))))))',
  },
  'C**': {
    bird: "Cardinal Twice Removed",
    5: 'abced',
    def: 'BC*',
    value: '(S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))',
  },
  'R**': {
    bird: "Robin Twice Removed",
    5: 'abdec',
    def: 'BR*',
    value: '(S(K((S(K((S(K((S(K((SS)(K((S(KK))K)))))K)))S)))(S(KS)))))',
  },
  'F**': {
    bird: "Finch Twice Removed",
    5: 'abedc',
    def: 'BF*',
    value: '((S(K(S(K((S(K((S(K((S(K((SS)(K((S(KK))K)))))K)))S)))(S(KS)))))))(S(K((S(K((S(K((SS)(KK))))K)))S))))',
  },
  'V**': {
    bird: "Vireo Twice Removed",
    5: 'abecd',
    def: 'BV*',
    value: '((S(K(S(K(S(K((S(K((S(K((SS)(KK))))K)))S)))))))(S(K((S(K((S(K((SS)(KK))))K)))S))))',
  },

  '_6': "Improper combinators",
  'ι': {
    name: "Iota (single-combinator basis)",
    1: 'aSK',
    def: 'VSK',
    value: '((S((SI)(KS)))(KK))',
  },
  'Y': {
    name: "Fixed-point combinator",
    bird: "Why Bird (aka Sage Bird)",
    1: 'a(Ya)',
    def: 'SLL',
    value: '(((SS)K)((S(K((SS)(S((SS)K)))))K)',
  },
  'Θ': {
    name: "Theta (Turing fixed-point combinator)",
    1: 'a(Θa)',
    def: 'U(B(SI)U)',
    value: '((((SS)K)((S(K((SS)(S((SS)K)))))K)(SI)))',
  },
};


const combinators = {
  ...Object.fromEntries(
    Object.entries(combinatorTable)
      .reverse()
      .flatMap(([k,v]) =>
        typeof v === 'object'
          ? [[k,v.value]]
          : []
      )
  ),
  'S': '3', 'K': '2', 'I': '1', 'P': '0', '`': '0',
};

for (const k in combinators) {
  if (combinators[k] === "0") continue;

  for (const i in combinatorTable[k]) {
    if (!+i) continue;
    combinatorTable[k].arity = +i;
    combinatorTable[k].lambda = 'λ' + ([...Array(+i)].map((_,j) => String.fromCharCode('a'.charCodeAt() + j)).join('')) + '. ' + combinatorTable[k][i];
  }
  if (combinatorTable[k].def === undefined) combinatorTable[k].def = combinatorTable[k].value;
  combinators[k] = combinators[k]
    .replaceAll('(', '0')
    .replaceAll('I', '1')
    .replaceAll('K', '2')
    .replaceAll('S', '3')
    .replaceAll(')', '');
  combinatorTable[k].value = combinators[k];
  assert(deltaExpecting_impl(combinators[k]) === -1, k, deltaExpecting_impl(combinators[k]));
  assert(reachesZero_impl(1, combinators[k]) === combinators[k].length*2, k, reachesZero_impl(1, combinators[k]));
}
delete combinators['KI'];

function isatomic(s) {
  return reachesZero_impl(1, s) === (s.length * 2);
}
function toatomic(s) {
  let zeropoint;
  // Drop leading zeros if needed
  while (!(zeropoint = reachesZero_impl(1, s) / 2)) {
    s = s.substring(1);
  }
  if (zeropoint === s.length) return s; // perfect match
  // Start with a baseline operand
  let operand = s.substring(0, zeropoint);
  let remaining = s.substring(zeropoint);
  let extraSpaces = 0;
  while (remaining) {
    extraSpaces = 0;
    while (!(zeropoint = reachesZero_impl(1, remaining) / 2)) {
      extraSpaces += 1;
      remaining = remaining.substring(1);
    }
    // Grab another argument
    let argument = remaining.substring(0, zeropoint);
    remaining = remaining.substring(zeropoint);
    // Add it onto the operand
    operand =
      '0' + // prefix application
      repeat(extraSpaces, '0302') + // `B` spacers, to wait for pending arguments
      operand + // head
      argument; // argument (possibly a function waiting for more arguments)
  }
  return repeat(extraSpaces, '0302') + operand;
  // [w0] = [0302w]
  // [x0y] = [00302xy]
  // [(x0y)0z] = [(00302xy)0z] = [0030200302xyz]
  // [x00y] = [(0302x)0y] = [003020302xy]
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
    s = s.replaceAll(/#[0-9]+/g, x => repeat(Number(x.substring(1)), '00SB')+'0KI');
    for (const k in vars) {
      s = s.replaceAll(k, vars[k]);
    }
    return s.replaceAll(/\(| |\)/g, '');
  };
}

// https://crypto.stanford.edu/~blynn/lambda/sk.html
const factorialfn = '0003020031100302003302003112003020030203003003003102020210220210300302320030200330200302003020302003302021030200302003302220030200330200302003020302003020312031222';
const factorial = n => '(' + 'P' + factorialfn + '(' + '#' + n + ')' + ')';
const factorial_calc = n => {let r=1; while(n) r*=n--; return r;};

const asNat = num => '00' + num + '0SB' + '0KI';


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

function display(crumbs, options=undefined) {
  if (!options) options = {};
  if (!options.depth) options.depth = 0;
  if (!options.argDepth) options.argDepth = 0;
  if (!options.position) options.position = 0;
  crumbs = sugar(crumbs);
  assert(isatomic(crumbs), "Needs to be atomic to convert to combinators", crumbs, reachesZero_impl(1, crumbs), crumbs.length);
  let i = 0; let r = '';
  let take1 = (needsParensHere) => {
    let l = reachesZero_impl(1, crumbs.substring(i))/2;
    assert(l, "Overran", crumbs.substring(i));
    let rec = {
      ...options,
      depth: options.depth+1,
      argDepth: options.argDepth+!!needsParensHere,
      needsParens: needsParensHere,
      position: options.position+i,
      len: l,
      end: options.position+i+l,
    };
    return [l, display(crumbs.substring(i, i+l), rec)];
  };
  if (crumbs[i] === '0') {
    i++;
    let [j,f] = take1(false);
    i+=j;
    let [k,x] = take1(true);
    i+=k;
    r = [f, x];
  } else {
    options = {
      ...options,
      len: 1,
      end: options.position+1,
    };
    r = crumbs[i];
    i++;
  }
  assert(i === crumbs.length, "Stopped at correct point", i, crumbs.length);
  return { value: r, ...options };
}
display.bracket_colors = [
  "#ff004d",
  "#fe7a29",
  "#fab30e",
  "#1dca24",
  "#22c79a",
  "#2372ff",
  "#d350fe",
  "#e86db7",
];
display.nice_colors = [
  "#d350fe",
  "#2372ff",
  "#1dca24",
  "#fab30e",
  "#ff004d",
];



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

export {
  q2b,
  b2q,
  prettyCrumbs,
  reachesZero_impl,
  deltaExpecting_impl,
  combinators,
  combinatorTable,
  isatomic,
  toatomic,
  sugar,
  factorialfn,
  factorial,
  factorial_calc,
  asNat,
  toCombinators,
  display,
  eval_impl,
};

