// regglomerate: a way to construct regex parsers out of template literals
// See the tests for examples, I guess.

// Sorry, we need to set the flags globally, we can't mix and match flags
// in a single RegExp
var flags = "u";


////////////////////////////////////////////////////////////////////////////////
// Establish the API
////////////////////////////////////////////////////////////////////////////////


// Sorry, I am hijacking the global RegExp object, so we need a symbol to keep
// track of our super secret special data
const $codec = Symbol("codec");

function codec(parser) {
  return mk(parser)[$codec] || (r=>r);
}
RegExp.prototype.map = RegExp.prototype.pmap = function(mapper) {
  return map(this, mapper);
};
RegExp.prototype.pfind = function(input) {
  return find(this, input);
};


// A Parser is a RegExp with a [$codec] key. This is its calling convention:
function run(parser, input) {
  const re = mk(parser);
  const re2 = new RegExp(`^(?:`+re.source+`)$`, re.flags);
  const result = re2.exec(input);
  if (result === null) {
    return null;
  }
  return codec(re)(...result);
}
// And if you want to allow it to match anywhere in the input:
function find(parser, input) {
  const re = mk(parser);
  const result = re.exec(input);
  if (result === null) {
    return null;
  }
  return codec(re)(...result);
}

// We overload our APIs so a Parser can be an Array of Parsers too.
// There are a few builtin objects that we also treat as RegExps.
function mk(parser) {
  if (parser instanceof RegExp) {
    return parser;
  }
  if (!Array.isArray(parser)) {
    for (const special of specialParsers) {
      if (parser === special[0]) {
        return special[1];
      }
    }
    throw new Error(`Parser must be RegExp or Array of Parser or another special parser.`);
  }
  return alt(...parser.map(mk));
}

var specialParsers = [
  // Lightly editorialized from the ECMA spec:
  // Added NaN
  // Removed leading/trailing whitespace
  [Number, map(/[+-]?(?:0[bB][01]+|0[oO][0-7]+|0[xX][0-9a-fA-F]+|NaN|Infinity|(?:\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?)/, Number)],
  // Apparently parseInt doesn't support 0b0101 binary literals??
  [Number.parseInt, map(/[+-]?(?:0[bB][01]+|0[oO][0-7]+|0[xX][0-9a-fA-F]+|\d+)/, Number.parseInt)],
  [Number.parseFloat, map(/[+-]?(?:NaN|Infinity|(?:\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?)/, Number.parseFloat)],
  // Uhh let's just do JSON strings for now
  [String, map(/"(((?=\\)\\(["\\\/bfnrt]|u[0-9a-fA-F]{4}))|[^"\\\0-\x1F\x7F])*"/u, JSON.parse)],
  // TODO: Date????
];

// TODO: make a proxy object that we can stick additional methods on

specialParsers.forEach(([special]) => {
  special.pmap = RegExp.prototype.pmap;
});


////////////////////////////////////////////////////////////////////////////////
// Combinators for the API
////////////////////////////////////////////////////////////////////////////////


// We can freely map the output of the parser.
function map(parser, mapper) {
  const re = mk(parser);
  const r = new RegExp(re.source, re.flags);
  if (re[$codec]) {
    const output = re[$codec];
    r[$codec] = (...matches) => mapper(output(...matches));
  } else {
    r[$codec] = mapper;
  }
  return r;
}

// A pure parser: consumes nothing and produces a constant output
function pure(result) {
  return map(new RegExp("", flags), () => result);
}

// We can sequence multiple parsers using template notation:
function seq(lits, ...parts) {
  const [sources, outputs] = mkN(parts);

  const re = new RegExp(untagged(lits.map(escapeRegExp), ...sources), flags);

  return map(re, (...matches) => {
    return outputs.map(([idxN, lenN, outputN]) => {
      return outputN(...matches.slice(idxN, idxN + lenN))
    });
  });
}

// We can alternate multiple parsers using variadics:
function alt(...options) {
  const [sources, outputs] = mkN(options);
  const re = new RegExp(sources.join(`|`), flags);

  return map(re, (...matches) => {
    for (const [idxN, lenN, outputN] of outputs) {
      if (matches[idxN] !== undefined) {
        return outputN(...matches.slice(idxN, idxN + lenN));
      }
    }
    return null;
  });
}


////////////////////////////////////////////////////////////////////////////////
// Helpers:
////////////////////////////////////////////////////////////////////////////////

function mkN(parsers) {
  const theParsers = parsers.map(mk);

  // Get the source of each regex, partitioned off into its own capturing
  // group:
  const regexes = theParsers.map(reN => {
    if (reN.source === `(?:)`) return `()`;
    return `(` + reN.source + `)`;
  });

  // Keep track of which indices to find it at:
  let idx = 1;
  const outputs = theParsers.map(reN => {
    const idxN = idx;
    const lenN = 1 + ncaps(reN)
    idx += lenN;
    return [idxN, lenN, codec(reN)];
  });

  return [regexes, outputs];
}


// Convert from the tagged template calling convention to an interleaved format
function interleave(lits, ...parts) {
  if (lits.length !== parts.length+1) {
    throw new Error(`Wrong lengths: ${lits.length}, ${parts.length}`);
  }
  const result = [];
  result.length = 2*lits.length-1;
  for (const i in lits) {
    if (i > 0) result[2*i-1] = parts[i-1];
    result[2*i] = lits[i];
  }
  return result
}
// untagged acts like a bare template literal
function untagged(...args) {
  return interleave(...args).join('');
}


// The number of capturing groups of a RegExp
function ncaps(re) {
  return (new RegExp(re.source + `|`, re.flags)).exec('').length - 1;
}

// Escape a bare string to be used in a RegExp
function escapeRegExp(string) {
  return string.replace(/[.*+?^${}()|[\]\\]/g, `\\$&`); // $& means the whole matched string
}

////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////


function tests() {
  // Run with
  `
  node -e 'require("./regglomerate.js").tests().assert()'
  `
  let p1 = [
    seq`${pure("float")}${/[+-]?\d+/.map(Number)}.${/\d+/}`,
    seq`${pure("int")}${Number.parseInt}`,
  ];
  return Object.assign([
    [
      p1,
      "+042.007",
      [ 'float', 42, '007' ],
    ],
    [
      p1,
      "@2#",
      null,
      [ 'int', 2 ],
    ],
    [
      p1,
      "@-0x0101#",
      null,
      [ 'int', -257 ],
    ],
    [
      Number,
      "-0.42e43",
      -0.42e43,
    ],
    [
      Number.parseInt,
      "-.42e43",
      null,
      42,
    ],
    [
      Number.parseInt,
      "-0xFADE",
      -0xFADE,
    ],
    [
      Number.parseFloat,
      "0xFADE",
      null,
      0,
    ],
    [
      Number.parseInt,
      "0x010",
      0x010,
    ],
  ], {
    assert: function() {
      function runTest(parser, label, actual, expected) {
        const X = JSON.stringify(actual);
        const Y = JSON.stringify(expected);
        if (X !== Y) {
          console.log(parser);
          console.log(parser.source);
          throw new Error(`Test ${label} failed:\nx = ${X}\ny = ${Y}`);
        }
      }
      this.forEach((test, i) => {
        if (test.length === 3) test.push(test[2]);
        const [p, input, whenRun, whenFound] = test;
        const parser = mk(p);
        runTest(parser, `${i}.run(${JSON.stringify(input)})`, run(parser, input), whenRun);
        runTest(parser, `${i}.find(${JSON.stringify(input)})`, find(parser, input), whenFound);
      });
      return "Tests succeeded!";
    }
  });
}

if (typeof module !== undefined) {
  module.exports = seq;
  Object.assign(module.exports, {
    flags, $codec, codec,
    run, find, map, seq, alt, pure,
    mk, mkN,
    interleave, untagged,
    ncaps, escapeRegExp,
    tests,
  });
}
