// Silly little syntax sugars :3

_.name({name:"hello"}) == "hello"
mk.name("hello") == {name: "hello"}
_pipe.join(", ").length.line_(["do", "re", "mi"]) == 10

// Create a simple proxy for syntactic sugar for method names and function calls
const sugar = (handler, applier=undefined) => new Proxy(applier ? ()=>{} : {}, {
  get: (_target, property, _thisArg) => {
    return handler(property);
  },
  apply: applier && ((_target, _thisArg, args) => applier(...args)),
});

// Create a getter for the property:
//     _.name = v => v.name
//     _.name({name:"hello"}) == "hello"
//
// Or create a function that applies fixed arguments:
//     _("la", "la") = fn => fn("la", "la")
//     _("la", "la")(Array) = ["la", "la"]
const _ = sugar(
  name => value => value[name],
  // Capture `this` from the second call, so that
  // ({ foo: "bar" }) == _().call({ foo: "bar" }, (function () { return this }))
  (...args) => function (fn) { return fn.call(this, ...args) }
);

// Create a wrapper for the property:
//     mk.name = name => ({ name })
//     mk.name("hello") == {name: "hello"}
const mk = sugar(
  name => value => ({ [name]: value }),
);

// Create a modifier for the property:
//     modifier.name(fn) = value => ({ ...value, name: fn(value.name) })
//     modifier.name(x => x.join(""))({ name: ["Monoid", "Musician"], value: 42 }) == { name: "MonoidMusician", value: 42 }
// Works in reverse too:
//     modifier(fn).name = value => ({ ...value, name: fn(value.name) })
//     modifier(x => x.join("")).name({ name: ["Monoid", "Musician"], value: 42 }) == { name: "MonoidMusician", value: 42 }
const modifier = sugar(
  name => fn => value => ({ ...value, [name]: fn(value[name]) }),
  fn => sugar(name => value => ({ ...value, [name]: fn(value[name]) })),
);

// That's the easy stuff. Let's dig deeper!

// The identity function. You know it and love it. <3
const identity = v => v;

// Could this be a name of a method? Note that `new String("")` objects do not
// count, nor do numbers. In particular, note that
//     typeof sugar(identity)[new String("x")] != typeof new String("x")
const _isMethod = p => typeof p === 'string' || typeof p === 'symbol';

// Compose left to right (the first function can take multiple arguments and
// also the implicit `this` reference). If a function is a string or symbol,
// it is treated as a method name (this preserves the parent object as
// the `this` reference for the next function in the composition pipeline).
const compose = (...allFns) => {
  // Comparing functions is bad but I just think it is funny if it strictly
  // satisfies the category laws for the wrong reasons:
  const fns = allFns.filter(x => x !== identity);

  if (!fns.length) return identity;
  // Thus this literally holds in JavaScript equality:
  //     compose(identity, identity, identity, identity) === identity

  const fn0 = _isMethod(fns[0]) ? (v => v[fns[0]]) : fns[0];
  if (fns.length === 1) return fn0;

  // Return a function that can capture `this` and thus operate like a method.
  return function(...args) {
    // Start by applying the original `this` and `args`
    let v = fn0.call(this, ...args);
    // We preserve the last object as `thisArg` iff the function is a string
    // for a method name
    let thisArg = _isMethod(fns[0]) ? args[0] : undefined;
    for (let fn of fns.slice(1)) {
      if (_isMethod(fn)) {
        // For method names we take the value of the property and preserve
        // the old object as the new `thisArg`
        thisArg = v;
        v = v[fn];
      } else {
        // For functions, we call them with the current value of `thisArg`
        // (which is only defined if the last function was a method name)
        // and then set the next `thisArg` to `undefined`.
        v = fn.call(thisArg, v);
        thisArg = undefined;
      }
    }
    return v;
  };
};

// Collect arguments to compose via proxied member/method/function applications.
const pipelining = pipelined => new Proxy(()=>{}, {
  get: (_target, property, _thisArg) => {
    // Special case to end the pipeline
    // (yeah, it could be a symbol to be pedantic about abstraction,
    // but that makes it less symmetrical!)
    if (property === "line_") return compose(...pipelined);

    // See documentation for `compose` for why this is inserted as
    // a string/symbol still, instead of `_[property]`:
    return pipelining([ ...pipelined, property ]);
  },
  apply: (_target, _thisArg, args) => {
    return pipelining([ ...pipelined, _(...args) ]);
  },
});

// Create a pipeline of `_`-style accessors that end with `.line_`:
//     _pipe.join("").line_ = x => x.join("")
//     _pipe.join(", ").line_(["do", "re", "mi"]) == "do, re, mi"
//     _pipe.join(", ").length.line_(["do", "re", "mi"]) == 10
const _pipe = pipelining([]);
