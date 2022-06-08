(() => {
  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq2) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq2 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqIntImpl = refEq;
  var eqCharImpl = refEq;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeSet = function(label4) {
    return function(value12) {
      return function(rec) {
        var copy = {};
        for (var key in rec) {
          if ({}.hasOwnProperty.call(rec, key)) {
            copy[key] = rec[key];
          }
        }
        copy[label4] = value12;
        return copy;
      };
    };
  };

  // output/Data.Eq/index.js
  var eqInt = {
    eq: eqIntImpl
  };
  var eqChar = {
    eq: eqCharImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();

  // output/Data.Ring/foreign.js
  var intSub = function(x) {
    return function(y) {
      return x - y | 0;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x) {
    return function(y) {
      return x + y | 0;
    };
  };
  var intMul = function(x) {
    return function(y) {
      return x * y | 0;
    };
  };
  var numAdd = function(n1) {
    return function(n2) {
      return n1 + n2;
    };
  };
  var numMul = function(n1) {
    return function(n2) {
      return n1 * n2;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Data.Semiring/index.js
  var semiringNumber = {
    add: numAdd,
    zero: 0,
    mul: numMul,
    one: 1
  };
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };
  var mul = function(dict) {
    return dict.mul;
  };
  var add = function(dict) {
    return dict.add;
  };

  // output/Data.Ring/index.js
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
  };

  // output/Data.Ord/index.js
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();
  var ordChar = /* @__PURE__ */ function() {
    return {
      compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqChar;
      }
    };
  }();
  var compare = function(dict) {
    return dict.compare;
  };

  // output/Control.Apply/foreign.js
  var arrayApply = function(fs) {
    return function(xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l * k);
      var n = 0;
      for (var i = 0; i < l; i++) {
        var f = fs[i];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };
  var compose = function(dict) {
    return dict.compose;
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b) {
      return function(a) {
        return f(a)(b);
      };
    };
  };
  var $$const = function(a) {
    return function(v) {
      return a;
    };
  };
  var applyFlipped = function(x) {
    return function(f) {
      return f(x);
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var mapFlipped = function(dictFunctor) {
    return function(fa) {
      return function(f) {
        return map(dictFunctor)(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var functorFn = {
    map: /* @__PURE__ */ compose(semigroupoidFn)
  };
  var functorArray = {
    map: arrayMap
  };
  var flap = function(dictFunctor) {
    return function(ff2) {
      return function(x) {
        return map(dictFunctor)(function(f) {
          return f(x);
        })(ff2);
      };
    };
  };

  // output/Control.Apply/index.js
  var applyArray = {
    apply: arrayApply,
    Functor0: function() {
      return functorArray;
    }
  };
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    return function(a) {
      return function(b) {
        return apply(dictApply)(map(dictApply.Functor0())($$const(identity(categoryFn)))(a))(b);
      };
    };
  };
  var lift2 = function(dictApply) {
    return function(f) {
      return function(a) {
        return function(b) {
          return apply(dictApply)(map(dictApply.Functor0())(f)(a))(b);
        };
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var liftA1 = function(dictApplicative) {
    return function(f) {
      return function(a) {
        return apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
      };
    };
  };
  var applicativeArray = {
    pure: function(x) {
      return [x];
    },
    Apply0: function() {
      return applyArray;
    }
  };

  // output/Control.Bind/foreign.js
  var arrayBind = function(arr) {
    return function(f) {
      var result = [];
      for (var i = 0, l = arr.length; i < l; i++) {
        Array.prototype.push.apply(result, f(arr[i]));
      }
      return result;
    };
  };

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
  };
  var bindArray = {
    bind: arrayBind,
    Apply0: function() {
      return applyArray;
    }
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var composeKleisli = function(dictBind) {
    return function(f) {
      return function(g) {
        return function(a) {
          return bind(dictBind)(f(a))(g);
        };
      };
    };
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };
  var join = function(dictBind) {
    return function(m) {
      return bind(dictBind)(m)(identity(categoryFn));
    };
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Control.Monad.ST.Global/index.js
  var toEffect = unsafeCoerce2;

  // output/Control.Monad.ST.Internal/foreign.js
  var map_ = function(f) {
    return function(a) {
      return function() {
        return f(a());
      };
    };
  };
  function newSTRef(val) {
    return function() {
      return { value: val };
    };
  }
  var read = function(ref) {
    return function() {
      return ref.value;
    };
  };
  var modifyImpl = function(f) {
    return function(ref) {
      return function() {
        var t = f(ref.value);
        ref.value = t.state;
        return t.value;
      };
    };
  };
  var write = function(a) {
    return function(ref) {
      return function() {
        return ref.value = a;
      };
    };
  };

  // output/Control.Monad/index.js
  var ap = function(dictMonad) {
    return function(f) {
      return function(a) {
        return bind(dictMonad.Bind1())(f)(function(f$prime) {
          return bind(dictMonad.Bind1())(a)(function(a$prime) {
            return pure(dictMonad.Applicative0())(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Semigroup/foreign.js
  var concatString = function(s1) {
    return function(s2) {
      return s1 + s2;
    };
  };
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupUnit = {
    append: function(v) {
      return function(v1) {
        return unit;
      };
    }
  };
  var semigroupString = {
    append: concatString
  };
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Control.Alt/index.js
  var alt = function(dict) {
    return dict.alt;
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var boundedChar = {
    top: topChar,
    bottom: bottomChar,
    Ord0: function() {
      return ordChar;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };

  // output/Data.Show/index.js
  var showInt = {
    show: showIntImpl
  };
  var show = function(dict) {
    return dict.show;
  };

  // output/Data.Generic.Rep/index.js
  var Inl = /* @__PURE__ */ function() {
    function Inl2(value0) {
      this.value0 = value0;
    }
    ;
    Inl2.create = function(value0) {
      return new Inl2(value0);
    };
    return Inl2;
  }();
  var Inr = /* @__PURE__ */ function() {
    function Inr2(value0) {
      this.value0 = value0;
    }
    ;
    Inr2.create = function(value0) {
      return new Inr2(value0);
    };
    return Inr2;
  }();
  var Product = /* @__PURE__ */ function() {
    function Product2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Product2.create = function(value0) {
      return function(value1) {
        return new Product2(value0, value1);
      };
    };
    return Product2;
  }();
  var NoArguments = /* @__PURE__ */ function() {
    function NoArguments2() {
    }
    ;
    NoArguments2.value = new NoArguments2();
    return NoArguments2;
  }();
  var from = function(dict) {
    return dict.from;
  };

  // output/Data.Maybe/index.js
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var showMaybe = function(dictShow) {
    return {
      show: function(v) {
        if (v instanceof Just) {
          return "(Just " + (show(dictShow)(v.value0) + ")");
        }
        ;
        if (v instanceof Nothing) {
          return "Nothing";
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 223, column 1 - line 225, column 28): " + [v.constructor.name]);
      }
    };
  };
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var fromMaybe = function(a) {
    return maybe(a)(identity(categoryFn));
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map(functorMaybe)(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };
  var applicativeMaybe = /* @__PURE__ */ function() {
    return {
      pure: Just.create,
      Apply0: function() {
        return applyMaybe;
      }
    };
  }();

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var hush = /* @__PURE__ */ function() {
    return either($$const(Nothing.value))(Just.create);
  }();

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x) {
    return Math.min(Math.abs(x), 2147483647);
  };
  var intDiv = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };
  var intMod = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };

  // output/Data.Monoid/index.js
  var monoidUnit = {
    mempty: unit,
    Semigroup0: function() {
      return semigroupUnit;
    }
  };
  var monoidString = {
    mempty: "",
    Semigroup0: function() {
      return semigroupString;
    }
  };
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/foreign.js
  var pureE = function(a) {
    return function() {
      return a;
    };
  };
  var bindE = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
      state3 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);
  var applyEffect = /* @__PURE__ */ $lazy_applyEffect(23);
  var semigroupEffect = function(dictSemigroup) {
    return {
      append: lift2(applyEffect)(append(dictSemigroup))
    };
  };
  var monoidEffect = function(dictMonoid) {
    return {
      mempty: pureE(mempty(dictMonoid)),
      Semigroup0: function() {
        return semigroupEffect(dictMonoid.Semigroup0());
      }
    };
  };

  // output/Effect.Ref/foreign.js
  var _new = function(val) {
    return function() {
      return { value: val };
    };
  };
  var read2 = function(ref) {
    return function() {
      return ref.value;
    };
  };
  var modifyImpl2 = function(f) {
    return function(ref) {
      return function() {
        var t = f(ref.value);
        ref.value = t.state;
        return t.value;
      };
    };
  };

  // output/Effect.Ref/index.js
  var $$new = _new;
  var modify$prime = modifyImpl2;
  var modify = function(f) {
    return modify$prime(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };

  // output/Control.Monad.ST.Internal/index.js
  var modify$prime2 = modifyImpl;
  var modify2 = function(f) {
    return modify$prime2(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var functorST = {
    map: map_
  };

  // output/Control.Monad.ST.Class/index.js
  var monadSTEffect = {
    liftST: toEffect,
    Monad0: function() {
      return monadEffect;
    }
  };
  var liftST = function(dict) {
    return dict.liftST;
  };

  // output/Data.Array/foreign.js
  var replicateFill = function(count2) {
    return function(value12) {
      if (count2 < 1) {
        return [];
      }
      var result = new Array(count2);
      return result.fill(value12);
    };
  };
  var replicatePolyfill = function(count2) {
    return function(value12) {
      var result = [];
      var n = 0;
      for (var i = 0; i < count2; i++) {
        result[n++] = value12;
      }
      return result;
    };
  };
  var replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons3(head4, tail2) {
      this.head = head4;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head4) {
      return function(tail2) {
        return new Cons3(head4, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count2 = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count2++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr2) {
      return function(xs) {
        return listToArray(foldr2(curryCons)(emptyList)(xs));
      };
    };
  }();
  var length = function(xs) {
    return xs.length;
  };
  var indexImpl = function(just) {
    return function(nothing) {
      return function(xs) {
        return function(i) {
          return i < 0 || i >= xs.length ? nothing : just(xs[i]);
        };
      };
    };
  };
  var findIndexImpl = function(just) {
    return function(nothing) {
      return function(f) {
        return function(xs) {
          for (var i = 0, l = xs.length; i < l; i++) {
            if (f(xs[i]))
              return just(i);
          }
          return nothing;
        };
      };
    };
  };
  var _deleteAt = function(just) {
    return function(nothing) {
      return function(i) {
        return function(l) {
          if (i < 0 || i >= l.length)
            return nothing;
          var l1 = l.slice();
          l1.splice(i, 1);
          return just(l1);
        };
      };
    };
  };
  var sortByImpl = function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to2) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from2 + (to2 - from2 >> 1);
      if (mid - from2 > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
      if (to2 - mid > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to2);
      i = from2;
      j = mid;
      k = from2;
      while (i < mid && j < to2) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare2(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to2) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare2) {
      return function(fromOrdering) {
        return function(xs) {
          var out;
          if (xs.length < 2)
            return xs;
          out = xs.slice(0);
          mergeFromTo(compare2, fromOrdering, out, xs.slice(0), 0, xs.length);
          return out;
        };
      };
    };
  }();
  var unsafeIndexImpl = function(xs) {
    return function(n) {
      return xs[n];
    };
  };

  // output/Data.Array.ST/foreign.js
  var sortByImpl2 = function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to2) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from2 + (to2 - from2 >> 1);
      if (mid - from2 > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
      if (to2 - mid > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to2);
      i = from2;
      j = mid;
      k = from2;
      while (i < mid && j < to2) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare2(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to2) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare2) {
      return function(fromOrdering) {
        return function(xs) {
          return function() {
            if (xs.length < 2)
              return xs;
            mergeFromTo(compare2, fromOrdering, xs, xs.slice(0), 0, xs.length);
            return xs;
          };
        };
      };
    };
  }();

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b) {
    return !b;
  };

  // output/Data.HeytingAlgebra/index.js
  var not = function(dict) {
    return dict.not;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a) {
      return function(b) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init2) {
      return function(xs) {
        var acc = init2;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init2) {
      return function(xs) {
        var acc = init2;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var snd = function(v) {
    return v.value1;
  };
  var functorTuple = {
    map: function(f) {
      return function(m) {
        return new Tuple(m.value0, f(m.value1));
      };
    }
  };
  var fst = function(v) {
    return v.value0;
  };

  // output/Data.Monoid.Endo/index.js
  var semigroupEndo = function(dictSemigroupoid) {
    return {
      append: function(v) {
        return function(v1) {
          return compose(dictSemigroupoid)(v)(v1);
        };
      }
    };
  };
  var monoidEndo = function(dictCategory) {
    return {
      mempty: identity(dictCategory),
      Semigroup0: function() {
        return semigroupEndo(dictCategory.Semigroupoid0());
      }
    };
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var unwrap = coerce;

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var oneOf = function(dictFoldable) {
    return function(dictPlus) {
      return foldr(dictFoldable)(alt(dictPlus.Alt0()))(empty(dictPlus));
    };
  };
  var oneOfMap = function(dictFoldable) {
    return function(dictPlus) {
      return function(f) {
        return foldr(dictFoldable)(function() {
          var $314 = alt(dictPlus.Alt0());
          return function($315) {
            return $314(f($315));
          };
        }())(empty(dictPlus));
      };
    };
  };
  var traverse_ = function(dictApplicative) {
    return function(dictFoldable) {
      return function(f) {
        return foldr(dictFoldable)(function() {
          var $316 = applySecond(dictApplicative.Apply0());
          return function($317) {
            return $316(f($317));
          };
        }())(pure(dictApplicative)(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    return function(dictFoldable) {
      return flip(traverse_(dictApplicative)(dictFoldable));
    };
  };
  var sequence_ = function(dictApplicative) {
    return function(dictFoldable) {
      return traverse_(dictApplicative)(dictFoldable)(identity(categoryFn));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var intercalate2 = function(dictFoldable) {
    return function(dictMonoid) {
      return function(sep) {
        return function(xs) {
          var go2 = function(v) {
            return function(x) {
              if (v.init) {
                return {
                  init: false,
                  acc: x
                };
              }
              ;
              return {
                init: false,
                acc: append(dictMonoid.Semigroup0())(v.acc)(append(dictMonoid.Semigroup0())(sep)(x))
              };
            };
          };
          return foldl(dictFoldable)(go2)({
            init: true,
            acc: mempty(dictMonoid)
          })(xs).acc;
        };
      };
    };
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(z) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return z;
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0)(z);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(z) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return z;
          }
          ;
          if (v1 instanceof Just) {
            return v(z)(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty(dictMonoid);
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    }
  };
  var foldMapDefaultR = function(dictFoldable) {
    return function(dictMonoid) {
      return function(f) {
        return foldr(dictFoldable)(function(x) {
          return function(acc) {
            return append(dictMonoid.Semigroup0())(f(x))(acc);
          };
        })(mempty(dictMonoid));
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
    function array1(a) {
      return [a];
    }
    function array2(a) {
      return function(b) {
        return [a, b];
      };
    }
    function array3(a) {
      return function(b) {
        return function(c) {
          return [a, b, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply3) {
      return function(map2) {
        return function(pure2) {
          return function(f) {
            return function(array) {
              function go2(bot, top2) {
                switch (top2 - bot) {
                  case 0:
                    return pure2([]);
                  case 1:
                    return map2(array1)(f(array[bot]));
                  case 2:
                    return apply3(map2(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply3(apply3(map2(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                    return apply3(map2(concat2)(go2(bot, pivot)))(go2(pivot, top2));
                }
              }
              return go2(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Traversable/index.js
  var traverse = function(dict) {
    return dict.traverse;
  };
  var sequenceDefault = function(dictTraversable) {
    return function(dictApplicative) {
      return traverse(dictTraversable)(dictApplicative)(identity(categoryFn));
    };
  };
  var traversableArray = {
    traverse: function(dictApplicative) {
      return traverseArrayImpl(apply(dictApplicative.Apply0()))(map(dictApplicative.Apply0().Functor0()))(pure(dictApplicative));
    },
    sequence: function(dictApplicative) {
      return sequenceDefault(traversableArray)(dictApplicative);
    },
    Functor0: function() {
      return functorArray;
    },
    Foldable1: function() {
      return foldableArray;
    }
  };

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust2) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b) {
              var result = [];
              var value12 = b;
              while (true) {
                var maybe2 = f(value12);
                if (isNothing2(maybe2))
                  return result;
                var tuple = fromJust2(maybe2);
                result.push(fst2(tuple));
                value12 = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust2) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b) {
              var result = [];
              var value12 = b;
              while (true) {
                var tuple = f(value12);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2))
                  return result;
                value12 = fromJust2(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/index.js
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(/* @__PURE__ */ fromJust())(fst)(snd)
  };

  // output/Data.Unfoldable/index.js
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(/* @__PURE__ */ fromJust())(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };

  // output/Data.Array/index.js
  var unsafeIndex = function() {
    return unsafeIndexImpl;
  };
  var toUnfoldable = function(dictUnfoldable) {
    return function(xs) {
      var len = length(xs);
      var f = function(i) {
        if (i < len) {
          return new Just(new Tuple(unsafeIndex()(xs)(i), i + 1 | 0));
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Array (line 156, column 3 - line 158, column 26): " + [i.constructor.name]);
      };
      return unfoldr(dictUnfoldable)(f)(0);
    };
  };
  var singleton2 = function(a) {
    return [a];
  };
  var index = /* @__PURE__ */ function() {
    return indexImpl(Just.create)(Nothing.value);
  }();
  var last = function(xs) {
    return index(xs)(length(xs) - 1 | 0);
  };
  var findIndex = /* @__PURE__ */ function() {
    return findIndexImpl(Just.create)(Nothing.value);
  }();
  var deleteAt = /* @__PURE__ */ function() {
    return _deleteAt(Just.create)(Nothing.value);
  }();
  var deleteBy = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2.length === 0) {
          return [];
        }
        ;
        return maybe(v2)(function(i) {
          return fromJust()(deleteAt(i)(v2));
        })(findIndex(v(v1))(v2));
      };
    };
  };
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
  var mapMaybe = function(f) {
    return concatMap(function() {
      var $99 = maybe([])(singleton2);
      return function($100) {
        return $99(f($100));
      };
    }());
  };
  var catMaybes = /* @__PURE__ */ mapMaybe(/* @__PURE__ */ identity(categoryFn));

  // output/Data.List.Types/index.js
  var Nil = /* @__PURE__ */ function() {
    function Nil3() {
    }
    ;
    Nil3.value = new Nil3();
    return Nil3;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons3.create = function(value0) {
      return function(value1) {
        return new Cons3(value0, value1);
      };
    };
    return Cons3;
  }();
  var listMap = function(f) {
    var chunkedRevMap = function($copy_chunksAcc) {
      return function($copy_v) {
        var $tco_var_chunksAcc = $copy_chunksAcc;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(chunksAcc, v) {
          if (v instanceof Cons && (v.value1 instanceof Cons && v.value1.value1 instanceof Cons)) {
            $tco_var_chunksAcc = new Cons(v, chunksAcc);
            $copy_v = v.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v1) {
            if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Nil)) {
              return new Cons(f(v1.value0), new Cons(f(v1.value1.value0), Nil.value));
            }
            ;
            if (v1 instanceof Cons && v1.value1 instanceof Nil) {
              return new Cons(f(v1.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v1) {
            return function($copy_acc) {
              var $tco_var_v1 = $copy_v1;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v1, acc) {
                if (v1 instanceof Cons && (v1.value0 instanceof Cons && (v1.value0.value1 instanceof Cons && v1.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v1 = v1.value1;
                  $copy_acc = new Cons(f(v1.value0.value0), new Cons(f(v1.value0.value1.value0), new Cons(f(v1.value0.value1.value1.value0), acc)));
                  return;
                }
                ;
                $tco_done1 = true;
                return acc;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v1, $copy_acc);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(chunksAcc)(unrolledMap(v));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_chunksAcc, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
  var foldableList = {
    foldr: function(f) {
      return function(b) {
        var rev3 = function() {
          var go2 = function($copy_acc) {
            return function($copy_v) {
              var $tco_var_acc = $copy_acc;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(acc, v) {
                if (v instanceof Nil) {
                  $tco_done = true;
                  return acc;
                }
                ;
                if (v instanceof Cons) {
                  $tco_var_acc = new Cons(v.value0, acc);
                  $copy_v = v.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [acc.constructor.name, v.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_acc, $copy_v);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        }();
        var $205 = foldl(foldableList)(flip(f))(b);
        return function($206) {
          return $205(rev3($206));
        };
      };
    },
    foldl: function(f) {
      var go2 = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go2;
    },
    foldMap: function(dictMonoid) {
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $207 = append(dictMonoid.Semigroup0())(acc);
          return function($208) {
            return $207(f($208));
          };
        })(mempty(dictMonoid));
      };
    }
  };
  var showList = function(dictShow) {
    return {
      show: function(v) {
        if (v instanceof Nil) {
          return "Nil";
        }
        ;
        return "(" + (intercalate2(foldableList)(monoidString)(" : ")(map(functorList)(show(dictShow))(v)) + " : Nil)");
      }
    };
  };
  var unfoldable1List = {
    unfoldr1: function(f) {
      return function(b) {
        var go2 = function($copy_source) {
          return function($copy_memo) {
            var $tco_var_source = $copy_source;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(source, memo) {
              var v = f(source);
              if (v.value1 instanceof Just) {
                $tco_var_source = v.value1.value0;
                $copy_memo = new Cons(v.value0, memo);
                return;
              }
              ;
              if (v.value1 instanceof Nothing) {
                $tco_done = true;
                return foldl(foldableList)(flip(Cons.create))(Nil.value)(new Cons(v.value0, memo));
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 135, column 22 - line 137, column 61): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_source, $copy_memo);
            }
            ;
            return $tco_result;
          };
        };
        return go2(b)(Nil.value);
      };
    }
  };
  var unfoldableList = {
    unfoldr: function(f) {
      return function(b) {
        var go2 = function($copy_source) {
          return function($copy_memo) {
            var $tco_var_source = $copy_source;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(source, memo) {
              var v = f(source);
              if (v instanceof Nothing) {
                $tco_done = true;
                return foldl(foldableList)(flip(Cons.create))(Nil.value)(memo);
              }
              ;
              if (v instanceof Just) {
                $tco_var_source = v.value0.value1;
                $copy_memo = new Cons(v.value0.value0, memo);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 142, column 22 - line 144, column 52): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_source, $copy_memo);
            }
            ;
            return $tco_result;
          };
        };
        return go2(b)(Nil.value);
      };
    },
    Unfoldable10: function() {
      return unfoldable1List;
    }
  };

  // output/Data.Map.Internal/index.js
  var Leaf = /* @__PURE__ */ function() {
    function Leaf2() {
    }
    ;
    Leaf2.value = new Leaf2();
    return Leaf2;
  }();
  var Two = /* @__PURE__ */ function() {
    function Two2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Two2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Two2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Two2;
  }();
  var Three = /* @__PURE__ */ function() {
    function Three2(value0, value1, value22, value32, value42, value52, value62) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
      this.value6 = value62;
    }
    ;
    Three2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return function(value62) {
                  return new Three2(value0, value1, value22, value32, value42, value52, value62);
                };
              };
            };
          };
        };
      };
    };
    return Three2;
  }();
  var TwoLeft = /* @__PURE__ */ function() {
    function TwoLeft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    TwoLeft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new TwoLeft2(value0, value1, value22);
        };
      };
    };
    return TwoLeft2;
  }();
  var TwoRight = /* @__PURE__ */ function() {
    function TwoRight2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    TwoRight2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new TwoRight2(value0, value1, value22);
        };
      };
    };
    return TwoRight2;
  }();
  var ThreeLeft = /* @__PURE__ */ function() {
    function ThreeLeft2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeLeft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeLeft2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeLeft2;
  }();
  var ThreeMiddle = /* @__PURE__ */ function() {
    function ThreeMiddle2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeMiddle2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeMiddle2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeMiddle2;
  }();
  var ThreeRight = /* @__PURE__ */ function() {
    function ThreeRight2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeRight2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeRight2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeRight2;
  }();
  var KickUp = /* @__PURE__ */ function() {
    function KickUp2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    KickUp2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new KickUp2(value0, value1, value22, value32);
          };
        };
      };
    };
    return KickUp2;
  }();
  var lookup = function(dictOrd) {
    return function(k) {
      var comp = compare(dictOrd);
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Two) {
            var v2 = comp(k)(v.value1);
            if (v2 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            if (v2 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          if (v instanceof Three) {
            var v3 = comp(k)(v.value1);
            if (v3 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            var v4 = comp(k)(v.value4);
            if (v4 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value5);
            }
            ;
            if (v3 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            if (v4 instanceof GT) {
              $copy_v = v.value6;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var fromZipper = function($copy_dictOrd) {
    return function($copy_v) {
      return function($copy_tree) {
        var $tco_var_dictOrd = $copy_dictOrd;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(dictOrd, v, tree) {
          if (v instanceof Nil) {
            $tco_done = true;
            return tree;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof TwoLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(tree, v.value0.value0, v.value0.value1, v.value0.value2);
              return;
            }
            ;
            if (v.value0 instanceof TwoRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(v.value0.value0, v.value0.value1, v.value0.value2, tree);
              return;
            }
            ;
            if (v.value0 instanceof ThreeLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(tree, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeMiddle) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, tree, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, tree);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): " + [v.constructor.name, tree.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_tree);
        }
        ;
        return $tco_result;
      };
    };
  };
  var insert = function(dictOrd) {
    return function(k) {
      return function(v) {
        var up = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v1 instanceof Nil) {
                $tco_done = true;
                return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
              }
              ;
              if (v1 instanceof Cons) {
                if (v1.value0 instanceof TwoLeft) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                }
                ;
                if (v1.value0 instanceof TwoRight) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
                }
                ;
                if (v1.value0 instanceof ThreeLeft) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeMiddle) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeRight) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): " + [v1.value0.constructor.name, v2.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
        var comp = compare(dictOrd);
        var down = function($copy_ctx) {
          return function($copy_v1) {
            var $tco_var_ctx = $copy_ctx;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(ctx, v1) {
              if (v1 instanceof Leaf) {
                $tco_done1 = true;
                return up(ctx)(new KickUp(Leaf.value, k, v, Leaf.value));
              }
              ;
              if (v1 instanceof Two) {
                var v2 = comp(k)(v1.value1);
                if (v2 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper(dictOrd)(ctx)(new Two(v1.value0, k, v, v1.value3));
                }
                ;
                if (v2 instanceof LT) {
                  $tco_var_ctx = new Cons(new TwoLeft(v1.value1, v1.value2, v1.value3), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }
                ;
                $tco_var_ctx = new Cons(new TwoRight(v1.value0, v1.value1, v1.value2), ctx);
                $copy_v1 = v1.value3;
                return;
              }
              ;
              if (v1 instanceof Three) {
                var v3 = comp(k)(v1.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper(dictOrd)(ctx)(new Three(v1.value0, k, v, v1.value3, v1.value4, v1.value5, v1.value6));
                }
                ;
                var v4 = comp(k)(v1.value4);
                if (v4 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper(dictOrd)(ctx)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, k, v, v1.value6));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_ctx = new Cons(new ThreeLeft(v1.value1, v1.value2, v1.value3, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }
                ;
                if (v3 instanceof GT && v4 instanceof LT) {
                  $tco_var_ctx = new Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value3;
                  return;
                }
                ;
                $tco_var_ctx = new Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
                $copy_v1 = v1.value6;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): " + [ctx.constructor.name, v1.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_ctx, $copy_v1);
            }
            ;
            return $tco_result;
          };
        };
        return down(Nil.value);
      };
    };
  };
  var empty2 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var fromFoldable = function(dictOrd) {
    return function(dictFoldable) {
      return foldl(dictFoldable)(function(m) {
        return function(v) {
          return insert(dictOrd)(v.value0)(v.value1)(m);
        };
      })(empty2);
    };
  };

  // output/Data.Filterable/index.js
  var filterMap = function(dict) {
    return dict.filterMap;
  };
  var filter4 = function(dict) {
    return dict.filter;
  };

  // output/Data.Monoid.Always/index.js
  var always2 = function(dictMonoid) {
    return {
      always: identity(categoryFn),
      Monoid0: function() {
        return dictMonoid;
      }
    };
  };

  // output/FRP.Event.Class/index.js
  var keepLatest = function(dict) {
    return dict.keepLatest;
  };
  var fold2 = function(dict) {
    return dict.fold;
  };
  var mapAccum = function(dictIsEvent) {
    return function(f) {
      return function(xs) {
        return function(acc) {
          return filterMap(dictIsEvent.Filterable1())(snd)(fold2(dictIsEvent)(function(a) {
            return function(v) {
              return map(functorTuple)(pure(applicativeMaybe))(f(a)(v.value0));
            };
          })(xs)(new Tuple(acc, Nothing.value)));
        };
      };
    };
  };
  var bang = function(dict) {
    return dict.bang;
  };

  // output/Unsafe.Reference/foreign.js
  function reallyUnsafeRefEq(a) {
    return function(b) {
      return a === b;
    };
  }

  // output/Unsafe.Reference/index.js
  var unsafeRefEq = reallyUnsafeRefEq;

  // output/FRP.Event/index.js
  var AnEvent = function(x) {
    return x;
  };
  var subscribe = function(v) {
    return function(k) {
      return v(k);
    };
  };
  var sampleOn2 = function(dictMonadST) {
    return function(dictApplicative) {
      return function(v) {
        return function(v1) {
          return function(k) {
            return bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(newSTRef(Nothing.value)))(function(latest) {
              return bind(dictMonadST.Monad0().Bind1())(v(function(a) {
                return liftST(dictMonadST)($$void(functorST)(write(new Just(a))(latest)));
              }))(function(c1) {
                return bind(dictMonadST.Monad0().Bind1())(v1(function(f) {
                  return bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(read(latest)))(traverse_(dictApplicative)(foldableMaybe)(function($108) {
                    return k(f($108));
                  }));
                }))(function(c2) {
                  return pure(dictApplicative)(applySecond(dictApplicative.Apply0())(c1)(c2));
                });
              });
            });
          };
        };
      };
    };
  };
  var makeEvent = AnEvent;
  var keepLatest2 = function(dictMonadST) {
    return function(v) {
      return function(k) {
        return bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(newSTRef(Nothing.value)))(function(cancelInner) {
          return bind(dictMonadST.Monad0().Bind1())(v(function(inner) {
            return discard(discardUnit)(dictMonadST.Monad0().Bind1())(bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(read(cancelInner)))(sequence_(dictMonadST.Monad0().Applicative0())(foldableMaybe)))(function() {
              return bind(dictMonadST.Monad0().Bind1())(subscribe(inner)(k))(function(c) {
                return liftST(dictMonadST)($$void(functorST)(write(new Just(c))(cancelInner)));
              });
            });
          }))(function(cancelOuter) {
            return pure(dictMonadST.Monad0().Applicative0())(discard(discardUnit)(dictMonadST.Monad0().Bind1())(bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(read(cancelInner)))(sequence_(dictMonadST.Monad0().Applicative0())(foldableMaybe)))(function() {
              return cancelOuter;
            }));
          });
        });
      };
    };
  };
  var functorEvent = {
    map: function(f) {
      return function(v) {
        return function(k) {
          return v(function($109) {
            return k(f($109));
          });
        };
      };
    }
  };
  var fold3 = function(dictMonadST) {
    return function(f) {
      return function(v) {
        return function(b) {
          return function(k) {
            return bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(newSTRef(b)))(function(result) {
              return v(function(a) {
                return bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(modify2(f(a))(result)))(k);
              });
            });
          };
        };
      };
    };
  };
  var filter5 = function(dictApplicative) {
    return function(p) {
      return function(v) {
        return function(k) {
          return v(function(a) {
            var v1 = p(a);
            if (v1 instanceof Just) {
              return k(v1.value0);
            }
            ;
            if (v1 instanceof Nothing) {
              return pure(dictApplicative)(unit);
            }
            ;
            throw new Error("Failed pattern match at FRP.Event (line 131, column 13 - line 133, column 27): " + [v1.constructor.name]);
          });
        };
      };
    };
  };
  var filter$prime = function(dictApplicative) {
    return function(f) {
      return filter5(dictApplicative)(function(a) {
        var v = f(a);
        if (v) {
          return new Just(a);
        }
        ;
        if (!v) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at FRP.Event (line 89, column 13 - line 91, column 25): " + [v.constructor.name]);
      });
    };
  };
  var create = function(dictMonadST) {
    return function(dictMonadST1) {
      return bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(newSTRef([])))(function(subscribers) {
        return pure(dictMonadST.Monad0().Applicative0())({
          event: function(k) {
            return bind(dictMonadST1.Monad0().Bind1())(liftST(dictMonadST1)(modify2(function(v) {
              return append(semigroupArray)(v)([k]);
            })(subscribers)))(function() {
              return pure(dictMonadST1.Monad0().Applicative0())(bind(dictMonadST1.Monad0().Bind1())(liftST(dictMonadST1)(modify2(deleteBy(unsafeRefEq)(k))(subscribers)))(function() {
                return pure(dictMonadST1.Monad0().Applicative0())(unit);
              }));
            });
          },
          push: function(a) {
            return bind(dictMonadST1.Monad0().Bind1())(liftST(dictMonadST1)(read(subscribers)))(traverse_(dictMonadST1.Monad0().Applicative0())(foldableArray)(function(k) {
              return k(a);
            }));
          }
        });
      });
    };
  };
  var fix3 = function(dictMonadST) {
    return function(dictMonad) {
      return function(f) {
        return function(k) {
          return bind(dictMonad.Bind1())(create(dictMonadST)(dictMonadST))(function(v) {
            var v1 = f(v.event);
            return bind(dictMonad.Bind1())(subscribe(v1.input)(v.push))(function(c1) {
              return bind(dictMonad.Bind1())(subscribe(v1.output)(k))(function(c2) {
                return pure(dictMonad.Applicative0())(applySecond(dictMonad.Bind1().Apply0())(c1)(c2));
              });
            });
          });
        };
      };
    };
  };
  var memoize = function(dictMonadST) {
    return function(e) {
      return function(f) {
        return makeEvent(function(k) {
          return bind(dictMonadST.Monad0().Bind1())(create(dictMonadST)(dictMonadST))(function(v) {
            return discard(discardUnit)(dictMonadST.Monad0().Bind1())(k(f(v.event)))(function() {
              return subscribe(e)(v.push);
            });
          });
        });
      };
    };
  };
  var compactableEvent = function(dictApplicative) {
    return {
      compact: filter5(dictApplicative)(identity(categoryFn)),
      separate: function(xs) {
        return {
          left: filter5(dictApplicative)(function(v) {
            if (v instanceof Left) {
              return new Just(v.value0);
            }
            ;
            if (v instanceof Right) {
              return Nothing.value;
            }
            ;
            throw new Error("Failed pattern match at FRP.Event (line 72, column 13 - line 74, column 33): " + [v.constructor.name]);
          })(xs),
          right: filter5(dictApplicative)(function(v) {
            if (v instanceof Right) {
              return new Just(v.value0);
            }
            ;
            if (v instanceof Left) {
              return Nothing.value;
            }
            ;
            throw new Error("Failed pattern match at FRP.Event (line 79, column 13 - line 81, column 32): " + [v.constructor.name]);
          })(xs)
        };
      }
    };
  };
  var filterableEvent = function(dictApplicative) {
    return {
      filter: filter$prime(dictApplicative),
      filterMap: filter5(dictApplicative),
      partition: function(p) {
        return function(xs) {
          return {
            yes: filter$prime(dictApplicative)(p)(xs),
            no: filter$prime(dictApplicative)(function() {
              var $110 = not(heytingAlgebraBoolean);
              return function($111) {
                return $110(p($111));
              };
            }())(xs)
          };
        };
      },
      partitionMap: function(f) {
        return function(xs) {
          return {
            left: filterMap(filterableEvent(dictApplicative))(function() {
              var $112 = either(Just.create)($$const(Nothing.value));
              return function($113) {
                return $112(f($113));
              };
            }())(xs),
            right: filterMap(filterableEvent(dictApplicative))(function($114) {
              return hush(f($114));
            })(xs)
          };
        };
      },
      Compactable0: function() {
        return compactableEvent(dictApplicative);
      },
      Functor1: function() {
        return functorEvent;
      }
    };
  };
  var bang2 = function(dictApplicative) {
    return function(a) {
      return function(k) {
        return map(dictApplicative.Apply0().Functor0())(function(v) {
          return pure(dictApplicative)(unit);
        })(k(a));
      };
    };
  };
  var altEvent = function(dictApplicative) {
    return {
      alt: function(v) {
        return function(v1) {
          return function(k) {
            return apply(dictApplicative.Apply0())(map(dictApplicative.Apply0().Functor0())(function(v2) {
              return function(v3) {
                return applySecond(dictApplicative.Apply0())(v2)(v3);
              };
            })(v(k)))(v1(k));
          };
        };
      },
      Functor0: function() {
        return functorEvent;
      }
    };
  };
  var plusEvent = function(dictApplicative) {
    return {
      empty: function(v) {
        return pure(dictApplicative)(pure(dictApplicative)(unit));
      },
      Alt0: function() {
        return altEvent(dictApplicative);
      }
    };
  };
  var eventIsEvent = function(dictMonadST) {
    return {
      fold: fold3(dictMonadST),
      keepLatest: keepLatest2(dictMonadST),
      sampleOn: sampleOn2(dictMonadST)(dictMonadST.Monad0().Applicative0()),
      fix: fix3(dictMonadST)(dictMonadST.Monad0()),
      bang: bang2(dictMonadST.Monad0().Applicative0()),
      Plus0: function() {
        return plusEvent(dictMonadST.Monad0().Applicative0());
      },
      Filterable1: function() {
        return filterableEvent(dictMonadST.Monad0().Applicative0());
      }
    };
  };

  // output/FRP.Event.VBus/foreign.js
  var _____$__$_$$_vbus = "_____$__$_$$_vbus";
  function unsafeDestroyS(s) {
    return () => {
      for (const key in s) {
        delete s[key];
      }
    };
  }
  function unsafePE(u) {
    return () => {
      const doAssigns = (s2, p2, e2, u2) => {
        const ok = Object.keys(u2);
        for (var i = 0; i < ok.length; i++) {
          if (u2[ok[i]] instanceof Object && u2[ok[i]][_____$__$_$$_vbus] === _____$__$_$$_vbus) {
            const p0 = {};
            const e0 = {};
            doAssigns(s2, p0, e0, u2[ok[i]]);
            p2[ok[i]] = p0;
            e2[ok[i]] = e0;
          } else {
            const rn = `${Math.random()}`;
            s2[rn] = {};
            p2[ok[i]] = (v) => () => {
              const rnk = Object.values(s2[rn]);
              for (var j = 0; j < rnk.length; j++) {
                rnk[j](v)();
              }
            };
            e2[ok[i]] = (f) => () => {
              const k = `${Math.random()}`;
              s2[rn][k] = f;
              return () => {
                delete s2[rn][k];
              };
            };
          }
        }
      };
      const s = {};
      const p = {};
      const e = {};
      doAssigns(s, p, e, u);
      return { p, e, s };
    };
  }

  // output/Record/index.js
  var insert2 = function(dictIsSymbol) {
    return function() {
      return function() {
        return function(l) {
          return function(a) {
            return function(r) {
              return unsafeSet(reflectSymbol(dictIsSymbol)(l))(a)(r);
            };
          };
        };
      };
    };
  };

  // output/FRP.Event.VBus/index.js
  var vbusNil = {
    vb: function(v) {
      return function(v1) {
        return function(v2) {
          return {};
        };
      };
    }
  };
  var vb = function(dict) {
    return dict.vb;
  };
  var vbus = function() {
    return function(dictMonadST) {
      return function(dictVBus) {
        return function(v) {
          return function(f) {
            var vbd = vb(dictVBus)($$Proxy.value)($$Proxy.value)($$Proxy.value);
            return makeEvent(function(k) {
              return bind(dictMonadST.Monad0().Bind1())(unsafePE(vbd))(function(upe) {
                return discard(discardUnit)(dictMonadST.Monad0().Bind1())(k(f(upe.p)(upe.e)))(function() {
                  return pure(dictMonadST.Monad0().Applicative0())(unsafeDestroyS(upe.s));
                });
              });
            });
          };
        };
      };
    };
  };
  var vbusCons2 = function(dictIsSymbol) {
    return function() {
      return function() {
        return function(dictVBus) {
          return function() {
            return function() {
              return function() {
                return function() {
                  return {
                    vb: function(v) {
                      return function(v1) {
                        return function(v2) {
                          return insert2(dictIsSymbol)()()($$Proxy.value)(unit)(vb(dictVBus)($$Proxy.value)($$Proxy.value)($$Proxy.value));
                        };
                      };
                    }
                  };
                };
              };
            };
          };
        };
      };
    };
  };

  // output/Bolson.Core/index.js
  var Local = /* @__PURE__ */ function() {
    function Local2(value0) {
      this.value0 = value0;
    }
    ;
    Local2.create = function(value0) {
      return new Local2(value0);
    };
    return Local2;
  }();
  var Global = /* @__PURE__ */ function() {
    function Global2() {
    }
    ;
    Global2.value = new Global2();
    return Global2;
  }();
  var Insert = /* @__PURE__ */ function() {
    function Insert2(value0) {
      this.value0 = value0;
    }
    ;
    Insert2.create = function(value0) {
      return new Insert2(value0);
    };
    return Insert2;
  }();
  var Remove = /* @__PURE__ */ function() {
    function Remove2() {
    }
    ;
    Remove2.value = new Remove2();
    return Remove2;
  }();
  var Logic = /* @__PURE__ */ function() {
    function Logic2(value0) {
      this.value0 = value0;
    }
    ;
    Logic2.create = function(value0) {
      return new Logic2(value0);
    };
    return Logic2;
  }();
  var DynamicChildren$prime = /* @__PURE__ */ function() {
    function DynamicChildren$prime2(value0) {
      this.value0 = value0;
    }
    ;
    DynamicChildren$prime2.create = function(value0) {
      return new DynamicChildren$prime2(value0);
    };
    return DynamicChildren$prime2;
  }();
  var FixedChildren$prime = /* @__PURE__ */ function() {
    function FixedChildren$prime2(value0) {
      this.value0 = value0;
    }
    ;
    FixedChildren$prime2.create = function(value0) {
      return new FixedChildren$prime2(value0);
    };
    return FixedChildren$prime2;
  }();
  var EventfulElement$prime = /* @__PURE__ */ function() {
    function EventfulElement$prime2(value0) {
      this.value0 = value0;
    }
    ;
    EventfulElement$prime2.create = function(value0) {
      return new EventfulElement$prime2(value0);
    };
    return EventfulElement$prime2;
  }();
  var Element$prime = /* @__PURE__ */ function() {
    function Element$prime2(value0) {
      this.value0 = value0;
    }
    ;
    Element$prime2.create = function(value0) {
      return new Element$prime2(value0);
    };
    return Element$prime2;
  }();
  var eqScope = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Local && y instanceof Local) {
          return x.value0 === y.value0;
        }
        ;
        if (x instanceof Global && y instanceof Global) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var vbussed = function() {
    return function(dictMonadST) {
      return function(dictVBus) {
        return function(px) {
          return function(f) {
            return new EventfulElement$prime(vbus()(dictMonadST)(dictVBus)(px)(f));
          };
        };
      };
    };
  };
  var fixed = function(a) {
    return new FixedChildren$prime(a);
  };

  // output/Foreign.Object/foreign.js
  function _copyST(m) {
    return function() {
      var r = {};
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r[k] = m[k];
        }
      }
      return r;
    };
  }
  var empty3 = {};
  function runST(f) {
    return f();
  }
  function _foldM(bind2) {
    return function(f) {
      return function(mz) {
        return function(m) {
          var acc = mz;
          function g(k2) {
            return function(z) {
              return f(z)(k2)(m[k2]);
            };
          }
          for (var k in m) {
            if (hasOwnProperty.call(m, k)) {
              acc = bind2(acc)(g(k));
            }
          }
          return acc;
        };
      };
    };
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys2 = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Foreign.Object.ST/foreign.js
  function poke2(k) {
    return function(v) {
      return function(m) {
        return function() {
          m[k] = v;
          return m;
        };
      };
    };
  }
  var deleteImpl = function(k) {
    return function(m) {
      return function() {
        delete m[k];
        return m;
      };
    };
  };

  // output/Foreign.Object/index.js
  var values = /* @__PURE__ */ toArrayWithKey(function(v) {
    return function(v1) {
      return v1;
    };
  });
  var thawST = _copyST;
  var mutate = function(f) {
    return function(m) {
      return runST(function __do() {
        var s = thawST(m)();
        f(s)();
        return s;
      });
    };
  };
  var insert3 = function(k) {
    return function(v) {
      return mutate(poke2(k)(v));
    };
  };
  var fold4 = /* @__PURE__ */ _foldM(applyFlipped);
  var foldMap2 = function(dictMonoid) {
    return function(f) {
      return fold4(function(acc) {
        return function(k) {
          return function(v) {
            return append(dictMonoid.Semigroup0())(acc)(f(k)(v));
          };
        };
      })(mempty(dictMonoid));
    };
  };
  var foldableObject = {
    foldl: function(f) {
      return fold4(function(z) {
        return function(v) {
          return f(z);
        };
      });
    },
    foldr: function(f) {
      return function(z) {
        return function(m) {
          return foldr(foldableArray)(f)(z)(values(m));
        };
      };
    },
    foldMap: function(dictMonoid) {
      return function(f) {
        return foldMap2(dictMonoid)($$const(f));
      };
    }
  };
  var $$delete3 = function(k) {
    return mutate(deleteImpl(k));
  };

  // output/Bolson.Control/index.js
  var Begin = /* @__PURE__ */ function() {
    function Begin2() {
    }
    ;
    Begin2.value = new Begin2();
    return Begin2;
  }();
  var Middle = /* @__PURE__ */ function() {
    function Middle2() {
    }
    ;
    Middle2.value = new Middle2();
    return Middle2;
  }();
  var End = /* @__PURE__ */ function() {
    function End2() {
    }
    ;
    End2.value = new End2();
    return End2;
  }();
  var switcher = function(dictMonadST) {
    return function(f) {
      return function(event) {
        var counter = function(ev) {
          var fn = function(a) {
            return function(b) {
              return new Tuple(b + 1 | 0, new Tuple(a, b));
            };
          };
          return mapAccum(eventIsEvent(dictMonadST))(fn)(ev)(0);
        };
        return new DynamicChildren$prime(keepLatest(eventIsEvent(dictMonadST))(memoize(dictMonadST)(counter(event))(function(cenv) {
          return map(functorEvent)(function(v) {
            return alt(altEvent(dictMonadST.Monad0().Applicative0()))(bang(eventIsEvent(dictMonadST))(new Insert(f(v.value0))))(map(functorEvent)($$const(Remove.value))(filter4(filterableEvent(dictMonadST.Monad0().Applicative0()))(function() {
              var $117 = eq(eqInt)(v.value1 + 1 | 0);
              return function($118) {
                return $117(snd($118));
              };
            }())(cenv)));
          })(cenv);
        })));
      };
    };
  };
  var flatten = function(dictApplicative) {
    return function(dictMonadST) {
      return function(v) {
        return function(psr) {
          return function(interpreter) {
            var element = function(v1) {
              return v1(psr)(interpreter);
            };
            return function(v1) {
              if (v1 instanceof FixedChildren$prime) {
                return oneOfMap(foldableArray)(plusEvent(dictApplicative))(flatten(dictApplicative)(dictMonadST)(v)(psr)(interpreter))(v1.value0);
              }
              ;
              if (v1 instanceof EventfulElement$prime) {
                return keepLatest(eventIsEvent(dictMonadST))(map(functorEvent)(flatten(dictApplicative)(dictMonadST)(v)(psr)(interpreter))(v1.value0));
              }
              ;
              if (v1 instanceof Element$prime) {
                return element(v.toElt(v1.value0));
              }
              ;
              if (v1 instanceof DynamicChildren$prime) {
                return makeEvent(function(v2) {
                  return bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(newSTRef(empty3)))(function(cancelInner) {
                    return bind(dictMonadST.Monad0().Bind1())(subscribe(v1.value0)(function(inner) {
                      return bind(dictMonadST.Monad0().Bind1())(v.ids(interpreter))(function(myUnsubId) {
                        return bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(newSTRef(pure(dictApplicative)(unit))))(function(myUnsub) {
                          return bind(dictMonadST.Monad0().Bind1())(v.ids(interpreter))(function(eltsUnsubId) {
                            return bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(newSTRef(pure(dictApplicative)(unit))))(function(eltsUnsub) {
                              return bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(newSTRef([])))(function(myIds) {
                                return bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(newSTRef(pure(dictApplicative)(unit))))(function(myImmediateCancellation) {
                                  return bind(dictMonadST.Monad0().Bind1())(map(dictApplicative.Apply0().Functor0())(Local.create)(v.ids(interpreter)))(function(myScope) {
                                    return bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(newSTRef(Begin.value)))(function(stageRef) {
                                      return bind(dictMonadST.Monad0().Bind1())(subscribe(inner)(function(kid$prime) {
                                        return bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(read(stageRef)))(function(stage) {
                                          if (kid$prime instanceof Logic && stage instanceof Middle) {
                                            return bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(read(myIds)))(traverse_(dictApplicative)(foldableArray)(function() {
                                              var $119 = v.doLogic(kid$prime.value0)(interpreter);
                                              return function($120) {
                                                return v2($119($120));
                                              };
                                            }()));
                                          }
                                          ;
                                          if (kid$prime instanceof Remove && stage instanceof Middle) {
                                            return discard(discardUnit)(dictMonadST.Monad0().Bind1())($$void(dictApplicative.Apply0().Functor0())(liftST(dictMonadST)(write(End.value)(stageRef))))(function() {
                                              var mic = applySecond(dictApplicative.Apply0())(applySecond(dictApplicative.Apply0())(applySecond(dictApplicative.Apply0())(applySecond(dictApplicative.Apply0())(bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(read(myIds)))(traverse_(dictApplicative)(foldableArray)(function(old) {
                                                return for_(dictApplicative)(foldableMaybe)(psr.parent)(function(pnt) {
                                                  return v2(v.disconnectElement(interpreter)({
                                                    id: old,
                                                    parent: pnt,
                                                    scope: myScope
                                                  }));
                                                });
                                              })))(join(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(read(myUnsub)))))(join(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(read(eltsUnsub)))))($$void(dictApplicative.Apply0().Functor0())(liftST(dictMonadST)(modify2($$delete3(myUnsubId))(cancelInner)))))($$void(dictApplicative.Apply0().Functor0())(liftST(dictMonadST)(modify2($$delete3(eltsUnsubId))(cancelInner))));
                                              return applySecond(dictApplicative.Apply0())($$void(dictApplicative.Apply0().Functor0())(liftST(dictMonadST)(write(mic)(myImmediateCancellation))))(mic);
                                            });
                                          }
                                          ;
                                          if (kid$prime instanceof Insert && stage instanceof Begin) {
                                            return discard(discardUnit)(dictMonadST.Monad0().Bind1())($$void(dictApplicative.Apply0().Functor0())(liftST(dictMonadST)(write(Middle.value)(stageRef))))(function() {
                                              return bind(dictMonadST.Monad0().Bind1())(subscribe(flatten(dictApplicative)(dictMonadST)(v)({
                                                parent: psr.parent,
                                                scope: myScope,
                                                raiseId: function(id) {
                                                  return $$void(dictApplicative.Apply0().Functor0())(liftST(dictMonadST)(modify2(append(semigroupArray)([id]))(myIds)));
                                                }
                                              })(interpreter)(kid$prime.value0))(v2))(function(c1) {
                                                return discard(discardUnit)(dictMonadST.Monad0().Bind1())($$void(dictApplicative.Apply0().Functor0())(liftST(dictMonadST)(modify2(insert3(eltsUnsubId)(c1))(cancelInner))))(function() {
                                                  return $$void(dictApplicative.Apply0().Functor0())(liftST(dictMonadST)(write(c1)(eltsUnsub)));
                                                });
                                              });
                                            });
                                          }
                                          ;
                                          return pure(dictApplicative)(unit);
                                        });
                                      }))(function(c0) {
                                        return discard(discardUnit)(dictMonadST.Monad0().Bind1())($$void(dictApplicative.Apply0().Functor0())(liftST(dictMonadST)(write(c0)(myUnsub))))(function() {
                                          return discard(discardUnit)(dictMonadST.Monad0().Bind1())($$void(dictApplicative.Apply0().Functor0())(liftST(dictMonadST)(modify2(insert3(myUnsubId)(c0))(cancelInner))))(function() {
                                            return join(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(read(myImmediateCancellation)));
                                          });
                                        });
                                      });
                                    });
                                  });
                                });
                              });
                            });
                          });
                        });
                      });
                    }))(function(cancelOuter) {
                      return pure(dictApplicative)(discard(discardUnit)(dictMonadST.Monad0().Bind1())(bind(dictMonadST.Monad0().Bind1())(liftST(dictMonadST)(read(cancelInner)))(foldl(foldableObject)(applySecond(dictApplicative.Apply0()))(pure(dictApplicative)(unit))))(function() {
                        return cancelOuter;
                      }));
                    });
                  });
                });
              }
              ;
              throw new Error("Failed pattern match at Bolson.Control (line 521, column 17 - line 604, column 20): " + [v1.constructor.name]);
            };
          };
        };
      };
    };
  };

  // output/Deku.Attribute/index.js
  var Cb = function(x) {
    return x;
  };
  var Prop$prime = /* @__PURE__ */ function() {
    function Prop$prime2(value0) {
      this.value0 = value0;
    }
    ;
    Prop$prime2.create = function(value0) {
      return new Prop$prime2(value0);
    };
    return Prop$prime2;
  }();
  var Cb$prime = /* @__PURE__ */ function() {
    function Cb$prime2(value0) {
      this.value0 = value0;
    }
    ;
    Cb$prime2.create = function(value0) {
      return new Cb$prime2(value0);
    };
    return Cb$prime2;
  }();
  var Attribute = function(x) {
    return x;
  };
  var unsafeUnAttribute = /* @__PURE__ */ coerce();
  var unsafeAttribute = Attribute;
  var prop$prime = /* @__PURE__ */ function() {
    return Prop$prime.create;
  }();
  var cb$prime = /* @__PURE__ */ function() {
    return Cb$prime.create;
  }();
  var cb = /* @__PURE__ */ function() {
    var $6 = map(functorFn)(map(functorEffect)($$const(true)));
    return function($7) {
      return Cb($6($7));
    };
  }();
  var attr = function(dict) {
    return dict.attr;
  };

  // output/Deku.Control/index.js
  var unsafeText = function(v) {
    return v.makeText;
  };
  var unsafeSetText = function(v) {
    return function(id) {
      return function(txt) {
        return map(functorEvent)(function($112) {
          return v.setText(function(v1) {
            return {
              id,
              text: v1
            };
          }($112));
        })(txt);
      };
    };
  };
  var unsafeSetAttribute = function(v) {
    return function(id) {
      return function(atts) {
        return map(functorEvent)(function($113) {
          return function(v1) {
            if (v1.value instanceof Prop$prime) {
              return v.setProp({
                id,
                key: v1.key,
                value: v1.value.value0
              });
            }
            ;
            if (v1.value instanceof Cb$prime) {
              return v.setCb({
                id,
                key: v1.key,
                value: v1.value.value0
              });
            }
            ;
            throw new Error("Failed pattern match at Deku.Control (line 75, column 26 - line 77, column 45): " + [v1.value.constructor.name]);
          }(unsafeUnAttribute($113));
        })(atts);
      };
    };
  };
  var unsafeElement = function(v) {
    return v.makeElement;
  };
  var unsafeConnect = function(v) {
    return v.attributeParent;
  };
  var text = function(dictMonad) {
    return function(txt) {
      var go2 = function(v) {
        return function(v1) {
          return makeEvent(function(k) {
            return bind(dictMonad.Bind1())(v1.ids)(function(me) {
              return discard(discardUnit)(dictMonad.Bind1())(v.raiseId(me))(function() {
                return map(dictMonad.Bind1().Apply0().Functor0())(applySecond(dictMonad.Bind1().Apply0())(k(v1.deleteFromCache({
                  id: me
                }))))(subscribe(oneOf(foldableArray)(plusEvent(dictMonad.Applicative0()))([bang2(dictMonad.Applicative0())(unsafeText(v1)({
                  id: me,
                  parent: v.parent,
                  scope: v.scope
                })), unsafeSetText(v1)(me)(txt)]))(k));
              });
            });
          });
        };
      };
      return new Element$prime(go2);
    };
  };
  var text_ = function(dictMonad) {
    return function(txt) {
      return text(dictMonad)(bang2(dictMonad.Applicative0())(txt));
    };
  };
  var __internalDekuFlatten = function(dictKorok) {
    return flatten(dictKorok.MonadST5().Monad0().Applicative0())(dictKorok.MonadST5())({
      doLogic: function(v) {
        return function(v1) {
          return function(id) {
            return v1.sendToTop({
              id
            });
          };
        };
      },
      ids: function() {
        var $114 = unwrap();
        return function($115) {
          return function(v) {
            return v.ids;
          }($114($115));
        };
      }(),
      disconnectElement: function(v) {
        return function(v1) {
          return v.disconnectElement({
            id: v1.id,
            scope: v1.scope,
            parent: v1.parent,
            scopeEq: eq(eqScope)
          });
        };
      },
      toElt: function(v) {
        return v;
      }
    });
  };
  var deku = function(dictKorok) {
    return function(root) {
      return function(children) {
        return function(v) {
          return makeEvent(function(k) {
            return bind(dictKorok.MonadST5().Monad0().Bind1())(v.ids)(function(me) {
              return subscribe(alt(altEvent(dictKorok.MonadST5().Monad0().Applicative0()))(bang2(dictKorok.MonadST5().Monad0().Applicative0())(v.makeRoot({
                id: me,
                root
              })))(__internalDekuFlatten(dictKorok)({
                parent: new Just(me),
                scope: new Local("rootScope"),
                raiseId: function(v1) {
                  return pure(dictKorok.MonadST5().Monad0().Applicative0())(unit);
                }
              })(v)(children)))(k);
            });
          });
        };
      };
    };
  };
  var elementify = function(dictKorok) {
    return function(tag) {
      return function(atts) {
        return function(children) {
          var go2 = function(v) {
            return function(v1) {
              return makeEvent(function(k) {
                return bind(dictKorok.MonadST5().Monad0().Bind1())(v1.ids)(function(me) {
                  return discard(discardUnit)(dictKorok.MonadST5().Monad0().Bind1())(v.raiseId(me))(function() {
                    return map(dictKorok.MonadST5().Monad0().Bind1().Apply0().Functor0())(applySecond(dictKorok.MonadST5().Monad0().Bind1().Apply0())(k(v1.deleteFromCache({
                      id: me
                    }))))(subscribe(alt(altEvent(dictKorok.MonadST5().Monad0().Applicative0()))(oneOf(foldableArray)(plusEvent(dictKorok.MonadST5().Monad0().Applicative0()))(append(semigroupArray)([bang2(dictKorok.MonadST5().Monad0().Applicative0())(unsafeElement(v1)({
                      id: me,
                      parent: v.parent,
                      scope: v.scope,
                      tag
                    })), unsafeSetAttribute(v1)(me)(atts)])(maybe([])(function(p) {
                      return [bang2(dictKorok.MonadST5().Monad0().Applicative0())(unsafeConnect(v1)({
                        id: me,
                        parent: p
                      }))];
                    })(v.parent))))(__internalDekuFlatten(dictKorok)({
                      parent: new Just(me),
                      scope: v.scope,
                      raiseId: function(v2) {
                        return pure(dictKorok.MonadST5().Monad0().Applicative0())(unit);
                      }
                    })(v1)(children)))(k));
                  });
                });
              });
            };
          };
          return go2;
        };
      };
    };
  };

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output/Data.Enum/index.js
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var defaultSucc = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a) {
        return toEnum$prime(fromEnum$prime(a) + 1 | 0);
      };
    };
  };
  var defaultPred = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a) {
        return toEnum$prime(fromEnum$prime(a) - 1 | 0);
      };
    };
  };
  var charToEnum = function(v) {
    if (v >= bottom(boundedInt) && v <= top(boundedInt)) {
      return new Just(fromCharCode(v));
    }
    ;
    return Nothing.value;
  };
  var enumChar = {
    succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
    pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
    Ord0: function() {
      return ordChar;
    }
  };
  var boundedEnumChar = /* @__PURE__ */ function() {
    return {
      cardinality: toCharCode(top(boundedChar)) - toCharCode(bottom(boundedChar)) | 0,
      toEnum: charToEnum,
      fromEnum: toCharCode,
      Bounded0: function() {
        return boundedChar;
      },
      Enum1: function() {
        return enumChar;
      }
    };
  }();

  // output/Deku.Core/index.js
  var korokGlobalEffect = {
    Always0: function() {
      return always2(monoidEffect(monoidUnit));
    },
    Always1: function() {
      return always2(monoidEffect(monoidEffect(monoidUnit)));
    },
    Always2: function() {
      return always2(monoidEffect(monoidUnit));
    },
    Always3: function() {
      return always2(monoidEndo(categoryFn));
    },
    Always4: function() {
      return always2(monoidEndo(categoryFn));
    },
    MonadST5: function() {
      return monadSTEffect;
    }
  };

  // output/Deku.DOM.Attr.OnInput/index.js
  var OnInput = /* @__PURE__ */ function() {
    function OnInput2() {
    }
    ;
    OnInput2.value = new OnInput2();
    return OnInput2;
  }();
  var attrOnInputCb = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "input",
          value: cb$prime(value12)
        });
      };
    }
  };

  // output/Deku.DOM.Attr.Style/index.js
  var Style = /* @__PURE__ */ function() {
    function Style2() {
    }
    ;
    Style2.value = new Style2();
    return Style2;
  }();
  var attrDiv_StyleString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "style",
          value: prop$prime(value12)
        });
      };
    }
  };

  // output/Deku.DOM.Elt.Div/index.js
  var div2 = function(dictKorok) {
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify(dictKorok)("div")(attributes)(fixed(kids)));
      };
    };
  };
  var div_ = function(dictKorok) {
    return div2(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.DOM.Elt.Input/index.js
  var input = function(dictKorok) {
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify(dictKorok)("input")(attributes)(fixed(kids)));
      };
    };
  };

  // output/Deku.DOM.Elt.Sub/index.js
  var sub2 = function(dictKorok) {
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify(dictKorok)("sub")(attributes)(fixed(kids)));
      };
    };
  };
  var sub_ = function(dictKorok) {
    return sub2(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.DOM.Elt.Table/index.js
  var table = function(dictKorok) {
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify(dictKorok)("table")(attributes)(fixed(kids)));
      };
    };
  };
  var table_ = function(dictKorok) {
    return table(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.DOM.Elt.Tbody/index.js
  var tbody = function(dictKorok) {
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify(dictKorok)("tbody")(attributes)(fixed(kids)));
      };
    };
  };
  var tbody_ = function(dictKorok) {
    return tbody(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.DOM.Elt.Td/index.js
  var td = function(dictKorok) {
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify(dictKorok)("td")(attributes)(fixed(kids)));
      };
    };
  };
  var td_ = function(dictKorok) {
    return td(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.DOM.Elt.Tr/index.js
  var tr = function(dictKorok) {
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify(dictKorok)("tr")(attributes)(fixed(kids)));
      };
    };
  };
  var tr_ = function(dictKorok) {
    return tr(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.Interpret/foreign.js
  var connectXToY_ = (maybe2, x, y$, state3) => {
    maybe2((y) => state3.units[y].main.appendChild(state3.units[x].main))(y$);
  };
  var attributeParent_ = (a) => (state3) => () => {
    if (!state3.units[a.id].main.parentNode) {
      state3.units[a.parent].main.appendChild(state3.units[a.id].main);
    }
  };
  var makeElement_ = (tryHydration) => (a) => (state3) => () => {
    var dom2;
    var ptr = a.id;
    if (!state3.scopes[a.scope]) {
      state3.scopes[a.scope] = [];
    }
    state3.scopes[a.scope].push(ptr);
    if (tryHydration && a.parent.value0 && (dom2 = document.body.querySelectorAll("[data-deku-ssr-" + ptr + "]").item(0))) {
      state3.units[ptr] = {
        listeners: {},
        parent: a.parent,
        scope: a.scope,
        main: dom2
      };
    } else {
      state3.units[ptr] = {
        listeners: {},
        parent: a.parent,
        scope: a.scope,
        main: document.createElement(a.tag)
      };
    }
  };
  var makeText_ = (tryHydration) => (maybe2) => (a) => (state3) => () => {
    var ptr = a.id;
    var dom2;
    if (!state3.scopes[a.scope]) {
      state3.scopes[a.scope] = [];
    }
    state3.scopes[a.scope].push(ptr);
    if (tryHydration && a.parent.value0 && (dom2 = document.body.querySelectorAll("[data-deku-ssr-" + a.parent.value0 + "]").item(0))) {
      state3.units[ptr] = {
        main: dom2.childNodes[0],
        parent: a.parent,
        scope: a.scope
      };
    } else {
      state3.units[ptr] = {
        main: document.createTextNode(""),
        parent: a.parent,
        scope: a.scope
      };
      connectXToY_(maybe2, ptr, a.parent, state3);
    }
  };
  function makeFFIDOMSnapshot() {
    return {
      units: {},
      scopes: {}
    };
  }
  var setProp_ = (tryHydration) => (a) => (state3) => () => {
    var ptr = a.id;
    var avv = a.value;
    if (tryHydration && !state3.units[ptr] && (dom = document.body.querySelectorAll("[data-deku-ssr-" + ptr + "]").item(0))) {
      state3.units[ptr] = {
        listeners: {},
        parent: a.parent,
        scope: a.scope,
        main: dom
      };
      if (!state3.scopes[a.scope]) {
        state3.scopes[a.scope] = [];
      }
      state3.scopes[a.scope].push(ptr);
    }
    if (state3.units[ptr].main.tagName === "INPUT" && a.key === "value") {
      state3.units[ptr].main.value = avv;
    } else if (state3.units[ptr].main.tagName === "INPUT" && a.key === "checked") {
      state3.units[ptr].main.checked = avv === "true";
    } else {
      state3.units[ptr].main.setAttribute(a.key, avv);
    }
  };
  var setCb_ = (tryHydration) => (a) => (state3) => () => {
    var ptr = a.id;
    var avv = a.value;
    if (tryHydration && !state3.units[ptr] && (dom = document.body.querySelectorAll("[data-deku-ssr-" + ptr + "]").item(0))) {
      state3.units[ptr] = {
        listeners: {},
        parent: a.parent,
        scope: a.scope,
        main: dom
      };
      if (!state3.scopes[a.scope]) {
        state3.scopes[a.scope] = [];
      }
      state3.scopes[a.scope].push(ptr);
    }
    if (a.key === "@self@") {
      avv(state3.units[ptr].main)();
    } else {
      if (state3.units[ptr].listeners[a.key]) {
        state3.units[ptr].main.removeEventListener(a.key, state3.units[ptr].listeners[a.key]);
      }
      var el = (e) => avv(e)();
      state3.units[ptr].main.addEventListener(a.key, el);
      state3.units[ptr].listeners[a.key] = el;
    }
  };
  var setText_ = (a) => (state3) => () => {
    var ptr = a.id;
    state3.units[ptr].main.nodeValue = a.text;
  };
  var makePursx_ = (tryHydration) => (maybe2) => (a) => (state3) => () => {
    var dom2;
    var tmp;
    var ptr = a.id;
    var html = a.html;
    var verb = a.verb;
    var cache = a.cache;
    var parent2 = a.parent;
    var scope2 = a.scope;
    var pxScope = a.pxScope;
    if (tryHydration && a.parent.value0 && (dom2 = document.body.querySelectorAll("[data-deku-ssr-" + ptr + "]").item(0))) {
      state3.units[ptr] = {
        listeners: {},
        scope: scope2,
        parent: parent2,
        main: dom2
      };
    } else {
      const entries = Object.entries(cache);
      for (var i = 0; i < entries.length; i++) {
        const key = entries[i][0];
        if (entries[i][1] === true) {
          html = html.replace(verb + key + verb, 'data-deku-attr-internal="' + key + '"');
        } else {
          html = html.replace(verb + key + verb, '<span style="display:contents;" data-deku-elt-internal="' + key + '"></span>');
        }
      }
      tmp = document.createElement("div");
      tmp.innerHTML = html.trim();
      state3.units[ptr] = {
        listeners: {},
        scope: scope2,
        parent: parent2,
        main: tmp.firstChild
      };
    }
    if (!state3.scopes[scope2]) {
      state3.scopes[scope2] = [];
    }
    state3.scopes[scope2].push(ptr);
    if (!tmp) {
      tmp = dom2;
    }
    tmp.querySelectorAll("[data-deku-attr-internal]").forEach(function(e) {
      var key = e.getAttribute("data-deku-attr-internal");
      const namespacedKey = key + pxScope;
      state3.units[namespacedKey] = {
        listeners: {},
        main: e,
        scope: scope2
      };
      state3.scopes[scope2].push(namespacedKey);
    });
    tmp.querySelectorAll("[data-deku-elt-internal]").forEach(function(e) {
      var key = e.getAttribute("data-deku-elt-internal");
      const namespacedKey = key + pxScope;
      state3.units[key + pxScope] = {
        listeners: {},
        main: e,
        scope: scope2
      };
      state3.scopes[scope2].push(namespacedKey);
    });
    if (!dom2) {
      connectXToY_(maybe2, ptr, parent2, state3);
    }
  };
  var makeRoot_ = (a) => (state3) => () => {
    var ptr = a.id;
    state3.units[ptr] = {
      main: a.root
    };
  };
  var giveNewParent_ = (a) => (state3) => () => {
    var ptr = a.id;
    var parent2 = a.parent;
    state3.units[ptr].containingScope = a.scope;
    state3.units[parent2].main.prepend(state3.units[ptr].main);
  };
  var disconnectElement_ = (a) => (state3) => () => {
    var ptr = a.id;
    if (state3.units[ptr].noop) {
      return;
    }
    if (state3.units[ptr].containingScope && !a.scopeEq(state3.units[ptr].containingScope)(a.scope)) {
      return;
    }
    state3.units[ptr].main.remove();
  };
  var deleteFromCache_ = (a) => (state3) => () => {
    delete state3.units[a.id];
  };
  var sendToTop_ = (a) => (state3) => () => {
    var ptr = a.id;
    state3.units[ptr].main.parentNode.prepend(state3.units[ptr].main);
  };

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber = function(n) {
    return n;
  };

  // output/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  var floor = Math.floor;
  var remainder = function(n) {
    return function(m) {
      return n % m;
    };
  };

  // output/Data.Int/index.js
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x) {
    if (!isFiniteImpl(x)) {
      return 0;
    }
    ;
    if (x >= toNumber(top(boundedInt))) {
      return top(boundedInt);
    }
    ;
    if (x <= toNumber(bottom(boundedInt))) {
      return bottom(boundedInt);
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
  };
  var floor2 = function($25) {
    return unsafeClamp(floor($25));
  };

  // output/Random.LCG/index.js
  var unSeed = function(v) {
    return v;
  };
  var seedMin = 1;
  var lcgM = 2147483647;
  var seedMax = /* @__PURE__ */ function() {
    return lcgM - 1 | 0;
  }();
  var mkSeed = function(x) {
    var ensureBetween = function(min5) {
      return function(max6) {
        return function(n) {
          var rangeSize = max6 - min5 | 0;
          var n$prime = mod(euclideanRingInt)(n)(rangeSize);
          var $13 = n$prime < min5;
          if ($13) {
            return n$prime + max6 | 0;
          }
          ;
          return n$prime;
        };
      };
    };
    return ensureBetween(seedMin)(seedMax)(x);
  };
  var lcgC = 0;
  var lcgA = 48271;
  var lcgPerturb = function(d) {
    return function(v) {
      return fromJust()(fromNumber(remainder(toNumber(lcgA) * toNumber(v) + toNumber(d))(toNumber(lcgM))));
    };
  };
  var lcgNext = /* @__PURE__ */ lcgPerturb(lcgC);

  // output/Data.Array.NonEmpty.Internal/foreign.js
  var traverse1Impl = function() {
    function Cont(fn) {
      this.fn = fn;
    }
    var emptyList = {};
    var ConsCell = function(head4, tail2) {
      this.head = head4;
      this.tail = tail2;
    };
    function finalCell(head4) {
      return new ConsCell(head4, emptyList);
    }
    function consList(x) {
      return function(xs) {
        return new ConsCell(x, xs);
      };
    }
    function listToArray(list) {
      var arr = [];
      var xs = list;
      while (xs !== emptyList) {
        arr.push(xs.head);
        xs = xs.tail;
      }
      return arr;
    }
    return function(apply3) {
      return function(map2) {
        return function(f) {
          var buildFrom = function(x, ys) {
            return apply3(map2(consList)(f(x)))(ys);
          };
          var go2 = function(acc, currentLen, xs) {
            if (currentLen === 0) {
              return acc;
            } else {
              var last2 = xs[currentLen - 1];
              return new Cont(function() {
                var built = go2(buildFrom(last2, acc), currentLen - 1, xs);
                return built;
              });
            }
          };
          return function(array) {
            var acc = map2(finalCell)(f(array[array.length - 1]));
            var result = go2(acc, array.length - 1, array);
            while (result instanceof Cont) {
              result = result.fn();
            }
            return map2(listToArray)(result);
          };
        };
      };
    };
  }();

  // output/Data.String.CodeUnits/foreign.js
  var length4 = function(s) {
    return s.length;
  };
  var drop2 = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };

  // output/Data.String.Unsafe/foreign.js
  var charAt = function(i) {
    return function(s) {
      if (i >= 0 && i < s.length)
        return s.charAt(i);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };

  // output/Control.Monad.State.Trans/index.js
  var functorStateT = function(dictFunctor) {
    return {
      map: function(f) {
        return function(v) {
          return function(s) {
            return map(dictFunctor)(function(v1) {
              return new Tuple(f(v1.value0), v1.value1);
            })(v(s));
          };
        };
      }
    };
  };
  var monadStateT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeStateT(dictMonad);
      },
      Bind1: function() {
        return bindStateT(dictMonad);
      }
    };
  };
  var bindStateT = function(dictMonad) {
    return {
      bind: function(v) {
        return function(f) {
          return function(s) {
            return bind(dictMonad.Bind1())(v(s))(function(v1) {
              var v3 = f(v1.value0);
              return v3(v1.value1);
            });
          };
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var applyStateT = function(dictMonad) {
    return {
      apply: ap(monadStateT(dictMonad)),
      Functor0: function() {
        return functorStateT(dictMonad.Bind1().Apply0().Functor0());
      }
    };
  };
  var applicativeStateT = function(dictMonad) {
    return {
      pure: function(a) {
        return function(s) {
          return pure(dictMonad.Applicative0())(new Tuple(a, s));
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var monadStateStateT = function(dictMonad) {
    return {
      state: function(f) {
        var $112 = pure(dictMonad.Applicative0());
        return function($113) {
          return $112(f($113));
        };
      },
      Monad0: function() {
        return monadStateT(dictMonad);
      }
    };
  };

  // output/Control.Monad.State/index.js
  var evalState = function(v) {
    return function(s) {
      var v1 = v(s);
      return v1.value0;
    };
  };

  // output/Test.QuickCheck.Gen/index.js
  var unGen = function(v) {
    return v;
  };
  var lcgStep = /* @__PURE__ */ function() {
    var f = function(s) {
      return new Tuple(unSeed(s.newSeed), function() {
        var $30 = {};
        for (var $31 in s) {
          if ({}.hasOwnProperty.call(s, $31)) {
            $30[$31] = s[$31];
          }
          ;
        }
        ;
        $30.newSeed = lcgNext(s.newSeed);
        return $30;
      }());
    };
    return state(monadStateStateT(monadIdentity))(f);
  }();
  var functorGen = /* @__PURE__ */ functorStateT(functorIdentity);
  var evalGen = function($40) {
    return evalState(unGen($40));
  };
  var applyGen = /* @__PURE__ */ applyStateT(monadIdentity);
  var chooseInt$prime = function(a) {
    return function(b) {
      var numB = toNumber(b);
      var numA = toNumber(a);
      var clamp = function(x) {
        return numA + remainder(x)(numB - numA + 1);
      };
      var choose31BitPosNumber = map(functorGen)(toNumber)(lcgStep);
      var choose32BitPosNumber = apply(applyGen)(map(functorGen)(add(semiringNumber))(choose31BitPosNumber))(map(functorGen)(mul(semiringNumber)(2))(choose31BitPosNumber));
      return map(functorGen)(function($45) {
        return floor2(clamp($45));
      })(choose32BitPosNumber);
    };
  };
  var chooseInt2 = function(a) {
    return function(b) {
      var $37 = a <= b;
      if ($37) {
        return chooseInt$prime(a)(b);
      }
      ;
      return chooseInt$prime(b)(a);
    };
  };

  // output/Test.QuickCheck.Arbitrary/index.js
  var arbitrary = function(dict) {
    return dict.arbitrary;
  };
  var arbInt = /* @__PURE__ */ function() {
    return {
      arbitrary: chooseInt2(-1e6 | 0)(1e6)
    };
  }();

  // output/Deku.Interpret/index.js
  var fullDOMInterpret = function(seed) {
    return {
      ids: function __do() {
        var s = read2(seed)();
        var o = show(showInt)(evalGen(arbitrary(arbInt))({
          newSeed: mkSeed(s),
          size: 5
        }));
        $$void(functorEffect)(modify(add(semiringInt)(1))(seed))();
        return o;
      },
      makeElement: makeElement_(false),
      attributeParent: attributeParent_,
      makeRoot: makeRoot_,
      makeText: makeText_(false)(maybe(unit)),
      makePursx: makePursx_(false)(maybe(unit)),
      setProp: setProp_(false),
      setCb: setCb_(false),
      setText: setText_,
      sendToTop: sendToTop_,
      deleteFromCache: deleteFromCache_,
      giveNewParent: giveNewParent_,
      disconnectElement: disconnectElement_
    };
  };

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.Internal.FFI/foreign.js
  function _unsafeReadProtoTagged(nothing, just, name15, value12) {
    if (typeof window !== "undefined") {
      var ty = window[name15];
      if (ty != null && value12 instanceof ty) {
        return just(value12);
      }
    }
    var obj = value12;
    while (obj != null) {
      var proto = Object.getPrototypeOf(obj);
      var constructorName = proto.constructor.name;
      if (constructorName === name15) {
        return just(value12);
      } else if (constructorName === "Object") {
        return nothing;
      }
      obj = proto;
    }
    return nothing;
  }

  // output/Web.Internal.FFI/index.js
  var unsafeReadProtoTagged = function(name15) {
    return function(value12) {
      return _unsafeReadProtoTagged(Nothing.value, Just.create, name15, value12);
    };
  };

  // output/Data.Nullable/foreign.js
  function nullable(a, r, f) {
    return a == null ? r : f(a);
  }

  // output/Data.Nullable/index.js
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Web.HTML.HTMLDocument/foreign.js
  function _body(doc) {
    return function() {
      return doc.body;
    };
  }

  // output/Web.HTML.HTMLDocument/index.js
  var body = /* @__PURE__ */ function() {
    var $10 = map(functorEffect)(toMaybe);
    return function($11) {
      return $10(_body($11));
    };
  }();

  // output/Web.HTML.HTMLElement/index.js
  var toElement = unsafeCoerce2;

  // output/Web.HTML.HTMLInputElement/foreign.js
  function value3(input2) {
    return function() {
      return input2.value;
    };
  }

  // output/Web.HTML.HTMLInputElement/index.js
  var fromEventTarget = /* @__PURE__ */ unsafeReadProtoTagged("HTMLInputElement");

  // output/Web.HTML.Window/foreign.js
  function document2(window2) {
    return function() {
      return window2.document;
    };
  }

  // output/Deku.Toplevel/index.js
  var runInElement$prime = function(elt) {
    return function(eee) {
      return function __do() {
        var ffi = makeFFIDOMSnapshot();
        var evt = mapFlipped(functorEffect)($$new(0))(function() {
          var $33 = deku(korokGlobalEffect)(elt)(eee);
          return function($34) {
            return $33(fullDOMInterpret($34));
          };
        }())();
        return subscribe(evt)(function(i) {
          return i(ffi);
        })();
      };
    };
  };
  var runInBody$prime = function(eee) {
    return function __do() {
      var b$prime = bind(bindEffect)(bind(bindEffect)(windowImpl)(document2))(body)();
      return maybe(mempty(monoidEffect(monoidEffect(monoidUnit))))(function(elt) {
        return runInElement$prime(elt)(eee);
      })(map(functorMaybe)(toElement)(b$prime))();
    };
  };
  var runInBody = function(a) {
    return $$void(functorEffect)(runInBody$prime(a));
  };

  // output/Data.Show.Generic/foreign.js
  var intercalate4 = function(separator) {
    return function(xs) {
      return xs.join(separator);
    };
  };

  // output/Data.Show.Generic/index.js
  var genericShowArgsNoArguments = {
    genericShowArgs: function(v) {
      return [];
    }
  };
  var genericShowArgsArgument = function(dictShow) {
    return {
      genericShowArgs: function(v) {
        return [show(dictShow)(v)];
      }
    };
  };
  var genericShowArgs = function(dict) {
    return dict.genericShowArgs;
  };
  var genericShowArgsProduct = function(dictGenericShowArgs) {
    return function(dictGenericShowArgs1) {
      return {
        genericShowArgs: function(v) {
          return append(semigroupArray)(genericShowArgs(dictGenericShowArgs)(v.value0))(genericShowArgs(dictGenericShowArgs1)(v.value1));
        }
      };
    };
  };
  var genericShowConstructor = function(dictGenericShowArgs) {
    return function(dictIsSymbol) {
      return {
        "genericShow'": function(v) {
          var ctor = reflectSymbol(dictIsSymbol)($$Proxy.value);
          var v1 = genericShowArgs(dictGenericShowArgs)(v);
          if (v1.length === 0) {
            return ctor;
          }
          ;
          return "(" + (intercalate4(" ")(append(semigroupArray)([ctor])(v1)) + ")");
        }
      };
    };
  };
  var genericShow$prime = function(dict) {
    return dict["genericShow'"];
  };
  var genericShowSum = function(dictGenericShow) {
    return function(dictGenericShow1) {
      return {
        "genericShow'": function(v) {
          if (v instanceof Inl) {
            return genericShow$prime(dictGenericShow)(v.value0);
          }
          ;
          if (v instanceof Inr) {
            return genericShow$prime(dictGenericShow1)(v.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Show.Generic (line 26, column 1 - line 28, column 40): " + [v.constructor.name]);
        }
      };
    };
  };
  var genericShow = function(dictGeneric) {
    return function(dictGenericShow) {
      return function(x) {
        return genericShow$prime(dictGenericShow)(from(dictGeneric)(x));
      };
    };
  };

  // output/Parser.Proto/index.js
  var Zero = /* @__PURE__ */ function() {
    function Zero2(value0) {
      this.value0 = value0;
    }
    ;
    Zero2.create = function(value0) {
      return new Zero2(value0);
    };
    return Zero2;
  }();
  var Snoc = /* @__PURE__ */ function() {
    function Snoc2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Snoc2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Snoc2(value0, value1, value22);
        };
      };
    };
    return Snoc2;
  }();
  var $$Error = /* @__PURE__ */ function() {
    function $$Error2() {
    }
    ;
    $$Error2.value = new $$Error2();
    return $$Error2;
  }();
  var Complete = /* @__PURE__ */ function() {
    function Complete2(value0) {
      this.value0 = value0;
    }
    ;
    Complete2.create = function(value0) {
      return new Complete2(value0);
    };
    return Complete2;
  }();
  var Step = /* @__PURE__ */ function() {
    function Step2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Step2.create = function(value0) {
      return function(value1) {
        return new Step2(value0, value1);
      };
    };
    return Step2;
  }();
  var topOf = function(v) {
    if (v instanceof Zero) {
      return v.value0;
    }
    ;
    if (v instanceof Snoc) {
      return v.value2;
    }
    ;
    throw new Error("Failed pattern match at Parser.Proto (line 21, column 1 - line 21, column 52): " + [v.constructor.name]);
  };
  var startParse = function(inputs) {
    return function(initialState) {
      return {
        stack: new Zero(initialState),
        inputs
      };
    };
  };
  var parseStep = function(v) {
    return function(v1) {
      if (v1.inputs instanceof Nil) {
        return new Left(new Just(v1.stack));
      }
      ;
      if (v1.inputs instanceof Cons) {
        var v2 = v.step(topOf(v1.stack))(v1.inputs.value0);
        if (v2 instanceof Nothing) {
          return new Left(Nothing.value);
        }
        ;
        if (v2 instanceof Just && v2.value0 instanceof Left) {
          var stack$prime = new Snoc(v1.stack, v.promote(v1.inputs.value0), v2.value0.value0);
          return new Right({
            stack: stack$prime,
            inputs: v1.inputs.value1
          });
        }
        ;
        if (v2 instanceof Just && v2.value0 instanceof Right) {
          var v3 = v["goto"](v2.value0.value0)(v1.stack);
          if (v3 instanceof Nothing) {
            return new Left(Nothing.value);
          }
          ;
          if (v3 instanceof Just) {
            return new Right({
              stack: v3.value0,
              inputs: v1.inputs
            });
          }
          ;
          throw new Error("Failed pattern match at Parser.Proto (line 63, column 9 - line 65, column 65): " + [v3.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Parser.Proto (line 57, column 24 - line 65, column 65): " + [v2.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Parser.Proto (line 55, column 3 - line 65, column 65): " + [v1.inputs.constructor.name]);
    };
  };
  var parseSteps = function(table2) {
    return function(inputs) {
      return function(initialState) {
        var go2 = function(step4) {
          var v = parseStep(table2)(step4);
          if (v instanceof Left && v.value0 instanceof Nothing) {
            return $$Error.value;
          }
          ;
          if (v instanceof Left && v.value0 instanceof Just) {
            return new Complete(v.value0.value0);
          }
          ;
          if (v instanceof Right) {
            return new Step(v.value0, go2(v.value0));
          }
          ;
          throw new Error("Failed pattern match at Parser.Proto (line 71, column 15 - line 74, column 43): " + [v.constructor.name]);
        };
        var step3 = startParse(inputs)(initialState);
        return new Step(step3, go2(step3));
      };
    };
  };

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _unsafeCodePointAt0 = function(fallback) {
    return hasCodePointAt ? function(str) {
      return str.codePointAt(0);
    } : fallback;
  };
  var _toCodePointArray = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasArrayFrom) {
        return function(str) {
          return Array.from(str, unsafeCodePointAt02);
        };
      }
      return fallback;
    };
  };

  // output/Data.String.CodePoints/index.js
  var CodePoint = function(x) {
    return x;
  };
  var unsurrogate = function(lead) {
    return function(trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };
  var isTrail = function(cu) {
    return 56320 <= cu && cu <= 57343;
  };
  var isLead = function(cu) {
    return 55296 <= cu && cu <= 56319;
  };
  var uncons2 = function(s) {
    var v = length4(s);
    if (v === 0) {
      return Nothing.value;
    }
    ;
    if (v === 1) {
      return new Just({
        head: fromEnum(boundedEnumChar)(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum(boundedEnumChar)(charAt(1)(s));
    var cu0 = fromEnum(boundedEnumChar)(charAt(0)(s));
    var $21 = isLead(cu0) && isTrail(cu1);
    if ($21) {
      return new Just({
        head: unsurrogate(cu0)(cu1),
        tail: drop2(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop2(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map(functorMaybe)(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons2(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr(unfoldableArray)(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum(boundedEnumChar)(charAt(0)(s));
    var $25 = isLead(cu0) && length4(s) > 1;
    if ($25) {
      var cu1 = fromEnum(boundedEnumChar)(charAt(1)(s));
      var $26 = isTrail(cu1);
      if ($26) {
        return unsurrogate(cu0)(cu1);
      }
      ;
      return cu0;
    }
    ;
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var eqCodePoint = {
    eq: function(x) {
      return function(y) {
        return x === y;
      };
    }
  };
  var ordCodePoint = {
    compare: function(x) {
      return function(y) {
        return compare(ordInt)(x)(y);
      };
    },
    Eq0: function() {
      return eqCodePoint;
    }
  };
  var codePointFromChar = /* @__PURE__ */ function() {
    var $55 = fromEnum(boundedEnumChar);
    return function($56) {
      return CodePoint($55($56));
    };
  }();

  // output/Parser.ProtoG8/index.js
  var $runtime_lazy2 = function(name15, moduleName, init2) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init2();
      state3 = 2;
      return val;
    };
  };
  var LParen = /* @__PURE__ */ function() {
    function LParen2() {
    }
    ;
    LParen2.value = new LParen2();
    return LParen2;
  }();
  var RParen = /* @__PURE__ */ function() {
    function RParen2() {
    }
    ;
    RParen2.value = new RParen2();
    return RParen2;
  }();
  var Comma = /* @__PURE__ */ function() {
    function Comma2() {
    }
    ;
    Comma2.value = new Comma2();
    return Comma2;
  }();
  var X = /* @__PURE__ */ function() {
    function X2() {
    }
    ;
    X2.value = new X2();
    return X2;
  }();
  var EOF = /* @__PURE__ */ function() {
    function EOF2() {
    }
    ;
    EOF2.value = new EOF2();
    return EOF2;
  }();
  var S0 = /* @__PURE__ */ function() {
    function S02() {
    }
    ;
    S02.value = new S02();
    return S02;
  }();
  var SA = /* @__PURE__ */ function() {
    function SA2() {
    }
    ;
    SA2.value = new SA2();
    return SA2;
  }();
  var S1 = /* @__PURE__ */ function() {
    function S12() {
    }
    ;
    S12.value = new S12();
    return S12;
  }();
  var S2 = /* @__PURE__ */ function() {
    function S22() {
    }
    ;
    S22.value = new S22();
    return S22;
  }();
  var S3 = /* @__PURE__ */ function() {
    function S32() {
    }
    ;
    S32.value = new S32();
    return S32;
  }();
  var S4 = /* @__PURE__ */ function() {
    function S42() {
    }
    ;
    S42.value = new S42();
    return S42;
  }();
  var S5 = /* @__PURE__ */ function() {
    function S52() {
    }
    ;
    S52.value = new S52();
    return S52;
  }();
  var S6 = /* @__PURE__ */ function() {
    function S62() {
    }
    ;
    S62.value = new S62();
    return S62;
  }();
  var S7 = /* @__PURE__ */ function() {
    function S72() {
    }
    ;
    S72.value = new S72();
    return S72;
  }();
  var S8 = /* @__PURE__ */ function() {
    function S82() {
    }
    ;
    S82.value = new S82();
    return S82;
  }();
  var RE = /* @__PURE__ */ function() {
    function RE3() {
    }
    ;
    RE3.value = new RE3();
    return RE3;
  }();
  var RL = /* @__PURE__ */ function() {
    function RL3() {
    }
    ;
    RL3.value = new RL3();
    return RL3;
  }();
  var RE1 = /* @__PURE__ */ function() {
    function RE12() {
    }
    ;
    RE12.value = new RE12();
    return RE12;
  }();
  var RE2 = /* @__PURE__ */ function() {
    function RE22() {
    }
    ;
    RE22.value = new RE22();
    return RE22;
  }();
  var RL1 = /* @__PURE__ */ function() {
    function RL12() {
    }
    ;
    RL12.value = new RL12();
    return RL12;
  }();
  var RL2 = /* @__PURE__ */ function() {
    function RL22() {
    }
    ;
    RL22.value = new RL22();
    return RL22;
  }();
  var T = /* @__PURE__ */ function() {
    function T2(value0) {
      this.value0 = value0;
    }
    ;
    T2.create = function(value0) {
      return new T2(value0);
    };
    return T2;
  }();
  var NT = /* @__PURE__ */ function() {
    function NT2(value0) {
      this.value0 = value0;
    }
    ;
    NT2.create = function(value0) {
      return new NT2(value0);
    };
    return NT2;
  }();
  var E1 = /* @__PURE__ */ function() {
    function E12(value0) {
      this.value0 = value0;
    }
    ;
    E12.create = function(value0) {
      return new E12(value0);
    };
    return E12;
  }();
  var E2 = /* @__PURE__ */ function() {
    function E22() {
    }
    ;
    E22.value = new E22();
    return E22;
  }();
  var L1 = /* @__PURE__ */ function() {
    function L12(value0) {
      this.value0 = value0;
    }
    ;
    L12.create = function(value0) {
      return new L12(value0);
    };
    return L12;
  }();
  var L2 = /* @__PURE__ */ function() {
    function L22(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    L22.create = function(value0) {
      return function(value1) {
        return new L22(value0, value1);
      };
    };
    return L22;
  }();
  var E = /* @__PURE__ */ function() {
    function E3(value0) {
      this.value0 = value0;
    }
    ;
    E3.create = function(value0) {
      return new E3(value0);
    };
    return E3;
  }();
  var L = /* @__PURE__ */ function() {
    function L3(value0) {
      this.value0 = value0;
    }
    ;
    L3.create = function(value0) {
      return new L3(value0);
    };
    return L3;
  }();
  var Tok = /* @__PURE__ */ function() {
    function Tok2(value0) {
      this.value0 = value0;
    }
    ;
    Tok2.create = function(value0) {
      return new Tok2(value0);
    };
    return Tok2;
  }();
  var genericTok = {
    to: function(x) {
      if (x instanceof Inl) {
        return LParen.value;
      }
      ;
      if (x instanceof Inr && x.value0 instanceof Inl) {
        return RParen.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && x.value0.value0 instanceof Inl)) {
        return Comma.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && x.value0.value0.value0 instanceof Inl))) {
        return X.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && x.value0.value0.value0 instanceof Inr))) {
        return EOF.value;
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 104, column 1 - line 104, column 44): " + [x.constructor.name]);
    },
    from: function(x) {
      if (x instanceof LParen) {
        return new Inl(NoArguments.value);
      }
      ;
      if (x instanceof RParen) {
        return new Inr(new Inl(NoArguments.value));
      }
      ;
      if (x instanceof Comma) {
        return new Inr(new Inr(new Inl(NoArguments.value)));
      }
      ;
      if (x instanceof X) {
        return new Inr(new Inr(new Inr(new Inl(NoArguments.value))));
      }
      ;
      if (x instanceof EOF) {
        return new Inr(new Inr(new Inr(new Inr(NoArguments.value))));
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 104, column 1 - line 104, column 44): " + [x.constructor.name]);
    }
  };
  var showTok = {
    show: /* @__PURE__ */ genericShow(genericTok)(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments)({
      reflectSymbol: function() {
        return "LParen";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments)({
      reflectSymbol: function() {
        return "RParen";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments)({
      reflectSymbol: function() {
        return "Comma";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments)({
      reflectSymbol: function() {
        return "X";
      }
    }))(/* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments)({
      reflectSymbol: function() {
        return "EOF";
      }
    }))))))
  };
  var genericState = {
    to: function(x) {
      if (x instanceof Inl) {
        return S0.value;
      }
      ;
      if (x instanceof Inr && x.value0 instanceof Inl) {
        return SA.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && x.value0.value0 instanceof Inl)) {
        return S1.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && x.value0.value0.value0 instanceof Inl))) {
        return S2.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0 instanceof Inl)))) {
        return S3.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0 instanceof Inl))))) {
        return S4.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0 instanceof Inl)))))) {
        return S5.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))) {
        return S6.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))) {
        return S7.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr)))))))) {
        return S8.value;
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 29, column 1 - line 29, column 48): " + [x.constructor.name]);
    },
    from: function(x) {
      if (x instanceof S0) {
        return new Inl(NoArguments.value);
      }
      ;
      if (x instanceof SA) {
        return new Inr(new Inl(NoArguments.value));
      }
      ;
      if (x instanceof S1) {
        return new Inr(new Inr(new Inl(NoArguments.value)));
      }
      ;
      if (x instanceof S2) {
        return new Inr(new Inr(new Inr(new Inl(NoArguments.value))));
      }
      ;
      if (x instanceof S3) {
        return new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))));
      }
      ;
      if (x instanceof S4) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))));
      }
      ;
      if (x instanceof S5) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))));
      }
      ;
      if (x instanceof S6) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))))));
      }
      ;
      if (x instanceof S7) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))))));
      }
      ;
      if (x instanceof S8) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(NoArguments.value)))))))));
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 29, column 1 - line 29, column 48): " + [x.constructor.name]);
    }
  };
  var showState = {
    show: /* @__PURE__ */ genericShow(genericState)(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments)({
      reflectSymbol: function() {
        return "S0";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments)({
      reflectSymbol: function() {
        return "SA";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments)({
      reflectSymbol: function() {
        return "S1";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments)({
      reflectSymbol: function() {
        return "S2";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments)({
      reflectSymbol: function() {
        return "S3";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments)({
      reflectSymbol: function() {
        return "S4";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments)({
      reflectSymbol: function() {
        return "S5";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments)({
      reflectSymbol: function() {
        return "S6";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments)({
      reflectSymbol: function() {
        return "S7";
      }
    }))(/* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments)({
      reflectSymbol: function() {
        return "S8";
      }
    })))))))))))
  };
  var genericParsed = {
    to: function(x) {
      if (x instanceof Inl) {
        return new E(x.value0);
      }
      ;
      if (x instanceof Inr && x.value0 instanceof Inl) {
        return new L(x.value0.value0);
      }
      ;
      if (x instanceof Inr && x.value0 instanceof Inr) {
        return new Tok(x.value0.value0);
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 122, column 1 - line 122, column 50): " + [x.constructor.name]);
    },
    from: function(x) {
      if (x instanceof E) {
        return new Inl(x.value0);
      }
      ;
      if (x instanceof L) {
        return new Inr(new Inl(x.value0));
      }
      ;
      if (x instanceof Tok) {
        return new Inr(new Inr(x.value0));
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 122, column 1 - line 122, column 50): " + [x.constructor.name]);
    }
  };
  var genericL = {
    to: function(x) {
      if (x instanceof Inl) {
        return new L1(x.value0);
      }
      ;
      if (x instanceof Inr) {
        return new L2(x.value0.value0, x.value0.value1);
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 138, column 1 - line 138, column 40): " + [x.constructor.name]);
    },
    from: function(x) {
      if (x instanceof L1) {
        return new Inl(x.value0);
      }
      ;
      if (x instanceof L2) {
        return new Inr(new Product(x.value0, x.value1));
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 138, column 1 - line 138, column 40): " + [x.constructor.name]);
    }
  };
  var genericE = {
    to: function(x) {
      if (x instanceof Inl) {
        return new E1(x.value0);
      }
      ;
      if (x instanceof Inr) {
        return E2.value;
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 130, column 1 - line 130, column 40): " + [x.constructor.name]);
    },
    from: function(x) {
      if (x instanceof E1) {
        return new Inl(x.value0);
      }
      ;
      if (x instanceof E2) {
        return new Inr(NoArguments.value);
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 130, column 1 - line 130, column 40): " + [x.constructor.name]);
    }
  };
  var showL = {
    show: function(x) {
      return genericShow(genericL)(genericShowSum(genericShowConstructor(genericShowArgsArgument($lazy_showE(0)))({
        reflectSymbol: function() {
          return "L1";
        }
      }))(genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument(showL))(genericShowArgsArgument($lazy_showE(0))))({
        reflectSymbol: function() {
          return "L2";
        }
      })))(x);
    }
  };
  var $lazy_showE = /* @__PURE__ */ $runtime_lazy2("showE", "Parser.ProtoG8", function() {
    return {
      show: genericShow(genericE)(genericShowSum(genericShowConstructor(genericShowArgsArgument(showL))({
        reflectSymbol: function() {
          return "E1";
        }
      }))(genericShowConstructor(genericShowArgsNoArguments)({
        reflectSymbol: function() {
          return "E2";
        }
      })))
    };
  });
  var showE = /* @__PURE__ */ $lazy_showE(131);
  var showParsed = {
    show: /* @__PURE__ */ genericShow(genericParsed)(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(showE))({
      reflectSymbol: function() {
        return "E";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(showL))({
      reflectSymbol: function() {
        return "L";
      }
    }))(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(showTok))({
      reflectSymbol: function() {
        return "Tok";
      }
    }))))
  };
  var g8Toks = /* @__PURE__ */ function() {
    return fromFoldable(ordCodePoint)(foldableArray)([new Tuple(codePointFromChar("("), new Just(LParen.value)), new Tuple(codePointFromChar(")"), new Just(RParen.value)), new Tuple(codePointFromChar(","), new Just(Comma.value)), new Tuple(codePointFromChar("x"), new Just(X.value)), new Tuple(codePointFromChar("$"), new Just(EOF.value)), new Tuple(codePointFromChar(" "), Nothing.value), new Tuple(codePointFromChar("	"), Nothing.value), new Tuple(codePointFromChar("\n"), Nothing.value)]);
  }();
  var g8Table = function() {
    var g8Step = function(v) {
      return function(v1) {
        if (v instanceof S0 && v1 instanceof EOF) {
          return new Just(new Left(SA.value));
        }
        ;
        if (v instanceof S1 && v1 instanceof LParen) {
          return new Just(new Left(S3.value));
        }
        ;
        if (v instanceof S1 && v1 instanceof X) {
          return new Just(new Left(S2.value));
        }
        ;
        if (v instanceof S2) {
          return new Just(new Right(RE2.value));
        }
        ;
        if (v instanceof S3 && v1 instanceof LParen) {
          return new Just(new Left(S3.value));
        }
        ;
        if (v instanceof S3 && v1 instanceof X) {
          return new Just(new Left(S2.value));
        }
        ;
        if (v instanceof S4 && v1 instanceof RParen) {
          return new Just(new Left(S6.value));
        }
        ;
        if (v instanceof S4 && v1 instanceof Comma) {
          return new Just(new Left(S7.value));
        }
        ;
        if (v instanceof S5) {
          return new Just(new Right(RL1.value));
        }
        ;
        if (v instanceof S6) {
          return new Just(new Right(RE1.value));
        }
        ;
        if (v instanceof S7 && v1 instanceof LParen) {
          return new Just(new Left(S3.value));
        }
        ;
        if (v instanceof S7 && v1 instanceof X) {
          return new Just(new Left(S2.value));
        }
        ;
        if (v instanceof S8) {
          return new Just(new Right(RL2.value));
        }
        ;
        return Nothing.value;
      };
    };
    var g8Rules = function(v) {
      if (v instanceof RE1) {
        return [new T(LParen.value), new NT(RL.value), new T(RParen.value)];
      }
      ;
      if (v instanceof RE2) {
        return [new T(X.value)];
      }
      ;
      if (v instanceof RL1) {
        return [new NT(RE.value)];
      }
      ;
      if (v instanceof RL2) {
        return [new NT(RL.value), new T(Comma.value), new NT(RE.value)];
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 146, column 3 - line 146, column 44): " + [v.constructor.name]);
    };
    var g8RuleType = function(v) {
      if (v instanceof RE1) {
        return RE.value;
      }
      ;
      if (v instanceof RE2) {
        return RE.value;
      }
      ;
      if (v instanceof RL1) {
        return RL.value;
      }
      ;
      if (v instanceof RL2) {
        return RL.value;
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 175, column 3 - line 175, column 22): " + [v.constructor.name]);
    };
    var g8Goto = function(v) {
      return function(v1) {
        if (v instanceof RE && v1 instanceof S1) {
          return new Just(S0.value);
        }
        ;
        if (v instanceof RE && v1 instanceof S3) {
          return new Just(S5.value);
        }
        ;
        if (v instanceof RE && v1 instanceof S7) {
          return new Just(S8.value);
        }
        ;
        if (v instanceof RL && v1 instanceof S3) {
          return new Just(S4.value);
        }
        ;
        throw new Error("Failed pattern match at Parser.ProtoG8 (line 180, column 3 - line 180, column 25): " + [v.constructor.name, v1.constructor.name]);
      };
    };
    var g8Reduce = function(v) {
      return function(v1) {
        var v2 = function(v3) {
          var v4 = function(v5) {
            var v6 = function(v7) {
              if (v instanceof RL2 && (v1 instanceof Snoc && (v1.value0 instanceof Snoc && (v1.value0.value0 instanceof Snoc && (v1.value0.value0.value1 instanceof L && (v1.value0.value1 instanceof Tok && (v1.value0.value1.value0 instanceof Comma && v1.value1 instanceof E))))))) {
                var $227 = g8Goto(RL.value)(topOf(v1.value0.value0.value0));
                if ($227 instanceof Just) {
                  return new Just(new Snoc(v1.value0.value0.value0, new L(new L2(v1.value0.value0.value1.value0, v1.value1.value0)), $227.value0));
                }
                ;
                throw new Error("Failed pattern match at Parser.ProtoG8 (line 142, column 1 - line 142, column 50): " + [$227.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Parser.ProtoG8 (line 142, column 1 - line 142, column 50): " + [v.constructor.name, v1.constructor.name]);
            };
            if (v instanceof RL1 && (v1 instanceof Snoc && v1.value1 instanceof E)) {
              var $243 = g8Goto(RL.value)(topOf(v1.value0));
              if ($243 instanceof Just) {
                return new Just(new Snoc(v1.value0, new L(new L1(v1.value1.value0)), $243.value0));
              }
              ;
              return v6(true);
            }
            ;
            return v6(true);
          };
          if (v instanceof RE2 && (v1 instanceof Snoc && (v1.value1 instanceof Tok && v1.value1.value0 instanceof X))) {
            var $251 = g8Goto(RE.value)(topOf(v1.value0));
            if ($251 instanceof Just) {
              return new Just(new Snoc(v1.value0, new E(E2.value), $251.value0));
            }
            ;
            return v4(true);
          }
          ;
          return v4(true);
        };
        if (v instanceof RE1 && (v1 instanceof Snoc && (v1.value0 instanceof Snoc && (v1.value0.value0 instanceof Snoc && (v1.value0.value0.value1 instanceof Tok && (v1.value0.value0.value1.value0 instanceof LParen && (v1.value0.value1 instanceof L && (v1.value1 instanceof Tok && v1.value1.value0 instanceof RParen)))))))) {
          var $259 = g8Goto(RE.value)(topOf(v1.value0.value0.value0));
          if ($259 instanceof Just) {
            return new Just(new Snoc(v1.value0.value0.value0, new E(new E1(v1.value0.value1.value0)), $259.value0));
          }
          ;
          return v2(true);
        }
        ;
        return v2(true);
      };
    };
    return {
      step: g8Step,
      promote: Tok.create,
      "goto": g8Reduce
    };
  };
  var g8ParseResult = function(v) {
    if (v instanceof Snoc && (v.value0 instanceof Snoc && (v.value0.value0 instanceof Zero && (v.value0.value0.value0 instanceof S1 && (v.value0.value1 instanceof E && (v.value0.value2 instanceof S0 && (v.value1 instanceof Tok && (v.value1.value0 instanceof EOF && v.value2 instanceof SA)))))))) {
      return new Just(v.value0.value1.value0);
    }
    ;
    return Nothing.value;
  };
  var ensureEOF = function(toks) {
    var v = last(toks);
    if (v instanceof Just && v.value0 instanceof EOF) {
      return toks;
    }
    ;
    return append(semigroupArray)(toks)([EOF.value]);
  };
  var g8FromString = /* @__PURE__ */ function() {
    var $364 = map(functorMaybe)(function() {
      var $367 = toUnfoldable(unfoldableList);
      return function($368) {
        return $367(ensureEOF(catMaybes($368)));
      };
    }());
    var $365 = traverse(traversableArray)(applicativeMaybe)(flip(lookup(ordCodePoint))(g8Toks));
    return function($366) {
      return $364($365(toCodePointArray($366)));
    };
  }();

  // output/Web.Event.Event/foreign.js
  function _target(e) {
    return e.target;
  }

  // output/Web.Event.Event/index.js
  var target5 = function($0) {
    return toMaybe(_target($0));
  };

  // output/Parser.Main/index.js
  var showStack = function(dictShow) {
    return function(dictShow1) {
      return function(i) {
        return function(dictKorok) {
          var go2 = function(v) {
            if (v instanceof Zero) {
              return [sub_(dictKorok)([text_(dictKorok.MonadST5().Monad0())(show(dictShow)(v.value0))])];
            }
            ;
            if (v instanceof Snoc) {
              return append(semigroupArray)(go2(v.value0))(append(semigroupArray)([text_(dictKorok.MonadST5().Monad0())(show(dictShow1)(v.value1))])([sub_(dictKorok)([text_(dictKorok.MonadST5().Monad0())(show(dictShow)(v.value2))])]));
            }
            ;
            throw new Error("Failed pattern match at Parser.Main (line 143, column 3 - line 143, column 54): " + [v.constructor.name]);
          };
          return fixed(go2(i));
        };
      };
    };
  };
  var showParseStep = function(dictShow) {
    return function(dictShow1) {
      return function(dictShow2) {
        return function(v) {
          return function(dictKorok) {
            if (v instanceof Left && v.value0 instanceof Nothing) {
              return text_(dictKorok.MonadST5().Monad0())("Parse error");
            }
            ;
            if (v instanceof Left && v.value0 instanceof Just) {
              return div_(dictKorok)([text_(dictKorok.MonadST5().Monad0())(show(showMaybe(showE))(g8ParseResult(v.value0.value0)))]);
            }
            ;
            if (v instanceof Right) {
              return div2(dictKorok)(bang2(dictKorok.MonadST5().Monad0().Applicative0())(attr(attrDiv_StyleString)(Style.value)("display: flex; justify-content: space-between")))([div_(dictKorok)([showStack(dictShow1)(dictShow)(v.value0.stack)(dictKorok)]), div_(dictKorok)([text_(dictKorok.MonadST5().Monad0())(show(dictShow2)(v.value0.inputs))])]);
            }
            ;
            throw new Error("Failed pattern match at Parser.Main (line 156, column 1 - line 166, column 9): " + [v.constructor.name]);
          };
        };
      };
    };
  };
  var showParseSteps = function(dictShow) {
    return function(i) {
      return function(dictKorok) {
        var go2 = function() {
          var s = function(v) {
            return showParseStep(showParsed)(showState)(showList(dictShow))(v)(dictKorok);
          };
          return function(v) {
            if (v instanceof $$Error) {
              return [s(new Left(Nothing.value))];
            }
            ;
            if (v instanceof Complete) {
              return [s(new Left(new Just(v.value0)))];
            }
            ;
            if (v instanceof Step) {
              return append(semigroupArray)([s(new Right(v.value0))])(go2(v.value1));
            }
            ;
            throw new Error("Failed pattern match at Parser.Main (line 180, column 7 - line 183, column 58): " + [v.constructor.name]);
          };
        }();
        return fixed(go2(i));
      };
    };
  };
  var showMaybeParseSteps = function(dictShow) {
    return function(v) {
      return function(dictKorok) {
        if (v instanceof Nothing) {
          return text_(dictKorok.MonadST5().Monad0())("Parse error");
        }
        ;
        if (v instanceof Just) {
          return showParseSteps(dictShow)(v.value0)(dictKorok);
        }
        ;
        throw new Error("Failed pattern match at Parser.Main (line 152, column 1 - line 152, column 125): " + [v.constructor.name]);
      };
    };
  };
  var main = /* @__PURE__ */ function() {
    return runInBody(vbussed()(monadSTEffect)(vbusCons2({
      reflectSymbol: function() {
        return "changeText";
      }
    })()()(vbusNil)()()()())($$Proxy.value)(function(push2) {
      return function(event) {
        var top2 = [input(korokGlobalEffect)(oneOfMap(foldableArray)(plusEvent(applicativeEffect))(bang2(applicativeEffect))([attr(attrOnInputCb)(OnInput.value)(cb(function(e) {
          return for_(applicativeEffect)(foldableMaybe)(bind(bindMaybe)(target5(e))(fromEventTarget))(composeKleisli(bindEffect)(value3)(push2.changeText));
        }))]))([])];
        var currentValue = alt(altEvent(applicativeEffect))(bang2(applicativeEffect)(""))(event.changeText);
        return div_(korokGlobalEffect)([table_(korokGlobalEffect)(pure(applicativeArray)(tbody_(korokGlobalEffect)(map(functorArray)(function() {
          var $154 = tr_(korokGlobalEffect);
          var $155 = map(functorArray)(td_(korokGlobalEffect));
          return function($156) {
            return $154($155($156));
          };
        }())([[[text_(monadEffect)("E")], [text_(monadEffect)("::=")], [text_(monadEffect)("("), text_(monadEffect)("L"), text_(monadEffect)(")")], [text_(monadEffect)("data E")], [text_(monadEffect)("=")], [text_(monadEffect)("E1"), text_(monadEffect)(" "), text_(monadEffect)("L")]], [[], [text_(monadEffect)("|")], [text_(monadEffect)("x")], [], [text_(monadEffect)("|")], [text_(monadEffect)("E2")]], [[text_(monadEffect)("L")], [text_(monadEffect)("::=")], [text_(monadEffect)("E")], [text_(monadEffect)("data L")], [text_(monadEffect)("=")], [text_(monadEffect)("L1"), text_(monadEffect)(" "), text_(monadEffect)("E")]], [[], [text_(monadEffect)("|")], [text_(monadEffect)("L"), text_(monadEffect)(","), text_(monadEffect)("E")], [], [text_(monadEffect)("|")], [text_(monadEffect)("L2"), text_(monadEffect)(" "), text_(monadEffect)("L"), text_(monadEffect)(" "), text_(monadEffect)("E")]]])))), div_(korokGlobalEffect)(top2), div_(korokGlobalEffect)(pure(applicativeArray)(flip(switcher(monadSTEffect))(currentValue)(function(v) {
          return div_(korokGlobalEffect)([showMaybeParseSteps(showTok)(flap(functorMaybe)(map(functorMaybe)(parseSteps(g8Table()))(g8FromString(v)))(S1.value))(korokGlobalEffect)]);
        })))]);
      };
    }));
  }();

  // output/Main/index.js
  var main2 = main;

  // <stdin>
  main2();
})();
