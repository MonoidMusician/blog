(() => {
  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq4) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq4 : gt;
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
  var eqArrayImpl = function(f) {
    return function(xs) {
      return function(ys) {
        if (xs.length !== ys.length)
          return false;
        for (var i = 0; i < xs.length; i++) {
          if (!f(xs[i])(ys[i]))
            return false;
        }
        return true;
      };
    };
  };

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
  var unsafeGet = function(label4) {
    return function(rec) {
      return rec[label4];
    };
  };

  // output/Data.Eq/index.js
  var eqRowNil = {
    eqRecord: function(v) {
      return function(v1) {
        return function(v2) {
          return true;
        };
      };
    }
  };
  var eqRecord = function(dict) {
    return dict.eqRecord;
  };
  var eqRec = function() {
    return function(dictEqRecord) {
      return {
        eq: eqRecord(dictEqRecord)($$Proxy.value)
      };
    };
  };
  var eqInt = {
    eq: eqIntImpl
  };
  var eqChar = {
    eq: eqCharImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };
  var eqArray = function(dictEq) {
    return {
      eq: eqArrayImpl(eq(dictEq))
    };
  };
  var eqRowCons = function(dictEqRecord) {
    var eqRecord1 = eqRecord(dictEqRecord);
    return function() {
      return function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        return function(dictEq) {
          var eq32 = eq(dictEq);
          return {
            eqRecord: function(v) {
              return function(ra) {
                return function(rb) {
                  var tail2 = eqRecord1($$Proxy.value)(ra)(rb);
                  var key2 = reflectSymbol2($$Proxy.value);
                  var get2 = unsafeGet(key2);
                  return eq32(get2(ra))(get2(rb)) && tail2;
                };
              };
            }
          };
        };
      };
    };
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
    var map1 = map(dictFunctor);
    return function(fa) {
      return function(f) {
        return map1(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map1 = map(dictFunctor);
    return function(f) {
      return function(x) {
        return map1($$const(x))(f);
      };
    };
  };
  var functorFn = {
    map: /* @__PURE__ */ compose(semigroupoidFn)
  };
  var functorArray = {
    map: arrayMap
  };
  var flap = function(dictFunctor) {
    var map1 = map(dictFunctor);
    return function(ff2) {
      return function(x) {
        return map1(function(f) {
          return f(x);
        })(ff2);
      };
    };
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
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
    var apply1 = apply(dictApply);
    var map11 = map(dictApply.Functor0());
    return function(a) {
      return function(b) {
        return apply1(map11($$const(identity2))(a))(b);
      };
    };
  };
  var lift2 = function(dictApply) {
    var apply1 = apply(dictApply);
    var map11 = map(dictApply.Functor0());
    return function(f) {
      return function(a) {
        return function(b) {
          return apply1(map11(f)(a))(b);
        };
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var when = function(dictApplicative) {
    var pure1 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure1(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply4 = apply(dictApplicative.Apply0());
    var pure1 = pure(dictApplicative);
    return function(f) {
      return function(a) {
        return apply4(pure1(f))(a);
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
  var identity3 = /* @__PURE__ */ identity(categoryFn);
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
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var composeKleisli = function(dictBind) {
    var bind1 = bind(dictBind);
    return function(f) {
      return function(g) {
        return function(a) {
          return bind1(f(a))(g);
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
    var bind1 = bind(dictBind);
    return function(m) {
      return bind1(m)(identity3);
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
  var pure_ = function(a) {
    return function() {
      return a;
    };
  };
  var bind_ = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };
  var foreach = function(as) {
    return function(f) {
      return function() {
        for (var i = 0, l = as.length; i < l; i++) {
          f(as[i])();
        }
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
    var bind3 = bind(dictMonad.Bind1());
    var pure3 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a) {
        return bind3(f)(function(f$prime) {
          return bind3(a)(function(a$prime) {
            return pure3(f$prime(a$prime));
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
  var showArrayImpl = function(f) {
    return function(xs) {
      var ss = [];
      for (var i = 0, l = xs.length; i < l; i++) {
        ss[i] = f(xs[i]);
      }
      return "[" + ss.join(",") + "]";
    };
  };

  // output/Data.Show/index.js
  var showInt = {
    show: showIntImpl
  };
  var show = function(dict) {
    return dict.show;
  };
  var showArray = function(dictShow) {
    return {
      show: showArrayImpl(show(dictShow))
    };
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
  var identity4 = /* @__PURE__ */ identity(categoryFn);
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
    var show3 = show(dictShow);
    return {
      show: function(v) {
        if (v instanceof Just) {
          return "(Just " + (show3(v.value0) + ")");
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
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var fromMaybe = function(a) {
    return maybe(a)(identity4);
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
  var eqMaybe = function(dictEq) {
    var eq4 = eq(dictEq);
    return {
      eq: function(x) {
        return function(y) {
          if (x instanceof Nothing && y instanceof Nothing) {
            return true;
          }
          ;
          if (x instanceof Just && y instanceof Just) {
            return eq4(x.value0)(y.value0);
          }
          ;
          return false;
        };
      }
    };
  };
  var ordMaybe = function(dictOrd) {
    var compare3 = compare(dictOrd);
    var eqMaybe1 = eqMaybe(dictOrd.Eq0());
    return {
      compare: function(x) {
        return function(y) {
          if (x instanceof Nothing && y instanceof Nothing) {
            return EQ.value;
          }
          ;
          if (x instanceof Nothing) {
            return LT.value;
          }
          ;
          if (y instanceof Nothing) {
            return GT.value;
          }
          ;
          if (x instanceof Just && y instanceof Just) {
            return compare3(x.value0)(y.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Maybe (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
        };
      },
      Eq0: function() {
        return eqMaybe1;
      }
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map2(v.value0)(v1);
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
    var state4 = 0;
    var val;
    return function(lineNumber) {
      if (state4 === 2)
        return val;
      if (state4 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state4 = 1;
      val = init2();
      state4 = 2;
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
  var lift22 = /* @__PURE__ */ lift2(applyEffect);
  var semigroupEffect = function(dictSemigroup) {
    return {
      append: lift22(append(dictSemigroup))
    };
  };
  var monoidEffect = function(dictMonoid) {
    var semigroupEffect1 = semigroupEffect(dictMonoid.Semigroup0());
    return {
      mempty: pureE(mempty(dictMonoid)),
      Semigroup0: function() {
        return semigroupEffect1;
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
  var $runtime_lazy2 = function(name15, moduleName, init2) {
    var state4 = 0;
    var val;
    return function(lineNumber) {
      if (state4 === 2)
        return val;
      if (state4 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state4 = 1;
      val = init2();
      state4 = 2;
      return val;
    };
  };
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
  var monadST = {
    Applicative0: function() {
      return applicativeST;
    },
    Bind1: function() {
      return bindST;
    }
  };
  var bindST = {
    bind: bind_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var applicativeST = {
    pure: pure_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var $lazy_applyST = /* @__PURE__ */ $runtime_lazy2("applyST", "Control.Monad.ST.Internal", function() {
    return {
      apply: ap(monadST),
      Functor0: function() {
        return functorST;
      }
    };
  });

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
    return function(foldr3) {
      return function(xs) {
        return listToArray(foldr3(curryCons)(emptyList)(xs));
      };
    };
  }();
  var length = function(xs) {
    return xs.length;
  };
  var unconsImpl = function(empty4) {
    return function(next) {
      return function(xs) {
        return xs.length === 0 ? empty4({}) : next(xs[0])(xs.slice(1));
      };
    };
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
  var filter = function(f) {
    return function(xs) {
      return xs.filter(f);
    };
  };
  var partition = function(f) {
    return function(xs) {
      var yes = [];
      var no = [];
      for (var i = 0; i < xs.length; i++) {
        var x = xs[i];
        if (f(x))
          yes.push(x);
        else
          no.push(x);
      }
      return { yes, no };
    };
  };
  var sortByImpl = function() {
    function mergeFromTo(compare3, fromOrdering, xs1, xs2, from2, to2) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from2 + (to2 - from2 >> 1);
      if (mid - from2 > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, from2, mid);
      if (to2 - mid > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, mid, to2);
      i = from2;
      j = mid;
      k = from2;
      while (i < mid && j < to2) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare3(x)(y));
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
    return function(compare3) {
      return function(fromOrdering) {
        return function(xs) {
          var out;
          if (xs.length < 2)
            return xs;
          out = xs.slice(0);
          mergeFromTo(compare3, fromOrdering, out, xs.slice(0), 0, xs.length);
          return out;
        };
      };
    };
  }();
  var slice = function(s) {
    return function(e) {
      return function(l) {
        return l.slice(s, e);
      };
    };
  };
  var any = function(p) {
    return function(xs) {
      var len = xs.length;
      for (var i = 0; i < len; i++) {
        if (p(xs[i]))
          return true;
      }
      return false;
    };
  };
  var unsafeIndexImpl = function(xs) {
    return function(n) {
      return xs[n];
    };
  };

  // output/Data.Array.ST/foreign.js
  function newSTArray() {
    return [];
  }
  var pushAll = function(as) {
    return function(xs) {
      return function() {
        return xs.push.apply(xs, as);
      };
    };
  };
  var unsafeFreeze = function(xs) {
    return function() {
      return xs;
    };
  };
  var sortByImpl2 = function() {
    function mergeFromTo(compare3, fromOrdering, xs1, xs2, from2, to2) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from2 + (to2 - from2 >> 1);
      if (mid - from2 > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, from2, mid);
      if (to2 - mid > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, mid, to2);
      i = from2;
      j = mid;
      k = from2;
      while (i < mid && j < to2) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare3(x)(y));
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
    return function(compare3) {
      return function(fromOrdering) {
        return function(xs) {
          return function() {
            if (xs.length < 2)
              return xs;
            mergeFromTo(compare3, fromOrdering, xs, xs.slice(0), 0, xs.length);
            return xs;
          };
        };
      };
    };
  }();

  // output/Data.Array.ST/index.js
  var push = function(a) {
    return pushAll([a]);
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

  // output/Data.Bifunctor/index.js
  var bimap = function(dict) {
    return dict.bimap;
  };

  // output/Data.Monoid.Endo/index.js
  var semigroupEndo = function(dictSemigroupoid) {
    var compose2 = compose(dictSemigroupoid);
    return {
      append: function(v) {
        return function(v1) {
          return compose2(v)(v1);
        };
      }
    };
  };
  var monoidEndo = function(dictCategory) {
    var semigroupEndo1 = semigroupEndo(dictCategory.Semigroupoid0());
    return {
      mempty: identity(dictCategory),
      Semigroup0: function() {
        return semigroupEndo1;
      }
    };
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var unwrap = function() {
    return coerce2;
  };

  // output/Data.Foldable/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var foldr = function(dict) {
    return dict.foldr;
  };
  var oneOf = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictPlus) {
      return foldr22(alt(dictPlus.Alt0()))(empty(dictPlus));
    };
  };
  var oneOfMap = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictPlus) {
      var alt5 = alt(dictPlus.Alt0());
      var empty4 = empty(dictPlus);
      return function(f) {
        return foldr22(function($448) {
          return alt5(f($448));
        })(empty4);
      };
    };
  };
  var traverse_ = function(dictApplicative) {
    var applySecond2 = applySecond(dictApplicative.Apply0());
    var pure3 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($449) {
          return applySecond2(f($449));
        })(pure3(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_1 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_1(dictFoldable));
    };
  };
  var sequence_ = function(dictApplicative) {
    var traverse_1 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return traverse_1(dictFoldable)(identity5);
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var intercalate2 = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictMonoid) {
      var append5 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
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
                acc: append5(v.acc)(append5(sep)(x))
              };
            };
          };
          return foldl22(go2)({
            init: true,
            acc: mempty3
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
      var mempty3 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty3;
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
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append5 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
          return function(acc) {
            return append5(f(x))(acc);
          };
        })(mempty3);
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
  var foldMap = function(dict) {
    return dict.foldMap;
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
    return function(apply4) {
      return function(map11) {
        return function(pure3) {
          return function(f) {
            return function(array) {
              function go2(bot, top4) {
                switch (top4 - bot) {
                  case 0:
                    return pure3([]);
                  case 1:
                    return map11(array1)(f(array[bot]));
                  case 2:
                    return apply4(map11(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply4(apply4(map11(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top4 - bot) / 4) * 2;
                    return apply4(map11(concat2)(go2(bot, pivot)))(go2(pivot, top4));
                }
              }
              return go2(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Traversable.Accum.Internal/index.js
  var stateL = function(v) {
    return v;
  };
  var functorStateL = {
    map: function(f) {
      return function(k) {
        return function(s) {
          var v = stateL(k)(s);
          return {
            accum: v.accum,
            value: f(v.value)
          };
        };
      };
    }
  };
  var applyStateL = {
    apply: function(f) {
      return function(x) {
        return function(s) {
          var v = stateL(f)(s);
          var v1 = stateL(x)(v.accum);
          return {
            accum: v1.accum,
            value: v.value(v1.value)
          };
        };
      };
    },
    Functor0: function() {
      return functorStateL;
    }
  };
  var applicativeStateL = {
    pure: function(a) {
      return function(s) {
        return {
          accum: s,
          value: a
        };
      };
    },
    Apply0: function() {
      return applyStateL;
    }
  };

  // output/Data.Traversable/index.js
  var identity6 = /* @__PURE__ */ identity(categoryFn);
  var traverse = function(dict) {
    return dict.traverse;
  };
  var sequenceDefault = function(dictTraversable) {
    var traverse2 = traverse(dictTraversable);
    return function(dictApplicative) {
      return traverse2(dictApplicative)(identity6);
    };
  };
  var traversableArray = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
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
  var mapAccumL = function(dictTraversable) {
    var traverse2 = traverse(dictTraversable)(applicativeStateL);
    return function(f) {
      return function(s0) {
        return function(xs) {
          return stateL(traverse2(function(a) {
            return function(s) {
              return f(s)(a);
            };
          })(xs))(s0);
        };
      };
    };
  };

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust6) {
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
                var tuple = fromJust6(maybe2);
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
    return function(fromJust6) {
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
                value12 = fromJust6(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
  };

  // output/Data.Unfoldable/index.js
  var fromJust3 = /* @__PURE__ */ fromJust();
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };

  // output/Data.Array/index.js
  var map3 = /* @__PURE__ */ map(functorST);
  var when2 = /* @__PURE__ */ when(applicativeST);
  var $$void2 = /* @__PURE__ */ $$void(functorST);
  var fromJust4 = /* @__PURE__ */ fromJust();
  var foldMap1 = /* @__PURE__ */ foldMap(foldableArray);
  var unsafeIndex = function() {
    return unsafeIndexImpl;
  };
  var unsafeIndex1 = /* @__PURE__ */ unsafeIndex();
  var uncons = /* @__PURE__ */ function() {
    return unconsImpl($$const(Nothing.value))(function(x) {
      return function(xs) {
        return new Just({
          head: x,
          tail: xs
        });
      };
    });
  }();
  var toUnfoldable = function(dictUnfoldable) {
    var unfoldr3 = unfoldr(dictUnfoldable);
    return function(xs) {
      var len = length(xs);
      var f = function(i) {
        if (i < len) {
          return new Just(new Tuple(unsafeIndex1(xs)(i), i + 1 | 0));
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Array (line 156, column 3 - line 158, column 26): " + [i.constructor.name]);
      };
      return unfoldr3(f)(0);
    };
  };
  var singleton2 = function(a) {
    return [a];
  };
  var $$null = function(xs) {
    return length(xs) === 0;
  };
  var nubByEq = function(eq22) {
    return function(xs) {
      return function __do() {
        var arr = newSTArray();
        foreach(xs)(function(x) {
          return function __do2() {
            var e = map3(function() {
              var $177 = any(function(v) {
                return eq22(v)(x);
              });
              return function($178) {
                return !$177($178);
              };
            }())(unsafeFreeze(arr))();
            return when2(e)($$void2(push(x)(arr)))();
          };
        })();
        return unsafeFreeze(arr)();
      }();
    };
  };
  var nubEq = function(dictEq) {
    return nubByEq(eq(dictEq));
  };
  var index = /* @__PURE__ */ function() {
    return indexImpl(Just.create)(Nothing.value);
  }();
  var last = function(xs) {
    return index(xs)(length(xs) - 1 | 0);
  };
  var span = function(p) {
    return function(arr) {
      var go2 = function($copy_i) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(i) {
          var v = index(arr)(i);
          if (v instanceof Just) {
            var $152 = p(v.value0);
            if ($152) {
              $copy_i = i + 1 | 0;
              return;
            }
            ;
            $tco_done = true;
            return new Just(i);
          }
          ;
          if (v instanceof Nothing) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          throw new Error("Failed pattern match at Data.Array (line 964, column 5 - line 966, column 25): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_i);
        }
        ;
        return $tco_result;
      };
      var breakIndex = go2(0);
      if (breakIndex instanceof Just && breakIndex.value0 === 0) {
        return {
          init: [],
          rest: arr
        };
      }
      ;
      if (breakIndex instanceof Just) {
        return {
          init: slice(0)(breakIndex.value0)(arr),
          rest: slice(breakIndex.value0)(length(arr))(arr)
        };
      }
      ;
      if (breakIndex instanceof Nothing) {
        return {
          init: arr,
          rest: []
        };
      }
      ;
      throw new Error("Failed pattern match at Data.Array (line 951, column 3 - line 957, column 30): " + [breakIndex.constructor.name]);
    };
  };
  var head = function(xs) {
    return index(xs)(0);
  };
  var fromFoldable = function(dictFoldable) {
    return fromFoldableImpl(foldr(dictFoldable));
  };
  var foldl2 = /* @__PURE__ */ foldl(foldableArray);
  var foldMap2 = function(dictMonoid) {
    return foldMap1(dictMonoid);
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
          return fromJust4(deleteAt(i)(v2));
        })(findIndex(v(v1))(v2));
      };
    };
  };
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
  var mapMaybe = function(f) {
    return concatMap(function() {
      var $185 = maybe([])(singleton2);
      return function($186) {
        return $185(f($186));
      };
    }());
  };
  var catMaybes = /* @__PURE__ */ mapMaybe(/* @__PURE__ */ identity(categoryFn));

  // output/Data.FoldableWithIndex/index.js
  var foldrWithIndex = function(dict) {
    return dict.foldrWithIndex;
  };
  var foldlWithIndex = function(dict) {
    return dict.foldlWithIndex;
  };
  var foldMapWithIndex = function(dict) {
    return dict.foldMapWithIndex;
  };

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
  var map4 = /* @__PURE__ */ map(functorList);
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
        var $281 = foldl(foldableList)(flip(f))(b);
        return function($282) {
          return $281(rev3($282));
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
      var append22 = append(dictMonoid.Semigroup0());
      var mempty3 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $283 = append22(acc);
          return function($284) {
            return $283(f($284));
          };
        })(mempty3);
      };
    }
  };
  var foldl3 = /* @__PURE__ */ foldl(foldableList);
  var intercalate3 = /* @__PURE__ */ intercalate2(foldableList)(monoidString);
  var showList = function(dictShow) {
    var show3 = show(dictShow);
    return {
      show: function(v) {
        if (v instanceof Nil) {
          return "Nil";
        }
        ;
        return "(" + (intercalate3(" : ")(map4(show3)(v)) + " : Nil)");
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
                return foldl3(flip(Cons.create))(Nil.value)(new Cons(v.value0, memo));
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
                return foldl3(flip(Cons.create))(Nil.value)(memo);
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

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
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
  var singleton4 = function(k) {
    return function(v) {
      return new Two(Leaf.value, k, v, Leaf.value);
    };
  };
  var lookup = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(k) {
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
            var v2 = compare3(k)(v.value1);
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
            var v3 = compare3(k)(v.value1);
            if (v3 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            var v4 = compare3(k)(v.value4);
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
  var functorMap = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Leaf) {
          return Leaf.value;
        }
        ;
        if (v1 instanceof Two) {
          return new Two(map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), map(functorMap)(v)(v1.value3));
        }
        ;
        if (v1 instanceof Three) {
          return new Three(map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), map(functorMap)(v)(v1.value3), v1.value4, v(v1.value5), map(functorMap)(v)(v1.value6));
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 116, column 1 - line 119, column 110): " + [v.constructor.name, v1.constructor.name]);
      };
    }
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
    var fromZipper1 = fromZipper(dictOrd);
    var compare3 = compare(dictOrd);
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
                  return fromZipper1(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                }
                ;
                if (v1.value0 instanceof TwoRight) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
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
                var v2 = compare3(k)(v1.value1);
                if (v2 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(ctx)(new Two(v1.value0, k, v, v1.value3));
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
                var v3 = compare3(k)(v1.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(ctx)(new Three(v1.value0, k, v, v1.value3, v1.value4, v1.value5, v1.value6));
                }
                ;
                var v4 = compare3(k)(v1.value4);
                if (v4 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(ctx)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, k, v, v1.value6));
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
  var pop = function(dictOrd) {
    var fromZipper1 = fromZipper(dictOrd);
    var compare3 = compare(dictOrd);
    return function(k) {
      var up = function($copy_ctxs) {
        return function($copy_tree) {
          var $tco_var_ctxs = $copy_ctxs;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(ctxs, tree) {
            if (ctxs instanceof Nil) {
              $tco_done = true;
              return tree;
            }
            ;
            if (ctxs instanceof Cons) {
              if (ctxs.value0 instanceof TwoLeft && (ctxs.value0.value2 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && (ctxs.value0.value0 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && (ctxs.value0.value2 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value3 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              $tco_done = true;
              return unsafeCrashWith("The impossible happened in partial function `up`.");
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): " + [ctxs.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_ctxs, $copy_tree);
          }
          ;
          return $tco_result;
        };
      };
      var removeMaxNode = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Two && (m.value0 instanceof Leaf && m.value3 instanceof Leaf)) {
              $tco_done1 = true;
              return up(ctx)(Leaf.value);
            }
            ;
            if (m instanceof Two) {
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three && (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf))) {
              $tco_done1 = true;
              return up(new Cons(new TwoRight(Leaf.value, m.value1, m.value2), ctx))(Leaf.value);
            }
            ;
            if (m instanceof Three) {
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            $tco_done1 = true;
            return unsafeCrashWith("The impossible happened in partial function `removeMaxNode`.");
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      var maxNode = function($copy_m) {
        var $tco_done2 = false;
        var $tco_result;
        function $tco_loop(m) {
          if (m instanceof Two && m.value3 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value1,
              value: m.value2
            };
          }
          ;
          if (m instanceof Two) {
            $copy_m = m.value3;
            return;
          }
          ;
          if (m instanceof Three && m.value6 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value4,
              value: m.value5
            };
          }
          ;
          if (m instanceof Three) {
            $copy_m = m.value6;
            return;
          }
          ;
          $tco_done2 = true;
          return unsafeCrashWith("The impossible happened in partial function `maxNode`.");
        }
        ;
        while (!$tco_done2) {
          $tco_result = $tco_loop($copy_m);
        }
        ;
        return $tco_result;
      };
      var down = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done3 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Leaf) {
              $tco_done3 = true;
              return Nothing.value;
            }
            ;
            if (m instanceof Two) {
              var v = compare3(k)(m.value1);
              if (m.value3 instanceof Leaf && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, up(ctx)(Leaf.value)));
              }
              ;
              if (v instanceof EQ) {
                var max6 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new TwoLeft(max6.key, max6.value, m.value3), ctx))(m.value0)));
              }
              ;
              if (v instanceof LT) {
                $tco_var_ctx = new Cons(new TwoLeft(m.value1, m.value2, m.value3), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three) {
              var leaves = function() {
                if (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf)) {
                  return true;
                }
                ;
                return false;
              }();
              var v = compare3(k)(m.value4);
              var v3 = compare3(k)(m.value1);
              if (leaves && v3 instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, fromZipper1(ctx)(new Two(Leaf.value, m.value4, m.value5, Leaf.value))));
              }
              ;
              if (leaves && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, fromZipper1(ctx)(new Two(Leaf.value, m.value1, m.value2, Leaf.value))));
              }
              ;
              if (v3 instanceof EQ) {
                var max6 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new ThreeLeft(max6.key, max6.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
              }
              ;
              if (v instanceof EQ) {
                var max6 = maxNode(m.value3);
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, removeMaxNode(new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max6.key, max6.value, m.value6), ctx))(m.value3)));
              }
              ;
              if (v3 instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeLeft(m.value1, m.value2, m.value3, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              if (v3 instanceof GT && v instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value3;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): " + [m.constructor.name]);
          }
          ;
          while (!$tco_done3) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      return down(Nil.value);
    };
  };
  var foldableMap = {
    foldr: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(z)(m.value3)))(m.value0);
          }
          ;
          if (m instanceof Three) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(f(m.value5)(foldr(foldableMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldl: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3);
          }
          ;
          if (m instanceof Three) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty3 = mempty(dictMonoid);
      var append22 = append(dictMonoid.Semigroup0());
      return function(f) {
        return function(m) {
          if (m instanceof Leaf) {
            return mempty3;
          }
          ;
          if (m instanceof Two) {
            return append22(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append22(f(m.value2))(foldMap(foldableMap)(dictMonoid)(f)(m.value3)));
          }
          ;
          if (m instanceof Three) {
            return append22(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append22(f(m.value2))(append22(foldMap(foldableMap)(dictMonoid)(f)(m.value3))(append22(f(m.value5))(foldMap(foldableMap)(dictMonoid)(f)(m.value6)))));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): " + [m.constructor.name]);
        };
      };
    }
  };
  var foldableWithIndexMap = {
    foldrWithIndex: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldrWithIndex(foldableWithIndexMap)(f)(f(m.value1)(m.value2)(foldrWithIndex(foldableWithIndexMap)(f)(z)(m.value3)))(m.value0);
          }
          ;
          if (m instanceof Three) {
            return foldrWithIndex(foldableWithIndexMap)(f)(f(m.value1)(m.value2)(foldrWithIndex(foldableWithIndexMap)(f)(f(m.value4)(m.value5)(foldrWithIndex(foldableWithIndexMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): " + [m.constructor.name]);
        };
      };
    },
    foldlWithIndex: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldlWithIndex(foldableWithIndexMap)(f)(f(m.value1)(foldlWithIndex(foldableWithIndexMap)(f)(z)(m.value0))(m.value2))(m.value3);
          }
          ;
          if (m instanceof Three) {
            return foldlWithIndex(foldableWithIndexMap)(f)(f(m.value4)(foldlWithIndex(foldableWithIndexMap)(f)(f(m.value1)(foldlWithIndex(foldableWithIndexMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): " + [m.constructor.name]);
        };
      };
    },
    foldMapWithIndex: function(dictMonoid) {
      var mempty3 = mempty(dictMonoid);
      var append22 = append(dictMonoid.Semigroup0());
      return function(f) {
        return function(m) {
          if (m instanceof Leaf) {
            return mempty3;
          }
          ;
          if (m instanceof Two) {
            return append22(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value0))(append22(f(m.value1)(m.value2))(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value3)));
          }
          ;
          if (m instanceof Three) {
            return append22(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value0))(append22(f(m.value1)(m.value2))(append22(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value3))(append22(f(m.value4)(m.value5))(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value6)))));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): " + [m.constructor.name]);
        };
      };
    },
    Foldable0: function() {
      return foldableMap;
    }
  };
  var foldlWithIndex2 = /* @__PURE__ */ foldlWithIndex(foldableWithIndexMap);
  var empty2 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var fromFoldable2 = function(dictOrd) {
    var insert1 = insert(dictOrd);
    return function(dictFoldable) {
      return foldl(dictFoldable)(function(m) {
        return function(v) {
          return insert1(v.value0)(v.value1)(m);
        };
      })(empty2);
    };
  };
  var $$delete = function(dictOrd) {
    var pop1 = pop(dictOrd);
    return function(k) {
      return function(m) {
        return maybe(m)(snd)(pop1(k)(m));
      };
    };
  };
  var alter = function(dictOrd) {
    var lookup1 = lookup(dictOrd);
    var delete1 = $$delete(dictOrd);
    var insert1 = insert(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = f(lookup1(k)(m));
          if (v instanceof Nothing) {
            return delete1(k)(m);
          }
          ;
          if (v instanceof Just) {
            return insert1(k)(v.value0)(m);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): " + [v.constructor.name]);
        };
      };
    };
  };
  var unionWith = function(dictOrd) {
    var alter1 = alter(dictOrd);
    return function(f) {
      return function(m1) {
        return function(m2) {
          var go2 = function(k) {
            return function(m) {
              return function(v) {
                return alter1(function() {
                  var $932 = maybe(v)(f(v));
                  return function($933) {
                    return Just.create($932($933));
                  };
                }())(k)(m);
              };
            };
          };
          return foldlWithIndex2(go2)(m2)(m1);
        };
      };
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
  var identity7 = /* @__PURE__ */ identity(categoryFn);
  var always2 = function(dictMonoid) {
    return {
      always: identity7,
      Monoid0: function() {
        return dictMonoid;
      }
    };
  };

  // output/Data.Profunctor/index.js
  var identity8 = /* @__PURE__ */ identity(categoryFn);
  var profunctorFn = {
    dimap: function(a2b) {
      return function(c2d) {
        return function(b2c) {
          return function($18) {
            return c2d(b2c(a2b($18)));
          };
        };
      };
    }
  };
  var dimap = function(dict) {
    return dict.dimap;
  };
  var lcmap = function(dictProfunctor) {
    var dimap1 = dimap(dictProfunctor);
    return function(a2b) {
      return dimap1(a2b)(identity8);
    };
  };

  // output/FRP.Event.Class/index.js
  var map5 = /* @__PURE__ */ map(functorTuple);
  var pure2 = /* @__PURE__ */ pure(applicativeMaybe);
  var keepLatest = function(dict) {
    return dict.keepLatest;
  };
  var fold2 = function(dict) {
    return dict.fold;
  };
  var mapAccum = function(dictIsEvent) {
    var filterMap2 = filterMap(dictIsEvent.Filterable1());
    var fold12 = fold2(dictIsEvent);
    return function(f) {
      return function(xs) {
        return function(acc) {
          return filterMap2(snd)(fold12(function(a) {
            return function(v) {
              return map5(pure2)(f(a)(v.value0));
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
  var identity9 = /* @__PURE__ */ identity(categoryFn);
  var $$void3 = /* @__PURE__ */ $$void(functorST);
  var discard2 = /* @__PURE__ */ discard(discardUnit);
  var append1 = /* @__PURE__ */ append(semigroupArray);
  var AnEvent = function(x) {
    return x;
  };
  var subscribe = function(v) {
    return function(k) {
      return v(k);
    };
  };
  var sampleOn2 = function(dictMonadST) {
    var bind1 = bind(dictMonadST.Monad0().Bind1());
    var liftST2 = liftST(dictMonadST);
    return function(dictApplicative) {
      var traverse_2 = traverse_(dictApplicative)(foldableMaybe);
      var pure1 = pure(dictApplicative);
      var applySecond2 = applySecond(dictApplicative.Apply0());
      return function(v) {
        return function(v1) {
          return function(k) {
            return bind1(liftST2(newSTRef(Nothing.value)))(function(latest) {
              return bind1(v(function(a) {
                return liftST2($$void3(write(new Just(a))(latest)));
              }))(function(c1) {
                return bind1(v1(function(f) {
                  return bind1(liftST2(read(latest)))(traverse_2(function($215) {
                    return k(f($215));
                  }));
                }))(function(c2) {
                  return pure1(applySecond2(c1)(c2));
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
    var Monad0 = dictMonadST.Monad0();
    var Bind1 = Monad0.Bind1();
    var bind1 = bind(Bind1);
    var liftST2 = liftST(dictMonadST);
    var discard22 = discard2(Bind1);
    var Applicative0 = Monad0.Applicative0();
    var sequence_2 = sequence_(Applicative0)(foldableMaybe);
    var pure1 = pure(Applicative0);
    return function(v) {
      return function(k) {
        return bind1(liftST2(newSTRef(Nothing.value)))(function(cancelInner) {
          return bind1(v(function(inner) {
            return discard22(bind1(liftST2(read(cancelInner)))(sequence_2))(function() {
              return bind1(subscribe(inner)(k))(function(c) {
                return liftST2($$void3(write(new Just(c))(cancelInner)));
              });
            });
          }))(function(cancelOuter) {
            return pure1(discard22(bind1(liftST2(read(cancelInner)))(sequence_2))(function() {
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
          return v(function($216) {
            return k(f($216));
          });
        };
      };
    }
  };
  var fold3 = function(dictMonadST) {
    var bind1 = bind(dictMonadST.Monad0().Bind1());
    var liftST2 = liftST(dictMonadST);
    return function(f) {
      return function(v) {
        return function(b) {
          return function(k) {
            return bind1(liftST2(newSTRef(b)))(function(result) {
              return v(function(a) {
                return bind1(liftST2(modify2(f(a))(result)))(k);
              });
            });
          };
        };
      };
    };
  };
  var filter5 = function(dictApplicative) {
    var pure1 = pure(dictApplicative);
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
              return pure1(unit);
            }
            ;
            throw new Error("Failed pattern match at FRP.Event (line 129, column 13 - line 131, column 27): " + [v1.constructor.name]);
          });
        };
      };
    };
  };
  var filter$prime = function(dictApplicative) {
    var filter1 = filter5(dictApplicative);
    return function(f) {
      return filter1(function(a) {
        var v = f(a);
        if (v) {
          return new Just(a);
        }
        ;
        if (!v) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at FRP.Event (line 87, column 13 - line 89, column 25): " + [v.constructor.name]);
      });
    };
  };
  var create = function(dictMonadST) {
    var Monad0 = dictMonadST.Monad0();
    var bind1 = bind(Monad0.Bind1());
    var liftST2 = liftST(dictMonadST);
    var pure1 = pure(Monad0.Applicative0());
    return function(dictMonadST1) {
      var Monad01 = dictMonadST1.Monad0();
      var bind22 = bind(Monad01.Bind1());
      var liftST1 = liftST(dictMonadST1);
      var Applicative0 = Monad01.Applicative0();
      var pure22 = pure(Applicative0);
      var traverse_2 = traverse_(Applicative0)(foldableArray);
      return bind1(liftST2(newSTRef([])))(function(subscribers) {
        return pure1({
          event: function(k) {
            return bind22(liftST1(modify2(function(v) {
              return append1(v)([k]);
            })(subscribers)))(function() {
              return pure22(bind22(liftST1(modify2(deleteBy(unsafeRefEq)(k))(subscribers)))(function() {
                return pure22(unit);
              }));
            });
          },
          push: function(a) {
            return bind22(liftST1(read(subscribers)))(traverse_2(function(k) {
              return k(a);
            }));
          }
        });
      });
    };
  };
  var fix3 = function(dictMonadST) {
    var create1 = create(dictMonadST)(dictMonadST);
    return function(dictMonad) {
      var Bind1 = dictMonad.Bind1();
      var bind1 = bind(Bind1);
      var pure1 = pure(dictMonad.Applicative0());
      var applySecond2 = applySecond(Bind1.Apply0());
      return function(f) {
        return function(k) {
          return bind1(create1)(function(v) {
            var v1 = f(v.event);
            return bind1(subscribe(v1.input)(v.push))(function(c1) {
              return bind1(subscribe(v1.output)(k))(function(c2) {
                return pure1(applySecond2(c1)(c2));
              });
            });
          });
        };
      };
    };
  };
  var memoize = function(dictMonadST) {
    var Bind1 = dictMonadST.Monad0().Bind1();
    var bind1 = bind(Bind1);
    var create1 = create(dictMonadST)(dictMonadST);
    var discard22 = discard2(Bind1);
    return function(e) {
      return function(f) {
        return makeEvent(function(k) {
          return bind1(create1)(function(v) {
            return discard22(k(f(v.event)))(function() {
              return subscribe(e)(v.push);
            });
          });
        });
      };
    };
  };
  var compactableEvent = function(dictApplicative) {
    var filter1 = filter5(dictApplicative);
    return {
      compact: filter1(identity9),
      separate: function(xs) {
        return {
          left: filter1(function(v) {
            if (v instanceof Left) {
              return new Just(v.value0);
            }
            ;
            if (v instanceof Right) {
              return Nothing.value;
            }
            ;
            throw new Error("Failed pattern match at FRP.Event (line 70, column 13 - line 72, column 33): " + [v.constructor.name]);
          })(xs),
          right: filter1(function(v) {
            if (v instanceof Right) {
              return new Just(v.value0);
            }
            ;
            if (v instanceof Left) {
              return Nothing.value;
            }
            ;
            throw new Error("Failed pattern match at FRP.Event (line 77, column 13 - line 79, column 32): " + [v.constructor.name]);
          })(xs)
        };
      }
    };
  };
  var filterableEvent = function(dictApplicative) {
    var filter$prime1 = filter$prime(dictApplicative);
    var compactableEvent1 = compactableEvent(dictApplicative);
    return {
      filter: filter$prime1,
      filterMap: filter5(dictApplicative),
      partition: function(p) {
        return function(xs) {
          return {
            yes: filter$prime1(p)(xs),
            no: filter$prime1(function($217) {
              return !p($217);
            })(xs)
          };
        };
      },
      partitionMap: function(f) {
        return function(xs) {
          return {
            left: filterMap(filterableEvent(dictApplicative))(function() {
              var $218 = either(Just.create)($$const(Nothing.value));
              return function($219) {
                return $218(f($219));
              };
            }())(xs),
            right: filterMap(filterableEvent(dictApplicative))(function($220) {
              return hush(f($220));
            })(xs)
          };
        };
      },
      Compactable0: function() {
        return compactableEvent1;
      },
      Functor1: function() {
        return functorEvent;
      }
    };
  };
  var bus = function(dictMonadST) {
    var Monad0 = dictMonadST.Monad0();
    var Bind1 = Monad0.Bind1();
    var bind1 = bind(Bind1);
    var create1 = create(dictMonadST)(dictMonadST);
    var discard22 = discard2(Bind1);
    var pure1 = pure(Monad0.Applicative0());
    return function(f) {
      return makeEvent(function(k) {
        return bind1(create1)(function(v) {
          return discard22(k(f(v.push)(v.event)))(function() {
            return pure1(pure1(unit));
          });
        });
      });
    };
  };
  var bang2 = function(dictApplicative) {
    var map1 = map(dictApplicative.Apply0().Functor0());
    var pure1 = pure(dictApplicative);
    return function(a) {
      return function(k) {
        return map1(function(v) {
          return pure1(unit);
        })(k(a));
      };
    };
  };
  var altEvent = function(dictApplicative) {
    var Apply0 = dictApplicative.Apply0();
    var apply4 = apply(Apply0);
    var map1 = map(Apply0.Functor0());
    var applySecond2 = applySecond(Apply0);
    return {
      alt: function(v) {
        return function(v1) {
          return function(k) {
            return apply4(map1(function(v2) {
              return function(v3) {
                return applySecond2(v2)(v3);
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
    var pure1 = pure(dictApplicative);
    var altEvent1 = altEvent(dictApplicative);
    return {
      empty: function(v) {
        return pure1(pure1(unit));
      },
      Alt0: function() {
        return altEvent1;
      }
    };
  };
  var eventIsEvent = function(dictMonadST) {
    var Monad0 = dictMonadST.Monad0();
    var Applicative0 = Monad0.Applicative0();
    var plusEvent1 = plusEvent(Applicative0);
    var filterableEvent1 = filterableEvent(Applicative0);
    return {
      fold: fold3(dictMonadST),
      keepLatest: keepLatest2(dictMonadST),
      sampleOn: sampleOn2(dictMonadST)(Applicative0),
      fix: fix3(dictMonadST)(Monad0),
      bang: bang2(Applicative0),
      Plus0: function() {
        return plusEvent1;
      },
      Filterable1: function() {
        return filterableEvent1;
      }
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
  var fixed = function(a) {
    return new FixedChildren$prime(a);
  };
  var dyn = function(a) {
    return new DynamicChildren$prime(a);
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
  function _foldM(bind3) {
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
              acc = bind3(acc)(g(k));
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
  var foldr2 = /* @__PURE__ */ foldr(foldableArray);
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
  var foldMap3 = function(dictMonoid) {
    var append13 = append(dictMonoid.Semigroup0());
    var mempty3 = mempty(dictMonoid);
    return function(f) {
      return fold4(function(acc) {
        return function(k) {
          return function(v) {
            return append13(acc)(f(k)(v));
          };
        };
      })(mempty3);
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
          return foldr2(f)(z)(values(m));
        };
      };
    },
    foldMap: function(dictMonoid) {
      var foldMap13 = foldMap3(dictMonoid);
      return function(f) {
        return foldMap13($$const(f));
      };
    }
  };
  var $$delete3 = function(k) {
    return mutate(deleteImpl(k));
  };

  // output/Bolson.Control/index.js
  var map6 = /* @__PURE__ */ map(functorEvent);
  var eq2 = /* @__PURE__ */ eq(eqInt);
  var discard3 = /* @__PURE__ */ discard(discardUnit);
  var oneOfMap2 = /* @__PURE__ */ oneOfMap(foldableArray);
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var foldl4 = /* @__PURE__ */ foldl(foldableObject);
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
    var eventIsEvent2 = eventIsEvent(dictMonadST);
    var mapAccum2 = mapAccum(eventIsEvent2);
    var keepLatest3 = keepLatest(eventIsEvent2);
    var memoize2 = memoize(dictMonadST);
    var Applicative0 = dictMonadST.Monad0().Applicative0();
    var alt5 = alt(altEvent(Applicative0));
    var bang3 = bang(eventIsEvent2);
    var filter7 = filter4(filterableEvent(Applicative0));
    return function(f) {
      return function(event) {
        var counter = function(ev) {
          var fn = function(a) {
            return function(b) {
              return new Tuple(b + 1 | 0, new Tuple(a, b));
            };
          };
          return mapAccum2(fn)(ev)(0);
        };
        return new DynamicChildren$prime(keepLatest3(memoize2(counter(event))(function(cenv) {
          return map6(function(v) {
            return alt5(bang3(new Insert(f(v.value0))))(map6($$const(Remove.value))(filter7(function() {
              var $248 = eq2(v.value1 + 1 | 0);
              return function($249) {
                return $248(snd($249));
              };
            }())(cenv)));
          })(cenv);
        })));
      };
    };
  };
  var flatten = function(dictApplicative) {
    var oneOfMap1 = oneOfMap2(plusEvent(dictApplicative));
    var pure3 = pure(dictApplicative);
    var Apply0 = dictApplicative.Apply0();
    var Functor0 = Apply0.Functor0();
    var map32 = map(Functor0);
    var traverse_2 = traverse_(dictApplicative)(foldableArray);
    var $$void5 = $$void(Functor0);
    var applySecond2 = applySecond(Apply0);
    var for_2 = for_(dictApplicative)(foldableMaybe);
    return function(dictMonadST) {
      var keepLatest3 = keepLatest(eventIsEvent(dictMonadST));
      var Bind1 = dictMonadST.Monad0().Bind1();
      var bind3 = bind(Bind1);
      var liftST2 = liftST(dictMonadST);
      var discard1 = discard3(Bind1);
      var join2 = join(Bind1);
      return function(v) {
        return function(psr) {
          return function(interpreter) {
            var element = function(v1) {
              return v1(psr)(interpreter);
            };
            return function(v1) {
              if (v1 instanceof FixedChildren$prime) {
                return oneOfMap1(flatten(dictApplicative)(dictMonadST)(v)(psr)(interpreter))(v1.value0);
              }
              ;
              if (v1 instanceof EventfulElement$prime) {
                return keepLatest3(map6(flatten(dictApplicative)(dictMonadST)(v)(psr)(interpreter))(v1.value0));
              }
              ;
              if (v1 instanceof Element$prime) {
                return element(v.toElt(v1.value0));
              }
              ;
              if (v1 instanceof DynamicChildren$prime) {
                return makeEvent(function(v2) {
                  return bind3(liftST2(newSTRef(empty3)))(function(cancelInner) {
                    return bind3(subscribe(v1.value0)(function(inner) {
                      return bind3(v.ids(interpreter))(function(myUnsubId) {
                        return bind3(liftST2(newSTRef(pure3(unit))))(function(myUnsub) {
                          return bind3(v.ids(interpreter))(function(eltsUnsubId) {
                            return bind3(liftST2(newSTRef(pure3(unit))))(function(eltsUnsub) {
                              return bind3(liftST2(newSTRef([])))(function(myIds) {
                                return bind3(liftST2(newSTRef(pure3(unit))))(function(myImmediateCancellation) {
                                  return bind3(map32(Local.create)(v.ids(interpreter)))(function(myScope) {
                                    return bind3(liftST2(newSTRef(Begin.value)))(function(stageRef) {
                                      return bind3(subscribe(inner)(function(kid$prime) {
                                        return bind3(liftST2(read(stageRef)))(function(stage) {
                                          if (kid$prime instanceof Logic && stage instanceof Middle) {
                                            return bind3(liftST2(read(myIds)))(traverse_2(function() {
                                              var $250 = v.doLogic(kid$prime.value0)(interpreter);
                                              return function($251) {
                                                return v2($250($251));
                                              };
                                            }()));
                                          }
                                          ;
                                          if (kid$prime instanceof Remove && stage instanceof Middle) {
                                            return discard1($$void5(liftST2(write(End.value)(stageRef))))(function() {
                                              var mic = applySecond2(applySecond2(applySecond2(applySecond2(bind3(liftST2(read(myIds)))(traverse_2(function(old) {
                                                return for_2(psr.parent)(function(pnt) {
                                                  return v2(v.disconnectElement(interpreter)({
                                                    id: old,
                                                    parent: pnt,
                                                    scope: myScope
                                                  }));
                                                });
                                              })))(join2(liftST2(read(myUnsub)))))(join2(liftST2(read(eltsUnsub)))))($$void5(liftST2(modify2($$delete3(myUnsubId))(cancelInner)))))($$void5(liftST2(modify2($$delete3(eltsUnsubId))(cancelInner))));
                                              return applySecond2($$void5(liftST2(write(mic)(myImmediateCancellation))))(mic);
                                            });
                                          }
                                          ;
                                          if (kid$prime instanceof Insert && stage instanceof Begin) {
                                            return discard1($$void5(liftST2(write(Middle.value)(stageRef))))(function() {
                                              return bind3(subscribe(flatten(dictApplicative)(dictMonadST)(v)({
                                                parent: psr.parent,
                                                scope: myScope,
                                                raiseId: function(id) {
                                                  return $$void5(liftST2(modify2(append2([id]))(myIds)));
                                                }
                                              })(interpreter)(kid$prime.value0))(v2))(function(c1) {
                                                return discard1($$void5(liftST2(modify2(insert3(eltsUnsubId)(c1))(cancelInner))))(function() {
                                                  return $$void5(liftST2(write(c1)(eltsUnsub)));
                                                });
                                              });
                                            });
                                          }
                                          ;
                                          return pure3(unit);
                                        });
                                      }))(function(c0) {
                                        return discard1($$void5(liftST2(write(c0)(myUnsub))))(function() {
                                          return discard1($$void5(liftST2(modify2(insert3(myUnsubId)(c0))(cancelInner))))(function() {
                                            return join2(liftST2(read(myImmediateCancellation)));
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
                      return pure3(discard1(bind3(liftST2(read(cancelInner)))(foldl4(applySecond2)(pure3(unit))))(function() {
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

  // output/Data.Map/index.js
  var semigroupSemigroupMap = function(dictOrd) {
    var unionWith2 = unionWith(dictOrd);
    return function(dictSemigroup) {
      var append5 = append(dictSemigroup);
      return {
        append: function(v) {
          return function(v1) {
            return unionWith2(append5)(v)(v1);
          };
        }
      };
    };
  };
  var monoidSemigroupMap = function(dictOrd) {
    var semigroupSemigroupMap1 = semigroupSemigroupMap(dictOrd);
    return function(dictSemigroup) {
      var semigroupSemigroupMap2 = semigroupSemigroupMap1(dictSemigroup);
      return {
        mempty: empty2,
        Semigroup0: function() {
          return semigroupSemigroupMap2;
        }
      };
    };
  };
  var functorSemigroupMap = functorMap;
  var foldableSemigroupMap = foldableMap;

  // output/Data.Show.Generic/foreign.js
  var intercalate4 = function(separator) {
    return function(xs) {
      return xs.join(separator);
    };
  };

  // output/Data.Show.Generic/index.js
  var append3 = /* @__PURE__ */ append(semigroupArray);
  var genericShowArgsNoArguments = {
    genericShowArgs: function(v) {
      return [];
    }
  };
  var genericShowArgsArgument = function(dictShow) {
    var show3 = show(dictShow);
    return {
      genericShowArgs: function(v) {
        return [show3(v)];
      }
    };
  };
  var genericShowArgs = function(dict) {
    return dict.genericShowArgs;
  };
  var genericShowArgsProduct = function(dictGenericShowArgs) {
    var genericShowArgs1 = genericShowArgs(dictGenericShowArgs);
    return function(dictGenericShowArgs1) {
      var genericShowArgs2 = genericShowArgs(dictGenericShowArgs1);
      return {
        genericShowArgs: function(v) {
          return append3(genericShowArgs1(v.value0))(genericShowArgs2(v.value1));
        }
      };
    };
  };
  var genericShowConstructor = function(dictGenericShowArgs) {
    var genericShowArgs1 = genericShowArgs(dictGenericShowArgs);
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return {
        "genericShow'": function(v) {
          var ctor = reflectSymbol2($$Proxy.value);
          var v1 = genericShowArgs1(v);
          if (v1.length === 0) {
            return ctor;
          }
          ;
          return "(" + (intercalate4(" ")(append3([ctor])(v1)) + ")");
        }
      };
    };
  };
  var genericShow$prime = function(dict) {
    return dict["genericShow'"];
  };
  var genericShowSum = function(dictGenericShow) {
    var genericShow$prime1 = genericShow$prime(dictGenericShow);
    return function(dictGenericShow1) {
      var genericShow$prime2 = genericShow$prime(dictGenericShow1);
      return {
        "genericShow'": function(v) {
          if (v instanceof Inl) {
            return genericShow$prime1(v.value0);
          }
          ;
          if (v instanceof Inr) {
            return genericShow$prime2(v.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Show.Generic (line 26, column 1 - line 28, column 40): " + [v.constructor.name]);
        }
      };
    };
  };
  var genericShow = function(dictGeneric) {
    var from2 = from(dictGeneric);
    return function(dictGenericShow) {
      var genericShow$prime1 = genericShow$prime(dictGenericShow);
      return function(x) {
        return genericShow$prime1(from2(x));
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
    var $10 = map(functorFn)(map(functorEffect)($$const(true)));
    return function($11) {
      return Cb($10($11));
    };
  }();
  var attr = function(dict) {
    return dict.attr;
  };

  // output/Deku.Control/index.js
  var map7 = /* @__PURE__ */ map(functorEvent);
  var discard4 = /* @__PURE__ */ discard(discardUnit);
  var oneOf2 = /* @__PURE__ */ oneOf(foldableArray);
  var unwrap2 = /* @__PURE__ */ unwrap();
  var eq3 = /* @__PURE__ */ eq(eqScope);
  var append4 = /* @__PURE__ */ append(semigroupArray);
  var unsafeText = function(v) {
    return v.makeText;
  };
  var unsafeSetText = function(v) {
    return function(id) {
      return function(txt) {
        return map7(function($187) {
          return v.setText(function(v1) {
            return {
              id,
              text: v1
            };
          }($187));
        })(txt);
      };
    };
  };
  var unsafeSetAttribute = function(v) {
    return function(id) {
      return function(atts) {
        return map7(function($188) {
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
          }(unsafeUnAttribute($188));
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
    var Bind1 = dictMonad.Bind1();
    var bind3 = bind(Bind1);
    var discard1 = discard4(Bind1);
    var Apply0 = Bind1.Apply0();
    var map23 = map(Apply0.Functor0());
    var applySecond2 = applySecond(Apply0);
    var Applicative0 = dictMonad.Applicative0();
    var oneOf1 = oneOf2(plusEvent(Applicative0));
    var bang3 = bang2(Applicative0);
    return function(txt) {
      var go2 = function(v) {
        return function(v1) {
          return makeEvent(function(k) {
            return bind3(v1.ids)(function(me) {
              return discard1(v.raiseId(me))(function() {
                return map23(applySecond2(k(v1.deleteFromCache({
                  id: me
                }))))(subscribe(oneOf1([bang3(unsafeText(v1)({
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
    var text1 = text(dictMonad);
    var bang3 = bang2(dictMonad.Applicative0());
    return function(txt) {
      return text1(bang3(txt));
    };
  };
  var __internalDekuFlatten = function(dictKorok) {
    var MonadST5 = dictKorok.MonadST5();
    return flatten(MonadST5.Monad0().Applicative0())(MonadST5)({
      doLogic: function(v) {
        return function(v1) {
          return function(id) {
            return v1.sendToTop({
              id
            });
          };
        };
      },
      ids: function($189) {
        return function(v) {
          return v.ids;
        }(unwrap2($189));
      },
      disconnectElement: function(v) {
        return function(v1) {
          return v.disconnectElement({
            id: v1.id,
            scope: v1.scope,
            parent: v1.parent,
            scopeEq: eq3
          });
        };
      },
      toElt: function(v) {
        return v;
      }
    });
  };
  var deku = function(dictKorok) {
    var Monad0 = dictKorok.MonadST5().Monad0();
    var bind3 = bind(Monad0.Bind1());
    var Applicative0 = Monad0.Applicative0();
    var alt5 = alt(altEvent(Applicative0));
    var bang3 = bang2(Applicative0);
    var __internalDekuFlatten1 = __internalDekuFlatten(dictKorok);
    var pure3 = pure(Applicative0);
    return function(root) {
      return function(children) {
        return function(v) {
          return makeEvent(function(k) {
            return bind3(v.ids)(function(me) {
              return subscribe(alt5(bang3(v.makeRoot({
                id: me,
                root
              })))(__internalDekuFlatten1({
                parent: new Just(me),
                scope: new Local("rootScope"),
                raiseId: function(v1) {
                  return pure3(unit);
                }
              })(v)(children)))(k);
            });
          });
        };
      };
    };
  };
  var deku1 = function(dictKorok) {
    var deku2 = deku(dictKorok);
    return function(root) {
      return function(children) {
        return deku2(root)(new EventfulElement$prime(children));
      };
    };
  };
  var elementify = function(dictKorok) {
    var Monad0 = dictKorok.MonadST5().Monad0();
    var Bind1 = Monad0.Bind1();
    var bind3 = bind(Bind1);
    var discard1 = discard4(Bind1);
    var Apply0 = Bind1.Apply0();
    var map23 = map(Apply0.Functor0());
    var applySecond2 = applySecond(Apply0);
    var Applicative0 = Monad0.Applicative0();
    var alt5 = alt(altEvent(Applicative0));
    var oneOf1 = oneOf2(plusEvent(Applicative0));
    var bang3 = bang2(Applicative0);
    var __internalDekuFlatten1 = __internalDekuFlatten(dictKorok);
    var pure3 = pure(Applicative0);
    return function(tag) {
      return function(atts) {
        return function(children) {
          var go2 = function(v) {
            return function(v1) {
              return makeEvent(function(k) {
                return bind3(v1.ids)(function(me) {
                  return discard1(v.raiseId(me))(function() {
                    return map23(applySecond2(k(v1.deleteFromCache({
                      id: me
                    }))))(subscribe(alt5(oneOf1(append4([bang3(unsafeElement(v1)({
                      id: me,
                      parent: v.parent,
                      scope: v.scope,
                      tag
                    })), unsafeSetAttribute(v1)(me)(atts)])(maybe([])(function(p) {
                      return [bang3(unsafeConnect(v1)({
                        id: me,
                        parent: p
                      }))];
                    })(v.parent))))(__internalDekuFlatten1({
                      parent: new Just(me),
                      scope: v.scope,
                      raiseId: function(v2) {
                        return pure3(unit);
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
  var top2 = /* @__PURE__ */ top(boundedInt);
  var bottom2 = /* @__PURE__ */ bottom(boundedInt);
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
    if (v >= bottom2 && v <= top2) {
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
  var monoidEffect2 = /* @__PURE__ */ monoidEffect(monoidUnit);
  var monoidEffect1 = /* @__PURE__ */ monoidEffect(monoidEffect2);
  var monoidEndo2 = /* @__PURE__ */ monoidEndo(categoryFn);
  var always22 = /* @__PURE__ */ always2(monoidEffect2);
  var always21 = /* @__PURE__ */ always2(monoidEffect1);
  var always222 = /* @__PURE__ */ always2(monoidEndo2);
  var korokGlobalEffect = {
    Always0: function() {
      return always22;
    },
    Always1: function() {
      return always21;
    },
    Always2: function() {
      return always22;
    },
    Always3: function() {
      return always222;
    },
    Always4: function() {
      return always222;
    },
    MonadST5: function() {
      return monadSTEffect;
    }
  };
  var sendToTop = /* @__PURE__ */ function() {
    return new Logic(unit);
  }();

  // output/Deku.DOM.Attr.Class/index.js
  var Class = /* @__PURE__ */ function() {
    function Class2() {
    }
    ;
    Class2.value = new Class2();
    return Class2;
  }();
  var attrSpan_ClassString = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "class",
          value: prop$prime(value12)
        });
      };
    }
  };

  // output/Deku.DOM.Attr.OnClick/index.js
  var voidLeft2 = /* @__PURE__ */ voidLeft(functorEffect);
  var OnClick = /* @__PURE__ */ function() {
    function OnClick2() {
    }
    ;
    OnClick2.value = new OnClick2();
    return OnClick2;
  }();
  var attrOnClickEffectUnit = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "click",
          value: cb$prime($$const(voidLeft2(value12)(true)))
        });
      };
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

  // output/Deku.DOM.Attr.OnKeyup/index.js
  var OnKeyup = /* @__PURE__ */ function() {
    function OnKeyup2() {
    }
    ;
    OnKeyup2.value = new OnKeyup2();
    return OnKeyup2;
  }();
  var attrOnKeyupCb = {
    attr: function(v) {
      return function(value12) {
        return unsafeAttribute({
          key: "keyup",
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

  // output/Deku.DOM.Elt.Button/index.js
  var button = function(dictKorok) {
    var elementify2 = elementify(dictKorok);
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify2("button")(attributes)(fixed(kids)));
      };
    };
  };

  // output/Deku.DOM.Elt.Div/index.js
  var div2 = function(dictKorok) {
    var elementify2 = elementify(dictKorok);
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify2("div")(attributes)(fixed(kids)));
      };
    };
  };
  var div_ = function(dictKorok) {
    return div2(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.DOM.Elt.Input/index.js
  var input = function(dictKorok) {
    var elementify2 = elementify(dictKorok);
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify2("input")(attributes)(fixed(kids)));
      };
    };
  };

  // output/Deku.DOM.Elt.Li/index.js
  var li = function(dictKorok) {
    var elementify2 = elementify(dictKorok);
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify2("li")(attributes)(fixed(kids)));
      };
    };
  };
  var li_ = function(dictKorok) {
    return li(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.DOM.Elt.Ol/index.js
  var ol = function(dictKorok) {
    var elementify2 = elementify(dictKorok);
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify2("ol")(attributes)(fixed(kids)));
      };
    };
  };
  var ol_ = function(dictKorok) {
    return ol(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.DOM.Elt.Span/index.js
  var span2 = function(dictKorok) {
    var elementify2 = elementify(dictKorok);
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify2("span")(attributes)(fixed(kids)));
      };
    };
  };

  // output/Deku.DOM.Elt.Style/index.js
  var style = function(dictKorok) {
    var elementify2 = elementify(dictKorok);
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify2("style")(attributes)(fixed(kids)));
      };
    };
  };
  var style_ = function(dictKorok) {
    return style(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.DOM.Elt.Sub/index.js
  var sub2 = function(dictKorok) {
    var elementify2 = elementify(dictKorok);
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify2("sub")(attributes)(fixed(kids)));
      };
    };
  };
  var sub_ = function(dictKorok) {
    return sub2(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.DOM.Elt.Table/index.js
  var table = function(dictKorok) {
    var elementify2 = elementify(dictKorok);
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify2("table")(attributes)(fixed(kids)));
      };
    };
  };
  var table_ = function(dictKorok) {
    return table(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.DOM.Elt.Tbody/index.js
  var tbody = function(dictKorok) {
    var elementify2 = elementify(dictKorok);
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify2("tbody")(attributes)(fixed(kids)));
      };
    };
  };
  var tbody_ = function(dictKorok) {
    return tbody(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.DOM.Elt.Td/index.js
  var td = function(dictKorok) {
    var elementify2 = elementify(dictKorok);
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify2("td")(attributes)(fixed(kids)));
      };
    };
  };
  var td_ = function(dictKorok) {
    return td(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.DOM.Elt.Tr/index.js
  var tr = function(dictKorok) {
    var elementify2 = elementify(dictKorok);
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify2("tr")(attributes)(fixed(kids)));
      };
    };
  };
  var tr_ = function(dictKorok) {
    return tr(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.DOM.Elt.Ul/index.js
  var ul = function(dictKorok) {
    var elementify2 = elementify(dictKorok);
    return function(attributes) {
      return function(kids) {
        return new Element$prime(elementify2("ul")(attributes)(fixed(kids)));
      };
    };
  };
  var ul_ = function(dictKorok) {
    return ul(dictKorok)(empty(plusEvent(dictKorok.MonadST5().Monad0().Applicative0())));
  };

  // output/Deku.Interpret/foreign.js
  var connectXToY_ = (maybe2, x, y$, state4) => {
    maybe2((y) => state4.units[y].main.appendChild(state4.units[x].main))(y$);
  };
  var attributeParent_ = (a) => (state4) => () => {
    if (!state4.units[a.id].main.parentNode) {
      state4.units[a.parent].main.appendChild(state4.units[a.id].main);
    }
  };
  var makeElement_ = (tryHydration) => (a) => (state4) => () => {
    var dom2;
    var ptr = a.id;
    if (!state4.scopes[a.scope]) {
      state4.scopes[a.scope] = [];
    }
    state4.scopes[a.scope].push(ptr);
    if (tryHydration && a.parent.value0 && (dom2 = document.body.querySelectorAll("[data-deku-ssr-" + ptr + "]").item(0))) {
      state4.units[ptr] = {
        listeners: {},
        parent: a.parent,
        scope: a.scope,
        main: dom2
      };
    } else {
      state4.units[ptr] = {
        listeners: {},
        parent: a.parent,
        scope: a.scope,
        main: document.createElement(a.tag)
      };
    }
  };
  var makeText_ = (tryHydration) => (maybe2) => (a) => (state4) => () => {
    var ptr = a.id;
    var dom2;
    if (!state4.scopes[a.scope]) {
      state4.scopes[a.scope] = [];
    }
    state4.scopes[a.scope].push(ptr);
    if (tryHydration && a.parent.value0 && (dom2 = document.body.querySelectorAll("[data-deku-ssr-" + a.parent.value0 + "]").item(0))) {
      state4.units[ptr] = {
        main: dom2.childNodes[0],
        parent: a.parent,
        scope: a.scope
      };
    } else {
      state4.units[ptr] = {
        main: document.createTextNode(""),
        parent: a.parent,
        scope: a.scope
      };
      connectXToY_(maybe2, ptr, a.parent, state4);
    }
  };
  function makeFFIDOMSnapshot() {
    return {
      units: {},
      scopes: {}
    };
  }
  var setProp_ = (tryHydration) => (a) => (state4) => () => {
    var ptr = a.id;
    var avv = a.value;
    if (tryHydration && !state4.units[ptr] && (dom = document.body.querySelectorAll("[data-deku-ssr-" + ptr + "]").item(0))) {
      state4.units[ptr] = {
        listeners: {},
        parent: a.parent,
        scope: a.scope,
        main: dom
      };
      if (!state4.scopes[a.scope]) {
        state4.scopes[a.scope] = [];
      }
      state4.scopes[a.scope].push(ptr);
    }
    if (state4.units[ptr].main.tagName === "INPUT" && a.key === "value") {
      state4.units[ptr].main.value = avv;
    } else if (state4.units[ptr].main.tagName === "INPUT" && a.key === "checked") {
      state4.units[ptr].main.checked = avv === "true";
    } else {
      state4.units[ptr].main.setAttribute(a.key, avv);
    }
  };
  var setCb_ = (tryHydration) => (a) => (state4) => () => {
    var ptr = a.id;
    var avv = a.value;
    if (tryHydration && !state4.units[ptr] && (dom = document.body.querySelectorAll("[data-deku-ssr-" + ptr + "]").item(0))) {
      state4.units[ptr] = {
        listeners: {},
        parent: a.parent,
        scope: a.scope,
        main: dom
      };
      if (!state4.scopes[a.scope]) {
        state4.scopes[a.scope] = [];
      }
      state4.scopes[a.scope].push(ptr);
    }
    if (a.key === "@self@") {
      avv(state4.units[ptr].main)();
    } else {
      if (state4.units[ptr].listeners[a.key]) {
        state4.units[ptr].main.removeEventListener(a.key, state4.units[ptr].listeners[a.key]);
      }
      var el = (e) => avv(e)();
      state4.units[ptr].main.addEventListener(a.key, el);
      state4.units[ptr].listeners[a.key] = el;
    }
  };
  var setText_ = (a) => (state4) => () => {
    var ptr = a.id;
    state4.units[ptr].main.nodeValue = a.text;
  };
  var makePursx_ = (tryHydration) => (maybe2) => (a) => (state4) => () => {
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
      state4.units[ptr] = {
        listeners: {},
        scope: scope2,
        parent: parent2,
        main: dom2
      };
    } else {
      const entries = Object.entries(cache);
      for (var i = 0; i < entries.length; i++) {
        const key2 = entries[i][0];
        if (entries[i][1] === true) {
          html = html.replace(verb + key2 + verb, 'data-deku-attr-internal="' + key2 + '"');
        } else {
          html = html.replace(verb + key2 + verb, '<span style="display:contents;" data-deku-elt-internal="' + key2 + '"></span>');
        }
      }
      tmp = document.createElement("div");
      tmp.innerHTML = html.trim();
      state4.units[ptr] = {
        listeners: {},
        scope: scope2,
        parent: parent2,
        main: tmp.firstChild
      };
    }
    if (!state4.scopes[scope2]) {
      state4.scopes[scope2] = [];
    }
    state4.scopes[scope2].push(ptr);
    if (!tmp) {
      tmp = dom2;
    }
    tmp.querySelectorAll("[data-deku-attr-internal]").forEach(function(e) {
      var key2 = e.getAttribute("data-deku-attr-internal");
      const namespacedKey = key2 + pxScope;
      state4.units[namespacedKey] = {
        listeners: {},
        main: e,
        scope: scope2
      };
      state4.scopes[scope2].push(namespacedKey);
    });
    tmp.querySelectorAll("[data-deku-elt-internal]").forEach(function(e) {
      var key2 = e.getAttribute("data-deku-elt-internal");
      const namespacedKey = key2 + pxScope;
      state4.units[key2 + pxScope] = {
        listeners: {},
        main: e,
        scope: scope2
      };
      state4.scopes[scope2].push(namespacedKey);
    });
    if (!dom2) {
      connectXToY_(maybe2, ptr, parent2, state4);
    }
  };
  var makeRoot_ = (a) => (state4) => () => {
    var ptr = a.id;
    state4.units[ptr] = {
      main: a.root
    };
  };
  var giveNewParent_ = (a) => (state4) => () => {
    var ptr = a.id;
    var parent2 = a.parent;
    state4.units[ptr].containingScope = a.scope;
    state4.units[parent2].main.prepend(state4.units[ptr].main);
  };
  var disconnectElement_ = (a) => (state4) => () => {
    var ptr = a.id;
    if (state4.units[ptr].noop) {
      return;
    }
    if (state4.units[ptr].containingScope && !a.scopeEq(state4.units[ptr].containingScope)(a.scope)) {
      return;
    }
    state4.units[ptr].main.remove();
  };
  var deleteFromCache_ = (a) => (state4) => () => {
    delete state4.units[a.id];
  };
  var sendToTop_ = (a) => (state4) => () => {
    var ptr = a.id;
    state4.units[ptr].main.parentNode.prepend(state4.units[ptr].main);
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
  var top3 = /* @__PURE__ */ top(boundedInt);
  var bottom3 = /* @__PURE__ */ bottom(boundedInt);
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x) {
    if (!isFiniteImpl(x)) {
      return 0;
    }
    ;
    if (x >= toNumber(top3)) {
      return top3;
    }
    ;
    if (x <= toNumber(bottom3)) {
      return bottom3;
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
  };
  var floor2 = function($39) {
    return unsafeClamp(floor($39));
  };

  // output/Random.LCG/index.js
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
  var fromJust5 = /* @__PURE__ */ fromJust();
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
          var n$prime = mod2(n)(rangeSize);
          var $25 = n$prime < min5;
          if ($25) {
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
      return fromJust5(fromNumber(remainder(toNumber(lcgA) * toNumber(v) + toNumber(d))(toNumber(lcgM))));
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
    return function(apply4) {
      return function(map11) {
        return function(f) {
          var buildFrom = function(x, ys) {
            return apply4(map11(consList)(f(x)))(ys);
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
            var acc = map11(finalCell)(f(array[array.length - 1]));
            var result = go2(acc, array.length - 1, array);
            while (result instanceof Cont) {
              result = result.fn();
            }
            return map11(listToArray)(result);
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
    var map11 = map(dictFunctor);
    return {
      map: function(f) {
        return function(v) {
          return function(s) {
            return map11(function(v1) {
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
    var bind3 = bind(dictMonad.Bind1());
    return {
      bind: function(v) {
        return function(f) {
          return function(s) {
            return bind3(v(s))(function(v1) {
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
    var functorStateT1 = functorStateT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadStateT(dictMonad)),
      Functor0: function() {
        return functorStateT1;
      }
    };
  };
  var applicativeStateT = function(dictMonad) {
    var pure3 = pure(dictMonad.Applicative0());
    return {
      pure: function(a) {
        return function(s) {
          return pure3(new Tuple(a, s));
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var monadStateStateT = function(dictMonad) {
    var pure3 = pure(dictMonad.Applicative0());
    var monadStateT1 = monadStateT(dictMonad);
    return {
      state: function(f) {
        return function($200) {
          return pure3(f($200));
        };
      },
      Monad0: function() {
        return monadStateT1;
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
  var monadStateStateT2 = /* @__PURE__ */ monadStateStateT(monadIdentity);
  var state2 = /* @__PURE__ */ state(monadStateStateT2);
  var functorStateT2 = /* @__PURE__ */ functorStateT(functorIdentity);
  var mul2 = /* @__PURE__ */ mul(semiringNumber);
  var add2 = /* @__PURE__ */ add(semiringNumber);
  var unGen = function(v) {
    return v;
  };
  var lcgStep = /* @__PURE__ */ function() {
    var f = function(s) {
      return new Tuple(unSeed(s.newSeed), function() {
        var $93 = {};
        for (var $94 in s) {
          if ({}.hasOwnProperty.call(s, $94)) {
            $93[$94] = s[$94];
          }
          ;
        }
        ;
        $93.newSeed = lcgNext(s.newSeed);
        return $93;
      }());
    };
    return state2(f);
  }();
  var functorGen = functorStateT2;
  var map22 = /* @__PURE__ */ map(functorGen);
  var evalGen = function($103) {
    return evalState(unGen($103));
  };
  var applyGen = /* @__PURE__ */ applyStateT(monadIdentity);
  var apply3 = /* @__PURE__ */ apply(applyGen);
  var chooseInt$prime = function(a) {
    return function(b) {
      var numB = toNumber(b);
      var numA = toNumber(a);
      var clamp = function(x) {
        return numA + remainder(x)(numB - numA + 1);
      };
      var choose31BitPosNumber = map22(toNumber)(lcgStep);
      var choose32BitPosNumber = apply3(map22(add2)(choose31BitPosNumber))(map22(mul2(2))(choose31BitPosNumber));
      return map22(function($108) {
        return floor2(clamp($108));
      })(choose32BitPosNumber);
    };
  };
  var chooseInt2 = function(a) {
    return function(b) {
      var $100 = a <= b;
      if ($100) {
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
  var show2 = /* @__PURE__ */ show(showInt);
  var arbitrary2 = /* @__PURE__ */ arbitrary(arbInt);
  var add3 = /* @__PURE__ */ add(semiringInt);
  var void1 = /* @__PURE__ */ $$void(functorEffect);
  var fullDOMInterpret = function(seed2) {
    return {
      ids: function __do() {
        var s = read2(seed2)();
        var o = show2(evalGen(arbitrary2)({
          newSeed: mkSeed(s),
          size: 5
        }));
        void1(modify(add3(1))(seed2))();
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
  var map8 = /* @__PURE__ */ map(functorEffect);
  var body = /* @__PURE__ */ function() {
    var $12 = map8(toMaybe);
    return function($13) {
      return $12(_body($13));
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
  var bind2 = /* @__PURE__ */ bind(bindEffect);
  var mapFlipped2 = /* @__PURE__ */ mapFlipped(functorEffect);
  var deku12 = /* @__PURE__ */ deku1(korokGlobalEffect);
  var mempty2 = /* @__PURE__ */ mempty(/* @__PURE__ */ monoidEffect(/* @__PURE__ */ monoidEffect(monoidUnit)));
  var map9 = /* @__PURE__ */ map(functorMaybe);
  var $$void4 = /* @__PURE__ */ $$void(functorEffect);
  var runInElement1$prime = function(elt) {
    return function(eee) {
      return function __do() {
        var ffi = makeFFIDOMSnapshot();
        var evt = mapFlipped2($$new(0))(function() {
          var $67 = deku12(elt)(eee);
          return function($68) {
            return $67(fullDOMInterpret($68));
          };
        }())();
        return subscribe(evt)(function(i) {
          return i(ffi);
        })();
      };
    };
  };
  var runInBody1$prime = function(eee) {
    return function __do() {
      var b$prime = bind2(bind2(windowImpl)(document2))(body)();
      return maybe(mempty2)(function(elt) {
        return runInElement1$prime(elt)(eee);
      })(map9(toElement)(b$prime))();
    };
  };
  var runInBody1 = function(a) {
    return $$void4(runInBody1$prime(a));
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
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map10 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var compare2 = /* @__PURE__ */ compare(ordInt);
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
        head: fromEnum2(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum2(charAt(1)(s));
    var cu0 = fromEnum2(charAt(0)(s));
    var $42 = isLead(cu0) && isTrail(cu1);
    if ($42) {
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
    return map10(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons2(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr2(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum2(charAt(0)(s));
    var $46 = isLead(cu0) && length4(s) > 1;
    if ($46) {
      var cu1 = fromEnum2(charAt(1)(s));
      var $47 = isTrail(cu1);
      if ($47) {
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
        return compare2(x)(y);
      };
    },
    Eq0: function() {
      return eqCodePoint;
    }
  };
  var codePointFromChar = function($76) {
    return CodePoint(fromEnum2($76));
  };

  // output/Parser.ProtoG8/index.js
  var $runtime_lazy3 = function(name15, moduleName, init2) {
    var state4 = 0;
    var val;
    return function(lineNumber) {
      if (state4 === 2)
        return val;
      if (state4 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state4 = 1;
      val = init2();
      state4 = 2;
      return val;
    };
  };
  var genericShowConstructor2 = /* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments);
  var L1IsSymbol = {
    reflectSymbol: function() {
      return "L1";
    }
  };
  var L2IsSymbol = {
    reflectSymbol: function() {
      return "L2";
    }
  };
  var append12 = /* @__PURE__ */ append(semigroupArray);
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
    show: /* @__PURE__ */ genericShow(genericTok)(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "LParen";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "RParen";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "Comma";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "X";
      }
    }))(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "EOF";
      }
    }))))))
  };
  var genericShowConstructor1 = /* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(showTok));
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
    show: /* @__PURE__ */ genericShow(genericState)(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "S0";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "SA";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "S1";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "S2";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "S3";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "S4";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "S5";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "S6";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "S7";
      }
    }))(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "S8";
      }
    })))))))))))
  };
  var genericSorts = {
    to: function(x) {
      if (x instanceof Inl) {
        return RE.value;
      }
      ;
      if (x instanceof Inr) {
        return RL.value;
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 67, column 1 - line 67, column 48): " + [x.constructor.name]);
    },
    from: function(x) {
      if (x instanceof RE) {
        return new Inl(NoArguments.value);
      }
      ;
      if (x instanceof RL) {
        return new Inr(NoArguments.value);
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 67, column 1 - line 67, column 48): " + [x.constructor.name]);
    }
  };
  var showSorts = {
    show: /* @__PURE__ */ genericShow(genericSorts)(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "RE";
      }
    }))(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "RL";
      }
    })))
  };
  var genericRule = {
    to: function(x) {
      if (x instanceof Inl) {
        return RE1.value;
      }
      ;
      if (x instanceof Inr && x.value0 instanceof Inl) {
        return RE2.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && x.value0.value0 instanceof Inl)) {
        return RL1.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && x.value0.value0 instanceof Inr)) {
        return RL2.value;
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 48, column 1 - line 48, column 46): " + [x.constructor.name]);
    },
    from: function(x) {
      if (x instanceof RE1) {
        return new Inl(NoArguments.value);
      }
      ;
      if (x instanceof RE2) {
        return new Inr(new Inl(NoArguments.value));
      }
      ;
      if (x instanceof RL1) {
        return new Inr(new Inr(new Inl(NoArguments.value)));
      }
      ;
      if (x instanceof RL2) {
        return new Inr(new Inr(new Inr(NoArguments.value)));
      }
      ;
      throw new Error("Failed pattern match at Parser.ProtoG8 (line 48, column 1 - line 48, column 46): " + [x.constructor.name]);
    }
  };
  var showRule = {
    show: /* @__PURE__ */ genericShow(genericRule)(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "RE1";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "RE2";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "RL1";
      }
    }))(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "RL2";
      }
    })))))
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
  var genericShow2 = /* @__PURE__ */ genericShow(genericL);
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
      return genericShow2(genericShowSum(genericShowConstructor(genericShowArgsArgument($lazy_showE(0)))(L1IsSymbol))(genericShowConstructor(genericShowArgsProduct(genericShowArgsArgument(showL))(genericShowArgsArgument($lazy_showE(0))))(L2IsSymbol)))(x);
    }
  };
  var $lazy_showE = /* @__PURE__ */ $runtime_lazy3("showE", "Parser.ProtoG8", function() {
    return {
      show: genericShow(genericE)(genericShowSum(genericShowConstructor(genericShowArgsArgument(showL))({
        reflectSymbol: function() {
          return "E1";
        }
      }))(genericShowConstructor2({
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
    }))(/* @__PURE__ */ genericShowConstructor1({
      reflectSymbol: function() {
        return "Tok";
      }
    }))))
  };
  var g8Toks = /* @__PURE__ */ function() {
    return fromFoldable2(ordCodePoint)(foldableArray)([new Tuple(codePointFromChar("("), new Just(LParen.value)), new Tuple(codePointFromChar(")"), new Just(RParen.value)), new Tuple(codePointFromChar(","), new Just(Comma.value)), new Tuple(codePointFromChar("x"), new Just(X.value)), new Tuple(codePointFromChar("$"), new Just(EOF.value)), new Tuple(codePointFromChar(" "), Nothing.value), new Tuple(codePointFromChar("	"), Nothing.value), new Tuple(codePointFromChar("\n"), Nothing.value)]);
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
                var $509 = g8Goto(RL.value)(topOf(v1.value0.value0.value0));
                if ($509 instanceof Just) {
                  return new Just(new Snoc(v1.value0.value0.value0, new L(new L2(v1.value0.value0.value1.value0, v1.value1.value0)), $509.value0));
                }
                ;
                throw new Error("Failed pattern match at Parser.ProtoG8 (line 142, column 1 - line 142, column 50): " + [$509.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Parser.ProtoG8 (line 142, column 1 - line 142, column 50): " + [v.constructor.name, v1.constructor.name]);
            };
            if (v instanceof RL1 && (v1 instanceof Snoc && v1.value1 instanceof E)) {
              var $525 = g8Goto(RL.value)(topOf(v1.value0));
              if ($525 instanceof Just) {
                return new Just(new Snoc(v1.value0, new L(new L1(v1.value1.value0)), $525.value0));
              }
              ;
              return v6(true);
            }
            ;
            return v6(true);
          };
          if (v instanceof RE2 && (v1 instanceof Snoc && (v1.value1 instanceof Tok && v1.value1.value0 instanceof X))) {
            var $533 = g8Goto(RE.value)(topOf(v1.value0));
            if ($533 instanceof Just) {
              return new Just(new Snoc(v1.value0, new E(E2.value), $533.value0));
            }
            ;
            return v4(true);
          }
          ;
          return v4(true);
        };
        if (v instanceof RE1 && (v1 instanceof Snoc && (v1.value0 instanceof Snoc && (v1.value0.value0 instanceof Snoc && (v1.value0.value0.value1 instanceof Tok && (v1.value0.value0.value1.value0 instanceof LParen && (v1.value0.value1 instanceof L && (v1.value1 instanceof Tok && v1.value1.value0 instanceof RParen)))))))) {
          var $541 = g8Goto(RE.value)(topOf(v1.value0.value0.value0));
          if ($541 instanceof Just) {
            return new Just(new Snoc(v1.value0.value0.value0, new E(new E1(v1.value0.value1.value0)), $541.value0));
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
  var eqTok = {
    eq: function(x) {
      return function(y) {
        if (x instanceof LParen && y instanceof LParen) {
          return true;
        }
        ;
        if (x instanceof RParen && y instanceof RParen) {
          return true;
        }
        ;
        if (x instanceof Comma && y instanceof Comma) {
          return true;
        }
        ;
        if (x instanceof X && y instanceof X) {
          return true;
        }
        ;
        if (x instanceof EOF && y instanceof EOF) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var ordTok = {
    compare: function(x) {
      return function(y) {
        if (x instanceof LParen && y instanceof LParen) {
          return EQ.value;
        }
        ;
        if (x instanceof LParen) {
          return LT.value;
        }
        ;
        if (y instanceof LParen) {
          return GT.value;
        }
        ;
        if (x instanceof RParen && y instanceof RParen) {
          return EQ.value;
        }
        ;
        if (x instanceof RParen) {
          return LT.value;
        }
        ;
        if (y instanceof RParen) {
          return GT.value;
        }
        ;
        if (x instanceof Comma && y instanceof Comma) {
          return EQ.value;
        }
        ;
        if (x instanceof Comma) {
          return LT.value;
        }
        ;
        if (y instanceof Comma) {
          return GT.value;
        }
        ;
        if (x instanceof X && y instanceof X) {
          return EQ.value;
        }
        ;
        if (x instanceof X) {
          return LT.value;
        }
        ;
        if (y instanceof X) {
          return GT.value;
        }
        ;
        if (x instanceof EOF && y instanceof EOF) {
          return EQ.value;
        }
        ;
        throw new Error("Failed pattern match at Parser.ProtoG8 (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqTok;
    }
  };
  var eqSorts = {
    eq: function(x) {
      return function(y) {
        if (x instanceof RE && y instanceof RE) {
          return true;
        }
        ;
        if (x instanceof RL && y instanceof RL) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var ordSorts = {
    compare: function(x) {
      return function(y) {
        if (x instanceof RE && y instanceof RE) {
          return EQ.value;
        }
        ;
        if (x instanceof RE) {
          return LT.value;
        }
        ;
        if (y instanceof RE) {
          return GT.value;
        }
        ;
        if (x instanceof RL && y instanceof RL) {
          return EQ.value;
        }
        ;
        throw new Error("Failed pattern match at Parser.ProtoG8 (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqSorts;
    }
  };
  var eqRule = {
    eq: function(x) {
      return function(y) {
        if (x instanceof RE1 && y instanceof RE1) {
          return true;
        }
        ;
        if (x instanceof RE2 && y instanceof RE2) {
          return true;
        }
        ;
        if (x instanceof RL1 && y instanceof RL1) {
          return true;
        }
        ;
        if (x instanceof RL2 && y instanceof RL2) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var ensureEOF = function(toks) {
    var v = last(toks);
    if (v instanceof Just && v.value0 instanceof EOF) {
      return toks;
    }
    ;
    return append12(toks)([EOF.value]);
  };
  var g8FromString = /* @__PURE__ */ function() {
    var $646 = map(functorMaybe)(function() {
      var $649 = toUnfoldable(unfoldableList);
      return function($650) {
        return $649(ensureEOF(catMaybes($650)));
      };
    }());
    var $647 = traverse(traversableArray)(applicativeMaybe)(flip(lookup(ordCodePoint))(g8Toks));
    return function($648) {
      return $646($647(toCodePointArray($648)));
    };
  }();

  // output/Web.Event.Event/foreign.js
  function _target(e) {
    return e.target;
  }

  // output/Web.Event.Event/index.js
  var target5 = function($3) {
    return toMaybe(_target($3));
  };

  // output/Web.UIEvent.KeyboardEvent/foreign.js
  function code(e) {
    return e.code;
  }

  // output/Web.UIEvent.KeyboardEvent/index.js
  var fromEvent = /* @__PURE__ */ unsafeReadProtoTagged("KeyboardEvent");

  // output/Parser.Main/index.js
  var NonTerminal = /* @__PURE__ */ function() {
    function NonTerminal2(value0) {
      this.value0 = value0;
    }
    ;
    NonTerminal2.create = function(value0) {
      return new NonTerminal2(value0);
    };
    return NonTerminal2;
  }();
  var Terminal = /* @__PURE__ */ function() {
    function Terminal2(value0) {
      this.value0 = value0;
    }
    ;
    Terminal2.create = function(value0) {
      return new Terminal2(value0);
    };
    return Terminal2;
  }();
  var UIShown = /* @__PURE__ */ function() {
    function UIShown2() {
    }
    ;
    UIShown2.value = new UIShown2();
    return UIShown2;
  }();
  var AddTodo = /* @__PURE__ */ function() {
    function AddTodo2() {
    }
    ;
    AddTodo2.value = new AddTodo2();
    return AddTodo2;
  }();
  var ChangeText = /* @__PURE__ */ function() {
    function ChangeText2(value0) {
      this.value0 = value0;
    }
    ;
    ChangeText2.create = function(value0) {
      return new ChangeText2(value0);
    };
    return ChangeText2;
  }();
  var Zipper = /* @__PURE__ */ function() {
    function Zipper2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Zipper2.create = function(value0) {
      return function(value1) {
        return new Zipper2(value0, value1);
      };
    };
    return Zipper2;
  }();
  var State = function(x) {
    return x;
  };
  var unTerminal = function(v) {
    if (v instanceof Terminal) {
      return new Just(v.value0);
    }
    ;
    return Nothing.value;
  };
  var unNonTerminal = function(v) {
    if (v instanceof NonTerminal) {
      return new Just(v.value0);
    }
    ;
    return Nothing.value;
  };
  var startRules = function(dictEq) {
    return function(v) {
      return function(p) {
        var filtered = filter(function(v1) {
          return eq(dictEq)(v1.pName)(p);
        })(v);
        return function(lookahead) {
          return mapFlipped(functorArray)(filtered)(function(v1) {
            return {
              rName: v1.rName,
              rule: new Zipper([], v1.rule),
              lookahead
            };
          });
        };
      };
    };
  };
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
            throw new Error("Failed pattern match at Parser.Main (line 270, column 3 - line 270, column 54): " + [v.constructor.name]);
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
            throw new Error("Failed pattern match at Parser.Main (line 283, column 1 - line 293, column 9): " + [v.constructor.name]);
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
            throw new Error("Failed pattern match at Parser.Main (line 307, column 7 - line 310, column 58): " + [v.constructor.name]);
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
        throw new Error("Failed pattern match at Parser.Main (line 279, column 1 - line 279, column 125): " + [v.constructor.name]);
      };
    };
  };
  var nextStep = function(v) {
    var v1 = uncons(v.rule.value1);
    if (v1 instanceof Just) {
      return singleton4(v1.value0.head)(pure(applicativeArray)({
        rName: v.rName,
        rule: new Zipper(append(semigroupArray)(v.rule.value0)([v1.value0.head]), v1.value0.tail),
        lookahead: v.lookahead
      }));
    }
    ;
    if (v1 instanceof Nothing) {
      return empty2;
    }
    ;
    throw new Error("Failed pattern match at Parser.Main (line 233, column 47 - line 239, column 38): " + [v1.constructor.name]);
  };
  var isNonTerminal = function(v) {
    if (v instanceof NonTerminal) {
      return true;
    }
    ;
    return false;
  };
  var preview = function(tail2) {
    var v = span(isNonTerminal)(tail2);
    var following = mapMaybe(unNonTerminal)(v.init);
    var $$continue = bind(bindMaybe)(head(v.rest))(unTerminal);
    return {
      following,
      "continue": $$continue
    };
  };
  var genericPart = {
    to: function(x) {
      if (x instanceof Inl) {
        return new NonTerminal(x.value0);
      }
      ;
      if (x instanceof Inr) {
        return new Terminal(x.value0);
      }
      ;
      throw new Error("Failed pattern match at Parser.Main (line 96, column 1 - line 96, column 58): " + [x.constructor.name]);
    },
    from: function(x) {
      if (x instanceof NonTerminal) {
        return new Inl(x.value0);
      }
      ;
      if (x instanceof Terminal) {
        return new Inr(x.value0);
      }
      ;
      throw new Error("Failed pattern match at Parser.Main (line 96, column 1 - line 96, column 58): " + [x.constructor.name]);
    }
  };
  var showPart = function(dictShow) {
    return function(dictShow1) {
      return {
        show: function(x) {
          return genericShow(genericPart)(genericShowSum(genericShowConstructor(genericShowArgsArgument(dictShow))({
            reflectSymbol: function() {
              return "NonTerminal";
            }
          }))(genericShowConstructor(genericShowArgsArgument(dictShow1))({
            reflectSymbol: function() {
              return "Terminal";
            }
          })))(x);
        }
      };
    };
  };
  var renderZipper = function(dictShow) {
    return function(dictShow1) {
      return function(v) {
        return function(dictKorok) {
          return span2(dictKorok)(bang2(dictKorok.MonadST5().Monad0().Applicative0())(attr(attrSpan_ClassString)(Class.value)("zipper")))([span2(dictKorok)(bang2(dictKorok.MonadST5().Monad0().Applicative0())(attr(attrSpan_ClassString)(Class.value)("before")))(map(functorArray)(function() {
            var $342 = text_(dictKorok.MonadST5().Monad0());
            var $343 = show(showPart(dictShow)(dictShow1));
            return function($344) {
              return $342($343($344));
            };
          }())(v.value0)), span2(dictKorok)(bang2(dictKorok.MonadST5().Monad0().Applicative0())(attr(attrSpan_ClassString)(Class.value)("after")))(map(functorArray)(function() {
            var $345 = text_(dictKorok.MonadST5().Monad0());
            var $346 = show(showPart(dictShow)(dictShow1));
            return function($347) {
              return $345($346($347));
            };
          }())(v.value1))]);
        };
      };
    };
  };
  var renderItem = function(dictShow) {
    return function(dictShow1) {
      return function(dictShow2) {
        return function(v) {
          return function(dictKorok) {
            return [span2(dictKorok)(bang2(dictKorok.MonadST5().Monad0().Applicative0())(attr(attrSpan_ClassString)(Class.value)("rule name")))([text_(dictKorok.MonadST5().Monad0())(show(dictShow1)(v.rName))]), text_(dictKorok.MonadST5().Monad0())(": "), renderZipper(dictShow)(dictShow2)(v.rule)(dictKorok), text_(dictKorok.MonadST5().Monad0())(" "), span2(dictKorok)(bang2(dictKorok.MonadST5().Monad0().Applicative0())(attr(attrSpan_ClassString)(Class.value)("lookahead")))([text_(dictKorok.MonadST5().Monad0())(show(showArray(dictShow2))(v.lookahead))])];
          };
        };
      };
    };
  };
  var renderState = function(dictShow) {
    return function(dictShow1) {
      return function(dictShow2) {
        return function(v) {
          return function(dictKorok) {
            return ul_(dictKorok)(map(functorArray)(function() {
              var $348 = li_(dictKorok);
              return function($349) {
                return $348(function(v1) {
                  return renderItem(dictShow)(dictShow1)(dictShow2)(v1)(dictKorok);
                }($349));
              };
            }())(v));
          };
        };
      };
    };
  };
  var g8Grammar = /* @__PURE__ */ function() {
    return [{
      pName: RE.value,
      rName: RE1.value,
      rule: [new Terminal(LParen.value), new NonTerminal(RL.value), new Terminal(RParen.value)]
    }, {
      pName: RE.value,
      rName: RE2.value,
      rule: [new Terminal(X.value)]
    }, {
      pName: RL.value,
      rName: RL1.value,
      rule: [new NonTerminal(RE.value)]
    }, {
      pName: RL.value,
      rName: RL2.value,
      rule: [new NonTerminal(RL.value), new Terminal(Comma.value), new NonTerminal(RE.value)]
    }];
  }();
  var firsts = function(dictEq) {
    return function(v) {
      return function(ps0) {
        return function(lookahead0) {
          var readyset = function(rules) {
            return function(ps) {
              return function(lookahead) {
                var v1 = uncons(ps);
                if (v1 instanceof Just) {
                  return go2(rules)(v1.value0.head)(v1.value0.tail)(lookahead);
                }
                ;
                return lookahead;
              };
            };
          };
          var go2 = function(rules) {
            return function(p) {
              return function(ps) {
                return function(lookahead) {
                  var v1 = partition(function(v2) {
                    return eq(dictEq)(v2.pName)(p);
                  })(rules);
                  return bind(bindArray)(v1.yes)(function($350) {
                    return function(v2) {
                      if (v2["continue"] instanceof Just) {
                        return readyset(v1.no)(v2.following)([v2["continue"].value0]);
                      }
                      ;
                      if (v2["continue"] instanceof Nothing) {
                        return readyset(v1.no)(append(semigroupArray)(v2.following)(ps))(lookahead);
                      }
                      ;
                      throw new Error("Failed pattern match at Parser.Main (line 227, column 9 - line 229, column 65): " + [v2["continue"].constructor.name]);
                    }(preview(function(v2) {
                      return v2.rule;
                    }($350)));
                  });
                };
              };
            };
          };
          return readyset(v)(ps0)(lookahead0);
        };
      };
    };
  };
  var findNT = function(v) {
    return bind(bindMaybe)(uncons(v.value1))(function(v1) {
      if (v1.head instanceof NonTerminal) {
        var v2 = preview(v1.tail);
        return new Just({
          nonterminal: v1.head.value0,
          following: v2.following,
          "continue": v2["continue"]
        });
      }
      ;
      return Nothing.value;
    });
  };
  var eqPart = function(dictEq) {
    return function(dictEq1) {
      return {
        eq: function(x) {
          return function(y) {
            if (x instanceof NonTerminal && y instanceof NonTerminal) {
              return eq(dictEq)(x.value0)(y.value0);
            }
            ;
            if (x instanceof Terminal && y instanceof Terminal) {
              return eq(dictEq1)(x.value0)(y.value0);
            }
            ;
            return false;
          };
        }
      };
    };
  };
  var eqZipper = function(dictEq) {
    return function(dictEq1) {
      return {
        eq: function(x) {
          return function(y) {
            return eq(eqArray(eqPart(dictEq)(dictEq1)))(x.value0)(y.value0) && eq(eqArray(eqPart(dictEq)(dictEq1)))(x.value1)(y.value1);
          };
        }
      };
    };
  };
  var minimizeState = function(dictEq) {
    return function(dictEq1) {
      return function(dictEq2) {
        var $351 = foldl2(function(items) {
          return function(newItem) {
            var accumulate = function(alreadyFound) {
              return function(item) {
                var $277 = eq(dictEq1)(item.rName)(newItem.rName) && eq(eqZipper(dictEq)(dictEq2))(item.rule)(newItem.rule);
                if ($277) {
                  return {
                    accum: true,
                    value: {
                      rName: item.rName,
                      rule: item.rule,
                      lookahead: nubEq(dictEq2)(append(semigroupArray)(item.lookahead)(newItem.lookahead))
                    }
                  };
                }
                ;
                return {
                  accum: alreadyFound,
                  value: item
                };
              };
            };
            var v = mapAccumL(traversableArray)(accumulate)(false)(items);
            if (v.accum) {
              return v.value;
            }
            ;
            return append(semigroupArray)(v.value)([newItem]);
          };
        })([]);
        return function($352) {
          return State($351($352));
        };
      };
    };
  };
  var eqState = function(dictEq) {
    return function(dictEq1) {
      return function(dictEq2) {
        return {
          eq: function(v) {
            return function(v1) {
              var v2 = minimizeState(dictEq)(dictEq1)(dictEq2)(v);
              var v3 = minimizeState(dictEq)(dictEq1)(dictEq2)(v1);
              var v4 = minimizeState(dictEq)(dictEq1)(dictEq2)(append(semigroupArray)(v2)(v3));
              var v5 = minimizeState(dictEq)(dictEq1)(dictEq2)(append(semigroupArray)(v3)(v2));
              return eq(eqArray(eqRec()(eqRowCons(eqRowCons(eqRowCons(eqRowNil)()({
                reflectSymbol: function() {
                  return "rule";
                }
              })(eqZipper(dictEq)(dictEq2)))()({
                reflectSymbol: function() {
                  return "rName";
                }
              })(dictEq1))()({
                reflectSymbol: function() {
                  return "lookahead";
                }
              })(eqArray(dictEq2)))))(v2)(v4) && eq(eqArray(eqRec()(eqRowCons(eqRowCons(eqRowCons(eqRowNil)()({
                reflectSymbol: function() {
                  return "rule";
                }
              })(eqZipper(dictEq)(dictEq2)))()({
                reflectSymbol: function() {
                  return "rName";
                }
              })(dictEq1))()({
                reflectSymbol: function() {
                  return "lookahead";
                }
              })(eqArray(dictEq2)))))(v3)(v5);
            };
          }
        };
      };
    };
  };
  var semigroupState = function(dictEq) {
    return function(dictEq1) {
      return function(dictEq2) {
        return {
          append: function(v) {
            return function(v1) {
              return minimizeState(dictEq)(dictEq1)(dictEq2)(append(semigroupArray)(v)(v1));
            };
          }
        };
      };
    };
  };
  var ordPart = function(dictOrd) {
    return function(dictOrd1) {
      return {
        compare: function(x) {
          return function(y) {
            if (x instanceof NonTerminal && y instanceof NonTerminal) {
              return compare(dictOrd)(x.value0)(y.value0);
            }
            ;
            if (x instanceof NonTerminal) {
              return LT.value;
            }
            ;
            if (y instanceof NonTerminal) {
              return GT.value;
            }
            ;
            if (x instanceof Terminal && y instanceof Terminal) {
              return compare(dictOrd1)(x.value0)(y.value0);
            }
            ;
            throw new Error("Failed pattern match at Parser.Main (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
          };
        },
        Eq0: function() {
          return eqPart(dictOrd.Eq0())(dictOrd1.Eq0());
        }
      };
    };
  };
  var nextSteps = function(dictOrd) {
    return function(dictOrd1) {
      return function(v) {
        return foldMap2(monoidSemigroupMap(ordPart(dictOrd)(dictOrd1))(semigroupArray))(nextStep)(v);
      };
    };
  };
  var continueOn = function($$continue) {
    return function(lookahead) {
      if ($$continue instanceof Just) {
        return [$$continue.value0];
      }
      ;
      if ($$continue instanceof Nothing) {
        return lookahead;
      }
      ;
      throw new Error("Failed pattern match at Parser.Main (line 188, column 33 - line 190, column 23): " + [$$continue.constructor.name]);
    };
  };
  var closeItem = function(dictEq) {
    return function(grammar) {
      return function(item) {
        var v = findNT(item.rule);
        if (v instanceof Nothing) {
          return [];
        }
        ;
        if (v instanceof Just) {
          return startRules(dictEq)(grammar)(v.value0.nonterminal)(firsts(dictEq)(grammar)(v.value0.following)(continueOn(v["value0"]["continue"])(item.lookahead)));
        }
        ;
        throw new Error("Failed pattern match at Parser.Main (line 198, column 26 - line 202, column 68): " + [v.constructor.name]);
      };
    };
  };
  var close1 = function(dictEq) {
    return function(grammar) {
      return function(v) {
        return bindFlipped(bindArray)(closeItem(dictEq)(grammar))(v);
      };
    };
  };
  var close = function($copy_dictEq) {
    return function($copy_dictEq1) {
      return function($copy_dictEq2) {
        return function($copy_grammar) {
          return function($copy_state0) {
            var $tco_var_dictEq = $copy_dictEq;
            var $tco_var_dictEq1 = $copy_dictEq1;
            var $tco_var_dictEq2 = $copy_dictEq2;
            var $tco_var_grammar = $copy_grammar;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(dictEq, dictEq1, dictEq2, grammar, state0) {
              var state$prime = close1(dictEq1)(grammar)(state0);
              var $317 = $$null(state$prime);
              if ($317) {
                $tco_done = true;
                return state0;
              }
              ;
              var state4 = append(semigroupState(dictEq1)(dictEq)(dictEq2))(state0)(state$prime);
              var $318 = eq(eqState(dictEq1)(dictEq)(dictEq2))(state4)(state0);
              if ($318) {
                $tco_done = true;
                return state0;
              }
              ;
              $tco_var_dictEq = dictEq;
              $tco_var_dictEq1 = dictEq1;
              $tco_var_dictEq2 = dictEq2;
              $tco_var_grammar = grammar;
              $copy_state0 = state4;
              return;
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_dictEq, $tco_var_dictEq1, $tco_var_dictEq2, $tco_var_grammar, $copy_state0);
            }
            ;
            return $tco_result;
          };
        };
      };
    };
  };
  var newStates = function(dictOrd) {
    return function(dictEq) {
      return function(dictOrd1) {
        return function(grammar) {
          return function(state4) {
            return nubEq(eqState(dictOrd.Eq0())(dictEq)(dictOrd1.Eq0()))(fromFoldable(foldableSemigroupMap)(map(functorSemigroupMap)(function() {
              var $356 = close(dictEq)(dictOrd.Eq0())(dictOrd1.Eq0())(grammar);
              var $357 = minimizeState(dictOrd.Eq0())(dictEq)(dictOrd1.Eq0());
              return function($358) {
                return $356($357($358));
              };
            }())(nextSteps(dictOrd)(dictOrd1)(state4))));
          };
        };
      };
    };
  };
  var closeStates1 = function(dictOrd) {
    return function(dictEq) {
      return function(dictOrd1) {
        return function(grammar) {
          return function(states) {
            return nubEq(eqState(dictOrd.Eq0())(dictEq)(dictOrd1.Eq0()))(append(semigroupArray)(states)(bind(bindArray)(states)(newStates(dictOrd)(dictEq)(dictOrd1)(grammar))));
          };
        };
      };
    };
  };
  var closeStates = function($copy_dictOrd) {
    return function($copy_dictEq) {
      return function($copy_dictOrd1) {
        return function($copy_grammar) {
          return function($copy_states) {
            var $tco_var_dictOrd = $copy_dictOrd;
            var $tco_var_dictEq = $copy_dictEq;
            var $tco_var_dictOrd1 = $copy_dictOrd1;
            var $tco_var_grammar = $copy_grammar;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(dictOrd, dictEq, dictOrd1, grammar, states) {
              var states$prime = closeStates1(dictOrd)(dictEq)(dictOrd1)(grammar)(states);
              var $319 = eq(eqArray(eqState(dictOrd.Eq0())(dictEq)(dictOrd1.Eq0())))(states$prime)(states);
              if ($319) {
                $tco_done = true;
                return states;
              }
              ;
              $tco_var_dictOrd = dictOrd;
              $tco_var_dictEq = dictEq;
              $tco_var_dictOrd1 = dictOrd1;
              $tco_var_grammar = grammar;
              $copy_states = states$prime;
              return;
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_dictEq, $tco_var_dictOrd1, $tco_var_grammar, $copy_states);
            }
            ;
            return $tco_result;
          };
        };
      };
    };
  };
  var bifunctorPart = {
    bimap: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof NonTerminal) {
            return new NonTerminal(v(v2.value0));
          }
          ;
          if (v2 instanceof Terminal) {
            return new Terminal(v1(v2.value0));
          }
          ;
          throw new Error("Failed pattern match at Parser.Main (line 100, column 1 - line 102, column 46): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    }
  };
  var seed = function(v) {
    return function(entry) {
      var rules$prime = mapFlipped(functorArray)(v)(function(v1) {
        return {
          pName: new Just(v1.pName),
          rName: new Just(v1.rName),
          rule: map(functorArray)(bimap(bifunctorPart)(Just.create)(Just.create))(v1.rule)
        };
      });
      var rule0 = {
        pName: Nothing.value,
        rName: Nothing.value,
        rule: [new NonTerminal(new Just(entry)), new Terminal(Nothing.value)]
      };
      return {
        augmented: append(semigroupArray)([rule0])(rules$prime),
        start: {
          rName: Nothing.value,
          rule: new Zipper([], rule0.rule),
          lookahead: []
        }
      };
    };
  };
  var generate = function(dictOrd) {
    return function(dictEq) {
      return function(dictOrd1) {
        return function(initial) {
          return function(entry) {
            var v = seed(initial)(entry);
            return closeStates(ordMaybe(dictOrd))(eqMaybe(dictEq))(ordMaybe(dictOrd1))(v.augmented)([close(eqMaybe(dictEq))(eqMaybe(dictOrd.Eq0()))(eqMaybe(dictOrd1.Eq0()))(v.augmented)(minimizeState(eqMaybe(dictOrd.Eq0()))(eqMaybe(dictEq))(eqMaybe(dictOrd1.Eq0()))([v.start]))]);
          };
        };
      };
    };
  };
  var g8Generated = function(v) {
    return generate(ordSorts)(eqRule)(ordTok)(g8Grammar)(RE.value);
  };
  var main = /* @__PURE__ */ runInBody1(/* @__PURE__ */ bus(monadSTEffect)(function(push2) {
    return lcmap(profunctorFn)(function(v) {
      return alt(altEvent(applicativeEffect))(bang2(applicativeEffect)(UIShown.value))(v);
    })(function(event) {
      var currentValue = alt(altEvent(applicativeEffect))(bang2(applicativeEffect)(""))(flip(filterMap(filterableEvent(applicativeEffect)))(event)(function(v) {
        if (v instanceof ChangeText) {
          return new Just(v.value0);
        }
        ;
        return Nothing.value;
      }));
      var top4 = [input(korokGlobalEffect)(oneOfMap(foldableArray)(plusEvent(applicativeEffect))(bang2(applicativeEffect))([attr(attrOnInputCb)(OnInput.value)(cb(function(e) {
        return for_(applicativeEffect)(foldableMaybe)(bind(bindMaybe)(target5(e))(fromEventTarget))(composeKleisli(bindEffect)(value3)(function($359) {
          return push2(ChangeText.create($359));
        }));
      })), attr(attrOnKeyupCb)(OnKeyup.value)(cb(function(e) {
        return for_(applicativeEffect)(foldableMaybe)(fromEvent(e))(function(evt) {
          return when(applicativeEffect)(code(evt) === "Enter")(push2(AddTodo.value));
        });
      }))]))([]), button(korokGlobalEffect)(bang2(applicativeEffect)(attr(attrOnClickEffectUnit)(OnClick.value)(push2(AddTodo.value))))([text_(monadEffect)("Add")])];
      return div_(korokGlobalEffect)([style_(korokGlobalEffect)(pure(applicativeArray)(text_(monadEffect)("\n            .before { color: lightgray; }\n          "))), table_(korokGlobalEffect)(pure(applicativeArray)(tbody_(korokGlobalEffect)(map(functorArray)(function() {
        var $360 = tr_(korokGlobalEffect);
        var $361 = map(functorArray)(td_(korokGlobalEffect));
        return function($362) {
          return $360($361($362));
        };
      }())([[[text_(monadEffect)("E")], [text_(monadEffect)("::=")], [text_(monadEffect)("("), text_(monadEffect)("L"), text_(monadEffect)(")")], [text_(monadEffect)("data E")], [text_(monadEffect)("=")], [text_(monadEffect)("E1"), text_(monadEffect)(" "), text_(monadEffect)("L")]], [[], [text_(monadEffect)("|")], [text_(monadEffect)("x")], [], [text_(monadEffect)("|")], [text_(monadEffect)("E2")]], [[text_(monadEffect)("L")], [text_(monadEffect)("::=")], [text_(monadEffect)("E")], [text_(monadEffect)("data L")], [text_(monadEffect)("=")], [text_(monadEffect)("L1"), text_(monadEffect)(" "), text_(monadEffect)("E")]], [[], [text_(monadEffect)("|")], [text_(monadEffect)("L"), text_(monadEffect)(","), text_(monadEffect)("E")], [], [text_(monadEffect)("|")], [text_(monadEffect)("L2"), text_(monadEffect)(" "), text_(monadEffect)("L"), text_(monadEffect)(" "), text_(monadEffect)("E")]]])))), div_(korokGlobalEffect)(pure(applicativeArray)(ol_(korokGlobalEffect)(map(functorArray)(function() {
        var $363 = li_(korokGlobalEffect);
        var $364 = pure(applicativeArray);
        return function($365) {
          return $363($364(function(v) {
            return renderState(showMaybe(showSorts))(showMaybe(showRule))(showMaybe(showTok))(v)(korokGlobalEffect);
          }($365)));
        };
      }())(g8Generated(unit))))), div_(korokGlobalEffect)(top4), div_(korokGlobalEffect)(pure(applicativeArray)(flip(switcher(monadSTEffect))(currentValue)(function(v) {
        return div_(korokGlobalEffect)([showMaybeParseSteps(showTok)(flap(functorMaybe)(map(functorMaybe)(parseSteps(g8Table()))(g8FromString(v)))(S1.value))(korokGlobalEffect)]);
      }))), div_(korokGlobalEffect)([dyn(map(functorEvent)(function(txt) {
        return keepLatest(eventIsEvent(monadSTEffect))(bus(monadSTEffect)(function(p$prime) {
          return function(e$prime) {
            return alt(altEvent(applicativeEffect))(bang2(applicativeEffect)(new Insert(div_(korokGlobalEffect)([text_(monadEffect)(txt), button(korokGlobalEffect)(bang2(applicativeEffect)(attr(attrOnClickEffectUnit)(OnClick.value)(p$prime(sendToTop))))([text_(monadEffect)("Prioritize")]), button(korokGlobalEffect)(bang2(applicativeEffect)(attr(attrOnClickEffectUnit)(OnClick.value)(p$prime(Remove.value))))([text_(monadEffect)("Delete")])]))))(e$prime);
          };
        }));
      })(filterMap(filterableEvent(applicativeEffect))(function(v) {
        if (v.value0) {
          return new Just(v.value1);
        }
        ;
        return Nothing.value;
      })(mapAccum(eventIsEvent(monadSTEffect))(function(a) {
        return function(b) {
          if (a instanceof ChangeText) {
            return new Tuple(a.value0, new Tuple(false, a.value0));
          }
          ;
          if (a instanceof AddTodo) {
            return new Tuple(b, new Tuple(true, b));
          }
          ;
          return new Tuple("", new Tuple(false, ""));
        };
      })(event)(""))))])]);
    });
  }));

  // output/Main/index.js
  var main2 = main;

  // <stdin>
  main2();
})();
