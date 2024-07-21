Verity = Ve = {};
(function Verity() {
  Ve.noop = ()=>{};

  function pythonic(fn) {
    return function(...args) {
      return fn(this, ...args);
    };
  };

  //////////////////////////////////////////////////////////////////////////////

  // Create a simple proxy for syntactic sugar for method names and function calls
  const sugar = (handler, applier=undefined) => new Proxy(applier ? ()=>{} : {}, {
    get: (_target, property, _thisArg) => {
      return handler(property);
    },
    apply: applier && ((_target, _thisArg, args) => applier(...args)),
  });
  const sugarArg = handler => sugar(handler, handler(undefined));

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

  //////////////////////////////////////////////////////////////////////////////

  Object.assign(Ve, {
    sugar, _, mk, modifier, compose, _pipe,
  });

  var SPECIALS = {
    style: (e, style) =>
      style && Object.assign(e.style, typeof style === 'string' ? {style} : style),
    attrs: (e, attrs) =>
      attrs && Object.assign(e.attrs, attrs),
    $parent: (e, parent) =>
      parent && parent !== e.parentNode && parent.appendChild(e),
    $textContent: (e, textContent) =>
      e.textContent = textContent,
    $children: (parent, topChildren) => {
      if (!topChildren) return;
      if (topChildren instanceof Node) {
        return parent.appendChild(topChildren);
      }
      function go(children) {
        for (let child of children) {
          if (child === undefined || child === null) continue;
          if (Array.isArray(child)) {
            go(child);
          } else {
            parent.appendChild(createChildOf(parent, child));
          }
        }
      }
      go(topChildren);
    },
    // $view: (e, view) =>
    //   view && (e.$view = view),
    // $preview: (e, preview) =>
    //   preview && (e.$preview = preview),
    // $model: (e, model) =>
    //   e.MVC(model, e.$view),
  };
  function applyAttrs(e, attrs) {
    attrs = attrs ? Object.assign({}, attrs) : {};
    var specialAttrs = {};
    for (let k in attrs) {
      if (k in SPECIALS) {
        specialAttrs[k] = attrs[k];
        delete attrs[k];
      } else if (k.startsWith('$')) {
        console.warn(`Warning: unknown special attribute ${k} on element`, e);
        delete attrs[k];
      }
    }
    var hadChildren = '$children' in specialAttrs;
    for (let k in attrs) {
      if (/\d+/.test(k)) {
        if (hadChildren) throw new Error("Cannot mix numeric and $children");
        if (!('$children' in specialAttrs)) specialAttrs['$children'] = [];
        specialAttrs['$children'][k] = attrs[k];
        delete attrs[k];
      }
    }
    Object.assign(e, attrs);
    for (let k in specialAttrs) {
      SPECIALS[k](e, specialAttrs[k]);
    }
    return e;
  };
  function createChildOf(parent, attrs, tag, ...children) {
    if (typeof attrs === 'string' || typeof attrs === 'number' || attrs instanceof String || attrs instanceof Number) {
      return document.createTextNode(String(attrs));
    }
    if (Array.isArray(attrs)) {
      return SPECIALS['$children'](document.createDocumentFragment(), attrs);
    }
    if (attrs instanceof Node) return attrs;
    attrs = attrs ? Object.assign({}, attrs) : {};
    if (children?.length) {
      attrs['$children'] = [attrs?.['$children'], children];
    }
    tag = tag || attrs['$tag'];
    if (!tag) tag = 'div';
    delete attrs['$tag'];
    if (tag instanceof Element) tag = tag.tagName;
    let e = parent?.namespaceURI ? document.createElementNS(parent?.namespaceURI || undefined, tag) : document.createElement(tag);
    return applyAttrs(e, attrs);
  };

  Object.assign(Ve, {
    applyAttrs, createChildOf,
  });

  Ve.ById = sugar(id => document.getElementById(id));
  Ve.HTML = sugarArg(tag => (attrs = {}, ...children) => createChildOf(undefined, attrs, tag, ...children));
  Ve.SVGNS = 'http://www.w3.org/2000/svg';
  Ve.SVG = sugarArg(ty => (attrs = {}, ...children) => applyAttrs(document.createElementNS(Ve.SVGNS, ty), attrs));

  Ve.on = sugar(ty => (tgt, handler, options) => {
    if (typeof options === 'function' && typeof handler !== 'function') {
      [handler, options] = [options, handler];
    };
    tgt.addEventListener(ty, handler, options);
    return () => {
      tgt.removeEventListener(ty, handler, options);
    };
  });
  Ve.once = sugar(ty => (tgt, handler, options) => {
    if (typeof options === 'function' && typeof handler !== 'function') {
      [handler, options] = [options, handler];
    };
    options = Object.assign({}, options, {once:true});
    return Ve.on[ty](tgt, handler, options);
  });
  Ve.immediately = (cb, ...args) => {
    cb(...args);
    return Ve.noop;
  };

  Ve.GET = async (url, options) => {
    const decisions = {
      method: 'GET',
      headers: {
        'Accept': 'application/json',
      },
    };
    if (typeof options === 'string') {
      decisions.headers['Accept'] = options;
    } else if (typeof options === 'object') {
      Object.assign(decisions, options, {
        headers: Object.assign(decisions.headers, options.headers),
      });
    }
    let r = await fetch(url, decisions);
    if (decisions.headers['Accept'] === 'application/json') r = r.json();
    return r;
  };

  Ve.DELETE = async (url, options) => {
    const decisions = {
      method: 'DELETE',
    };
    if (typeof options === 'object') {
      Object.assign(decisions, options);
    }
    let r = await fetch(url, decisions);
    r = r.text();
    return r;
  };

  Ve.POST = async (url, data, options) => {
    const decisions = {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(data),
    };
    if (typeof options === 'string') {
      decisions.headers['Accept'] = options;
    } else if (typeof options === 'object') {
      Object.assign(decisions, options, {
        headers: Object.assign(decisions.headers, options.headers),
      });
    }
    let r = await fetch(url, decisions);
    if (decisions.headers['Accept'] === 'application/json') r = r.json();
    return r;
  };

  Ve.PUT = async (url, data, options) => {
    const decisions = {
      method: 'PUT',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(data),
    };
    if (typeof options === 'string') {
      decisions.headers['Accept'] = options;
    } else if (typeof options === 'object') {
      Object.assign(decisions, options, {
        headers: Object.assign(decisions.headers, options.headers),
      });
    }
    let r = await fetch(url, decisions);
    if (decisions.headers['Accept'] === 'application/json') r = r.json();
    return r;
  };

  //////////////////////////////////////////////////////////////////////////////

  Object.defineProperty(Node.prototype, "attrs", {
    get: function () {
      return new Proxy(this, {
        get: (t, p) => t.getAttribute(p),
        set: (t, p, v, x) => { t.setAttribute(p, v); return true; },
      });
    },
  });

  Node.prototype.applyAttrs = pythonic(applyAttrs);
  Element.prototype.getAttributes = function() {
    return Object.fromEntries(this.getAttributeNames().map(name => [name, this.getAttribute(name)]));
  };
  Element.prototype.setAttributes = function(attrs) {
    for (let [name, value] in Object.entries(attrs)) {
      this.setAttribute(name, value);
    }
  };
  Element.prototype.copyAttributes = function(copyFrom) {
    this.setAttributes(copyFrom.getAttributes());
  };
  Node.prototype.removeSelf = function() {
    this.parentNode.removeChild(this);
  };

  // Useful for typed DOM APIs like `document.querySelectorAll`
  Ve.forEach = Array.prototype.forEach.call.bind(Array.prototype.forEach);
  Ve.forQuery = (query, cb) => {
    const results = [];
    const selected = document.querySelectorAll(query);
    Ve.forEach(selected, (...args) => {
      results.push(cb ? cb(...args) : args[0]);
    });
    return results;
  };

  var PromiseOrCb = executor => Object.assign(executor, { then: (...args) => executor(...args) });

  Ve.TimeoutMs = (timeout, cb) => {
    if (!cb) return new PromiseOrCb(resolve => Ve.TimeoutMs(timeout, resolve));
    let id = setTimeout(cb, timeout);
    return () => { clearTimeout(id); };
  };
  Ve.ContentLoad = PromiseOrCb(cb => {
    // https://developer.mozilla.org/en-US/docs/Web/API/Document/readyState
    // 'loading', 'interactive', 'completed'
    //
    // interactive
    //   The document has finished loading and the document has been parsed but
    //   sub-resources such as scripts, images, stylesheets and frames are still
    //   loading. The state indicates that the DOMContentLoaded event is about
    //   to fire.
    if (document.readyState === 'completed') {
      return Ve.immediately(cb);
    } else {
      // https://developer.mozilla.org/en-US/docs/Web/API/Document/DOMContentLoaded_event
      // The DOMContentLoaded event fires when the HTML document has been
      // completely parsed, and all deferred scripts (<script defer src="…"> and
      // <script type="module">) have downloaded and executed. It doesn't wait
      // for other things like images, subframes, and async scripts to finish
      // loading.
      //
      // DOMContentLoaded does not wait for stylesheets to load, however
      // deferred scripts do wait for stylesheets, and the DOMContentLoaded
      // event is queued after deferred scripts. Also, scripts which aren't
      // deferred or async (e.g. <script>) will wait for already-parsed
      // stylesheets to load.
      //
      // A different event, load, should be used only to detect a fully-loaded
      // page. It is a common mistake to use load where DOMContentLoaded would
      // be more appropriate.
      return Ve.once.DOMContentLoaded(window, cb);
    }
  });
  Ve.FullLoad = PromiseOrCb(cb => {
    if (document.readyState === 'completed') {
      return Ve.immediately(cb);
    } else {
      return Ve.once.load(window, cb);
    }
  });

  //////////////////////////////////////////////////////////////////////////////

  return this;
}).call(Ve);
