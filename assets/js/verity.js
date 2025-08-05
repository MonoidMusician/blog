////////////////////////////////////////////////////////////////////////////////
// You should not consider this file as normal JavaScript objects that play   //
// nicely with the runtime and assumptions that code and handlers make. You   //
// should consider this (not-so-smol) library as a way of enabling syntax     //
// that you (I) might like to write. Hopefully there are enough examples to   //
// give you a good idea of what uses we intend for them, what the suggested   //
// syntax is.                                                                 //
////////////////////////////////////////////////////////////////////////////////
Verity = Ve = {};
(function Veritification() {
  Ve.noop = ()=>{};

  function pythonic(fn) {
    return function(...args) {
      return fn(this, ...args);
    };
  };

  //////////////////////////////////////////////////////////////////////////////
  // Lots of silly syntax sugar, that makes use of Proxies! (also getters and //
  // `defineProperty` and so on). This is basically to maintain my philosophy //
  // that fixed identifiers should appear as identifiers in the source code   //
  // (kind of like how Erlang treats “atoms”), rather than as strings.        //
  //////////////////////////////////////////////////////////////////////////////

  // Create a simple proxy for syntactic sugar for method names and function calls
  const sugar = (handler, applier=undefined) => new Proxy(applier ? ()=>{} : {}, {
    get: (_target, property, _thisArg) => {
      return handler(property);
    },
    apply: applier && ((_target, _thisArg, args) => applier(...args)),
  });
  const sugarArg = handler => sugar(handler, handler(undefined));
  const sugarStr = handler => sugar(handler, handler)

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

  // Another kind of getter, on `this` instead of the first argument.
  //     _this.prop = function(){ return this.prop };
  //     _this.prop.call({ prop: 5 }) == 5
  // As a function, it just returns `this`, which is useful in composition
  // pipelines or so forth.
  const _this = sugar(
    name => function() { return this[name] },
    function() { return this },
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

  // Thatʼs the easy stuff. Letʼs dig deeper!

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
  const pipelining = (pipelined, finit="line_") => new Proxy(()=>{}, {
    get: (_target, property, _thisArg) => {
      // Special case to end the pipeline
      // (yeah, it could be a symbol to be pedantic about abstraction,
      // but that makes it less symmetrical!)
      if (property === finit) return compose(...pipelined);

      // See documentation for `compose` for why this is inserted as
      // a string/symbol still, instead of `_[property]`:
      return pipelining([ ...pipelined, property ], finit);
    },
    apply: (_target, _thisArg, args) => {
      return pipelining([ ...pipelined, _(...args) ], finit);
    },
  });

  // Create a pipeline of `_`-style accessors that end with `.line_`:
  //     _pipe.join("").line_ = x => x.join("")
  //     _pipe.join(", ").line_(["do", "re", "mi"]) == "do, re, mi"
  //     _pipe.join(", ").length.line_(["do", "re", "mi"]) == 10
  const _pipe = pipelining([], "line_");

  const intercalate = (fixed, ...intermediates) => {
    const result = [];
    fixed.forEach((item, i) => {
      result.push(item);
      if (i < fixed.length - 1)
        result.push(intermediates[i]);
    });
    return result;
  };
  intercalate.raw = ({ raw: fixed }, ...intermediates) => {
    return intercalate(fixed, ...intermediates);
  };

  //////////////////////////////////////////////////////////////////////////////
  // That was originally made as its own little thing. Now we get into the    //
  // rest of my convenience functions and library, under the `Ve`/`Verity`    //
  // namespace.                                                               //
  //////////////////////////////////////////////////////////////////////////////

  Object.assign(Ve, {
    sugar, _, _this, mk, modifier, compose, _pipe, intercalate,
  });

  // Create a `String` object whose value is computed on demand
  //     Ve.StringOnDemand(() => "hi") instanceof String == true
  //     String(Ve.StringOnDemand(() => "hi")) == "hi"
  //     Ve.StringOnDemand(() => "hello") + " world" == "hello world"
  Ve.StringOnDemand = fn => Object.create(Object.assign(new String(), { toString: fn, valueOf: fn }));

  // Create a `Number` object whose value is computed on demand
  //     Ve.NumberOnDemand(() => Math.random()) instanceof Number == true
  //     Number(Ve.NumberOnDemand(() => Math.PI)) == Math.PI
  //     Ve.NumberOnDemand(() => 2) + 3 == 5
  Ve.NumberOnDemand = fn => Object.create(Object.assign(new Number(), { valueOf: fn }));

  //////////////////////////////////////////////////////////////////////////////
  // DOM library. It is not a VDOM library, just convenience helpers for      //
  // making constructing DOM nodes much more pleasant!                        //
  //////////////////////////////////////////////////////////////////////////////

  // Special properties, or properties that already exist but deserve some
  // special handling.
  //
  // There is currently an unresolved tension between wanting to have the
  // properties apply cleanly, versus having it be compositional. We really
  // should have a DOM that is properly monoidal, but that is beyond the scope
  // here... (think about if all DOM properties and attributes and CSS
  // properties had sensible ways to combine together and work compositionally!)
  var SPECIALS = {
    style: (e, style) =>
      style && Object.assign(e.style, typeof style === 'string' ? {style} : style),
    attrs: (e, attrs) =>
      attrs && Object.assign(e.attrs, attrs), // see Element.prototype.attrs below
    data: (e, attrs) =>
      attrs && Object.assign(e.dataset, attrs),
    dataset: (e, attrs) =>
      attrs && Object.assign(e.dataset, attrs),
    $parent: (e, parent) =>
      parent && parent !== e.parentNode && parent.appendChild(e),
    $textContent: (e, textContent) =>
      e.textContent = textContent,
    $children: (parent, topChildren) => {
      parent.clearChildren();
      function go(children) {
        if (typeof children === 'function') children = children();
        if
          ( typeof children === 'string' || children instanceof String
          || typeof children === 'number' || children instanceof Number
          || children instanceof Node
          )
          children = [children];
        if (!children) return;
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
      return parent;
    },
    classList: (e, classes) => {
      e.classList = '';
      if (classes) {
        if (typeof classes === 'string' || classes instanceof String) {
          e.classList = String(classes);
        } else {
          e.classList.add(...classes.flatMap(x=>x.split(/\s+/u).filter(Boolean)));
        }
      }
    },
    // I probably donʼt need so many names for it ...
    class: (...arg) => SPECIALS.classList(...arg),
    // Yeah I kind of wanted a MVC type thing, but I never got around to it.
    // $view: (e, view) =>
    //   view && (e.$view = view),
    // $preview: (e, preview) =>
    //   preview && (e.$preview = preview),
    // $model: (e, model) =>
    //   e.MVC(model, e.$view),
  };
  // Apply (special) properties to the specified DOM element.
  function applyProps(e, props) {
    props = props ? Object.assign({}, props) : {};
    var specialProps = {};
    for (let k in props) {
      if (k in SPECIALS) {
        specialProps[k] = props[k];
        delete props[k];
      } else if (k.startsWith('$')) {
        console.warn(`Warning: unknown special attribute ${k} on element`, e);
        delete props[k];
      }
    }
    var hadChildren = '$children' in specialProps;
    for (let k in props) {
      if (/\d+/.test(k)) {
        if (hadChildren) throw new Error("Cannot mix numeric and $children");
        if (!('$children' in specialProps)) specialProps['$children'] = [];
        specialProps['$children'][k] = props[k];
        delete props[k];
      }
    }
    Object.assign(e, props);
    for (let k in specialProps) {
      SPECIALS[k](e, specialProps[k]);
    }
    return e;
  };
  function createChildOf(parent, props, tag, ...children) {
    return createElementNS(parent?.namespaceURI, props, tag, ...children);
  }
  function createElementNS(namespaceURI, props, tag, ...children) {
    const resolved = precreateElementNS(namespaceURI, props, tag, ...children);
    return createElementResolved(resolved);
  }
  function createElementResolved({ namespaceURI, props, tag, children }) {
    if (!tag && !props) {
      if (children?.length === 1) {
        return children[0];
      } else {
        const result = document.createDocumentFragment();
        SPECIALS['$children'](result, children);
        return result;
      }
    }
    let e = namespaceURI
      ? document.createElementNS(namespaceURI || undefined, tag)
      : document.createElement(tag);
    if (children?.length) props['$children'] = children;
    applyProps(e, props);
    return e;
  }
  function precreateElementNS(namespaceURI, props, tag, ...children) {
    if (typeof tag === 'function') tag = tag();
    if (typeof props === 'string' || typeof props === 'number' || props instanceof String || props instanceof Number) {
      children = [document.createTextNode(String(props)), ...children];
      props = undefined;
    } else if (props instanceof Node) {
      children = [props, ...children];
      props = undefined;
    } else if (Array.isArray(props)) {
      children = [...props, ...children];
      props = undefined;
    }
    props = props ? Object.assign({}, props) : undefined;
    if (props) {
      tag = tag || props['$tag'];
      if (!tag) tag = 'div';
      delete props['$tag'];
      namespaceURI = namespaceURI || props['$NS'];
      delete props['$NS'];
      if (props['$children'])
        children = [props?.['$children'], children];
      delete props['$children'];
    } else if (tag) {
      props = {};
    }
    if (tag instanceof Element) tag = tag.tagName;
    if (namespaceURI instanceof Element) namespaceURI = namespaceURI.namespaceURI;
    return { namespaceURI, tag, props, children };
  };

  Object.assign(Ve, {
    applyProps, createChildOf,
    createElementNS, createElementResolved, precreateElementNS,
  });

  // This is nice little sugar to grab an element by ID out of the DOM!
  Ve.ById = sugar(id => document.getElementById(id));

  // Common XML namespaces ... SVG is the only one that really matters fo
  // making DOM operations work, but XHTML is the default for HTML documents
  // already (it is not necessary to specify, but it exists).
  Ve.NS = {
    SVG: "http://www.w3.org/2000/svg",
    XHTML: "http://www.w3.org/1999/xhtml",
    HTML: "XHTML", // DRY
    XLink: "http://www.w3.org/1999/xlink",
    XML: "http://www.w3.org/XML/1998/namespace",
    XMLNS: "http://www.w3.org/2000/xmlns/",
  };
  for (let k in Ve.NS) if (Ve.NS[k] in Ve.NS) Ve.NS[k] = Ve.NS[Ve.NS[k]];
  for (let k in Ve.NS) Ve.NS[k.toLowerCase()] = Ve.NS[k];

  // Templating a DocumentFragment out of strings (text nodes) and other
  // nodes/descriptions of node properties.
  Ve.DOM = (fixed, ...args) => {
    return createChildOf(null, Ve.dedent.intercalate(fixed, ...args));
  };
  // Template a DOM node on a (sugared) namespace. There is a looot of
  // flexibilty in how it can be used. See below for examples.
  Ve.DOM.NS = sugarStr(ns => sugarArg(tag => sugarArg(cls => (props = {}, ...children) => {
    if (ns in Ve.NS) ns = Ve.NS[ns];
    // hint that props needs to exist
    if (!props && cls) props = {};
    const resolved = precreateElementNS(ns, props, tag, ...children);
    if (cls) resolved.props.class = cls;
    return createElementResolved(resolved);
  })));

  // Create an HTML DOM node based on describing its properties, children, and
  // so on.
  Ve.HTML = Ve.DOM.NS.HTML;
  Ve.SVG = Ve.DOM.NS.SVG;

  // Helper for an icon, based on its class.
  Ve.ico = sugarStr(classList => Ve.HTML.i({classList}));
  // Helper for an iconoir icons, based on its class.
  Ve.iconoir = sugarStr(classList => {
    const prepend = s => `iconoir-${s.replaceAll('_','-')}`;
    if (typeof classList === 'string' || classList instanceof String) {
      classList = [prepend(classList)];
    } else {
      classList = [...prepend(classList[0]), classList.slice(1)];
    }
    return Ve.HTML.i({classList});
  });
  Ve.button = onclick => ({})

  // A helper for event handlers that returns an unsubscribe function, instead
  // of having to keep the original function around. For `Element`s,
  // `Ve.on.click(tgt, handler, options)` can also be written as
  // `tgt.on.click(handler, options)` with the magic of prototype pollution,
  // but `Ve.on` is a global method if you prefer that, or are working with
  // other event sources (like `window`).
  Ve.on = sugarStr(tys => (tgt, handler, options) => {
    if (!handler && !options) {
      return CbAndPromise(
        (handler, options) => Ve.on(tys)(tgt, handler, options),
        handler => Ve.once(tys)(tgt, handler),
      );
    }
    if (typeof options === 'function' && typeof handler !== 'function') {
      [handler, options] = [options, handler];
    };
    if (typeof tys === 'string' || tys instanceof String) tys = tys.split(',');
    for (const ty of tys) tgt.addEventListener(ty, handler, options);
    return () => {
      for (const ty of tys) tgt.removeEventListener(ty, handler, options);
      tgt = null; handler = null; options = null;
    };
  });
  // `Ve.once` is like `Ve.on` but it sets the `{once:true}` option.
  Ve.once = sugarStr(tys => (tgt, handler, options) => {
    if (!handler && !options) {
      return PromiseOrCb((handler, options) => Ve.once(tys)(tgt, handler, options));
    }
    if (typeof options === 'function' && typeof handler !== 'function') {
      [handler, options] = [options, handler];
    };
    options = Object.assign({}, options, {once:true});
    return Ve.on(tys)(tgt, handler, options);
  });
  // Run synchronously without delay.
  Ve.immediately = (cb, ...args) => {
    cb(...args);
    return Ve.noop;
  };

  // Some useful shorthands. Should be used with object spread if you need
  // multiple.
  Ve.styl = {
    inline: { display: 'inline' },
    block: { display: 'block' },
    flex: { display: 'flex' },
    flexColumn: { display: 'flex', flexDirection: 'column' },
    flexRow: { display: 'flex', flexDirection: 'row' },
    marginAuto: { margin: 'auto' },
    pointer: { cursor: 'pointer' },
    textCursor: { cursor: 'text' },
  };

  Ve.CSS = Object.create(window.CSS);
  // Just so you can remember what `:root` corresponds to, since it is all
  // the rage now. Specified as a getter just in case it mutates or something.
  Object.defineProperty(Ve.CSS, ":root", { get: () => window.document.documentElement });
  // Return the (root) `Element`, where we should look for styles, on the
  // specified document, an element itself, or the default `<html>` root.
  Ve.CSS.styleRoot = function(tgt=undefined) {
    if (tgt instanceof Element) return tgt;
    if (tgt instanceof Document) return tgt.documentElement;
    return window.document.documentElement;
  };
  // Return the `.style` attribute of the `styleRoot` (as defined above).
  // Can return the computed style, if requested with `computed=true`.
  Ve.CSS.rootStyle = function(tgt=undefined, computed=false) {
    if (tgt instanceof CSSStyleDeclaration) return tgt;
    tgt = Ve.CSS.styleRoot(tgt);
    return computed ? getComputedStyle(tgt) : tgt.style;
  };

  // `getVars` would require parsing stylesheets :-/
  // Ve.CSS.getVars = function(tgt) {};

  // Get a CSS variable, optionally on the target, but by default on the
  // document root (CSS `:root`). Note: this uses the computed style, since that
  // is more relevant for getting variables specified in style sheets, but
  // this means that its original string value (with `var()`s) is not available!
  Ve.CSS.getVar = function(name, tgt=undefined, computed=true) {
    const style = Ve.CSS.rootStyle(tgt || this, computed);
    return style.getPropertyValue(`--${name}`);
  };
  // Get or set a (global) CSS variable (depending on the number of arguments).
  Ve.CSS.etVar = function(...args) {
    if (args.length > 1) return Ve.CSS.setVar.call(this, ...args);
    return Ve.CSS.getVar.call(this, ...args);
  };
  // Set a variable, on the target or on `:root`.
  Ve.CSS.setVar = function(name, value, tgt=undefined) {
    const style = Ve.CSS.rootStyle(tgt || this, false);
    if (value === null || value === undefined) {
      style.removeProperty(`--${name}`);
    } else {
      style.setProperty(`--${name}`, value);
    }
  };
  // Set variables, specified as an object.
  Ve.CSS.setVars = function(values, tgt) {
    const style = Ve.CSS.rootStyle(tgt || this, false);
    for (let [name, value] of Object.entries(values)) {
      if (value === null || value === undefined) {
        style.removeProperty(`--${name}`);
      } else {
        style.setProperty(`--${name}`, value);
      }
    }
  };
  // I could not decide what to name this.
  Ve.__var = Ve.CSS.var = Ve.CSS.__ = Ve.CSS.__var = new Proxy(Ve.CSS.etVar, {
    get(__, name, tgt) {
      return Ve.CSS.getVar.call(tgt, name);
    },
    set(_, name, value, tgt) {
      Ve.CSS.setVar.call(tgt, name, value);
      return true;
    },
  });

  //////////////////////////////////////////////////////////////////////////////

  Ve.DynParams = function(getFragment, setFragment, plural=false) {
    if (!setFragment) setFragment = undefined;

    let fragment;
    let params;
    const getParams = () => {
      let last = fragment;
      fragment = getFragment();
      if (!params || last !== fragment)
        return params = new URLSearchParams(fragment);
      return params;
    };
    const modifyParams = (fn) => {
      const params = getParams();
      const result = fn(params) ?? params;
      if (result === false) return false;
      if (result) {
        setFragment(result);
        fragment = undefined;
      }
      return true;
    };

    return new Proxy({}, {
      get: (_, name) => plural ? getParams().getAll(name) : getParams().get(name),
      ownKeys: () => [...new Set(getParams().keys())],
      has: (_, name) => getParams().has(name),
      set: setFragment && ((_, name, value) => modifyParams(params => {
        if (value instanceof Array) {
          params.delete(name);
          for (const v of value) {
            params.append(name, v);
          }
        } else {
          params.set(name, value);
        }
      })),
      deleteProperty: setFragment && ((_, name) => modifyParams(params => {
        params.delete(name);
      })),
    });
  };
  Ve.Params = {
    _withPrefix: (prefix, setter, that=undefined) => {
      if (typeof setter === 'string') {
        return [
          () => {
            let v = (that||window.location)[setter];
            if (v.startsWith(prefix)) {
              v = v.substring(prefix.length);
            }
            return v;
          },
          Ve.Params._withPrefix(prefix, v => {
            if (that) that[setter] = v; else
              window.history.replaceState(null, "", Object.assign(new URL(window.location), {[setter]: v}));
          }),
        ];
      }
      return params => {
        console.log(params, String(params));
        params = String(params);
        if (params) setter.call(that, prefix + params);
        else setter.call(that, "");
      };
    },
  };
  Object.assign(Ve.Params, {
    _search: Ve.Params._withPrefix(`?`, 'search'),
    _hash: Ve.Params._withPrefix(`#`, 'hash'),
  });
  Object.assign(Ve.Params, {
    search: Ve.DynParams(...Ve.Params._search, false),
    hash: Ve.DynParams(...Ve.Params._hash, false),
    all: {
      search: Ve.DynParams(...Ve.Params._search, true),
      hash: Ve.DynParams(...Ve.Params._hash, true),
    },
  });
  Ve.Params.query = Ve.Params.search;
  Ve.Params.all.query = Ve.Params.all.search;

  //////////////////////////////////////////////////////////////////////////////
  // Convenience network methods, wrappers around `fetch()`, defaulting to    //
  // JSON but can also return the underlying request if you set the `Accept`  //
  // header to another mime type                                              //
  //////////////////////////////////////////////////////////////////////////////

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
  // ~~Prototype pollution~~ Global amenities ^^                              //
  //////////////////////////////////////////////////////////////////////////////

  Object.defineProperties(EventTarget.prototype, {
    // Event handlers (permanent and single-shot). Returns a destructor.
    //     ignore = Ve.ById.my_canvas.on.mousemove(ev => console.log(ev), {});
    on: {
      get: function () {
        return sugar(ty => (handler, options) => Ve.on[ty](this, handler, options));
      },
    },
    once: {
      get: function () {
        return sugar(ty => (handler, options) => Ve.once[ty](this, handler, options));
      },
    },
  });

  Object.defineProperties(Element.prototype, {
    // Attributes of the DOM Element
    attrs: {
      get: function () {
        // arrow functions so `this` still refers to the `Element`
        return new Proxy({}, {
          get: (_, p) => this.getAttribute(p),
          set: (_, p, v) => { this.setAttribute(p, v); return true; },
          ownKeys: (_) => this.getAttributeNames(),
        });
      },
      // Set them all at once, thus overwriting what was previously there
      // (so `getAttributeNames` should return the same keys, barring DOM
      // weirdness).
      set: function (newAttrs) {
        this.clearAttributes();
        Object.assign(this.attrs, newAttrs);
        return true;
      },
    },
    // Scoped CSS variables
    __var: {
      get: function () {
        return new Proxy({}, {
          // arrow functions so `this` still refers to the `Element`
          get: (_, p) => Ve.CSS.getVar.call(this, p),
          set: (_, p, v) => { Ve.CSS.setVar.call(this, p, v); return true; },
        });
      },
    },
  });


  Element.prototype.applyProps = pythonic(applyProps);
  Element.prototype.getAttributes = function() {
    return Object.fromEntries(this.getAttributeNames().map(name => [name, this.getAttribute(name)]));
  };
  Element.prototype.setAttributes = function(attrs) {
    for (let [name, value] in Object.entries(attrs)) {
      this.setAttribute(name, value);
    }
  };
  Element.prototype.clearAttributes = function() {
    for (let attr of this.getAttributeNames()) {
      this.removeAttribute(attr);
    }
  };
  Element.prototype.copyAttributes = function(copyFrom) {
    this.setAttributes(copyFrom.getAttributes());
  };
  Node.prototype.clearChildren = function() {
    for (let c of Array.from(this.children)) {
      c.removeSelf();
    }
  };
  Node.prototype.removeSelf = function() {
    this.parentNode.removeChild(this);
  };

  // Useful for typed DOM APIs like `document.querySelectorAll`
  Ve.forEach = Array.prototype.forEach.call.bind(Array.prototype.forEach);
  Ve.forQuery = (query, cb) => {
    const results = [];
    const selected = window.document.querySelectorAll(query);
    Ve.forEach(selected, (...args) => {
      results.push(cb ? cb(...args) : args[0]);
    });
    return results;
  };

  // Itʼs useful to be able to use some APIs as either promises or callback functions.
  // Most of the callbacks below are in the style that return their destructors,
  // instead of holding onto IDs. Note that this is not a proper `Promise`,
  // since that queues on the microtask queue! And also because it does not
  // execute until `.then()` is called! (Lazy promise? Coroutine? idk.)
  // It is really just a hack for `await` syntax, letʼs be honest.
  // (*shakes fist* why donʼt promises support cancellation.)
  var CbAndPromise = (executor, then) =>
    Object.assign(executor, {
      then: then || ((...args) => executor(...args))
    });
  var PromiseOrCb = executor =>
    CbAndPromise(executor, (cb) => executor(cb)); // no extra arguments

  // A `setTimeout` that returns its `clearTimeout` for you to call later,
  // if you need to. Or also useable as a promise. Fixes my main gripe with the
  // standard API: now we can have the timeout come first, and then the function
  // afterwards, so that the syntax is nicer and the specific number does not
  // get lost after the whole body of the function. Supports partial application
  // and use as a promise. Also passes extra arguments if provided.
  Ve.TimeoutMs = (timeout, cb, ...args) => {
    if (!cb) return PromiseOrCb(
      (resolve, ...args) => Ve.TimeoutMs(timeout, resolve, ...args),
    );
    let id = setTimeout(() => {
      const r = cb(...args);
      cb = undefined; args = undefined;
      return r;
    }, timeout);
    // Try to avoid some space leaks. Ugh.
    return () => { clearTimeout(id); cb = undefined; args = undefined; };
  };

  // `Promise.resolve` (and any promise that resolves “immediately”) is the
  // only way to access the microtask queue. Fun fact: I didnʼt know that it
  // existed until ~last year! I thought callbacks were called immediately/
  // synchronously, like you would do if you were implementing Promises naïvely.
  Ve.Microtask = PromiseOrCb((cb, ...args) => {
    Promise.resolve().then(() => { if (cb) return cb(...args) });
    return () => { cb = undefined; args = undefined; };
  });

  // Good olʼ `requestAnimationFrame`/`cancelAnimationFrame`.
  Ve.RAF = Ve.AnimationFrame = PromiseOrCb((cb, ...args) => {
    let id = requestAnimationFrame(() => {
      const r = cb(...args);
      cb = undefined; args = undefined;
      return r;
    });
    // Try to avoid some space leaks. Ugh.
    return () => { cancelAnimationFrame(id); cb = undefined; args = undefined; };
  });

  // `requestIdleCallback`/`cancelIdleCallback`. I also didnʼt know this API
  // existed until it showed up in autocomplete.
  Ve.Idle = PromiseOrCb((cb, ...args) => {
    let id = requestIdleCallback(() => {
      const r = cb(...args);
      cb = undefined; args = undefined;
      return r;
    });
    // Try to avoid some space leaks. Ugh.
    return () => { cancelIdleCallback(id); cb = undefined; args = undefined; };
  });

  // `DOMContentLoaded` unless `document.readyState === 'complete'` already
  Ve.ContentLoad = PromiseOrCb((cb, ...args) => {
    // https://developer.mozilla.org/en-US/docs/Web/API/Document/readyState
    // 'loading', 'interactive', 'complete'
    //
    // interactive
    //   The document has finished loading and the document has been parsed but
    //   sub-resources such as scripts, images, stylesheets and frames are still
    //   loading. The state indicates that the DOMContentLoaded event is about
    //   to fire.
    if (window.document.readyState === 'complete') {
      return Ve.immediately(cb, undefined, ...args);
    } else {
      // https://developer.mozilla.org/en-US/docs/Web/API/Document/DOMContentLoaded_event
      // The DOMContentLoaded event fires when the HTML document has been
      // completely parsed, and all deferred scripts (<script defer src="…"> and
      // <script type="module">) have downloaded and executed. It doesnʼt wait
      // for other things like images, subframes, and async scripts to finish
      // loading.
      //
      // DOMContentLoaded does not wait for stylesheets to load, however
      // deferred scripts do wait for stylesheets, and the DOMContentLoaded
      // event is queued after deferred scripts. Also, scripts which arenʼt
      // deferred or async (e.g. <script>) will wait for already-parsed
      // stylesheets to load.
      //
      // A different event, load, should be used only to detect a fully-loaded
      // page. It is a common mistake to use load where DOMContentLoaded would
      // be more appropriate.
      return Ve.once.DOMContentLoaded(window, (event) => cb(event, ...args));
    }
  });
  // `window.onload` unless `document.readyState === 'complete'` already
  Ve.FullLoad = PromiseOrCb((cb, ...args) => {
    if (window.document.readyState === 'complete') {
      return Ve.immediately(cb, undefined);
    } else {
      return Ve.once.load(window, (event) => cb(event, ...args));
    }
  });

  Ve.lifecycle = {
    close: function (tgt) {
      if (tgt instanceof WebSocket) {
        if (tgt.readyState === tgt.CLOSED) return;
        return new Promise((resolve, reject) => {
          Ve.once.close(tgt, resolve);
          Ve.once.error(tgt, reject);
        });
      }
      if (tgt instanceof RTCDataChannel) {
        if (tgt.readyState === "closed") return;
        return new Promise((resolve, reject) => {
          Ve.once.close(tgt, resolve);
          Ve.once.error(tgt, reject);
        });
      }
      throw new TypeError("Unknown lifecycle object");
    },
  };

  //////////////////////////////////////////////////////////////////////////////

  Ve.RAFBuffer = function() {
    let buffer = [];
    let flushing = false;
    function flush() {
      flushing = true;
      let buffered = buffer;
      while (buffered.length) {
        console.log("Buffered", buffered.length);
        buffer = [];
        for (let event of buffered) {
          event[0].call(event[1], ...event[2]);
        }
        buffered = buffer;
      }
      flushing = false;
    }
    return function(cb) {
      return function(...event) {
        if (!buffer.length && !flushing) requestAnimationFrame(flush);
        buffer.push([cb, this, event]);
      };
    };
  }
  Ve.AggregateRAF = function(handle) {
    let buffer = [];
    function flush() {
      const buffered = buffer;
      buffer = [];
      handle(buffered);
    }
    return function(datum) {
      if (!buffer.length) requestAnimationFrame(flush);
      buffer.push(datum);
    };
  }

  // Fucked code that does not work.
  Ve.$Buffer = Symbol("buffer");
  Ve.BufferEv = (upstream, cb) => {
    let buffer = [];
    let buffered = new Proxy(upstream, {
      get: (tgt, name, receiver) => {
        if (name === Ve.$Buffer) return buffer;
        if (name === "addEventListener") {
          return Object.assign(function addEventListener(...args) {
            if (!this[Ve.$Buffer]) {
              return this.addEventListener(...args);
            }
            const outer = this;
            const originalCb = args[1];
            function bufferEvent(...event) {
              console.log("Capture", event);
              if (cb && !outer[Ve.$Buffer].length) {
                cb(() => { console.debug("FLUSH"); buffered.flush(); });
              }
              outer[Ve.$Buffer].push([originalCb, this, ...event]);
            }
            return this.addEventListener.original.call(this.addEventListener.upstream, args[0], bufferEvent, ...args.slice(2));
          }, { upstream, original: tgt.addEventListener });
        }
        if (name === "flush") {
          return function flush() {
            const toRun = buffer;
            buffer = [];
            for (let event of toRun) {
              console.log(event);
              event[0].call(event[1], event.slice(2));
            }
          };
        }
        return Reflect.get(tgt, name);
      }
    });
    return buffered;
  };
  Ve.BufferEvRAF = (upstream) => Ve.BufferEv(upstream, requestAnimationFrame);

  //////////////////////////////////////////////////////////////////////////////
  // Mutation observer stuff (TODO)                                           //
  //////////////////////////////////////////////////////////////////////////////

  // https://stackoverflow.com/questions/31659567/performance-of-mutationobserver-to-detect-nodes-in-entire-dom#39332340

  //////////////////////////////////////////////////////////////////////////////

  // Intended for embedding files with some interpolations. This is my custom
  // algorithm for dedenting a template literal. It takes the raw string,
  // without escape codes, and it computes and strips leading indentation from
  // each line (if it is not whitespace). Finally it
  //
  // Notes on behavior:
  // - Takes raw string values
  // - Ignores the interpolations for the sake of indentation
  // - Computes and removes common indentation from non-blank lines
  // - Lines that consist solely of whitespace are ignored for the purposes of
  //   indentation:
  //   - If all lines lack content, it returns '\n'*(number of newlines - 1)
  //     (that is: it should return two newlines when your editor shows
  //     two blank lines)
  //   - Otherwise, it removes up to one newline from the start and end, so that
  //     the backtick delimiters can be on their own lines. Again: a fully
  //     blank line in the editor becomes a '\n' in the output.
  //
  // This should allow you to represent almost any string you want in a
  // reasonable and nicely indented manner, with the exception of strings that
  // contain trailing whitespace on lines (shudder), and strings with '\r' or
  // '\f' (it makes very little effort to preserve newlines other than '\n').
  Ve.dedent = (strings, ...values) => {
    return Ve.dedent.intercalate(strings, ...values).join('');
  };
  // This returns the alternating [string, value, string, value, ..., string]
  // array (where the values are not coerced to strings!), as opposed to the
  // concatenated string.
  Ve.dedent.intercalate = (strings, ...values) => {
    if ('raw' in strings) strings = strings['raw'];
    if (typeof strings === 'string' || strings instanceof String) strings = [strings];
    if (!strings.length) return '';
    const lines = strings.flatMap((string, i) => {
      const these_lines = string.split('\n').slice(1);
      return these_lines.map((line, j) => {
        return {line, follows: i !== strings.length-1 && j === these_lines.length-1};
      });
    });

    // The algorithm doesnʼt work for single-line literals
    if (!lines.length) return Ve.intercalate(strings, ...values);

    // Compute the common prefix
    let commonPrefix = null;
    for (const {line, follows} of lines) {
      const prefix = line.match(/^[^\S\r\n\f]*/u)[0];
      if (!follows && prefix === line) continue; // no content
      if (!commonPrefix || commonPrefix.startsWith(prefix)) {
        commonPrefix = prefix;
      }
      if (!commonPrefix) break;
    }
    // Remove the common prefix from the strings, or if they are all blank,
    // zero them out so that it returns only newlines.
    const replaced =
      commonPrefix === null ? strings.map(s => s.split('\n').map(_=>'').join('\n')) :
      strings.map(string => string.replaceAll('\n'+commonPrefix, '\n'));
    // If there is no content on the first line, remove it
    if (!replaced[0].split("\n")[0].trim())
      // https://forum.keyboardmaestro.com/t/regex-for-horizontal-whitespace-s-h-t-blank-etc/8287/12
      replaced[0] = replaced[0].replace(/^[^\S\r\n\f]*\n/, '');
    const last = replaced.length-1;
    // Trim the last line if it has no content but there is content in the string
    const last_lines = replaced[last].split('\n');
    if (commonPrefix !== null || values.length) {
      if (!last_lines[last_lines.length - 1].trim()) {
        replaced[last] = replaced[last].replace(/\n[^\S\r\n\f]*$/, '');
      }
    }
    return intercalate.raw({ raw: replaced }, ...values);
  };

  Ve.scripty = (source, ...values) =>
    Ve.dedent(source, ...values.map(v => `(${JSON.stringify(v)})`));

  // Some helpers for escaping strings into various languages
  Ve.escape = {
    HTML: s => s
      .replaceAll('&', '&amp;')
      .replaceAll('<', '&lt;')
      .replaceAll('>', '&gt;')
      .replaceAll('"', '&quot;')
      .replaceAll("'", '&#039;'),
    CSS: s => CSS.escape(s),
    JS: s => JSON.stringify(s),
    JSON: s => JSON.stringify(s),
    // https://stackoverflow.com/questions/3561493/is-there-a-regexp-escape-function-in-javascript#30851002
    // see also: https://github.com/tc39/proposal-regex-escaping?tab=readme-ov-file#polyfills
    RegExp: Object.assign(
      s => s.replace(/[-\[\]{}()*+!<=:?.\/\\^$|#\s,]/g, '\\$&'),
      // Might add more specialized versions later
      {
      }
    ),
  };

  //////////////////////////////////////////////////////////////////////////////
  // Verbal Expressions! A classic way of constructing and composing regexes  //
  // using combinators, now brought to you by the magic of template literals. //
  //////////////////////////////////////////////////////////////////////////////

  // Template literals, where the interpolated regexes are sequenced (and
  // interpolated strings are converted to literal regexes, interpolated arrays
  // are considered alternative options).
  //
  // Might add some syntax, at least for assignments, to it, but not right now,
  // so currently the literal strings in between the interpolations are just
  // ignored. In any case, it will give good space for comments and whitespace.
  Ve.VerbExp = (_, ...values) => Ve.VerbExp.scoped(({ defined }) => {
    let flags = "";
    let source = "";
    for (let value of values) {
      if (!value) continue;
      let limit = 10;
      while (limit-- && typeof value === 'function')
        value = value();

      // Interpolated arrays give alternatives
      if (value instanceof Array) {
        value = Ve.VerbExp.alt(...value.map(Ve.VerbExp.coe));
      }

      if (value instanceof Error) throw value;

      if (typeof value === 'string' || value instanceof String) {
        // Interpolated strings are escaped, so they are literal values
        source += Ve.escape.RegExp(String(value));
      } else if (value instanceof RegExp) {
        // Regexes are sequenced (concatenated after wrapping in a non-capturing
        // group as necessary)
        if (value.source === Ve.VerbExp.always.source) continue;
        source += Ve.VerbExp.wrap.seq(value).source;
        flags += value.flags;
      }
    }
    // … there is nothing really good to do with flags other than to take
    // their union, heh
    flags = Ve.VerbExp.flags(flags);
    return Object.assign(new RegExp(source, flags), {defs: defined, [Ve.VerbExp.needsWrap.$]: 2});
  });
  Ve.VerbExp.raw = (...args) => String.raw(...args);


  // Save and restore `Ve.VerbExp.defined` while running `fn`
  Ve.VerbExp.scoped = fn => {
    const previous = Ve.VerbExp.defined;
    const defined = Ve.VerbExp.defined = Object.create(previous);
    const ret = fn({ VX: Ve.VerbExp, ...Ve.VerbExp, defined });
    for (const k of Object.keys(defined).reverse()) {
      if (!Object.hasOwn(defined, k)) continue;
      const v = defined[k];
      if (typeof v === 'function') {
        defined[k] = () => { throw new Error("Recursive definition of " + v) };
        defined[k] = v();
      }
    }
    Ve.VerbExp.defined = previous;
    return ret;
  };

  // just in case anyone would typo it (dunno who that would be ...)
  Ve.VerbExpr = Ve.VerbExp;

  // (reverse of normal precedence)
  // 0: atom
  // 1: postfix repetition operators
  // 2: sequence
  // 3: alternatives
  Ve.VerbExp.prec = Object.assign(['atom', 'postfix', 'seq', 'alt']);

  Ve.VerbExp.lit = lit => new RegExp(Ve.escape.RegExp(String(lit)));
  Ve.VerbExp.coe = re => {
    let limit = 10;
    while (limit-- && typeof re === 'function')
      re = re();
    if (typeof re === 'string' || re instanceof String)
      re = Ve.VerbExp.lit(re);
    if (!(re instanceof RegExp)) {
      if (re instanceof Error) throw re;
      throw new Error("Expected RegExp, got " + re);
    }
    return re;
  };
  Ve.VerbExp.src = re => Ve.VerbExp.coe(re).source;
  Ve.VerbExp.flags = flags => Array.from(new Set(flags)).join('');

  // A space for definitions
  Ve.VerbExp.defined = {};
  Ve.VerbExp.defs = new Proxy(Ve.VerbExp.defined, {
    get(_, name) {
      if (name in Ve.VerbExp.defined) return Ve.VerbExp.defined[name];
      return Object.assign(
        () => Ve.VerbExp.defined[name] ?? new Error("Ve.VerbExp.defs does not include " + name),
        { pending: name },
      );
    },
    set(_, name, value) {
      Ve.VerbExp.defined[name] = value;
      return true;
    },
  });

  // Helpers for the combinators
  Ve.VerbExp.withDef = f => sugarArg(named => {
    if (named) {
      const re = Ve.VerbExp.defs[named];
      if (!re) throw new Error("Ve.VerbExp.defs does not include " + name);
      return typeof re === 'function' ? (...args) => f(re(...args)) : f(re);
    } else return (...args) => {
      if (args.length && args[0] instanceof Array && 'raw' in args[0])
        args = [Ve.VerbExp(...args)];
      if (args.length && args.every(arg => typeof arg === 'function'))
        return (...argz) => f(Ve.VerbExp.alt(...args.map(f => f(...argz))));
      return f(Ve.VerbExp.alt(...args));
    }
  });
  Ve.VerbExp.withWrapped = sugarArg(expected => f => Ve.VerbExp.withDef(re => {
    return f(Ve.VerbExp.needsWrap(re, expected) ? `(?:${re.source})` : re.source);
  }));
  Ve.VerbExp.overSource = f => Ve.VerbExp.withDef(re => {
    const r = f(re.source);
    if (r !== re.source) return new RegExp(f(re.source), re.flags);
    return re;
  });
  Ve.VerbExp.overWrapped = sugarArg(expected => f => Ve.VerbExp.withDef(re => {
    const r = f(Ve.VerbExp.needsWrap(re, expected) ? `(?:${re.source})` : re.source);
    if (r !== re.source) return new RegExp(r, re.flags);
    return re;
  }));

  // Combinators
  Ve.VerbExp.always = /(?:)/; // always match
  Ve.VerbExp.never = /(?!)/; // never match
  // Turn a regex into a capturing group
  Ve.VerbExp.capture = Ve.VerbExp.withDef(re =>
    Ve.VerbExp.known(new RegExp(`(${re.source})`, re.flags), 0)
  );
  // (Unsafe) Never wrap this regex in parens (it is an atom)
  Ve.VerbExp.nowrap = Ve.VerbExp.withDef(re =>
    Ve.VerbExp.known(new RegExp(re.source, re.flags), 0)
  );
  // (Unsafe) Specify the precedence of the regex source
  Ve.VerbExp.known = (re, prec) => Object.assign(re, {[Ve.VerbExp.needsWrap.$]: prec});
  // Wrap a regex in a non-capturing group if necessary
  Ve.VerbExp.wrap = sugarArg(expected => Ve.VerbExp.overWrapped[expected](source => source));
  // Match zero or more times (`*`)
  Ve.VerbExp.star = Ve.VerbExp.overWrapped.postfix(source => source+'*');
  // Match one or more times (`+`)
  Ve.VerbExp.plus = Ve.VerbExp.overWrapped.postfix(source => source+'+');
  // Match zero or one time (`?`)
  Ve.VerbExp.opt = Ve.VerbExp.overWrapped.postfix(source => source+'?');
  // Repeat `n,m` times. `repeat[[1,25]]` or `repeat(1,25)` or `repeat[3]`
  Ve.VerbExp.repeat = sugarStr((...bounds) => Ve.VerbExp.overWrapped.postfix(source => {
    if (bounds === '+' || bounds === '*') return source+bounds;
    if (bounds === '0' || bounds === 0) return '(?:)';
    if (bounds === '1' || bounds === 1) return source;
    bounds = String(bounds);
    return source+'{'+bounds+'}';
  }));
  // A list of alternative regexes to match
  Ve.VerbExp.alt = (...items) => {
    const regexes = items.map(Ve.VerbExp.coe);
    const flags = Ve.VerbExp.flags(items.map(_.flags));
    const filtered = regexes.filter(({source}) => source !== Ve.VerbExp.never.source);
    if (!filtered.length) return Ve.VerbExp.never;
    if (filtered.length === 1) return items[0];
    const re = new RegExp(filtered.map(_.source).join('|'), flags);
    re[Ve.VerbExp.needsWrap.$] = 3;
    return re;
  };
  // A list of regexes to match in sequence
  Ve.VerbExp.seq = (...items) => {
    const regexes = items.map(Ve.VerbExp.coe);
    const flags = Ve.VerbExp.flags(items.map(_.flags));
    const filtered = regexes.filter(({source}) => source !== Ve.VerbExp.always.source);
    if (!filtered.length) return Ve.VerbExp.always;
    if (filtered.length === 1) return items[0];
    // TODO: escape boundaries of like escape sequences and such
    const re = new RegExp(filtered.map(Ve.VerbExp.wrap.seq).map(_.source).join(''), flags);
    re[Ve.VerbExp.needsWrap.$] = 2;
    return re;
  };

  // Guess what the precedence of `re.source` is (an over-approximation),
  // so that we know if we need to wrap it in a non-capturing group
  Ve.VerbExp.needsWrap = (re, expected) => {
    let NEEDSWRAP = Ve.VerbExp.needsWrap.$;
    let level = 0;
    let charcls = false;
    let i = 0;
    let escape = false;
    let ret = 0;
    if (re instanceof RegExp && NEEDSWRAP in re) {
      ret = re[NEEDSWRAP];
    } else {
      const str = re instanceof RegExp ? re.source : String(re);
      if (!str.match(/^[\w\s]+$/) && !str.match(/^\[[^\]]+\]$/)) {
        if (str.length > 1 && (str.length > 2 || source[0] !== '\\'))
          ret = 2;
      }
      const source = Array.from(str);
      const handlers = {
        ['\\']() { escape = true },
        ['[']() { charcls = true },
        [']']() { charcls = false },
        ['(']() { if (!charcls) level += 1 },
        [')']() { if (!charcls) level -= 1 },
        ['|']() { if (!charcls && !level) ret = 3 },
      };
      for (let char of source) {
        if (!escape) {
          handlers[char]?.();
          if (ret === 3) break;
        } else escape = false;
        i += 1;
      }
      if (re instanceof RegExp) re[NEEDSWRAP] = ret;
    }
    if (expected === 'undefined') expected=undefined;
    if (expected) {
      if (expected in Ve.VerbExp.prec)
        expected = Ve.VerbExp.prec[expected];
      if (Ve.VerbExp.prec.includes(expected))
        expected = Ve.VerbExp.prec.indexOf(expected);
      if (typeof expected !== 'number')
        throw new Error('Unknown precedence ' + expected);
      return expected < ret;
    }
    return ret;
  };
  Ve.VerbExp.needsWrap.$ = Symbol("needsWrap");

  //////////////////////////////////////////////////////////////////////////////

  // Named capturing groups are still recent (2024)...
  Ve.dataURI = Ve.VerbExp.scoped(({ defs, VX, defined: _defined }) => {
    // https://datatracker.ietf.org/doc/html/rfc6838#section-4.2
    ` type-name = restricted-name
      subtype-name = restricted-name
    `
    defs.type_name = VX.capture.restricted_name; // automatic forward reference
    defs.subtype_name = VX.capture.restricted_name;
    ` restricted-name = restricted-name-first *126restricted-name-chars
      restricted-name-first  = ALPHA / DIGIT
      restricted-name-chars  = ALPHA / DIGIT / "!" / "#" /
                              "$" / "&" / "-" / "^" / "_"
      restricted-name-chars =/ "." ; Characters before first dot always
                                  ; specify a facet name
      restricted-name-chars =/ "+" ; Characters after last plus always
                                  ; specify a structured syntax suffix
    `
    defs.restricted_name_first = /[a-zA-Z0-9]/;
    defs.restricted_name_chars = /[-a-zA-Z0-9!#$&^_+.]/;
    defs.restricted_name = VX`
      // total length: 1-127 characters
      ${defs.restricted_name_first}
      ${VX.repeat[[0,126]].restricted_name_chars}
    `;

    // https://datatracker.ietf.org/doc/html/rfc6838#section-4.3
    // > It is an error for a specific parameter to be specified more than once.
    ` parameter-name = restricted-name `
    defs.parameter_name = defs.restricted_name;
    defs.attribute = defs.parameter_name; // different standards, different names

    // https://datatracker.ietf.org/doc/html/rfc2045#section-5.1
    ` content := "Content-Type" ":" type "/" subtype
            *(";" parameter)
            ; Matching of media type and subtype
            ; is ALWAYS case-insensitive.
    `
    defs.parameters = () => VX.star`${";"}${defs.parameter}`; // the template literals
    // defs.mime = VX.capture`${defs.type}${"/"}${defs.subtype}`;
    ` parameter := attribute "=" value `
    defs.parameter = () => VX`${defs.attribute}${"="}${defs.value}`;
    ` value := token / quoted-string `
    defs.value = () => VX.alt(defs.token, defs.quoted_string);
    ` token := 1*<any (US-ASCII) CHAR except SPACE, CTLs,
                  or tspecials>
      tspecials :=  "(" / ")" / "<" / ">" / "@" /
                    "," / ";" / ":" / "\" / <">
                    "/" / "[" / "]" / "?" / "="
                    ; Must be in quoted-string,
                    ; to use within parameter values
    `
    defs.token = /[-a-zA-Z0-9!#$&^_+.]+/; // '%| ???
    // https://www.rfc-editor.org/rfc/rfc7230#section-3.2.6
    ` quoted-string := ${/"(?:[\t \x21\x23-\x5B\x5D-\x7E\x80-\xFF]|\\[\t \x21-\x7E\x80-\xFF])*"/}
      quoted-string  = DQUOTE *( qdtext / quoted-pair ) DQUOTE
      qdtext         = HTAB / SP /%x21 / %x23-5B / %x5D-7E / obs-text
      obs-text       = %x80-FF
      quoted-pair    = "\" ( HTAB / SP / VCHAR / obs-text )
    `
    defs.qdtext = /[\t \x21\x23-\x5B\x5D-\x7E\x80-\xFF]/;
    defs.quoted_pair = /\\[\t \x21-\x7E\x80-\xFF]/;
    defs.quoted_string = VX`${'"'} ${VX.star(defs.qdtext, defs.quoted_pair)} ${'"'}`;

    defs.dataURI = VX`
      ${/^data:/}
      ${VX.capture`${defs.type_name()} ${"/"} ${defs.subtype_name()}`}
      ${VX.capture.parameters}
      ${VX.opt(VX.capture(/;base64/))}
      ${","}
    `;

    const reference = /^data:(([a-zA-Z0-9][-a-zA-Z0-9!#$&^_+.]{0,126})\/([a-zA-Z0-9][-a-zA-Z0-9!#$&^_+.]{0,126}))((?:;[a-zA-Z0-9][-a-zA-Z0-9!#$&^_+.]{0,126}\=(?:[-a-zA-Z0-9!#$&^_+.]+|"(?:[\t \x21\x23-\x5B\x5D-\x7E\x80-\xFF]|\\[\t \x21-\x7E\x80-\xFF])*"))*)(;base64)?\,/;
    if (defs.dataURI.source != reference.source) {
      console.error('Discrepancy:', defs.dataURI, reference);
    }

    return Object.assign(defs.dataURI, { defs: VX.defined });
  });

  Ve.fromDataURI = function fromDataURI(uri) {
    let match = Ve.dataURI.exec(uri);
    if (!match) throw new Error("Invalid data URI", uri);
    let parameters = match[4].substring(1).split(";");
    parameters = parameters.filter(Boolean).map(parameter => {
      let [attribute, value] = parameter.split("=", 2);
      attribute = attribute.toLowerCase();
      if (value.startsWith('"') && value.endsWith('"')) {
        value = value.substring(1, value.length-1)
          .replaceAll(/\\([\t \x21-\x7E\x80-\xFF])/g, '$1');
      }
      return [attribute, value];
    });
    parameters = Object.fromEntries(parameters);
    const base64 = !!match[5];
    const encoded = uri.substring(match[0].length);
    let data = undefined;
    let text = undefined;
    const extra = Object.assign({}, [
      match[2] === 'text' && { charset: parameters['charset'] || 'UTF-8' },
    ].filter(o=>typeof o === 'object'));
    return {
      ...extra,
      mime: match[1].toLowerCase(),
      type: match[2].toLowerCase(),
      subtype: match[3].toLowerCase(),
      parameters: parameters,
      base64,
      encoded,
      get data() {
        if (data === undefined) {
          data = base64
            ? fromB64.bytes(encoded)
            // hmm, binary URI encoded??
            : (new TextEncoder()).encode(decodeURI(encoded));
        }
        return data;
      },
      get text() {
        if (text === undefined) {
          text =
            base64
              ? data !== undefined
                ? (new TextDecoder(extra.charset)).decode(data)
                : fromB64.text(encoded, extra.charset)
              : decodeURI(encoded);
        }
        return text;
      },
    }
  };

  //////////////////////////////////////////////////////////////////////////////

  Ve.Interface = function(init=undefined) {
    const object = init || {};
    const prev = Object.assign({}, object);
    let shared;

    const interfaces = [];

    const mkHandler = handler => {
      if (typeof handler === 'function') return handler;
      return (type, ...arg) => {
        return handler[type]?.(...arg);
      };
    };
    const handle = (idx, type, ...arg) => {
      interfaces.forEach((handler, i) => {
        if (i !== idx) handler(type, ...arg);
      });
    };

    return (handler=undefined, loopback=undefined) => {
      if (loopback) {
        interfaces.push(mkHandler(loopback));
      }
      let idx;
      if (handler) {
        idx = interfaces.length;
        interfaces.push(mkHandler(handler));
      }
      if (!handler && !loopback) {
        if (shared) return shared;
      }
      const iface = new Proxy(init, {
        set: (_target, property, newValue, _thisArg) => {
          const oldValue = property in prev ? [prev[property]] : [];
          object[property] = newValue;
          prev[property] = newValue;
          handle(idx, property, newValue, ...oldValue);
          return true;
        },
      });
      if (!handler && !loopback) {
        shared = iface;
      }
      return iface;
    };
  };

  //////////////////////////////////////////////////////////////////////////////

  return this;
}).call(Verity);
