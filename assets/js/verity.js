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
      parent.clearChildren();
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
    classList: (e, classes) => {
      e.classList = '';
      if (classes) {
        if (typeof classes === 'string' || classes instanceof String) {
          e.classList = String(classes);
        } else {
          e.classList.add(...classes);
        }
      }
    },
    // $view: (e, view) =>
    //   view && (e.$view = view),
    // $preview: (e, preview) =>
    //   preview && (e.$preview = preview),
    // $model: (e, model) =>
    //   e.MVC(model, e.$view),
  };
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
    if (typeof props === 'string' || typeof props === 'number' || props instanceof String || props instanceof Number) {
      return document.createTextNode(String(props));
    }
    if (Array.isArray(props)) {
      return SPECIALS['$children'](document.createDocumentFragment(), props);
    }
    if (props instanceof Node) return props;
    props = props ? Object.assign({}, props) : {};
    if (children?.length) {
      props['$children'] = [props?.['$children'], children];
    }
    tag = tag || props['$tag'];
    if (!tag) tag = 'div';
    delete props['$tag'];
    if (tag instanceof Element) tag = tag.tagName;
    let e = parent?.namespaceURI ? document.createElementNS(parent?.namespaceURI || undefined, tag) : document.createElement(tag);
    return applyProps(e, props);
  };

  Object.assign(Ve, {
    applyProps, createChildOf,
  });

  Ve.ById = sugar(id => document.getElementById(id));
  Ve.HTML = sugarArg(tag => (props = {}, ...children) => createChildOf(undefined, props, tag, ...children));
  Ve.SVGNS = 'http://www.w3.org/2000/svg';
  Ve.SVG = sugarArg(ty => (props = {}, ...children) => applyProps(document.createElementNS(Ve.SVGNS, ty), props));

  Ve.ico = sugarStr(classList => Ve.HTML.i({classList}));
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

  Ve.on = sugar(ty => (tgt, handler, options) => {
    if (!handler && !options) {
      return new Promise(handler => Ve.on[ty](tgt, handler, {once:true}));
    }
    if (typeof options === 'function' && typeof handler !== 'function') {
      [handler, options] = [options, handler];
    };
    tgt.addEventListener(ty, handler, options);
    return () => {
      tgt.removeEventListener(ty, handler, options);
    };
  });
  Ve.once = sugar(ty => (tgt, handler, options) => {
    if (!handler && !options) {
      return new Promise(handler => Ve.on[ty](tgt, handler, {once:true}));
    }
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

  Object.defineProperty(Element.prototype, "attrs", {
    get: function () {
      return new Proxy(this, {
        get: (t, p) => t.getAttribute(p),
        set: (t, p, v, x) => { t.setAttribute(p, v); return true; },
      });
    },
    set: function (newAttrs) {
      this.clearAttributes();
      Object.assign(this.attrs, newAttrs);
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

  Ve.RAFBuffer = function() {
    let buffer = [];
    function flush() {
      const buffered = buffer;
      console.log("Buffered", buffered.length);
      buffer = [];
      for (let event of buffered) {
        event[0].call(event[1], ...event[2]);
      }
    }
    return function(cb) {
      return function(...event) {
        if (!buffer.length) requestAnimationFrame(flush);
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

  Ve.dedent = (strings, ...values) => {
    if ('raw' in strings) strings = strings['raw'];
    if (typeof strings === 'string' || strings instanceof String) strings = [strings];
    if (!strings.length) return '';
    const lines = strings.flatMap((string, i) => {
      const these_lines = string.split('\n').slice(1);
      return these_lines.map((line, j) => {
        return {line, follows: i !== strings.length-1 && j === these_lines.length-1};
      });
    });
    // console.log({ lines });
    let commonPrefix = null;
    for (const {line, follows} of lines) {
      const prefix = line.match(/^[^\S\r\n\f]*/u)[0];
      if (!follows && prefix === line) continue; // no content
      if (!commonPrefix || commonPrefix.startsWith(prefix)) {
        // console.log({ commonPrefix, prefix });
        commonPrefix = prefix;
      }
      if (!commonPrefix) break;
    }
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
    const result = String.raw({ raw: replaced }, ...values);
    // console.log({ replaced, result });
    return result;
  };

  Ve.escape = {
    HTML: s => s
      .replaceAll('&', '&amp;')
      .replaceAll('<', '&lt;')
      .replaceAll('>', '&gt;')
      .replaceAll('"', '&quot;')
      .replaceAll("'", '&#039;'),
  };

  //////////////////////////////////////////////////////////////////////////////

  return this;
}).call(Ve);
