// https://stackoverflow.com/questions/48343436/how-to-convert-svg-element-coordinates-to-screen-coordinates
function screenToSVG(screenX, screenY) {
  var p = svg.createSVGPoint();
  p.x = screenX;
  p.y = screenY;
  var p2 = p.matrixTransform(svg.getScreenCTM().inverse());
  return [p2.x, p2.y];
}

function SVGToScreen(svgX, svgY) {
  var p = svg.createSVGPoint();
  p.x = svgX;
  p.y = svgY;
  var p2 = p.matrixTransform(svg.getScreenCTM());
  return [p2.x, p2.y];
}
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
function bsplit(points, t0) {
  var n = points.length - 1; // number of control points
  var b = [];		   	   // coefficients as in De Casteljau's algorithm
  var res1 = [];		   // first curve resulting control points
  var res2 = [];		   // second curve resulting control points
  var t1 = 1 - t0;

  // multiply point with scalar factor
  var pf = function(p, f) {
    var res = [];
    for(var i = 0; i < p.length; i++) {
      res.push(f * p[i]);
    }
    return res;
  };
  // add points as vectors
  var pp = function(p1, p2) {
    var res = [];
    for(var i = 0; i < Math.min(p1.length, p2.length); i++) {
      res.push(p1[i] + p2[i]);
    }
    return res;
  };

  // set original coefficients: b[i][0] = points[i]
  for(var i = 0; i <= n; i++) {
    points[i] = (typeof points[i] == "object") ? points[i] : [points[i]];
    b.push([ points[i] ]);
  }
  // get all coefficients
  for(var j = 1; j <= n; j++) {
    for(var i = 0; i <= (n-j); i++) {
      b[i].push( pp(
        pf(b[i][j-1], t1),
        pf(b[i+1][j-1], t0)
      ));
    }
  }
  // set result: res1 & res2
  for(var j = 0; j <= n; j++) {
    res1.push(b[0][j]);
    res2.push(b[j][n-j]);
  }

  return [res1, res2];
}

function bsplitMany(points, ts) {
  var ts = ts.sort();
  var r = [points];
  var t0 = 0;
  for (let t of ts) {
    if (t <= 0 || t >= 1) continue;
    var ti = (t - t0)/(1 - t0);
    var [a, b] = bsplit(r[r.length-1], ti);
    r[r.length-1] = a;
    r.push(b);
    t0 = t;
  }
  return r;
}
////////////////////////////////////////////////////////////////////////////////




function debug(msg) {
  if (typeof msg !== "string") {
    msg = JSON.stringify(msg);
  }
  the("debug").textContent += "\n" + msg;
}

function the(id) {
  return document.getElementById(id);
}

function cmpBy(score) {
  return (a, b) => {
    var A = score(a); var B = score(b);
    if (A < B) return -1;
    if (A > B) return 1;
    return 0;
  };
}

function pythonic(fn) {
  return function(...args) {
    return fn(this, ...args);
  };
};
function proxied(symbol) {
  return function(o, ...args) {
    return o[symbol](...args);
  };
};
function methody(fn) {
  return fn;
}

function clamp(lower, value, upper) {
  return Math.min(upper, Math.max(lower, value));
};

Object.defineProperty(Node.prototype, "attrs", {
  get: function () {
    return new Proxy(this, {
      get: (t, p) => t.getAttribute(p),
      set: (t, p, v, x) => { t.setAttribute(p, v); return true; },
    });
  },
});

var svgNS = "http://www.w3.org/2000/svg";
document.createSVGElement = function(nodeName) {
  return document.createElementNS(svgNS, nodeName);
};
function SVG(nodeName, attrs) {
  var e = document.createSVGElement(nodeName);
  attrs && applyAttrs(e, attrs);
  if (this && this instanceof Element && (!attrs || !attrs.$parent)) {
    this.appendChild(e);
  }
  return e;
};
Element.prototype.SVG = methody(SVG);
function DOM(nodeName, attrs) {
  var e = document.createElement(nodeName);
  attrs && applyAttrs(e, attrs);
  if (this && this instanceof Element && (!attrs || !attrs.$parent)) {
    this.appendChild(e);
  }
  return e;
}
Element.prototype.DOM = methody(DOM);
var SPECIALS = {
  $style: (e, style) =>
    style && Object.assign(e.style, style),
  $parent: (e, parent) =>
    parent && parent !== e.parentNode && parent.appendChild(e),
  $view: (e, view) =>
    view && (e.$view = view),
  $preview: (e, preview) =>
    preview && (e.$preview = preview),
  $model: (e, model) =>
    e.MVC(model, e.$view),
  $textContent: (e, textContent) =>
    e.textContent = textContent,
};
function applyAttrs(e, attrs) {
  var specialAttrs = {};
  for (let k in attrs) {
    if (k in SPECIALS) {
      specialAttrs[k] = attrs[k];
      delete attrs[k];
    } else if (k.startsWith('$')) {
      console.log(`Warning: unknown special attribute ${k} on element`, e);
      delete attrs[k];
    }
  }
  Object.assign(e.attrs, attrs);
  for (let k in specialAttrs) {
    SPECIALS[k](e, specialAttrs[k]);
  }
  return e;
};
Node.prototype.applyAttrs = pythonic(applyAttrs);

Node.prototype.listen = methody(function listen(eventNames, callback) {
  if (!Array.isArray(eventNames)) return this.listen([eventNames], callback);
  for (const eventName of eventNames) this.addEventListener(eventName, callback);
  return () => { for (const eventName of eventNames) this.removeEventListener(eventName, callback) };
});

Element.prototype.MVC = function MVC(model, view, preview) {
  if (!(Model.$update in this)) {
    let previewing = false;
    this[Model.$update] = () => {
      if (previewing) return;
      if (this.$preview) {
        previewing = true;
        try {
          Model.transaction($proxy, () => {
            this.$preview(this.$model);
          });
        } finally {
          previewing = false;
        }
      }
      this.$view && applyAttrs(this, this.$view(this.$model));
    };
    const $proxy = Model.listening(Model({
      model, view, preview,
    }), this);
    this.$proxy = $proxy;
    for (const k in $proxy) {
      Object.defineProperty(this, `$`+k, {
        get: () => $proxy[k],
        set: (v) => ($proxy[k] = v, true),
      });
    }
  } else {
    Model.assign(this.$proxy, {
      model, view,
    });
  }
  return this;
};

// A model is an object with data. You can update the data and notify its
// listeners. A listener is just like the model, but it can unlisten.
function Model(initialData) {
  let data = initialData;
  if (typeof data !== 'object') {
    data = {};
  }
  for (let k in data) {
    if (Model.is(data[k])) {
      data[k] = Model.listening(data[k], data);
    } else if (Model.isPlainObject(data[k])) {
      data[k] = Model.listening(Model(data[k]), data);
    }
  }
  let listeners = [];
  let locked = null;
  let model = new Proxy(data, Model.Proxy);
  Object.assign(data, {
    [Model.$data]: data,
    [Model.$listening]: (listener) => {
      const i = listeners.length;
      listeners.push(listener);
      if (listeners[i] !== listener) throw new Error("uhhh");
      const unlisten = () => {
        delete listeners[i];
        return true;
      };
      return new Proxy(model, {
        get: (t, p) => {
          if (p === Model.$unlisten) {
            return unlisten;
          }
          if (p === Model.$listener) {
            return listener;
          }
          return t[p];
        },
        set: Reflect.set,
      });
    },
    [Model.$unlisten]: Model.notListening,
    [Model.$transaction]: (action, ...args) => {
      if (locked !== null) return action(...args);
      locked = false;
      let ret;
      try {
        ret = action(...args);
        if (locked === true) {
          locked = null;
          data[Model.$update]();
        }
      } finally {
        locked = null;
      }
      return ret;
    },
    [Model.$update]: () => {
      if (locked !== null) {
        locked = true;
        return false;
      }
      locked = true;
      try {
        for (const i in listeners) {
          const listener = listeners[i];
          const r = Model.notify(listener, model);
          if (r === Model.$unsub) {
            delete listeners[i];
          }
        }
      } finally {
        locked = null;
      }
      return true;
    },
  });
  return model;
}
Model.$data = Symbol("Model.$data");
Model.$unsub = Symbol("Model.$unsub");
Model.$listener = Symbol("Model.$listener");
Model.$methods = [
  `listening`,
  `unlisten`,
  `transaction`,
  `update`,
];
Model.is = function(o) {
  if (o === null) return false;
  if (typeof o !== 'object') return false;
  return Model.$methods.every(method => Model[`$`+method] in o);
};
Model.isPlainObject = function(o) {
  return o !== null && typeof o === 'object' && (Object.getPrototypeOf(o) === Object.prototype || Array.isArray(o)) && !Model.is(o);
};
Model.notListening = function notListening() {};
Model.Proxy = {
  set: (t, p, v) => {
    if (!(p instanceof Symbol)) {
      t[p] = Model.swap(t, t[p], v);
      t[Model.$update]();
    } else {
      console.log("ASDF");
      t[p] = v;
    }
    return true;
  },
};
Model.$methods.forEach(method => {
  Model[method] = proxied(Model[`$`+method] = Symbol(`Model.$`+method));
});
Model.notify = function notify(listener, ...args) {
  if (Model.$update in listener) {
    return listener[Model.$update](...args);
  }
  if (listener instanceof Function) {
    return listener(...args);
  }
};
Model.swap = function swap(listener, oldModel, newModel) {
  if (oldModel === newModel) {
    if (Model.is(oldModel)) {
      if (listener !== oldModel[Model.$listener]) {
        Model.unlisten(oldModel);
        return Model.listening(newModel, listener);
      }
    }
    return newModel;
  }

  if (Model.is(oldModel) && Model.isPlainObject(newModel)) {
    return Object.assign(oldModel, newModel);
  }

  if (Model.is(oldModel)) {
    Model.unlisten(oldModel);
  }

  if (Model.is(newModel)) {
    return Model.listening(newModel, listener);
  }
  if (Model.isPlainObject(newModel)) {
    return Model.listening(Model(newModel), listener);
  }
  return newModel;
};
Model.assign = function assign(model, values) {
  return Model.transaction(model, () => {
    Object.assign(model, values);
  });
};

Model.map = function map(model, mappers) {
  function mapped() {
    const r = {};
    for (const k in mappers) {
      r[k] = mappers[k](model);
    }
    return r;
  }
  const newModel = Model(mapped());
  Model.listening(model, () => {
    Model.assign(newModel, mapped());
  });
  return newModel;
};



var globalMouse = Model({
  pageX: 0, pageY: 0,
});
document.listen("mousemove", e => {
  Model.transaction(globalMouse, () => {
    for (const k in globalMouse) {
      globalMouse[k] = e[k];
    }
  });
});
document.listen(["touchstart", "touchmove"], e => {
  if (e.touches.length) {
    globalMouse.pageX = e.touches[0].pageX;
    globalMouse.pageY = e.touches[0].pageY;
  }
});

function Bubble(e) {
  if (!e) return e;
  if (!e.parentNode) return e;
  if (e === e.parentNode.lastChild) return e;
  e.parentNode.appendChild(e);
  return e;
}

function Handling(element) {
  const model = Model({ active: null });
  element.listen(["mousedown", "touchstart"], event => {
    if (!model.active) model.active = Mouse(event);
    event.stopPropagation();
    event.preventDefault();
  });
  document.listen(["mouseup", "touchend"], () => {
    if (model.active) model.active = null;
    yes = undefined;
  });
  return model;
}
function Mouse(event) {
  if (event.type === 'touchstart' && event.changedTouches.length === 1) {
    let touch = event.changedTouches[0];
    const model = Model({ pageX: touch.pageX, pageY: touch.pageY });
    // FIXME: don't leak
    document.listen("touchmove", e => {
      if (e.touches.length) {
        var touches = Array.from(e.touches);
        touches.sort(cmpBy(touch => Math.hypot(touch.pageX - model.pageX, touch.pageY - model.pageY)));
        model.pageX = touches[0].pageX;
        model.pageY = touches[0].pageY;
      }
    });
    return model;
  } else {
    return globalMouse;
  }
}
function Hovering(e) {
  const model = Model({ hover: false });
  e.listen("mouseover", () => {
    if (!model.hover) model.hover = true;
  });
  e.listen("mouseout", () => {
    if (model.hover) model.hover = false;
  });
  return model;
}




var svg = SVG("svg", {
  viewBox: "0 0 1 1",
  $style: {
    width: "100%",
    height: "calc(80svh - 2em)",
    display: "block",
    strokeLinecap: "round",
  },
});
var defs = svg.SVG("defs");

var answer = [[0.1, 0.1], [0.1, 0.9], [0.9, 0.1], [0.9, 0.9]].map(Model);
var guess = [answer[0], Model([0.1, 0.9]), Model([0.9, 0.1]), answer[3]];
var maxDist = 2*Math.sqrt(2);
var minDist = 0.5*maxDist;

var gameState = Model({
  showAnswer: true,
  answer, guess,
  get score() {
    let dist = 0;
    for (let i in answer) {
      dist += Math.hypot(guess[i][0] - answer[i][0], guess[i][1] - answer[i][1]);
    }
    dist = maxDist - dist - minDist;
    return dist/(maxDist - minDist);
  },
});

function linearGradient(attrs, stops) {
  var gradient = defs.SVG("linearGradient", attrs);
  stops.forEach((stopColor, i) => {
    gradient.SVG("stop", {
      offset: i/(stops.length-1),
      $style: { stopColor },
    });
  });
  return gradient;
}

var colors = [
  "#880000",
  "#00AA00",
  "#0000FF",
];
// https://observablehq.com/@d3/working-with-color
// d3.quantize(d3.piecewise(d3.interpolateHsl, ["#880000", "#00DD00", "#0000FF", "#880000"]), 13)
var radiant = [
  "rgb(136, 0, 0)",
  "rgb(141, 18, 0)",
  "rgb(147, 37, 0)",
  "rgb(152, 57, 0)",
  "rgb(157, 79, 0)",
  "rgb(163, 102, 0)",
  "rgb(168, 126, 0)",
  "rgb(173, 152, 0)",
  "rgb(179, 179, 0)",
  "rgb(161, 184, 0)",
  "rgb(142, 189, 0)",
  "rgb(122, 194, 0)",
  "rgb(100, 200, 0)",
  "rgb(77, 205, 0)",
  "rgb(53, 210, 0)",
  "rgb(27, 216, 0)",
  "rgb(0, 221, 0)",
  "rgb(0, 223, 28)",
  "rgb(0, 225, 56)",
  "rgb(0, 227, 85)",
  "rgb(0, 230, 115)",
  "rgb(0, 232, 145)",
  "rgb(0, 234, 175)",
  "rgb(0, 236, 206)",
  "rgb(0, 238, 238)",
  "rgb(0, 210, 240)",
  "rgb(0, 182, 242)",
  "rgb(0, 153, 244)",
  "rgb(0, 123, 247)",
  "rgb(0, 93, 249)",
  "rgb(0, 63, 251)",
  "rgb(0, 32, 253)",
  "rgb(0, 0, 255)",
  "rgb(31, 0, 248)",
  "rgb(60, 0, 240)",
  "rgb(87, 0, 233)",
  "rgb(113, 0, 225)",
  "rgb(136, 0, 218)",
  "rgb(158, 0, 210)",
  "rgb(178, 0, 203)",
  "rgb(195, 0, 196)",
  "rgb(188, 0, 165)",
  "rgb(181, 0, 135)",
  "rgb(173, 0, 108)",
  "rgb(166, 0, 83)",
  "rgb(158, 0, 59)",
  "rgb(151, 0, 38)",
  "rgb(143, 0, 18)",
  "rgb(136, 0, 0)"
];
colors.push(colors[0]);

var gradient01 = linearGradient({
  id: "gradient01",
  $model: [guess[0], guess[1]],
  $view: ([[x1, y1], [x2, y2]]) => ({ x1, y1, x2, y2 }),
}, [colors[0], colors[1]]);
var gradient23 = linearGradient({
  id: "gradient23",
  $model: [guess[2], guess[3]],
  $view: ([[x1, y1], [x2, y2]]) => ({ x1, y1, x2, y2 }),
}, [colors[2], colors[3]]);

var splits = [0,1,2,3,4,5,6,7,8,9,10,11];
var segmentsM = Model.map(Model(answer), {
  segments: values => bsplitMany(values, splits.slice(1).map(x => x / splits.length)),
});

var lower = svg.SVG("g");

var curves = splits.map(i => lower.SVG("path", {
  $model: segmentsM,
  $view: ({ segments }) => ({ d: "M"+segments[i][0]+"C"+segments[i].slice(1) }),
  $style: {
    fill: "none",
    stroke: "url(#gradient"+i+")",
    strokeWidth: "0.05",
  },
}));
var grab = (radiant.length-1)/splits.length;
var gradients = splits.map(i => linearGradient({
  id: "gradient"+i,
  $model: segmentsM,
  $view: ({ segments: { [i]: [[x1, y1], _, __, [x2, y2]] } }) => ({ x1, y1, x2, y2 }),
}, radiant.slice(grab*i, grab*(i+1)+1)));

var lines = [[0,1], [2,3]].map(([i,j]) =>
  lower.SVG("line", {
    $model: [guess[i], guess[j]],
    $view: ([[x1, y1], [x2, y2]]) => ({ x1, y1, x2, y2 }),
    $style: {
      // stroke: `url(#gradient${i}${j})`,
      stroke: colors[i ? i : j],
      strokeWidth: 0.02,
    },
  })
);
var points = guess.map((value, i) => {
  var point = lower.SVG("circle");
  var isActive = i == 1 || i == 2;
  var ActivePoint = {
    $preview: ({ value, mouse }) => {
      const r = point?.r?.animValue?.value ?? 0.05;
      const R = 1.0 - r;
      if (mouse.active) {
        [value[0], value[1]] = screenToSVG(mouse.active.pageX, mouse.active.pageY);
        Bubble(point);
      }
      value[0] = clamp(r, value[0], R);
      value[1] = clamp(r, value[1], R);
    },
    $style: {
      cursor: "pointer",
    },
  };
  point.applyAttrs({
    $model: { value, mouse: Handling(point), hovering: Hovering(point) },
    $view: ({ value: [cx, cy], hovering, mouse }) => ({
      cx, cy,
      $style: {
        fill: isActive && (hovering.hover || mouse.active) ? colors[i] : colors[i]+"77",
      },
    }),
    r: 0.05,
    ...(isActive ? ActivePoint : {}),
  });
  return point;
});

var overlay = svg.SVG("g");

var answerPoints = answer.map((value, i) => {
  var point = overlay.SVG("circle");
  point.applyAttrs({
    $model: { value, mouse: Handling(point), hovering: Hovering(point), gameState },
    $view: ({ value: [cx, cy], hovering, mouse }) => ({
      cx, cy,
      $style: {
        fill: "white",
        display: gameState.showAnswer ? "" : "none",
        pointerEvents: "none",
      },
    }),
    r: 0.02,
  });
  return point;
});

var answerLines = [[0,1], [2,3]].map(([i,j]) =>
  overlay.SVG("line", {
    $model: [answer[i], answer[j], gameState],
    $view: ([[x1, y1], [x2, y2]]) => ({ x1, y1, x2, y2, $style: {
      display: gameState.showAnswer ? "" : "none" } }),
    $style: {
      stroke: "white",
      strokeWidth: 0.01,
    },
  })
);

var guessCurve = overlay.SVG("path", {
  $model: { guess, gameState },
  $view: ({ guess, gameState }) => ({ d: "M"+guess[0]+"C"+guess.slice(1), $style: {
      display: gameState.showAnswer ? "" : "none" } }),
  $style: {
    fill: "none",
    stroke: "black",
    strokeWidth: "0.02",
  },
});

the("game").appendChild(svg);

var controls = the("controls");

var buttons = controls.DOM("div");
var newGame = buttons.DOM("button", {
  $model: gameState,
  class: "big add",
  $textContent: "New",
  $view: gameState => ({
    $style: {
      display: gameState.showAnswer ? "" : "none",
    },
  })
});
var scoreIt = buttons.DOM("button", {
  $model: gameState,
  class: "big",
  $textContent: "Guess!",
  $view: gameState => ({
    $style: {
      display: gameState.showAnswer ? "none" : "",
    },
  })
});
controls.DOM("div").DOM("span", {
  $model: gameState,
  $view: ({ score }) => ({
    $textContent: Math.ceil(score*100) + "%",
    $style: {
      fontSize: "1.5em",
      display: gameState.showAnswer ? "" : "none",
    },
  })
});

newGame.listen("click", () => {
  var rand = () => Math.random()*0.8+0.1;
  for (let value of answer) {
    value[0] = rand();
    value[1] = rand();
  }
  gameState.showAnswer = false;
});

scoreIt.listen("click", () => {
  gameState.showAnswer = true;
});
