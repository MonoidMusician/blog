// https://observablehq.com/@monoidmusician/svg-calligraphy@1926

var guideS = ["M60.745553,110.30621C63.233853,109.12941,64.635756,107.36009,65.218193,105.40807C66.092387,102.47824999999999,65.236723,99.03791,63.205139,96.76966399999999C58.747629,91.79289399999999,49.573695,91.10018399999998,44.47936,95.21885699999999C37.480662,100.87716999999998,34.314857,111.56303999999999,39.176101,119.17626999999999C43.918945,126.60405999999999,48.529125,129.69159,57.683391,130.10899999999998C58.394831,130.14139999999998,58.971176,129.90328999999997,60.22455,128.95573", "M 45.1864 110.3062 C 45.8085 109.1294 46.1589 107.3601 46.3045 105.4081 C 46.5231 102.4782 46.3092 99.0379 45.8013 96.7697 C 44.6869 91.7929 42.3934 91.1002 41.1198 95.2189 C 39.3702 100.8772 38.5787 111.563 39.794 119.1763 C 40.9797 126.6041 42.1323 129.6916 44.4208 130.109 C 44.5987 130.1414 44.7428 129.9033 45.0561 128.9557"][0];

var guideS = [
  {type: "M", values:[60.745553,110.30621]},
  {type: "C", values:[63.233853,109.12941,64.635756,107.36009,65.218193,105.40807]},
  {type: "C", values:[66.092387,102.47824999999999,65.236723,99.03791,63.205139,96.76966399999999]},
  {type: "C", values:[58.747629,91.79289399999999,49.573695,91.10018399999998,44.47936,95.21885699999999]},
  {type: "C", values: [37.480662,100.87716999999998,34.314857,111.56303999999999,39.176101,119.17626999999999]},
  {type: "C", values:[43.918945,126.60405999999999,48.529125,129.69159,57.683391,130.10899999999998]},
  {type: "C", values:[58.394831,130.14139999999998,58.971176,129.90328999999997,60.22455,128.95573]},
];

var nibS = ["M60.224551,128.95573C61.461901999999995,128.18440999999999,62.316894,127.40829999999998,62.955560999999996,126.62948999999999C63.798970999999995,125.60100999999999,64.265124,124.56781,64.736392,123.53466999999999C63.876065,123.99437999999999,62.907411999999994,124.68327,62.023517,125.75223C61.344727,126.57314,60.715922,127.6182,60.224551,128.95573","M60.224551,128.95573L62.955560999999996,126.62948999999999L64.736392,123.53466999999999L62.023517,125.75223L60.224551,128.95573"][0];

var segments = [[[[19.07441902522832,17.446785423636857],[20.370738764806944,16.891219820960305],[21.66705850438557,15.965277149832716]],[[22.593001175513155,15.039334478705127],[23.704132380866263,13.928203273352022],[24.444886517768335,12.44669499954788]],[[22.593001175513155,12.817072067998915],[21.48186997016005,13.18744913644995],[20.00036169635591,13.928203273352022]],[[18.889230491002802,14.85414594447961],[18.148476354100733,16.150465684058233],[17.592910751424178,17.631973957862375]]],[[[18.33366488832625,19.113482231666516],[20.370738764806944,16.891219820960305],[21.66705850438557,15.965277149832716]],[[22.593001175513155,15.039334478705127],[24.81526358621937,13.743014739126503],[24.444886517768335,12.44669499954788]],[[22.407812641287638,13.372637670675468],[21.48186997016005,13.18744913644995],[19.815173162130392,14.298580341803056]],[[17.7780992856497,15.409711547156162],[17.222533682973143,16.33565421828375],[17.592910751424178,17.631973957862375]]]][1];

function toPath(segs) {
  if (!segs.length) return "";
  var p = segs[segs.length-1][2];
  var r = [{type:"M", values:p}];
  for (let seg of segs) {
    r.push({type:"C", values:seg.flatMap(x => x)});
  }
  return r;
}
function singleCurve(points) {
  return [{type:"M", values:points[0]}, {type:"C", values:points.slice(1).flatMap(x => x)}];
}

var curves = (function(segs) {
  if (!segs.length) return "";
  var p = segs[segs.length-1][2];
  var r = [];
  for (let seg of segs) {
    var c = [p, ...seg];
    c.seg = seg;
    r.push(c);
    p = seg[seg.length-1];
  }
  return r;
})(segments)

var getSVG = el => {
  while (el && el.tagName !== "svg")
    el = el.parentElement;
  return el;
}

var scaled = (event,el) => {
  var svg = getSVG(el || event.sourceEvent.target);
  var disp = svg?.getBoundingClientRect();
  var vb = svg?.viewBox?.animVal || svg?.viewBox?.baseVal;
  var scale = (outer, offset, inner, value) => offset + value*inner/outer;
  return {
    x: scale(disp.width, vb.x, vb.width, event.x),
    y: scale(disp.height, vb.y, vb.height, event.y),
  };
}

var curvature = (b0,b1,b2) => {
  let b21 = [b2[0]-b1[0],b2[1]-b1[1]];
  let d0 = [b1[0]-b0[0],b1[1]-b0[1]];
  let d0xb21 = d0[0]*b21[1] - d0[1]*b21[0];
  let delta = Math.sqrt(d0[0]*d0[0] + d0[1]*d0[1]);
  return (2/3)*d0xb21/Math.pow(delta, 3);
}

var oscu = (b0,b1,b2) => {
  let d0 = [b1[0]-b0[0],b1[1]-b0[1]];
  let delta = Math.sqrt(d0[0]*d0[0] + d0[1]*d0[1]);
  let norm = [-d0[1]/delta, d0[0]/delta];
  let r = 1/curvature(b0,b1,b2);
  let c = [norm[0]*r + b0[0], norm[1]*r + b0[1]];
  return svg`<circle cx=${c[0]} cy=${c[1]} r=${Math.abs(r)}>`;
}

var xmlns = "http://www.w3.org/2000/svg";

class Path extends Array {
  constructor(path) {
    super();
    Object.assign(this, path);
  }
  toString() {
    return renderPathData(this);
  }
}

function mkSVG(n) {
  if (n instanceof SVGElement) return n;
  var node = document.createElementNS(xmlns, n.type);
  var attrs = Object.assign({}, n);
  delete attrs.type;
  delete attrs.children;
  for (let a in attrs) {
    var v = attrs[a];
    if (a === "d") v = renderPathData(v);
    else if (a === "style" && typeof v === "object") {
      let r = "";
      for (let k in v) {
        r += (r ? "; " : "") + k + ": " + v[k];
      }
      v = r;
    }
    if (a.startsWith("on")) {
      node[a] = v;
    } else {
      node.setAttribute(a, v);
    }
  }
  if (n.children) {
    for (let c of n.children) {
      if (!(c instanceof SVGElement)) c = mkSVG(c);
      node.appendChild(c);
    }
  }
  if (attrs.id) {
    var other = document.getElementById(attrs.id);
    if (other) {
      other.replaceWith(node);
    }
  }
  return node;
}
function mkPath(d) {
  path = document.createElementNS(xmlns, "path");
  path.setAttribute("d", renderPathData(d));
  return path;
}


function placesI(iters=400) {
  var r = []; for (var i = 0; i < 1; i += 1/iters) r.push(i); return r;
}

var places = placesI();

function pathToCurves(path) {
  path = getPathData(path);
  var r = [];
  var p = [0,0]; var s = null;
  for (let idx in path) {
    let seg = path[idx];
    var [n, s] = getNextStart(seg, p, s);
    var a = absolutize(seg, p);
    switch (seg.type.toLowerCase()) {
      case "c":
        r.push([p].concat(group(a.values, 2)));
        break;
      case "l":
        break;
    }
    p = n;
  }
  return r;
}
function curvesToPath(curves) {
  var r = [];
  var p = null;
  for (let curve of curves) {
    let p1 = curve[0];
    if (!p || p1.toString() !== p.toString()) {
      r.push({type:"M",values:p1});
    }
    r.push({type:"C",values:curve.slice(1).flatMap(x => x)});
    p = curve[curve.length - 1];
  }
  return r;
}

var time_depth = 0;
function time(label, fn, ...args) {
  return fn(...args);
  var start = performance.now();
  time_depth += 1;
  var r = fn(...args);
  time_depth -= 1;
  var end = performance.now();
  console.log(`${label}: ${(end - start).toFixed(2)}ms`);
  return r;
}

function doTheThing(p1, p2, mode) {
  if (mode === "simplest" || mode === "simple") {
    p1 = startingAt(calligraphy.add(getOrigin(p1), getOrigin(p2)), p1);
    return (mode === "simplest" ? fillProduct : fullFillProduct)(p1, p2);
  }
  var start = performance.now();
  registry = {};
  composites = [];
  ps = getPathData(p2);
  p2 = (splitPathAtInflections(p2));
  let c2s = pathToCurves(p2);
  p1 = getPathData(p1);
  let c1s = pathToCurves(p1);
  let rs = [];
  // Then get tangents and concavities to split p1 at
  let allTangents = c2s.flatMap(getTangentsL);
  function getTangentsL(c2) {
    return [tangent(c2[0], c2[1]), tangent(c2[2], c2[3]), tangent(c2[0], c2[3])];
  }
  let allKeys = [];
  for (let c2 of c2s) {
    let tangents = getTangentsL(c2);
    let c1s2 = c1s.flatMap(c1 => splitBezierAtTangents(c1, tangents));
    for (let c1 of c1s2) {
      let r = doTheTask(c1, c2);
      rs.push(...r.map(curvesToPath));
      allKeys.push(c1[0], c1[3]);
    }
  }
  console.log((performance.now() - start).toFixed(2));
  return rs;
}
function chop(points, t0, t1) {
  if (t1 < t0) {
    return chop(points.slice().reverse(), 1-t0, 1-t1);
  }
  if (t0 === 0) {
    if (t1 === 1) return points;
    return bsplit(points, t1)[0];
  } else {
    if (t1 === 1) return bsplit(points, t0)[1];
    return bsplitMany(points, [t0,t1])[1];
  }
}
var registry = {};
var composites = [];
let lookup = path => {
  curves = pathToCurves(getPathData(path));
  for (let curve of curves) {
    let r = registry[curve.toString()];
    if (r) return r;
  }
  throw new Error("Unknown path " + path.toString());
}
function doTheTask(P, Q) {
  let {T_SOL,compositeI} = calligraphy;
  function add([x0,y0], [x1,y1]) {
    return [x0+x1,y0+y1];
  }
  let Q0 = Q.map(p => add(p, P[0]));
  let Q1 = Q.map(p => add(p, P[3]));
  let ts = [
    [0, P.map(p => add(p, Q[0])), 0],
    [1, P.map(p => add(p, Q[3])), 1],
  ];
  registry[ts[0][1].toString()] = [P,Q];
  registry[ts[1][1].toString()] = [P,Q];
  let tts = [
    [ts[0], ts[1]],
  ];
  let err = 0;
  try {
    ts.push(compositeI(P,Q));
    composites.push(ts[2]);
    if (ts[2][1].every(p => p.every(Number.isFinite))) {
      //tts.pop();
      tts.push(
        [ts[0], ts[2]],
        [ts[1], ts[2]],
      );
    }
  } catch(e) {
    try {
      T_SOL(P,Q)(0);
      T_SOL(P,Q)(1);
      //console.error(e);
      err = 1;
      let t = Math.random();
      try {
        let [P0,P1] = bsplit(P, t);
        let C0 = compositeI(P0,Q);
        let C1 = compositeI(P1,Q);
        ts.push(C0, C1);
        composites.push(C0, C1);
        console.log("New", C0, C1, "from", {P, t, Q});
        if (ts[2][1].every(p => p.every(Number.isFinite)) && ts[3][1].every(p => p.every(Number.isFinite))) {
          tts.push(
            [ts[0], ts[2]],
            [ts[1], ts[3]],
            [ts[2], ts[3]],
          );
        }
      } catch(e3) {
        //console.log("Failed", {P, t, Q});
      }
    } catch(e2) {
      //console.log(e);
    }
  }
  function chopped([[t0, conn0, d0], [t1, conn1, d1]]) {
    return [
      chop(Q0, t1, t0),
      conn0,
      chop(Q1, d0, d1),
      conn1.slice().reverse(),
    ];
  }
  return tts.map(chopped);
}

function guesstimate(curves) {
  const {Curve,sub,Bcurvature0,Bcurvature1,fit_check} = calligraphy;
  if (curves.length === 1) return new Curve(curves[0]);
  const c0 = curves[0]; const c1 = curves[curves.length-1];
  const f0 = c0[0];
  const f1 = c1[3];
  const d0 = sub(c0[1], c0[0]);
  const d1 = sub(c1[3], c1[2]);
  const k0 = Bcurvature0(c0);
  const k1 = Bcurvature1(c1);
  return fit_check(f0,f1,d0,d1,k0,k1);
}

function center(path) {
  //return startingAt([0,0],path);
  path = getPathData(path);
  let points = getPoints(path);
  let avg = getOrigin(path).map(x => x*points.length);
  for (let point of points) {
    avg[0] -= point[0];
    avg[1] -= point[1];
  }
  avg = avg.map(x => x/points.length);
  return startingAt(avg, path);
}

document.addEventListener("DOMContentLoaded", () => {
  document.getElementById("main-calligraphy-demo").appendChild(interactive());
  document.getElementById("simplest-calligraphy-demo").appendChild(interactive("simplest"));
  document.getElementById("simple-calligraphy-demo").appendChild(interactive("simple"));
});

document.addEventListener("DOMContentLoaded", () => {
  document.getElementById("delooper-demo").appendChild(delooper());
});

document.addEventListener("DOMContentLoaded", () => {
  document.getElementById("all-fits-demo").appendChild(all_fits());
});

document.addEventListener("DOMContentLoaded", () => {
  document.getElementById("main-calligraphy-demo").appendChild(visualize(guideS, toPath(segmentsCModes[""])));
  document.getElementById("simplest-calligraphy-demo").appendChild(visualize(guideS, toPath(segmentsCModes["simplest"]), 0, "simplest"));
  document.getElementById("simple-calligraphy-demo").appendChild(visualize(guideS, toPath(segmentsCModes["simple"]), 0, "simple"));
  document.getElementById("anatomy").checked = anatomy;
  document.getElementById("anatomy").addEventListener("input", e => {
    anatomy = !!e.target.checked;
    visualize(guideS, toPath(segmentsCModes[""]));
  });
  document.getElementById("anatomy-simplest").checked = anatomy;
  document.getElementById("anatomy-simplest").addEventListener("input", e => {
    anatomy = !!e.target.checked;
    visualize(guideS, toPath(segmentsCModes["simplest"]), 0, "simplest");
  });
  document.getElementById("anatomy-simple").checked = anatomy;
  document.getElementById("anatomy-simple").addEventListener("input", e => {
    anatomy = !!e.target.checked;
    visualize(guideS, toPath(segmentsCModes["simple"]), 0, "simple");
  });
});

var anatomy = true;

function listen(to, name, listener) {
  if (!to) to = document;
  let event = e => {
    if (listener)
      return listener(e);
  };
  to.addEventListener(name, event);
  return () => {
    to.removeEventListener(name, event);
  }
}
function mkListeners(obj, df_tgt) {
  for (let k in obj) {
    let v = obj[k];
    let t = v.target;
    if (t === undefined) t = df_tgt;
    if (!t) t = document;
    let a = v.active;
    let is_active = false;
    Object.defineProperty(v, "active", {
      get: () => !!is_active,
      set: actv => {
        if (!!is_active === !!actv) return;
        if (is_active) {
          is_active(); is_active = null;
        } else {
          is_active = listen(t, k, v);
        }
      },
    });
    v.active = a;
  }
}

var localized = (event, el, to) => {
  if (!to) to = {x:0,y:0};
  var svg = getSVG(el || event.target);
  var disp = svg?.getBoundingClientRect();
  var vb = svg?.viewBox?.animVal || svg?.viewBox?.baseVal;
  var scale = (origin, outer, offset, inner, value) => offset + (value-origin)*inner/outer;
  //console.log(svg, disp, vb);
  return {
    x: scale(disp.left + window.scrollX, disp.width, vb.x, vb.width, event.pageX) - to.x,
    y: scale(disp.top + window.scrollY, disp.height, vb.y, vb.height, event.pageY) - to.y,
  };
}

function draggable(node, datum=undefined, done=undefined) {
  let dragging = false;
  let o = null;
  let debounce = null;
  let listeners = {
    mousedown: Object.assign(e => {
      o = localized(e, node);
      if (datum) {
        datum(node, o, o, {x:0,y:0}, e);
      }
      listeners.active = true;
    }, {target: node, active: true}),
    mousemove: Object.assign(e => {
      if (datum) {
        let c = localized(e, node);
        datum(node, c, o, {x:c.x-o.x,y:c.y-o.y}, e);
      }
      if (done) {
        if (debounce) clearTimeout(debounce);
        debounce = setTimeout(() => {
          done(node, false);
        }, 30);
      }
    }, {target: null}),
    mouseup: Object.assign(e => {
      o = null;
      listeners.active = false;
      if (done) {
        if (debounce) clearTimeout(debounce);
        done(node, true, e);
      }
    }, {target: null}),
  };
  Object.defineProperty(listeners, "active", {
    set: actv => {
      listeners.mousemove.active = actv;
      listeners.mouseup.active = actv;
    }
  });
  mkListeners(listeners);
  return node;
}
var segmentsCModes = {
  "": [[[-1.8889230491002778,3.5185821502848356],[0.14815082738041596,1.2963197395786246],[1.4444705669590405,0.3703770684510346]],[[2.370413238086627,-0.5555656026765536],[4.5926756487928415,-1.8518853422551782],[4.222298580341807,-3.148205081833801]],[[2.1852247038611097,-2.2222624107062128],[1.2592820327335232,-2.40745094493173],[-0.4074147752961359,-1.2963197395786246]],[[-2.4444886517768296,-0.18518853422551906],[-3.000054254453385,0.7407541369020691],[-2.6296771860023505,2.0370738764806937]]],
  "simplest": [[[-1.8889230491002778,3.5185821502848356],[0.14815082738041596,1.2963197395786246],[1.4444705669590405,0.3703770684510346]],[[2.370413238086627,-0.5555656026765536],[4.5926756487928415,-1.8518853422551782],[4.222298580341807,-3.148205081833801]],[[2.1852247038611097,-2.2222624107062128],[1.2592820327335232,-2.40745094493173],[-0.4074147752961359,-1.2963197395786246]],[[-2.4444886517768296,-0.18518853422551906],[-3.000054254453385,0.7407541369020691],[-2.6296771860023505,2.0370738764806937]]],
  "simple": [[[-1.8889230491002778,3.5185821502848356],[0.14815082738041596,1.2963197395786246],[1.4444705669590405,0.3703770684510346]],[[2.370413238086627,-0.5555656026765536],[4.5926756487928415,-1.8518853422551782],[4.222298580341807,-3.148205081833801]],[[2.1852247038611097,-2.2222624107062128],[1.2592820327335232,-2.40745094493173],[-0.4074147752961359,-1.2963197395786246]],[[-2.4444886517768296,-0.18518853422551906],[-3.000054254453385,0.7407541369020691],[-2.6296771860023505,2.0370738764806937]]],
};
var INPUT = {
  nib: {
    value: [],
  },
};
function rotate([x,y], a) {
  var c = Math.cos(a);
  var s = Math.sin(a);
  return [x*c + y*s, y*c - x*s];
}
function rotateXY({x,y}, a) {
  var c = Math.cos(a);
  var s = Math.sin(a);
  return {x:x*c + y*s, y:y*c - x*s};
}
function toDegrees (angle) {
  return angle * (180 / Math.PI);
}
function toRadians (angle) {
  return angle * (Math.PI / 180);
}
function roundAngle(angle) {
  return toRadians((Math.round(toDegrees(angle) + 45 + 360) % 90) - 45);
}
function interactive(mode) {
  var segmentsC = segmentsCModes[mode||""];
  var suffix = mode ? "-"+mode : "";
  var path = mkPath(toPath(segmentsC));
  var copySegments = () => segmentsC.map(ps => ps.map(p => p.map(xy => xy)));
  var mutateSegments = (fn,against) => {
    if (!against) against = segmentsC;
    for (let i in segmentsC) {
      for (let j in segmentsC[i]) {
        var p = fn(against[i][j]);
        Object.assign(segmentsC[i][j], p);
        segmentsC[i][j].node.setAttribute("cx", segmentsC[i][j][0]);
        segmentsC[i][j].node.setAttribute("cy", segmentsC[i][j][1]);
        path.setAttribute("d", renderPathData(toPath(segmentsC)));
      }
    }
  };
  var restoreSegments = against => mutateSegments(p => p, against);
  var rotating = f => ([x,y]) => {
    return rotate(f(rotate([x,y], angle)), -angle);
  };
  let update = () => requestAnimationFrame(() => visualize(guideS, toPath(segmentsC), 0, mode));
  let update2 = (_, reallyDone) => {
    mkSVG({
      type: "g",
      id: "superpath"+suffix,
      fill: "none",
      //children: pathToCurves(splitPathAtInflections(toPath(segmentsC))).map(c => mkPath(curvesToPath([c]))),
    });
    if (reallyDone) update();
  };
  let angle = toRadians(1 ? -44 : -37);
  var superpath = mkSVG({
    type: "g",
    id: "superpath"+suffix,
    fill: "none",
    //children: pathToCurves(splitPathAtInflections(toPath(segmentsC))).map(c => mkPath(curvesToPath([c]))),
  });
  let arcs = [
    "M 0 9.95 A 10 10 0 0 0 2 9.75 M 0 9.95 A 10 10 0 0 1 -2 9.75",
    "M 0 -9.95 A 10 10 0 0 1 2 -9.75 M 0 -9.95 A 10 10 0 0 0 -2 -9.75",
    "M 9.95 0 A 10 10 0 0 1 9.75 2 M 9.95 0 A 10 10 0 0 0 9.75 -2",
    "M -9.95 0 A 10 10 0 0 0 -9.75 2 M -9.95 0 A 10 10 0 0 1 -9.75 -2",
  ]
  return mkSVG({
    type: "svg",
    id: "vis"+suffix,
    "viewBox": [-10, -10, 20, 20],
    "style": {
      "stroke-linecap": "round",
      "stroke-linejoin": "round",
      "fill": "#000",
      "stroke": "#444",
      "stroke-width": 0.8,
    },
    children: [
      {type:"g",style:{"fill":"none"},children:[path]},
      //superpath,
      picks = mkSVG({
        type: "g",
        style: {
          "fill": "none",
          "stroke": "transparent",
          "stroke-width": 0.25,
          "stroke-linecap": "butt",
          "transform": `rotate(${angle}rad)`,
        },
        children: [
          draggable(mkSVG({
            type: "line",
            style: {
              "cursor": "ns-resize",
            },
            x1: -0, x2: 0, y1: -10, y2: 10,
          }), (_, current, start) => {
            if (current === start) {
              start.points = copySegments();
            } else {
              var scale = rotateXY(current, angle).y / rotateXY(start, angle).y;
              mutateSegments(rotating(([x,y]) => ([x,scale*y])), start.points);
            }
          }, update2),
          draggable(mkSVG({
            type: "line",
            style: {
              "cursor": "ew-resize",
            },
            x1: -10, x2: 10, y1: -0, y2: 0,
          }), (_, current, start) => {
            if (current === start) {
              start.points = copySegments();
            } else {
              var scale = rotateXY(current, angle).x / rotateXY(start, angle).x;
              mutateSegments(rotating(([x,y]) => ([scale*x,y])), start.points);
            }
          }, update2),
          draggable(mkSVG({
            type: "circle",
            style: {
              "fill": "transparent",
              "cursor": "move",
            },
            cx: 0, cy: 0, r: 0.25,
          }), (_, current, start, delta) => {
            if (current === start) {
              start.points = copySegments();
            } else {
              let factor = 1;
              mutateSegments(([x,y]) => ([x + factor*delta.x, y + factor*delta.y]), start.points);
            }
          }, update2),
          ...arcs.map(arc => draggable(mkSVG({
            type: "path",
            d: arc,
            style: {cursor:"crosshair"},
          }), (_node, current, start, _delta, event) => {
            if (current === start) {
              start.original_angle = angle;
              start.angle = Math.atan2(start.y, start.x);
              start.points = copySegments();
            } else {
              current.angle = Math.atan2(current.y, current.x);
              angle = roundAngle(current.angle - start.angle) + start.original_angle;
              gui.style.transform = `rotate(${angle}rad)`;
              picks.style.transform = `rotate(${angle}rad)`;
              if (!event.shiftKey) {
                mutateSegments(p => rotate(p, toRadians(Math.round(toDegrees(start.angle - current.angle)))), start.points);
              } else {
                restoreSegments(start.points);
              }
            }
          }, update2)),
        ]
      }),
      {
        type:"g",
        children: segmentsC.flatMap(x=>x).map(p =>
          draggable(p.node = mkSVG({
            type: "circle",
            "cx": p[0],
            "cy": p[1],
            "r": .15,
            "fill": "gray",
            "stroke": "lightgray",
            "stroke-width": 0.05,
          }), (node, current, start) => {
            p[0] = current.x;
            p[1] = current.y;
            node.setAttribute("cx", current.x);
            node.setAttribute("cy", current.y);
            path.setAttribute("d", renderPathData(toPath(segmentsC)));
            mkSVG({
              type: "g",
              id: "superpath"+suffix,
              fill: "none",
              //children: pathToCurves(splitPathAtInflections(toPath(segmentsC))).map(c => mkPath(curvesToPath([c]))),
            });
          }, update)
        ),
      },
      gui = mkSVG({
        type: "g",
        style: {
          "fill": "none",
          "stroke": "#76cbe79e",
          "stroke-width": 0.05,
          "stroke-dasharray": 20/21*1/5,
          "stroke-linecap": "butt",
          "pointer-events": "none",
          "transform": `rotate(${angle}rad)`,
        },
        children: [
          {
            type: "line",
            x1: -0, x2: 0, y1: -10, y2: 10,
          },
          {
            type: "line",
            x1: -10, x2: 10, y1: -0, y2: 0,
          },
          ...arcs.map(mkPath),
        ]
      }),
    ],
  });
}

function delooper() {
  var scale = 1;
  var points = [[-1.8889230491002778,3.5185821502848356],[4.185463659147869,-0.1309915413533833],[-4.364160401002507,-0.6573073308270683],[2.8320802005012524,3.8539708646616546]].map(p => p.map(c => c*scale));
  var path = mkPath(singleCurve(points));
  var delooped = mkPath(singleCurve(calligraphy.fit_existing(points)));
  return mkSVG({
    type: "svg",
    "viewBox": [-5, -5, 10, 10],
    "style": {
      "stroke-linecap": "round",
      "stroke-linejoin": "round",
      "fill": "#000",
      "stroke": "#444",
      "stroke-width": 0.2,
      "max-width": "350px",
      "max-height": "350px",
      "display": "block",
      "margin": "auto",
      "background-color": "#0005",
    },
    children: [
      {
        type: "g",
        "style": {
          "mix-blend-mode": "hard-light",
          "fill": "none",
        },
        children: [
          {type:"g",style:{"stroke":"#00F9"},children:[path]},
          {type:"g",style:{"fill":"none","stroke":"#F007"},children:[delooped]},
        ],
      },
      {
        type: "g",
        children: points.map(p =>
          draggable(p.node = mkSVG({
            type: "circle",
            "cx": p[0],
            "cy": p[1],
            "r": .15,
            "fill": "gray",
            "stroke": "lightgray",
            "stroke-width": 0.05,
          }), (node, current, start) => {
            p[0] = current.x;
            p[1] = current.y;
            node.setAttribute("cx", current.x);
            node.setAttribute("cy", current.y);
            path.setAttribute("d", renderPathData(singleCurve(points)));
            delooped.setAttribute("d", renderPathData(singleCurve(calligraphy.fit_existing(points))));
          }, () => false&&console.log(JSON.stringify(points)))
        ),
      },
    ],
  });
}

function all_fits() {
  var scale = 1;
  var points = [[2.8,0.6316964285714288],[2.4000000000000004,-2.711160714285714],[-2.142857142857143,-2.711160714285714],[-2.4857142857142858,0.4888392857142856]].map(p => p.map(c => c*scale));
  function render() {
    return [
      mkSVG({
        type: "g",
        id: "all-fits-handles",
        "style": {
          "fill": "none",
          "stroke": "#76cbe79e",
          "stroke-width": 0.05,
          "stroke-dasharray": 20/21*1/5,
          "stroke-linecap": "butt",
        },
        children: points.flatMap((p1,i) => {
          if (i == 0) return [];
          let p0 = points[i-1];
          return [
            mkSVG({
              type: "line",
              x1: p0[0], x2: p1[0], y1: p0[1], y2: p1[1],
            })
          ];
        }),
      }),
      mkSVG({
        type: "g",
        id: "all-fits",
        "style": {
          "fill": "none",
          "stroke": "#0006",
        },
        children: calligraphy.fit_all(...calligraphy.from_existing(points)).map(curve => mkPath(singleCurve(curve))),
      }),
    ];
  }
  return mkSVG({
    type: "svg",
    "viewBox": [-5, -5, 10, 10],
    "style": {
      "stroke-linecap": "round",
      "stroke-linejoin": "round",
      "fill": "#000",
      "stroke": "#444",
      "stroke-width": 0.2,
      "max-width": "350px",
      "max-height": "350px",
      "display": "block",
      "margin": "auto",
      "overflow": "visible"
    },
    children: [
      ...render(),
      {
        type: "g",
        children: points.map(p =>
          draggable(p.node = mkSVG({
            type: "circle",
            "cx": p[0],
            "cy": p[1],
            "r": .15,
            "fill": "gray",
            "stroke": "lightgray",
            "stroke-width": 0.05,
          }), (node, current, start) => {
            p[0] = current.x;
            p[1] = current.y;
            node.setAttribute("cx", current.x);
            node.setAttribute("cy", current.y);
            render();
          }, () => false&&console.log(JSON.stringify(points)))
        ),
      },
    ],
  });
}

// easySVG
// - drag n drop
// - underlying re-coordinator
//   and listeners for … stuff
// - prioritizer

function visualize(p1, p2, prec=100, mode) {
  var suffix = mode ? "-"+mode : "";
  var YEAH = doTheThing(p1, p2, mode);
  return mkSVG({
    type: "svg",
    id: "PREVIEW"+suffix,
    "viewBox": [30, 85, 50, 50],
    "fill": "#000",
    "stroke": "#000",
    "stroke-width": 0.8,
    "style": {
      "stroke-linecap": "round",
      "stroke-linejoin": "round",
    },
    "ondblclick": () => {
      if (!prec || prec < 500) {
        visualize(p1, p2, (prec || 00) + 100, mode);
      }
    },
    children: [
      {
        type: "g",
        "fill": "red",
        "stroke": "red",
        children: placesI(prec || 00).map(i => mkPath(startingAt(calcP(p1, i), p2, true).toString())),
      },
      {
        type: "g",
        children: YEAH.map(mkPath),
      },
      !anatomy?{}:{
        type: "g",
        "class": "annot",
        "fill": "none",
        "stroke": "#0804",
        "stroke-width": 0.1,
        children: YEAH.map(d => ({
          type: "path",
          "d": d,
          "onclick": e => {
            let [P,Q] = lookup(e.target);
            //let cs = pathToCurves(getPathData(e.target));
            console.log(P, Q);
            try {
              console.log(calligraphy.composite(P, Q));
            } catch(e) {
              console.error(e);
            }
          },
          "ondblclick": e => {e.target.remove();e.stopPropagation()},
        })),
      },
      !anatomy||mode?{}:{
        type: "g",
        "class": "annot",
        "fill": "none",
        "stroke": "#9008",
        "stroke-width": 0.1,
        "style": "pointer-events: none",
        children: composites.map(d => ({
          type: "path",
          "d": curvesToPath([d[1]]),
        })),
      },
    ],
  });
}

function getPathData(path, copy) {
  if (typeof path === 'string') {
    path = mkPath(path);
  }
  if (path instanceof SVGPathElement) {
    path = path.getPathData();
  } else if (copy) {
    path = path.map(({ type, values }) => ({ type, values: values.map(v => v) }));
  }
  return new Path(path);
}

function trunc(path) {
  return getPathData(path).map(({ type, values }) => ({ type, values: values.map(v => +v.toFixed(3)) }));
}

function group(vs, l=2) {
  var vs = Array.from(vs);
  var r = [];
  while (vs.length) {
    r.push(vs.slice(0, l));
    vs.splice(0, l);
  }
  return r;
}

function toBs(path) {
  path = getPathData(path);
  var p = [0, 0];
  var s = getOrigin(path);
  var bs = [];
  for (let seg of path) {
    var t = seg.type.toLowerCase();
    var adj = q => [q[0]+p[0], q[1]+p[1]];
    if (t !== seg.type) {
      adj = q => q;
    }
    switch (t) {
      case "m":
        p = seg.values;
        break;
      case "z":
        if (p[0] !== s[0] || p[1] !== s[1]) {
          bs.push([p, s]);
        }
        p = s;
      case "c":
        bs.push([p, ...group(seg.values, 2).map(adj)]);
        p = seg.values.slice(-2);
        break;
      default:
        throw new Error("Unknown command: " + seg.type);
    }
  }
  return bs;
}

function reversePath(path) {
  path = getPathData(path, true);
  var rev = [];
  var p = [0,0]; var s = 0;
  for (let seg of path) {
    var s0 = Object.assign({}, seg);
    var [n,s] = getNextStart(seg, p, s);
    var abs = (seg.type.toLowerCase() !== seg.type);
    switch (seg.type) {
      case "h":
      case "v":
        seg.values[0] *= -1;
        break;
      case "H":
        seg.values[0] = n[0];
        break;
      case "V":
        seg.values[0] = n[1];
        break;
      default:
        if (abs) {
          var adj = (q,i) => i > 0 ? q : p;
        } else {
          var adj = (q,i) => i > 0 ? [p[0]+q[0]-n[0], p[1]+q[1]-n[1]] : [-q[0], -q[1]];
        }
        var vs = group(seg.values, 2).reverse().map(adj);
        vs.push(vs.shift());
        seg.values = [].concat(...vs);
    }
    p = n;
    rev.unshift(seg);
  }
  if (rev[rev.length-1].type === "M")
    rev.pop();
  rev.unshift({ type: "M", values: p });
  return getPathData(rev);
}

function calcB(coords, t) {
  if (coords.length === 1) return coords[0];
  var c1 = coords.slice(0, -1);
  var p1 = calcB(c1, t);
  var c2 = coords.slice(1);
  var p2 = calcB(c2, t);
  return [(1-t)*p1[0] + t*p2[0], (1-t)*p1[1] + t*p2[1]];
}

function lengthB(coords) {
  var dx = coords[0][0] - coords[coords.length-1][0];
  var dy = coords[0][1] - coords[coords.length-1][1];
  return Math.sqrt(dx*dx + dy*dy);
}

function calcP(path, t) {
  var bs = toBs(path);
  var l = bs.reduce((r, b) => r+lengthB(b), 0);
  var i = 0; var li = lengthB(bs[i]);
  var lt = l * t;
  while (lt > li) {
    lt -= li;
    i += 1;
    li = lengthB(bs[i]);
  }
  return calcB(bs[i], lt/li);
}

function renderPathData(segs) {
  if (typeof segs === 'string') return segs;
  return segs.reduce((r, c) => {
    return (r ? r + " " : r) + (c.type ? c.type + c.values : renderPathData(c));
  }, "");
  var path = document.createElementNS(xmlns, "path");
  path.setPathData(segs);
  return path.getAttribute("d");
}

function getPoints(path) {
  var points = [];
  path = getPathData(path);
  var p = [0, 0]; var s = [0, 0];
  for (let seg of path) {
    var t = seg.type.toLowerCase();
    points.push(p = getNext(seg, p, s));
    if (t === "m")
      s = p;
  }
  return points;
}

function getNext(seg, prev, start) {
  var t = seg.type.toLowerCase();
  if (!prev || seg.type !== t) {
    prev = [0,0];
  }
  if ("mclsaqt".includes(t)) {
    var v = seg.values.slice(-2);
    return [prev[0] + v[0], prev[1] + v[1]];
  } else if (t === "h") {
    return [prev[0] + seg.values[0], prev[1]];
  } else if (t === "v") {
    return [prev[0], prev[1] + seg.values[0]];
  } else if (t === "z") {
    return start;
  } else {
    console.log("Unknown: ", seg);
  }
}

function getNextStart(seg, prev, start) {
  var next = getNext(seg, prev, start);
  if (!start) start = next;
  else if (seg.type === "m") start = null;
  return [next, start];
}

function getNextStart2(seg, prev, start) {
  var n = getNext(seg, prev, start);
  var p = prev;
  var s = start;
  switch (seg.type.toLowerCase()) {
    case "m":
      if (s && p && n.toString() !== p.toString()) {
        s = null;
      }
      break;
    case "z":
      if (s) {
        s = null;
      }
      break;
    default:
      if (!s) {
        s = p;
      } else if (n.toString() === s.toString()) {
        s = null;
      }
  }
  return [n, s];
}

function getEndpoints(path) {
  path = getPathData(path);
  var r = [];
  var p = null;
  var s = null;
  for (let seg of path) {
    var t = seg.type.toLowerCase();
    var n = getNext(seg, p, s);
    switch (t) {
      case "m":
        if (s && p && n.toString() !== p.toString()) {
          r.push(p);
          s = null;
        }
        break;
      case "z":
        if (s) {
          r.pop();
          s = null;
        }
        break;
      default:
        if (!s) {
          s = p;
          r.push(s);
        } else if (n.toString() === s.toString()) {
          r.pop();
          s = null;
        }
    }
    p = n;
  }
  if (s && p) {
    r.push(p);
  }
  return r;
}

function getSections(path) {
  path = getPathData(path);
  var r = [];
  var p = null;
  var s = null;
  for (let seg of path) {
    var t = seg.type.toLowerCase();
    //if (trivial(seg, p, s)) continue;
    if (seg.type.toLowerCase() !== "m") {
      if (!s) {
        r.push([{ type: "M", values: p }]);
      }
      r[r.length-1].push(seg);
    }
    var [n,s] = getNextStart2(seg, p, s);
    p = n;
  }
  return r;
}

function getOrigin(path) {
  var points = getPoints(path);
  return points[0] || [0,0];
}

function adjust(seg, prev) {
  var t = seg.type.toLowerCase();
  if (seg.type === t) {
    return function(p) {
      return [p[0]+prev[0], p[1]+prev[1]];
    }
  } else {
    return function(p) {
      return p;
    }
  }
}

function absolutize(seg, prev) {
  var t = seg.type.toLowerCase();
  var T = seg.type.toUpperCase();
  if (!prev || seg.type !== t) {
    prev = [0,0];
  }
  if ("mclsaqt".includes(t)) {
    return { ...seg, type: T, values: [].concat(...group(seg.values, 2).map(adjust(seg, prev))) };
  } else if (seg.type === "h") {
    return { ...seg, type: T, values: [seg.values[0]+prev[0]] };
  } else if (seg.type === "v") {
    return { ...seg, type: T, values: [seg.values[0]+prev[1]] };
  } else {
    return seg;
  }
}

function tangent(p1, p2) {
  var delta = [p2[0]-p1[0], p2[1]-p1[1]];
  return { slope: delta[1]/delta[0], p1, p2, delta };
}

function bezierDeriv([p0, p1, p2, p3]) {
  return [0,1].map(i => [-3*p0[i] + 9*p1[i] - 9*p2[i] + 3*p3[i], 6*p0[i] - 12*p1[i] + 6*p2[i], -3*p0[i] + 3*p1[i]]);
}

function solveTangent(c1, c2, v1, v2) {
  return solveQuadratic([0,1,2].map(i => c1[i]*v2 - c2[i]*v1)).filter(t => 0 <= t && t <= 1).sort();
}

function solveQuadratic([a,b,c], v) {
  if (v) c -= v;
  var disc = b*b - 4*a*c;
  if (disc === 0) return [-b/a/2];
  if (disc < 0) return [];
  return [Math.sqrt(disc),-Math.sqrt(disc)].map(sdisc => (-b + sdisc)/a/2);
}

function findBezierTangents(c, tangents) {
  var d = bezierDeriv(c);
  var sols = [];
  for (let tan of tangents) {
    sols.push(...solveTangent(...d, ...tan.delta).map(t => ({t, tan})));
  }
  return uniqBy(sols, x => x).sort((a,b) => a.t - b.t);
}

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

function splitBezierAtTangents(c, tangents) {
  let ep = 0.001;
  return bsplitMany(c, findBezierTangents(c, tangents).map(({t}) => t).flatMap(t => [t-ep, t+ep]));
}

function splitSegAtTangents(seg, p, tangents) {
  if (seg.type.toLowerCase() !== "c") throw new Error("Cannot split segment: ", seg);
  var c = [p, ...group(absolutize(seg, p).values, 2)];
  var cs = splitBezierAtTangents(c, tangents);
  return cs.map(ci => ({ type: "C", values: [].concat(...ci.slice(1)) }));
}

function splitPathAtTangents(path1, tangents) {
  path1 = getPathData(path1);
  var p = [0,0]; var s = null;
  var r = [];
  for (let seg of path1) {
    if (seg.type.toLowerCase() === "c") {
      var segs = splitSegAtTangents(seg, p, tangents);
      r.push(...segs);
    } else {
      r.push(seg);
    }
    [p, s] = getNextStart(seg, p, s);
  }
  return r;
}

function splitPathAtExtremities(path1) {
  path1 = getPathData(path1);
  var p = [0,0]; var s = null;
  var r = [];
  for (let seg of path1) {
    if (seg.type.toLowerCase() === "c") {
      var segs = splitSegAtTangents(seg, p, [tangent(p, seg.values.slice(-2))]);
      r.push(...segs);
    } else {
      r.push(seg);
    }
    [p, s] = getNextStart(seg, p, s);
  }
  return r;
}

function splitPathAtTangentsOf(path1, path2) {
  return splitPathAtTangents(path1, getTangents(path2));
}

function findBezierInflections(c) {
  return calligraphy.INFLXNS(c).filter(t => 0 <= t && t <= 1);
}

function splitBezierAtInflections(c) {
  return bsplitMany(c, findBezierInflections(c));
}

function splitSegAtInflections(seg, p) {
  if (seg.type.toLowerCase() !== "c") throw new Error("Cannot split segment: ", seg);
  var c = [p, ...group(absolutize(seg, p).values, 2)];
  var cs = splitBezierAtInflections(c);
  return cs.map(ci => ({ type: "C", values: [].concat(...ci.slice(1)) }));
}

function splitPathAtInflections(path1) {
  path1 = getPathData(path1);
  var p = [0,0]; var s = null;
  var r = [];
  for (let seg of path1) {
    if (seg.type.toLowerCase() === "c") {
      var segs = splitSegAtInflections(seg, p);
      r.push(...segs);
    } else {
      r.push(seg);
    }
    [p, s] = getNextStart(seg, p, s);
  }
  return r;
}

function getTangents(path) {
  path = getPathData(path);
  var r = [];
  var p = [0,0]; var s = null;
  for (let idx in path) {
    let seg = path[idx];
    var [n, s] = getNextStart(seg, p, s);
    var a = absolutize(seg, p);
    switch (seg.type.toLowerCase()) {
      case "c":
        var ps = [p].concat(group(a.values, 2));
        r.push(Object.assign(tangent(ps[0], ps[1]), {idx, end:0, c: ps}));
        r.push(Object.assign(tangent(ps[2], ps[3]), {idx, end:1, c: ps.slice().reverse()}));
        break;
      case "l":
        r.push(tangent(p, a.values));
        break;
    }
    p = n;
  }
  return r;
}

function startingAt([x,y], path, relative=false) {
  path = getPathData(path, true);
  var origin = getOrigin(path);
  var diff = [x - +!relative*origin[0], y - +!relative*origin[1]];
  for (let seg of path) {
    if (seg.type !== seg.type.toLowerCase()) {
      if (seg.type === "H") {
        seg.values[0] += diff[0];
      } else if (seg.type === "V") {
        seg.values[0] += diff[1];
      } else {
        for (let i in seg.values) {
          seg.values[i] += diff[i % 2];
        }
      }
    }
  }
  return path;
}

function smashProduct(p1, p2) {
  var o = getOrigin(p1);
  p1 = startingAt(o, getPathData(p1));
  p2 = startingAt(o, getPathData(p2));
  var p1ps = uniqBy(getPoints(p1), v => v.toString());
  var p2ps = uniqBy(getPoints(p2), v => v.toString());
  var p1s = p2ps.map(p => startingAt(p, p1));
  var p2s = p1ps.map(p => startingAt(p, p2));
  return [p1s, p2s];
}

function uniqBy(vs, keyer) {
  const found = [];
  const r = [];
  for (const v of vs) {
    const k = keyer(v);
    if (found.indexOf(k) === -1) {
      found.push(k);
      r.push(v);
    }
  }
  return r;
}

function fillProduct(p1, p2) {
  p1 = getPathData(p1);
  p2 = getPathData(p2);
  var r = [];
  for (let section of getSections(p1)) {
    var reversed = reversePath(section);
    var o = getOrigin(section);
    var es = getEndpoints(section);
    var p = o; var s = null;
    var p2o = startingAt(o, p2);
    for (let seg of p2o) {
      var e = !!es.length;
      var [n, s, t] = getNextStart2(seg, p, s);
      if (!t && seg.type.toLowerCase() !== "m") {
        if (!e) {
          r.push([...startingAt(p, section), ...reversePath(startingAt(n, section))]);
        } else {
          r.push([
            ...reversePath(trunc([{type: "M", values: p}, seg])),
            ...startingAt(p, section).slice(1),
            ...startingAt([p[0] + es[1][0] - o[0], p[1] + es[1][1] - o[1]], [{type: "M", values: [p[0], p[1]]}, seg]).slice(1),
            ...reversePath(startingAt(n, section)).slice(1),
          ]);
        }
      }
      p = n;
    }
  }
  return r;
}

function fullFillProduct(p1, p2) {
  p2 = getPathData(p2);
  p1 = splitPathAtTangentsOf(p1, p2);
  return [...fillProduct(p1, p2), ...smashProduct(p1, p2)[1]];
}
