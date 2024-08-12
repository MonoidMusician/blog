function HFStoString(x) {
  if (x === 0) {
    return "∅";
  }
  var items = [];
  Array.from(x.toString(2)).forEach((d, i, digits) => {
    if (d === "1") items.push(HFStoString(digits.length - 1 - i));
  });
  if (items.length < 2) {
    return `{${items.join(",")}}`;
  }
  return `{ ${items.join(", ")} }`;
}
function HFStoTree(x) {
  var items = [];
  Array.from(x.toString(2)).forEach((d, i, digits) => {
    if (d === "1") items.push(HFStoTree(digits.length - 1 - i));
  });
  return items;
}
function HFListToString(tree) {
  var items = tree.map(HFListToString);
  if (items.length < 2) {
    return `{${items.join(",")}}`;
  }
  return `{ ${items.join(", ")} }`;
}
function HFStoSpans(tree, depth=0) {
  var colorful = _ => ({className:"opacity-hover-child", style:{color:bracket_color(depth)}, _});
  if (!tree.length) {
    return colorful("∅");
  }
  var result = [];
  tree.forEach((subtree, i) => {
    if (i) result.push(", ");
    result.push(HFStoSpans(subtree, depth+1));
  })
  if (result.length < 2) {
    return [colorful("{"), result, colorful("}")];
  }
  return [colorful("{ "), result, colorful(" }")];
}
function HFStoSpan(x, depth=0) {
  return toSpan(HFStoSpans(x, depth));
}
function HFStoGraph(s) {
  var vs = [];
  var v = 0;
  var max = 0;
  for (let c of s) {
    if (c === `{`) {
      if (v > max) max = v;
      vs.push(v);
      v += 1;
    }
    if (c === `}`) {
      vs.push(v);
      v -= 1;
    }
    if (c === `∅`) {
      if (v > max) max = v;
      vs.push(v, v+1);
    }
  }
  vs.push(v);
  if (v !== 0) {
    throw new Error("Unbalanced HFS representation");
  }
  return [vs, max];
}
function HFSfromGraph(points) {
  if (!points.length) return 0n;
  if (points[0] !== 0 || points[points.length-1] !== 0) {
    throw new Error("HFS graph should start and end with 0 " + points);
  }
  if (points.length < 3) return 0n;
  const inner = points.slice(1, -1).map(point => point-1);
  if (inner.some(point => point < 0)) {
    throw new Error("HFS graph should be a single segment " + points);
  }
  let result = 0n;
  let segment = undefined;
  for (let point of inner) {
    if (point === 0) {
      if (segment !== undefined) {
        segment.push(0);
        result |= 1n << HFSfromGraph(segment);
      }
      segment = [0];
    } else {
      segment.push(point);
    }
  }
  return result;
}
function toNodes(data) {
  if (Array.isArray(data)) {
    return data.flatMap(toNodes);
  }
  if (typeof data === "object") {
    const el = document.createElement("span");
    const props = Object.assign({}, data);
    const children = props._;
    delete props._;
    Object.assign(el.style, props.style);
    delete props.style;
    Object.assign(el, props);
    for (let child of toNodes(children)) {
      el.appendChild(child);
    }
    return [el];
  }
  return [document.createTextNode(data)];
}
function toSpan(data) {
  return toNodes({_:data})[0];
}

var bracket_colors = [
  "#ff004d",
  "#fe7a29",
  "#fab30e",
  "#1dca24",
  "#2372ff",
  "#d350fe",
  "#e86db7",
];
function bracket_color(current_depth) {
  return bracket_colors[current_depth % bracket_colors.length];
}

function HFStoSVG(tree, options) {
  if (typeof tree === 'number') tree = HFStoTree(tree);
  if (!options) options = {};
  hfs = { tree };
  hfs.string = HFListToString(hfs.tree);
  [hfs.graph, hfs.depth] = HFStoGraph(hfs.string);
  hfs.large = hfs.graph.length > 300;
  if (hfs.large) hfs.graph = [];
  hfs.span = hfs.large ? hfs.string.length > 1000 ? toSpan("–") : toSpan(hfs.string) : HFStoSpan(hfs.tree);
  const limitTo = (n, s) => s.length <= n ? s : undefined;
  hfs.bin = hfs.large ? undefined : hfs.depth < 6 ? limitTo(45, HFSfromGraph(hfs.graph).toString(2)) : undefined;
  hfs.dec = hfs.large ? undefined : hfs.depth < 6 ? limitTo(30, HFSfromGraph(hfs.graph).toString(10)) : undefined;

  var w = 500;
  var h = 100;
  var h2 = 10;
  const visual = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  visual.setAttribute("width", w);
  visual.setAttribute("height", h+h2);
  visual.setAttribute("viewBox", `-2 -1 ${w+4} ${h+h2}`);
  visual.setAttribute("preserveAspectRatio", "none");
  visual.setAttribute("style", `
    width: 100%; height: ${h+h2}px;
    fill: currentColor;
    stroke: currentColor;
    stroke-width: 0.5px;
    stroke-linecap: round;
    stroke-linejoin: round;
  `);
  visual.classList.add("stark");
  const mountainRange = document.createElementNS("http://www.w3.org/2000/svg", "g");
  visual.appendChild(mountainRange);
  const shadowRange = document.createElementNS("http://www.w3.org/2000/svg", "g");
  visual.appendChild(shadowRange);
  shadowRange.setAttribute("style", `
    fill: transparent;
    stroke: transparent;
  `);
  const watermarks = document.createElementNS("http://www.w3.org/2000/svg", "g");
  visual.appendChild(watermarks);
  watermarks.setAttribute("style", `
    fill: none;
    stroke-width: 1px;
    opacity: 0.6;
    pointer-events: none;
  `);
  const outlineg = document.createElementNS("http://www.w3.org/2000/svg", "g");
  outlineg.setAttribute("style", `
    fill: none;
    stroke-width: 1px;
    pointer-events: none;
  `);
  visual.appendChild(outlineg);

  const ds = [];
  const segments = [];
  let segment = [];
  let o = [0,h];
  let d = "M"+[0,h];
  const l = hfs.graph.length - 1;
  const m = Math.max(...hfs.graph);
  for (let i=0; i<=m; i++) {
    const watermark = document.createElementNS("http://www.w3.org/2000/svg", "path");
    const mark = h*(1.0-i/m);
    watermark.setAttribute("d", "M"+[w*Math.max(0,i-1)/l, mark]+"L"+[w*(1.0-Math.max(0,i-1)/l), mark]);
    watermark.setAttribute("style", `stroke: ${bracket_color(Math.max(0, i-1))}`);
    watermark.classList.add("opacity-hover-child");
    watermarks.appendChild(watermark);
  }

  let x = 0, y = 0;
  const points = hfs.graph.slice(1, -1).map(
    (point, i, points) => ([i === 0 || i === points.length-1, point])
  );
  for (let [isEndpoint, point] of points) {
    segment.push(point-1);
    x += w/l;
    y = h*(1.0 - point/m);
    d += "L"+[x,y];
    if (point === 1 && !isEndpoint) {
      d += "L"+[x,h];
      let o1 = [(1*o[0] + 2*x)/3, h+h2];
      let o2 = [(2*o[0] + 1*x)/3, h+h2];
      d += "C"+[o1,o2,o];
      ds.push(d);
      segments.push(segment);
      segment = [0];
      d = "M"+[x,h]+"L"+[x,y];
      o = [x,h];
    }
  }
  x = w; y = h;
  d += "L"+[x,y];
  let o1 = [(1*o[0] + 2*x)/3, h+h2];
  let o2 = [(2*o[0] + 1*x)/3, h+h2];
  d += "C"+[o1,o2,o];
  ds.push(d);
  segments.push(segment);
  const count = ds.length - +(hfs === 0.0);
  let j = 0;
  for (let [isEndpoint, [d, segment]] of ds.map((d, i) => [d, segments[i]]).map(
    (point, i, points) => ([i === 0 || i === points.length-1, point])
  )) {
    const segmentValue = Math.max(...segment) <= 6 ? HFSfromGraph(segment) : "";

    const mountain = document.createElementNS("http://www.w3.org/2000/svg", "path");
    const shadow = document.createElementNS("http://www.w3.org/2000/svg", "rect");
    mountain.style.opacity = 0.75;
    mountain.setAttribute("d", d);
    mountainRange.appendChild(mountain);
    shadow.addEventListener("mouseenter", () => {
      mountain.style.opacity = 1;
      hoverLeft.textContent = padding + segmentValue.toString(10);
      hoverRight.textContent = hfs.bin ? "0b" + segmentValue.toString(2) + padding : "";
    });
    for (let [k, v] of Object.entries({x:w*j/l,y:0,width:w*(segment.length-+!isEndpoint)/l,height:h+h2})) {
      shadow.setAttribute(k, v);
    }
    j += segment.length-+!isEndpoint;
    shadow.addEventListener("mouseleave", () => {
      mountain.style.opacity = 0.75;
      hoverLeft.textContent = padding + "depth: " + hfs.depth;
      hoverRight.textContent = "count: " + count + padding;
    });
    shadowRange.appendChild(shadow);
  }

  const h3 = 24;
  const bracketsvg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  bracketsvg.setAttribute("width", w);
  bracketsvg.setAttribute("height", h3);
  bracketsvg.setAttribute("viewBox", `-2 -1 ${w+4} ${h3}`);
  bracketsvg.setAttribute("preserveAspectRatio", "none");
  bracketsvg.setAttribute("style", `
    width: 100%; height: ${h3}px;
    fill: currentColor;
    stroke: currentColor;
    stroke-width: 1px;
    stroke-linecap: round;
    stroke-linejoin: round;
    font-size: ${h3*0.5}px;
  `);
  const bracketg = document.createElementNS("http://www.w3.org/2000/svg", "text");
  if (!options.simple) {
    bracketsvg.appendChild(bracketg);
  }
  const angleg = document.createElementNS("http://www.w3.org/2000/svg", "g");
  bracketsvg.appendChild(angleg);

  const brackets = [];
  let last = undefined;
  for (let point of hfs.graph) {
    if (last !== undefined) {
      brackets.push(point > last);
    }
    last = point;
  }
  let last_depth = -1;
  let acc_depth = -1;
  for (let i in brackets) {
    let b = brackets[i];
    last_depth = acc_depth;
    acc_depth += b ? 1 : -1;
    let current_depth = Math.max(last_depth, acc_depth);
    let x1 = (+i+0)*w/brackets.length;
    let x2 = (+i+1)*w/brackets.length;
    let p = 2;
    let y1 = b ? h3-p : p;
    let y2 = b ? p : h3-p;

    const angle = document.createElementNS("http://www.w3.org/2000/svg", "path");
    angle.setAttribute("d", "M"+[x1,y1]+"L"+[x2,y2]);
    angle.style.stroke = bracket_color(current_depth);
    angle.classList.add("opacity-hover-child");
    angleg.appendChild(angle);

    const bracket = document.createElementNS("http://www.w3.org/2000/svg", "tspan");
    bracket.setAttribute("x", (x1+x2)/2);
    bracket.setAttribute("y", h3*0.7);
    bracket.setAttribute("textLength", w/brackets.length);
    bracket.setAttribute("style", `
      text-anchor: middle;
      stroke-width: 0px;
    `);
    bracket.textContent = "{}"[1-b];
    bracket.style.fill = bracket_color(current_depth);
    bracket.classList.add("opacity-hover-child");
    bracketg.appendChild(bracket);

    const outline = document.createElementNS("http://www.w3.org/2000/svg", "path");
    outline.setAttribute("d", "M"+[x1,h*(1.0 - (last_depth+1)/m)]+"L"+[x2,h*(1.0 - (acc_depth+1)/m)]);
    outline.style.stroke = bracket_color(current_depth);
    // Stack higher depth on top of lower depth, visually speaking.
    outlineg.insertBefore(outline, b ? null : outlineg.firstChild);
  }
  angleg.style.display = "revert";
  bracketg.style.display = "none";
  if (!options.simple) {
    bracketsvg.addEventListener("mouseenter", () => {
      bracketg.style.display = "revert";
      angleg.style.display = "none";
    });
    bracketsvg.addEventListener("mouseleave", () => {
      angleg.style.display = "revert";
      bracketg.style.display = "none";
    });
  }

  const wrapper = document.createElement("div");
  wrapper.classList.add("code");
  wrapper.classList.add("opacity-hover-container");

  const reprs = document.createElement("div");
  reprs.style = `display: flex; justify-content: space-between; white-space: pre; overflow-x: auto;`;
  if (!hfs.dec && !hfs.bin) {
    reprs.appendChild(toSpan("–"));
  } else {
    reprs.appendChild(toSpan(hfs.dec ? {className: "numeric", _:hfs.dec} : []));
    reprs.appendChild(toSpan(hfs.bin ? [" 0b", {className: "numeric", _:hfs.bin}] : []));
  }

  const codeRepr = document.createElement("div");
  codeRepr.style = `display: flex; justify-content: space-between; white-space: pre; overflow-x: auto; font-size: 65%; font-weight: normal`;
  codeRepr.appendChild(hfs.span);

  const hovers = document.createElement("div");
  const padding = "   ";
  hovers.style = `display: flex; justify-content: space-between; white-space: pre; margin-bottom: 1em;`;
  const hoverLeft = document.createElement("span");
  hoverLeft.textContent = padding + "depth: " + hfs.depth;
  hovers.appendChild(hoverLeft);
  const hoverRight = document.createElement("span");
  hoverRight.textContent = "count: " + count + padding;
  hovers.appendChild(hoverRight);

  wrapper.appendChild(reprs);
  if (!options.simple) wrapper.appendChild(codeRepr);
  wrapper.appendChild(visual);
  wrapper.appendChild(bracketsvg);
  if (!options.simple) wrapper.appendChild(hovers);

  return wrapper;
}
