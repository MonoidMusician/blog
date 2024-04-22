(function() {
function nearer(color) {
  var [r,g,b] = [[1,3],[3,5],[5,7]].map(([i,j]) => parseInt(color.slice(i,j), 16));
  if ((r+g+b)/255 > 3/2) return '#fffff';
  return '#000000';
}
function drawPixelGraph(canvas, ...fns) {
  justDrawPixelGraph(canvas, detectColor(canvas), fns);
}
function detectColor(canvas) {
  return getComputedStyle(canvas).color;
}
function justDrawPixelGraph(canvas, color) {
  const ctx = canvas.children[0] || document.createElementNS("http://www.w3.org/2000/svg", "path");
  ctx.style.fill = 'transparent';
  ctx.style.stroke = color;
  canvas.appendChild(ctx);
  canvas.style.border = '1px solid ' + color;
}
var last;
var components = [];
var pending = [];
var diffT = 0;
var diffC = 0;
function asdf(canvas, color, event) {
  const ctx = canvas.children[0];
  var bb = canvas.getBoundingClientRect();
  // var ratio = canvas.width / bb.width;
  var ratio = 1;
  var x = event.pageX - (bb.x + window.scrollX);
  var y = event.pageY - (bb.y + window.scrollY);
  var active = event.buttons === 1;
  x *= ratio; y *= ratio;
  var next = {x, y, active, time: +new Date()};
  if (active) {
    if (last && last.active) {
      if (last.x !== next.x || last.y !== next.y) {
        if (!pending.length) requestAnimationFrame(() => {
          ctx.setPathData(ctx.getPathData().concat(pending));
          pending = [];
        });
        if (!components[components.length-1].length) {
          pending.push({type: "M", values: [last.x, last.y]});
        }
        pending.push({type: "L", values: [x, y]});
        components[components.length-1].push([last, next, next.time - last.time]);
        var diff = next.time - last.time;
        if (diff < 0.22*1000) {
          diffT += diff;
          diffC += 1;
          console.log(1000 / (diffT / diffC));
        }
      }
    } else {
      if (!components.length || components[components.length-1].length) {
        components.push([]);
      }
    }
  }
  last = next;
}
function autoDrawPixelGraph(canvas, ...fns) {
  var color = detectColor(canvas);
  var listen = event => asdf(canvas, color, event);
  drawPixelGraph(canvas, ...fns);
  canvas.addEventListener("mousedown", listen);
  canvas.addEventListener("mouseup", listen);
  canvas.addEventListener("mousemove", listen);
}
function loadPixelGraphs() {
  Array.prototype.forEach.call(document.querySelectorAll('svg[data-draw]'), (canvas) => {
    autoDrawPixelGraph(canvas);
  });
}
(function() {
  if (typeof exports !== 'undefined') {
    Object.assign(exports, { graphsFromAttr, justDrawPixelGraph });
  }
  if (typeof document === 'undefined') return;
  if (document.readyState === 'complete') {
    setTimeout(loadPixelGraphs, 15);
  } else {
    window.addEventListener('load', () => {
      setTimeout(loadPixelGraphs, 15);
    });
  }
})();
})();
