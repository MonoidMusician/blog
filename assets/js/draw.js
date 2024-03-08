function nearer(color) {
  var [r,g,b] = [[1,3],[3,5],[5,7]].map(([i,j]) => parseInt(color.slice(i,j), 16));
  if ((r+g+b)/255 > 3/2) return '#fffff';
  return '#000000';
}
function sizeCanvas(canvas) {
  // Guard against exponential size blow-up if setting the canvas size affects the display size
  var original_width = canvas.width;
  var display_width = canvas.getBoundingClientRect().width;
  var desired_width = Math.round(display_width / 2)*4;
  canvas.width = desired_width;
  if (canvas.getBoundingClientRect().width != display_width) {
    console.log("Resetting from ", canvas.width, " to ", original_width, " since ", canvas.getBoundingClientRect().width," != ",display_width);
    canvas.width = original_width;
  }
  canvas.height = Math.round(canvas.width / 4)*2;
}
function drawPixelGraph(canvas, ...fns) {
  sizeCanvas(canvas);
  justDrawPixelGraph(canvas, detectColor(canvas), fns);
}
function detectColor(canvas) {
  return getComputedStyle(canvas).color;
}
function justDrawPixelGraph(canvas, color) {
  const ctx = canvas.getContext('2d');

  ctx.strokeStyle = color;
  ctx.fillStyle = color;
  ctx.lineWidth = 2;

  ctx.beginPath();
  ctx.moveTo(0, 0);
  ctx.lineTo(0, canvas.height);
  ctx.lineTo(canvas.width, canvas.height);
  ctx.lineTo(canvas.width, 0);
  ctx.lineTo(0, 0);
  ctx.stroke();

  ctx.fillStyle = nearer(ctx.fillStyle);
}
var last;
function asdf(canvas, color, event) {
  const ctx = canvas.getContext('2d');
  var bb = canvas.getBoundingClientRect();
  var ratio = canvas.width / bb.width;
  var x = event.pageX - (bb.x + window.scrollX);
  var y = event.pageY - (bb.y + window.scrollY);
  var active = event.buttons === 1;
  x *= ratio; y *= ratio;
  if (last) {
    if (active && last.active) {
      ctx.beginPath();
      ctx.moveTo(last.x, last.y);
      ctx.lineTo(x, y);
      ctx.stroke();
    }
  } else {
    console.log(event);
  }
  last = {x, y, active};
}
function autoDrawPixelGraph(canvas, ...fns) {
  var color = detectColor(canvas);
  var listen = event => asdf(canvas, color, event);
  drawPixelGraph(canvas, ...fns);
  const resizeObserver = new ResizeObserver(entries => {
    drawPixelGraph(canvas, ...fns)
  });
  resizeObserver.observe(canvas);
  canvas.addEventListener("mousedown", listen);
  canvas.addEventListener("mouseup", listen);
  canvas.addEventListener("mousemove", listen);
}
function loadPixelGraphs() {
  Array.prototype.forEach.call(document.querySelectorAll('canvas[data-draw]'), (canvas) => {
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
