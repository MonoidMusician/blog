(function() {
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
function justDrawPixelGraph(canvas, color, fns) {
  const ctx = canvas.getContext('2d');
  ctx.imageSmoothingEnabled = false;

  ctx.strokeStyle = color;
  ctx.fillStyle = color;
  ctx.font = "italic 36px 'KaTeX_Math'";
  ctx.lineWidth = 2;

  ctx.beginPath();
  ctx.moveTo(0, canvas.height/2);
  ctx.lineTo(canvas.width, canvas.height/2);
  ctx.stroke();
  ctx.moveTo(canvas.width-15, canvas.height/2-12);
  ctx.lineTo(canvas.width, canvas.height/2);
  ctx.lineTo(canvas.width-15, canvas.height/2+12);
  ctx.closePath();
  ctx.fill();
  ctx.stroke();
  ctx.fillText('x', canvas.width-50, canvas.height/2+40);


  ctx.beginPath();
  ctx.moveTo(canvas.width/2, 0);
  ctx.lineTo(canvas.width/2, canvas.height);
  ctx.stroke();
  ctx.moveTo(canvas.width/2-12, 15);
  ctx.lineTo(canvas.width/2, 0);
  ctx.lineTo(canvas.width/2+12, 15);
  ctx.closePath();
  ctx.fill();
  ctx.stroke();
  ctx.fillText('y', canvas.width/2-40, 50);

  ctx.fillStyle = nearer(ctx.fillStyle);

  let block = 10;
  let buffer = 1;
  let m = -1/Math.PI;
  let x0 = canvas.width/2;
  let steps = Math.floor(x0/block);
  let y0 = canvas.height/2;

  for (let fn of fns) {
    for (let i = -steps; i <= steps; i+=1) {
      x = i*block;
      y = Math.round(-fn(i))*block;
      ctx.fillRect(x + x0 - block/2 + buffer, y + y0 - block/2 + buffer, block - buffer*2, block - buffer*2);
    }
  }
}
function autoDrawPixelGraph(canvas, ...fns) {
  drawPixelGraph(canvas, ...fns);
  const resizeObserver = new ResizeObserver(entries => {
    drawPixelGraph(canvas, ...fns)
  });
  resizeObserver.observe(canvas);
}
function graphsFromAttr(data_graph) {
  return eval(data_graph);
}
function loadPixelGraphs() {
  Array.prototype.forEach.call(document.querySelectorAll('canvas[data-graph].pixelated'), (canvas) => {
    autoDrawPixelGraph(canvas, ...graphsFromAttr(canvas.dataset.graph));
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

if (window.onLoadStyle) window.onLoadStyle(loadPixelGraphs);
})();
