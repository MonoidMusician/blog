let transpose = false;
let widescreen = true;

//ctx.canvas.style.backgroundColor = '#4d0536';
document.body.style.background = document.fullscreenElement ? '#4d0536' : '';
if (document.fullscreenElement) {
  document.body.style.background =
    transpose
    ? 'linear-gradient(to bottom, #3e042c, #4d0536)'
    : 'linear-gradient(to right, #3e042c, #4d0536)';
}
ctx.canvas.width = width = 1500;
ctx.canvas.height = height = width*(widescreen ? 9/16 : 1/3);
if (transpose) {
  [ctx.canvas.height, ctx.canvas.width] = [ctx.canvas.width, ctx.canvas.height];
}

ctx.reset();
if (transpose) {
  ctx.rotate(Math.PI / 2);
  ctx.scale(1, -1);
}

let dim = [0, 0, width, height];
let diag = [0, height, width, 0];
let ctr = [0, height/2, width, height/2];

let oo = 3600;
let looped = p => (oo/2)/Math.round((oo/2)/p);

let bounce = (range,v=undefined) => {
  if (v === undefined) v = frame;
  range = looped(range);
  let period = 2*range;
  let r = v % period;
  r = range - r;
  r = Math.abs(r);
  r = range - r;
  return r;
};
let sbounce = (range,v=undefined) => {
  range = looped(range);
  let r = bounce(range, v);
  r /= range;
  r *= 2;
  r -= 1;
  r = Math.sin(r * Math.PI/2);
  r += 1;
  r /= 2;
  r *= range;
  return r;
};

var gradient = ctx.createLinearGradient(...dim);
gradient.addColorStop(0, '#3e042cf0');
gradient.addColorStop(1, '#4d0536d0');
ctx.fillStyle = gradient;
ctx.fillRect(...dim);

let styles = [
  ['#f26db7', 15, 0.08],
  ['#ffe0f2', 1, 0.5],
];
for ([ctx.strokeStyle, ctx.lineWidth, ctx.globalAlpha] of styles) {
  for (let arc of [100, -120])
  for (let [j, k] of [173, -177, -135, 133, 151, -105].entries()) {
    ctx.beginPath();
    k += 4 * (arc > 0);
    let [x,y] = [0, height/2];
    let [x0, y0] = [x, y];
    ctx.moveTo(x, y);
    for (let i = 0; i < 9; i += 1) {
      x += width/10;
      y = y0 + (45 + 20 * Math.sin(x * Math.PI / width)) * Math.sin((arc*k > 0 ? 1500 - x : x) / k * Math.PI) + arc * Math.sin((x + Math.sign(arc*k) * bounce(3*(13+i))*3) * Math.PI / width);
      ctx.lineTo(x, y);
    }
    ctx.lineTo(width, y0);
    ctx.stroke();
  }
}

var gradient = ctx.createLinearGradient(...ctr);
gradient.addColorStop(0, '#3e042c00');
gradient.addColorStop(0.5, '#4d0536dd');
gradient.addColorStop(1, '#4d053600');
ctx.fillStyle = gradient;
ctx.fillRect(...dim);

let strokes = [
  '#fffb',
  '#cbfff144',
  '#8effe240',
  '#0aef5c33',
  '#0aef5c33',
];

ctx.globalCompositeOperation = 'hard-light';

const step = 0.091*Math.PI;
let placements = [[0, height/2, step, 50]];
for (let i = 1; i < 5; i += 1) {
  let placement = [...placements[placements.length - 1]];
  placement[0] -= 10;
  placement[2] *= 1.1236 - i/200;
  placement[3] -= 2 + i;
  placements.push(placement);
}
for (let [j, [x0,y0,k,a]] of placements.entries()) {
  for (let [i, stroke] of Array.from(strokes.entries()).reverse()) {
    ctx.globalAlpha = Math.min(1.0, 1-j/placements.length/1.1);
    ctx.strokeStyle = stroke;
    ctx.lineWidth = 4*i+1;
    let [x,y] = [x0, y0];
    ctx.beginPath();
    ctx.moveTo(x,y);
    let step = 0.5;
    for (let i=-2; i<=152; i+=step) {
      x += width/150*step;
      y = y0 - Math.sin(i * k + 0.1*sbounce(52+j)) * (a*.75 + a*1.75 * Math.sin((x + 3*sbounce(17)) * Math.PI / width));
      let pct = 0.8;
      let n = x/width*2-1;
      let phi = (Math.sin(n*Math.PI/2*pct)/Math.sin(Math.PI/2*pct)+1)/2*width;
      ctx.lineTo(phi,y);
    }
    ctx.stroke();
  }
}

ctx.globalCompositeOperation = 'source-over';

var gradient = ctx.createLinearGradient(...dim);
gradient.addColorStop(0, '#3e042c77');
gradient.addColorStop(0.25, '#3e042c00');
gradient.addColorStop(0.75, '#4d053600');
gradient.addColorStop(1, '#4d053677');
ctx.fillStyle = gradient;
ctx.fillRect(...dim);
