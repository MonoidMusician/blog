//ctx.canvas.style.backgroundColor = '#4d0536';
document.body.style.backgroundColor = '#1f232b';

let {width, height} = ctx.canvas;
let dim = [0, 0, width, height];
let diag = [0, height, width, 0];
let ctr = [0, height/2, width, height/2];

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
  for (let k of [173, -177, -135, 133, 151, -105]) {
    ctx.beginPath();
    k += 4 * (arc > 0);
    let [x,y] = [0, 250];
    let [x0, y0] = [x, y];
    ctx.moveTo(x, y);
    for (let i = 0; i < 9; i += 1) {
      x += 150;
      y = y0 + (45 + 20 * Math.sin(x * Math.PI / ctx.canvas.width)) * Math.sin((arc*k > 0 ? 1500 - x : x) / k * Math.PI) + arc * Math.sin(x * Math.PI / ctx.canvas.width);
      ctx.lineTo(x, y);
    }
    ctx.lineTo(ctx.canvas.width, y0);
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
/*strokes = [
  '#fff9',
  '#2d72c033',
  '#2f568d33',
  '#2f568d22',
  '#0a7d9133',
];/**/

ctx.globalCompositeOperation = 'hard-light';

const step = 0.091*Math.PI;
let placements = [[0, 250, step, 50]];
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
    for (let i=-4; i<=154; i+=0.3) {
      x = x0 + i*10;
      y = y0 - Math.sin(i * k) * (a + a * Math.sin(x * Math.PI / ctx.canvas.width));
      ctx.lineTo(x,y);
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

