let strokes = [
  '#fff4',
  '#3bf14233',
  '#3bf14222',
  '#00a82a44',
];

//ctx.canvas.style.backgroundColor = '#000000';
const step = 0.075*Math.PI;
for (let [i, stroke] of Array.from(strokes.entries()).reverse()) {
  let [x,y] = [0, 250];
  ctx.strokeStyle = stroke;
  ctx.lineWidth = 7*i+1;
  ctx.moveTo(x,y);
  for (let i=-1; i<=51; i+=0.1) {
    x = i*10;
    y = 250 - Math.sin(i * step) * 40;
    ctx.lineTo(x,y);
  }
  ctx.stroke();
}
