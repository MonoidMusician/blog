var found = true;
var canvas = document.getElementById("texture");
if (!canvas) {
  found = false;
  canvas = document.createElement("canvas");
  canvas.width = 300;
  canvas.height = 300;
}

var width = canvas.width;
var height = canvas.height;
var size = Math.min(width, height);
var ctx = canvas.getContext("2d");

function setBackground() {
  document.body.style.backgroundImage = `url(${canvas.toDataURL()})`
  if (found) {
    document.body.style.backgroundSize = `${canvas.clientWidth}px ${canvas.clientHeight}px`;
  } else {
    document.body.style.backgroundSize = `${width/2}px ${height/2}px`;
  }
  canvas.style.opacity = 0;
}

if (found) {
  canvas.onclick = setBackground;
} else {
  setTimeout(setBackground, 0);
}

function dim(value, against) {
  if (Array.isArray(value))
    return value[0]*against + (value[1] || 0);
  return value*against;
}

function circle(f, x, y, r, dashes, duty_off, rot) {
  if (duty_off >= 1) return;
  if (!dashes || !duty_off) {
    ctx.beginPath();
    ctx.ellipse(dim(x, size), dim(y, size), dim(r, size)/2, dim(r, size)/2, 0, 0, 2*Math.PI);
    f();
  } else {
    var dash_angle = 2*Math.PI / dashes;
    var dash_width = dash_angle * (1 - (duty_off || 0));
    rot = rot || 0;
    rot *= dash_angle;
    rot -= dash_width / 2;
    for (var d = 0; d < dashes; d += 1) {
      ctx.beginPath();
      ctx.ellipse(dim(x, size), dim(y, size), dim(r, size)/2, dim(r, size)/2, 0, rot + d*dash_angle, rot + d*dash_angle + dash_width, false);
      f();
    }
  }
}
function rect(f, x, y, w, h) {
  ctx.beginPath();
  x = dim(x, size);
  y = dim(y, size);
  w = dim(w, size);
  h = dim(h, size);
  ctx.rect(x - w/2, y - h/2, w, h);
  f();
}
function crispRect(f, x, y, w, h) {
  ctx.beginPath();
  x = Math.round(dim(x, size));
  y = Math.round(dim(y, size));
  w = Math.round(dim(w, size)/2)*2;
  h = Math.round(dim(h, size)/2)*2;
  ctx.rect(x - w/2, y - h/2, w, h);
  f();
}

function squareGrid(xs, ys, primary, secondary) {
  for (var xi = 0; xi <= xs; xi += 1) {
    var x = xi/xs;
    for (var yi = 0; yi <= ys; yi += 1) {
      var y = yi/ys;
      primary(x, y, 1/xs, 1/ys);
      var x2 = x + 0.5/xs;
      var y2 = y + 0.5/ys;
      if (secondary && x2 <= 1 && y2 <= 1) {
        secondary(x2, y2, 1/xs, 1/ys);
      }
    }
  }
}
function squareGridAlt(xs, ys, primary) {
  for (var yi = 0; yi <= ys; yi += 1) {
    var y = yi/ys;
    for (var xi = (yi % 2) / 2; xi <= xs; xi += 1) {
      var x = xi/xs;
      primary(x, y, 1/xs, 1/ys);
    }
  }
}

function filter(i) {
  return i;
  return 0xF-i;
}
function color(c, a) {
  var cf = Array.from(c).map(i => filter(Number.parseInt(i, 16)).toString(16)).join("");
  return "#"+cf+a.toString(16);
}

function siny(p) {
  return Math.sin((p % 1)*Math.PI);
}
function cosy(p) {
  return Math.cos((p % 1)*Math.PI);
}
function sq(p) {
  p -= 0.5;
  p = p*p*Math.sign(p);
  p += 0.5;
  return p;
}
function sqr(p) {
  p -= 0.5;
  p = Math.sqrt(Math.abs(p))*Math.sign(p);
  p += 0.5;
  return p;
}
function saw(p, r) {
  return p * r % r;
}

var n = Math.round(50/1500*size / 2) * 2;
var s = 2;
ctx.globalAlpha = 0.1;
squareGridAlt(n, n, function(x, y, d) {
  ctx.strokeStyle = color("700", 0xb);
  ctx.lineWidth = s;
  circle(() => ctx.stroke(), x, y, [d * 0.9, -s/2], 20, 0.5);
  ctx.strokeStyle = color("b00", 0x3);
  ctx.lineWidth = s*3;
  circle(() => ctx.stroke(), x, y, [d * 0.7, -s/2*3], 5, 0.25, 3*x+2*y);
  ctx.strokeStyle = color("700", 0x5);
  ctx.lineWidth = s;
  circle(() => ctx.stroke(), x, y, [d * 0.3, s/2], 5, 0.3, x);
  ctx.fillStyle = color("700", 0x1);
  circle(() => ctx.fill(), x, y, d * 0.15);
}, function(x, y, d) {
  ctx.fillStyle = color("700", 0x2);
  circle(() => ctx.fill(), x, y, d * 0.15);
});

console.log(n);
//ctx.fillStyle = "#000"; ctx.fillRect(0, 0, width, height);

squareGridAlt(n, n, function(x, y, d) {
  var sc = v => 0.1 + 0.4 * v;
  ctx.globalAlpha = 1;
  ctx.globalCompositeOperation = "source-over";
  ctx.fillStyle = "#fff";
  crispRect(() => ctx.fill(), x, y, [d * sc(siny(saw(x, 4) + 0.5 + siny(saw(y, 2)))), 0], [d * sc(0.2+0.4*siny(saw(y, 4))), 0]);
  ctx.globalCompositeOperation = "xor";
  ctx.globalAlpha = 1;
  ctx.fillStyle = "#000";
  crispRect(() => ctx.fill(), x, y, [d * sc(siny(saw(x, 4) + 0.5 + siny(saw(y, 2)))), 0], [d * sc(0.2+0.4*siny(saw(y, 4))), 0]);
});
