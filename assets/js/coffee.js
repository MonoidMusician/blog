var canvas = document.getElementById('canvas');
var ctx = canvas.getContext('2d');
var generation = 0;
function spy(v) {
  console.log(v);
  return v;
}
function getImageData(xtc=undefined) {
  xtc = xtc || ctx;
  return xtc.getImageData(0, 0, xtc.canvas.width, xtc.canvas.height);
}
function setImageData(imageData, xtc=undefined) {
  return (xtc||ctx).putImageData(imageData, 0, 0);
}
function getAt(imageData, x, y) {
  const base = 4 * (x + imageData.width * y);
  return { r: imageData.data[base], g: imageData.data[base+1], b: imageData.data[base+2], a: imageData.data[base+3] };
}
function randomish(...bits) {
  return bits.reduce((a,b) => a+b, generation);
}
function toV(c, x, y) {
  var velocity = moddiv(c.r, 0x10).map(vi => {
    if (vi === 0xF) vi = randomish(x, y) % 0xF;
    if (vi < 0x8) {
      return vi;
    } else if (vi < 0xF) {
      return vi - 0xF;
    }
  });
  return { velocity, particles: c.g*0x100 + c.b, water: c.a };
}
function getVAt(imageData, x, y) {
  return toV(getAt(imageData, x, y));
}
function setAt(imageData, x, y, rgba) {
  const base = 4 * (x + imageData.width * y);
  imageData.data[base] = rgba.r;
  imageData.data[base+1] = rgba.g;
  imageData.data[base+2] = rgba.b;
  imageData.data[base+3] = rgba.a;
}
function moddiv(v, b=0x100) {
  return [v%0x100, (v-v%0x100)/0x100];
}
function fromV(v) {
  const c = {r:0,g:0,b:0,a:v.water};
  [c.b, v.particles] = moddiv(v.particles);
  [c.g, v.particles] = moddiv(v.particles);
  if (v.particles > 0) c.b = c.g = 0xFF;
  const vel = [...v.velocity].map(vi => {
    vi = Math.round(vi);
    if (Math.abs(vi) < 0x7) {
      if (vi >= 0) {
        return vi;
      } else {
        return vi + 0xF;
      }
    } else {
      return 0xF;
    }
  });
  c.r = vel[0] + vel[1]*0x100;
  return c;
}
function setVAt(imageData, x, y, v) {
  return setAt(imageData, x, y, fromV(v));
}
function modifyAt(imageData, x, y, f) {
  const c = getAt(imageData, x, y);
  setAt(imageData, x, y, f(c) || c);
}
function modifyVAt(imageData, x, y, f) {
  const v = getVAt(imageData, x, y);
  const r = f(v);
  setVAt(imageData, x, y, r || v);
}
function scan(f, xtc=undefined) {
  xtc = xtc || ctx;
  for (let x=0; x<xtc.canvas.width; x++) {
    for (let y=0; y<xtc.canvas.height; y++) {
      f(x,y);
    }
  }
}
console.log(getImageData());

//ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
ctx.arc(ctx.canvas.width/2, ctx.canvas.height/2, Math.min(ctx.canvas.width, ctx.canvas.height)/2.5, 0, 2*Math.PI, false);
ctx.strokeStyle = '#fff';
ctx.lineWidth = 4;
ctx.stroke();

function onImageData(f, xtc=undefined) {
  generation += 1;
  const imageDataIn = getImageData(xtc);
  const imageDataOut = getImageData(xtc);
  return setImageData(f(imageDataIn, imageDataOut) || imageDataOut, xtc);
}


// Each node only sets itself
// First water spills then particles
// Water dries up with time, locking particles in place
// Velocity …
// Particles bunch up on edges
function compare(V1, V2, dir, V2_) {
  let V1vel = V1.velocity[0]*dir[0] + V1.velocity[1]*dir[1];
  let V2vel = V2.velocity[0]*dir[0] + V2.velocity[1]*dir[1];
  let avg_vel = (V1vel+V2vel)/2;
  let water_diff = (V1.water - V2.water)/4;
  let particles_diff = (V1.particles - V2.particles)/4;

  V2_.velocity[0] += (avg_vel - V2vel)*dir[0];
  V2_.velocity[1] += (avg_vel - V2vel)*dir[1];
  V2_.water += water_diff;
  V2_.particles += particles_diff;
}

function spillToNeighbor(xtc=undefined) {
  return onImageData(function(imageDataIn, imageDataOut) {
    scan(function(x, y) {
      let { water, particles } = getVAt(imageDataIn, x, y);
      if (water < 0x10) return;
      const particles_available = (Math.max(0, particles - 20)) | 0;
      const water_available = (Math.max(0, water - 100) / 1) | 0;
      let water_used = 0;
      let particles_used = 0;
      if (particles_available > 0 || water_available > 0) {
        for (let dir of [[-1,0],[1,0],[0,1],[0,-1]]) {
          let [i, j] = dir;
          modifyVAt(imageDataOut, x, y, v => compare(getVAt(imageDataIn, x+i, y+j), getVAt(imageDataIn, x, y), dir, v));
        }
      }
    });
  }, xtc);
}
function render(xtc=undefined) {
  return onImageData(function(imageDataIn, imageDataOut) {
    scan(function(x, y) {
      const V = getVAt(imageDataIn, x, y);
      const c = {
        r: 0x18,
        g: 0x0b,
        b: 0x06,
        a: V.particles/0x100/0.25,
      };
      setAt(imageDataOut, x, y, c);
    });
  }, xtc);
}
canvas.addEventListener('click', (e) => {e.shiftKey?render():null});
canvas.addEventListener('mousedown', (e) => {spillToNeighbor();e.preventDefault()});

function spillToNeighbor0(xtc=undefined) {
  return onImageData(function(imageDataIn, imageDataOut) {
    scan(function(x, y) {
      let { water, particles } = getVAt(imageDataIn, x, y);
      if (water < 0x10) return;
      const particles_available = (Math.max(0, particles - 20)) | 0;
      const water_available = (Math.max(0, water - 100) / 1) | 0;
      let water_used = 0;
      let particles_used = 0;
      if (particles_available > 0 || water_available > 0) {
        for (let i of [-1, 1]) {
          for (let j of [-1, 1]) {
            modifyVAt(imageDataOut, x+i, y+j, v => {
              if (v.water < 0x10) {
                v.water += water_available;
                water_used += water_available / 4;
              } else {
                v.water += water_available / 4;
                water_used += water_available / 4 / 4;
                v.particles += particles_available / 4;
                particles_used += particles_available / 4;
              }
            });
          }
        }
        modifyVAt(imageDataOut, x, y, v => {
          v.water -= water_used;
          v.particles -= particles_used;
        });
      }
    });
  }, xtc);
}
