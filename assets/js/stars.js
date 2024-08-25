function paintStarsIn(ctx) {
  var w = ctx.canvas.width;
  var h = ctx.canvas.height;
  function rgb(R,G,B) {
    function rgba(A) {
      return `rgba(${[R,G,B,A]})`;
    }
    rgba.toString = () => `rgba(${[R,G,B,1]})`;
    return rgba;
  }
  var white = rgb(255, 255, 255);
  var primaries = [
    rgb(175, 68, 27),
    rgb(7, 73, 168),
    rgb(49, 144, 109),
  ];
  var r0 = 10;
  function radial(cx, cy, r, colors) {
    ctx.beginPath();
    ctx.ellipse(cx, cy, r, r, 0, 0, 2*Math.PI);
    var grad = ctx.createRadialGradient(cx, cy, 0, cx, cy, r);
    for (let [norm, color] of Object.entries(colors)) {
      grad.addColorStop(norm, color);
    }
    ctx.fillStyle = grad;
    ctx.fill();
  }
  function burst(cx, cy, r, colors, angle) {
    var coordss = [
      [cx, cy, cx, cy+r],
      [cx, cy, cx+r, cy],
      [cx, cy, cx-r, cy],
      [cx, cy, cx, cy-r],
    ];
    if (angle) angle = Math.PI*angle/180;
    if (angle) {
      ctx.translate(cx, cy);
      ctx.rotate(angle);
      ctx.translate(-cx, -cy);
    }
    for (let coords of coordss) {
      ctx.beginPath();
      ctx.moveTo(...coords.slice(0, 2));
      ctx.lineTo(...coords.slice(2, 4));
      var grad = ctx.createLinearGradient(...coords);
      for (let [norm, color] of Object.entries(colors)) {
        grad.addColorStop(norm, color);
      }
      ctx.strokeStyle = grad;
      ctx.lineWidth = 1.5;
      ctx.stroke();
    }
    if (angle) {
      ctx.translate(cx, cy);
      ctx.rotate(-angle);
      ctx.translate(-cx, -cy);
    }
  }
  var whiteRadial = {
    0.00: white(1.0),
    0.13: white(1.0),
    0.23: white(0.70),
    0.50: white(0.5),
    0.70: white(0.15),
    0.85: white(0.10),
    1.00: white(0.0),
  };
  var redRadial = {
    0.00: primaries[0](1.0),
    0.13: primaries[0](1.0),
    0.23: primaries[0](0.70),
    0.50: primaries[0](0.5),
    0.70: primaries[0](0.15),
    0.85: primaries[0](0.10),
    1.00: primaries[0](0.0),
  };
  var colorBurst = {
    0.00: white(0.7),
    0.10: white(0.8),
    0.25: primaries[0](1.0),
    0.35: primaries[0](1.0),
    0.50: primaries[1](0.75),
    0.65: primaries[0](0.4),
    0.90: white(0.05),
    1.00: white(0.0),
  };
  var colorBurstUnder = {
    0.00: white(0.05),
    0.15: white(0.1),
    0.55: white(0.2),
    0.85: white(0.01),
    1.00: white(0.0),
  };
  var blueing = {
    0.00: primaries[1](0.0),
    0.35: primaries[1](0.2),
    0.55: primaries[1](0.7),
    0.70: primaries[1](0.2),
    0.80: primaries[1](0.6),
    1.00: primaries[1](0.0),
  };
  var redding = {
    0.00: primaries[0](0.00),
    0.525: primaries[0](0.25),
    0.75: primaries[0](0.08),
    1.00: primaries[0](0.00),
  };
  var blueBurst = {
    0.00: white(0.9),
    0.25: primaries[1](0.9),
    0.55: primaries[1](0.7),
    0.80: primaries[1](0.6),
    1.00: primaries[1](0.0),
  };
  var stars = {
    point: (cx, cy) => {
      ctx.globalAlpha = 0.5;
      radial(cx, cy, r0*0.35, whiteRadial);
      ctx.globalAlpha = 1.0;
    },
    pointed: (cx, cy) => {
      radial(cx, cy, r0*0.35, redRadial);
      ctx.globalAlpha = 0.5;
      burst(cx, cy, r0*0.35*3, whiteRadial);
      ctx.globalAlpha = 1.0;
    },
    multi: (cx, cy) => {
      radial(cx-0.25, cy, r0*2, whiteRadial);
      radial(cx+0.25, cy+0.5, r0*4, redding);
      for (let angle of [35, 20, 65]) {
        ctx.globalAlpha = 0.5;
        burst(cx, cy, r0*5, redding, angle);
        ctx.globalAlpha = 1;
      }
      // ctx.globalCompositeOperation = "hue";
      radial(cx+0.25, cy+0.5, r0*2, blueing);
      ctx.globalCompositeOperation = "";
      burst(cx, cy, r0*7, colorBurstUnder);
      burst(cx, cy, r0*7, colorBurst);
    },
    bluey: (cx, cy) => {
      radial(cx, cy, r0, whiteRadial);
      radial(cx, cy, r0*1.5, blueing);
      radial(cx+0.25, cy+0.5, r0*2, redding);
      burst(cx, cy, r0*3, blueBurst);
      for (let angle of [65, 35]) {
        ctx.globalAlpha = 0.5;
        burst(cx, cy, r0*2, blueBurst, angle);
        ctx.globalAlpha = 1;
      }
    },
  };
  return stars;
}
