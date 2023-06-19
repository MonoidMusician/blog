(function() {
var canvas = document.getElementById('canvas');
var textarea = document.getElementById('textarea');
var color = document.getElementById('color');
var error = document.getElementById('error');
var ctx = canvas.getContext('webgl') || canvas.getContext('2d');
var prefix = "genart-code-";
var items = Array.from(localStorage).map((_, i) => localStorage.key(i)).filter(k => k.startsWith(prefix)).map(k => k.slice(prefix.length));
if (!localStorage.getItem("genart-active")) {
  localStorage.setItem("genart-active", "default");
}
var active = document.getElementById('active');
active.value = localStorage.getItem("genart-active");
var styling = document.querySelector('#canvas + style');
var initialStyle = canvas.style.cssText;

function loadScript(src) {
  var script = document.querySelector(`[src='${src}']`);
  if (script) return script;
  return document.head.appendChild(Object.assign(document.createElement("script"), {src}));
}
async function downloadFrames() {
  let zip = new JSZip();
  for (let i in framestore) {
    let z = `${i}`;
    while (z.length < 4) z = "0"+z;
    zip.file(`frame${z}.png`, framestore[i]);
  }
  let blob = await zip.generateAsync({
    type:"blob",
    compression: "DEFLATE",
    compressionOptions: {
        level: 9
    }
  });
  console.log(blob);
  let U = URL.createObjectURL(blob);

  let A = document.createElement("a");
  A.href = U; A.click();

  URL.revokeObjectURL(U);
}

var frame = 0;
var last_code = "";
window.framestore = [];

textarea.value = localStorage.getItem(prefix+localStorage.getItem("genart-active")) || "";
render();
if (localStorage.getItem("genart-active") === "default" && (!textarea.value || (items.length === 1 && items[0] === "default"))) {
  fetch("./assets/js/genart/default.js").then(r => r.text()).then(v => {
    textarea.value = v;
    render();
  });
}

function render() {
  if (textarea.value === last_code) {
    frame += 1;
    if (ctx instanceof CanvasRenderingContext2D) {
      ctx.clearRect(0, 0, canvas.width, canvas.height);
    }
  } else {
    last_code = textarea.value;
    frame = 0;
    framestore = [];
    canvas.width = canvas.width;
    canvas.style = initialStyle;
  }
  let code = textarea.value;
  document.getElementById('frame').textContent = frame;
  let args = {
    ctx,
    width: canvas.width,
    height: canvas.height,
    frame,
  };
  let errored;
  try {
    new Function(...Object.keys(args), code)(...Object.values(args));
    error.textContent = "";
    error.style.display = "none";
    errored = false;
  } catch(e) {
    errored = true;
    error.textContent = e.toString();
    error.style.display = "revert";
  }
  document.getElementById('frame').style.textDecoration =
    !errored ? 'none' : 'line-through';
  if (styling) {
    styling.textContent = `#canvas::backdrop { background: ${getComputedStyle(document.body).background} }`
  }
  localStorage.setItem(prefix+localStorage.getItem("genart-active"), textarea.value);
  if (document.getElementById('framestore').checked) {
    framestore[frame] = "";
    (async () => {
      framestore[frame] = await (await fetch(canvas.toDataURL('image/png'))).blob();
    })();
    if (renderloop && frame+1 === 3600) {
      clearInterval(renderloop); renderloop = undefined;
      downloadFrames();
    }
  }
  document.getElementById('framesstored').textContent = framestore.length;
}

let fps = 60;
let renderloop;
let click = false;
canvas.addEventListener('mousedown', (e) => {
  clearInterval(renderloop); renderloop = undefined;
  render();
  renderloop = setInterval(render, 1000/fps);
  click = true;
});
canvas.addEventListener('mouseup', (e) => {
  if (e.shiftKey) { click = false; return; }
  clearInterval(renderloop); renderloop = undefined;
});
canvas.addEventListener('mouseleave', (e) => {
  if (click) {
    clearInterval(renderloop); renderloop = undefined;
    click = false;
  }
});
let spaced = false;
document.addEventListener('keypress', (e) => {
  if (e.target === textarea) return;
  if (e.key === ' ') {
    e.preventDefault();
    if (spaced) return;
    spaced = !e.shiftKey; // no clue why this is not just `true`
    if (renderloop) {
      clearInterval(renderloop); renderloop = undefined;
      if (e.shiftKey) document.exitFullscreen();
    } else {
      render();
      renderloop = setInterval(render, 1000/fps);
      if (e.shiftKey) canvas.requestFullscreen().then(() => {
        frame -= 1;
        render();
      });
    }
  }
});
document.addEventListener('keyup', (e) => {
  if (e.key === ' ') {
    spaced = false;
  }
});
document.getElementById('enjoy').addEventListener('click', () => {
  canvas.requestFullscreen().then(() => {
    frame -= 1;
    render();
    if (!renderloop)
      renderloop = setInterval(render, 1000/fps);
  });
});
textarea.addEventListener('change', (e) => {render()});
textarea.addEventListener('keydown', (e) => {
  if (e.key === "Enter" && (e.ctrlKey || e.metaKey)) {
    if (!renderloop) {
      render();
      renderloop = setInterval(render, 1000/fps);
    }
  }
  if (e.key === "Escape") {
    e.target.blur();
  }
});
textarea.addEventListener('keyup', (e) => {
  if (e.key === "Enter" || !(e.ctrlKey || e.metaKey)) {
    clearInterval(renderloop); renderloop = undefined;
  }
});
textarea.addEventListener('input', () => {
  document.getElementById('frame').style.textDecoration =
    textarea.value === last_code ? 'none' : 'line-through';
});
color.addEventListener('input', (e) => {
  textarea.setRangeText(color.value);
  render();
});
active.addEventListener('change', (e) => {
  if (!e.target.value) {
    e.target.value = localStorage.getItem("genart-active");
    return;
  }
  localStorage.setItem("genart-active", e.target.value);
  if (localStorage.getItem(prefix+localStorage.getItem("genart-active"))) {
    textarea.value = localStorage.getItem(prefix+localStorage.getItem("genart-active"));
  }
  render();
});
active.addEventListener('keydown', (e) => {
  if (e.key === "Enter" && (e.ctrlKey || e.metaKey)) {
    if (!e.target.value) return;
    localStorage.setItem("genart-active", e.target.value);
    render();
  }
});
document.getElementById('fullscreen').addEventListener('click', () => {
  canvas.requestFullscreen().then(() => {
    frame -= 1;
    render();
  });
});
document.getElementById('download').addEventListener('click', () => {
  clearInterval(renderloop); renderloop = undefined;
  downloadFrames();
});
document.getElementById('framestore').addEventListener('click', () => {
  loadScript("./assets/js/lib/jszip.min.js");
});
canvas.addEventListener('fullscreenchange', () => {
  if (document.fullscreenElement) return;
  frame -= 1;
  render();
});
})();
