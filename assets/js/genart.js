(function() {
var canvas = document.getElementById('canvas');
var textarea = document.getElementById('textarea');
var color = document.getElementById('color');
var ctx = canvas.getContext('2d');
var prefix = "genart-code-";
var items = Array.from(localStorage).map((_, i) => localStorage.key(i)).filter(k => k.startsWith(prefix)).map(k => k.slice(prefix.length));
if (!localStorage.getItem("genart-active")) {
  localStorage.setItem("genart-active", "default");
}
var active = document.getElementById('active');
active.value = localStorage.getItem("genart-active");
var initialStyle = canvas.style.cssText;

textarea.value = localStorage.getItem(prefix+localStorage.getItem("genart-active")) || "";
render();
if (localStorage.getItem("genart-active") === "default" && (!textarea.value || (items.length === 1 && items[0] === "default"))) {
  fetch("./assets/js/genart/default.js").then(r => r.text()).then(v => {
    textarea.value = v;
    render();
  });
}

function render() {
  canvas.width = canvas.width;
  canvas.style = initialStyle;
  let code = textarea.value;
  Function("ctx", code)(ctx);
  localStorage.setItem(prefix+localStorage.getItem("genart-active"), textarea.value);
}

canvas.addEventListener('click', (e) => {render()});
textarea.addEventListener('change', (e) => {render()});
textarea.addEventListener('keydown', (e) => {
  if (e.key === "Enter" && (e.ctrlKey || e.metaKey)) {
    render();
  }
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
})();
