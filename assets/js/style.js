function getStyle() {
  return JSON.parse(localStorage['style-choice'] || "{}");
}
function onLoadStyle(fn) {
  var fns = onLoadStyle.fns || (onLoadStyle.fns = []);
  if (fn === loadStyle) {
    return function(...args) {
      for (let exec of fns) {
        exec(...args);
      }
    }
  } else {
    fns.push(fn);
  }
}
function loadStyle(new_choice, nowait) {
  var el =
    document.head.querySelector("link[href^='styles/bundled']")
    || document.head.appendChild(Object.assign(document.createElement("link"), {rel:"stylesheet"}));
  if (new_choice !== undefined) localStorage['style-choice'] = JSON.stringify(new_choice, undefined, 2);
  var style_choice = getStyle();
  var slug = "";
  for (let option of ["light", "sans", "highcontrast"]) {
    if (style_choice[option]) slug += "_" + option;
  }
  var src = "styles/bundled" + slug + ".css";

  function acknowledge() {
    if (!el.href.endsWith(src))
      el.href = src;

    var user_style = document.head.querySelector('style.user-style');
    if (style_choice['user'] && !user_style) {
      user_style = document.createElement('style');
      user_style.className = 'user-style';
      document.head.appendChild(user_style);
    }
    if (user_style) user_style.textContent = style_choice['user'];

    onLoadStyle(loadStyle)(new_choice);
  }
  if (nowait) {
    acknowledge();
  } else {
    // Load it into the browser cache first, to avoid flashing?
    fetch(src).then(r => r.text()).finally(acknowledge);
  }
}
loadStyle(undefined, true);
