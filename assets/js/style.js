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
    function finish_load() {
      onLoadStyle(loadStyle)(new_choice);
      setTimeout(() => onLoadStyle(loadStyle)(new_choice), 10);
    }
    // https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link#stylesheet_load_events
    // Note: The load event fires once the stylesheet and all of its imported content has been loaded and parsed, and immediately before the styles start being applied to the content.
    el.addEventListener("load", () => {
      console.debug("Stylesheet loaded");
      requestAnimationFrame(() => finish_load());
    }, { once: true });
    // load is not firing on Chromium after initial load???
    requestAnimationFrame(() => finish_load());

    if (!el.href.endsWith(src))
      el.href = src;

    var user_style = document.head.querySelector('style.user-style');
    if (style_choice['user'] && !user_style) {
      user_style = document.createElement('style');
      user_style.className = 'user-style';
      document.head.appendChild(user_style);
    }
    if (user_style) user_style.textContent = style_choice['user'];
  }
  if (nowait) {
    acknowledge();
  } else {
    // Load it into the browser cache first, to avoid flashing?
    fetch(src).then(r => r.text()).finally(acknowledge);
  }
}
loadStyle(undefined, true);
