---
title: Preferences
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

Set your preferences for this website.
(Stored in [`localStorage`{.js}](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage).)

## Styling

Note: website will flash while loading styles, sorry.

- Light mode:
  <input type="checkbox" id="light" value="light">

- Sans-serif:
  <input type="checkbox" id="sans" value="sans">

- User styles:
  <div class="sourceCode css"><pre><code><textarea id="user" style="min-height: 200px"></textarea></code></pre></div>
  <input id="color" type="color"></input>

<script>
var els = document.querySelectorAll('input[type="checkbox"][value]');
console.log(els);

var oldLoadStyle = loadStyle;
loadStyle = function shimmedLoadStyle(...args) {
  oldLoadStyle(...args);
  var style_choice = getStyle();
  Array.prototype.forEach.call(els, el => {
    el.checked = !!style_choice[el.value];
  });
}
var style_choice = getStyle();
Array.prototype.forEach.call(els, el => {
  el.checked = !!style_choice[el.value];
  console.log(style_choice, el.value, el.checked);
  el.onchange = function() {
    var style = getStyle();
    Array.prototype.forEach.call(els, el => {
      style[el.value] = el.checked;
    });
    loadStyle(style);
  };
});

var user = document.querySelector('textarea#user');
user.value = style_choice['user'] || "";
user.onchange = function() {
  var style = getStyle();
  style['user'] = user.value;
  loadStyle(style);
}

var color = document.querySelector('input#color');
color.addEventListener('input', (e) => {
  user.setRangeText(color.value);
  var style = getStyle();
  style['user'] = user.value;
  loadStyle(style);
});
</script>

## [TODO]{t=}
- generate permutations
- don't have whole style reload
- Sass user styles
- Nice user style editor?
