async function addStyleSettings() {
  var title = document.querySelector('h1.title');
  if (!title) return;
  async function svg(url, type) {
    let request = await fetch(url);
    let response = await request.text();
    let icon = response.replace(/#000000(?:00)?/g, 'currentColor');
    let span = document.createElement(type || 'span');
    span.className = 'icon iconoir';
    span.innerHTML = icon;
    return span;
  }
  var settings = await svg('assets/icons/iconoir/settings.svg', 'a');
  if (window.location.href.endsWith("/preferences.html")) {
    settings.onclick = () => history.back();
    settings.href = 'javascript:void(0)';
  } else {
    settings.href = 'preferences.html';
  }
  var brightness = await svg('assets/icons/iconoir/brightness.svg', 'button');
  brightness.onclick = function() {
    var style = getStyle();
    style.light = !style.light;
    loadStyle(style);
    brightness.style.transform = style.light ? 'rotateY(180deg)' : '';
  };
  brightness.style.transition = 'transform 1s';
  title.insertBefore(document.createTextNode(" "), title.firstChild);
  title.insertBefore(brightness, title.firstChild);
  title.appendChild(document.createTextNode(" "));
  title.appendChild(settings);
}
// https://developer.mozilla.org/en-US/docs/Web/API/Document/readyState
// 'loading', 'interactive', 'completed'
if (document.readyState === 'completed') {
  addStyleSettings();
} else {
  // https://developer.mozilla.org/en-US/docs/Web/API/Document/DOMContentLoaded_event
  // The DOMContentLoaded event fires when the HTML document has been completely parsed, and all deferred scripts (<script defer src="…"> and <script type="module">) have downloaded and executed. It doesn't wait for other things like images, subframes, and async scripts to finish loading.
  //
  // DOMContentLoaded does not wait for stylesheets to load, however deferred scripts do wait for stylesheets, and the DOMContentLoaded event is queued after deferred scripts. Also, scripts which aren't deferred or async (e.g. <script>) will wait for already-parsed stylesheets to load.
  //
  // A different event, load, should be used only to detect a fully-loaded page. It is a common mistake to use load where DOMContentLoaded would be more appropriate.
  window.addEventListener('DOMContentLoaded', () => {
    addStyleSettings();
  });
}
