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
if (document.readyState === 'complete') {
  addStyleSettings();
} else {
  window.addEventListener('load', () => {
    addStyleSettings();
  });
}
