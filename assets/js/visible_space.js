function makeVisibleSpaces(tab_width=2) {
  var cls = 'visible-space';
  var chars = [' ', '\u00A0', '\t', '\n'];
  function findNext(str, idx) {
    var r = -1;
    chars.forEach(function(char) {
      var f = str.indexOf(char, idx);
      if (f !== -1 && (r === -1 || f < r)) {
        r = f;
      }
    });
    return r;
  }
  var forEach = Array.prototype.forEach.call.bind(Array.prototype.forEach);
  function visit(e) {
    if (e.nodeName === '#text') {
      if (e.parentNode.nodeName === 'SPAN' && e.parentNode.classList.contains(cls)) {
        return;
      }
      var value = e.nodeValue;
      var i = 0;
      var found = -1;
      var replacements = [];
      while ((found = findNext(value, i)) !== -1) {
        if (found > i) {
          replacements.push(document.createTextNode(value.substring(i, found)));
        }
        var span = document.createElement('SPAN');
        span.classList.add(cls);
        span.dataset['whitespace'] = "0x"+value.charCodeAt(found).toString(16);
        var ws = value.substring(found, found+1);
        if (ws === '\t' && tab_width) {
          ws = '';
          for (var z=0;z<tab_width;z++) {
            ws += ' ';
          }
        }
        span.appendChild(document.createTextNode(ws));
        replacements.push(span);
        i = found+1;
      }
      if (i === 0) return;
      if (i < value.length) {
        replacements.push(document.createTextNode(value.substring(i, value.length)));
      }
      replacements.forEach(function(r) {
        e.parentNode.insertBefore(r, e);
      });
      e.parentNode.removeChild(e);
    } else if (e.nodeName !== 'TEXTAREA') {
      // Convert the children to a static list so we don't iterate over what we are mutating
      forEach(Array.from(e.childNodes), visit);
      if (e.nodeName === 'CODE' && e.parentNode.nodeName === 'PRE'
        && e.lastChild.textContent && e.lastChild.textContent !== "\n"
        && !e.querySelector('textarea')) {
        var x = tab_width;
        if (+e.dataset['tab-width']) {
          tab_width = +e.dataset['tab-width'];
        }
        var span = document.createElement('SPAN');
        span.classList.add(cls);
        span.dataset['whitespace'] = "0x"+"\n".charCodeAt(0).toString(16);
        span.appendChild(document.createTextNode("\n"));
        e.appendChild(span);
        tab_width = x;
      }
    }
  }
  forEach(document.querySelectorAll('pre, code'), visit);
}
if (document.readyState === 'complete') {
  makeVisibleSpaces();
} else {
  window.addEventListener('load', () => {
    makeVisibleSpaces();
  });
}
