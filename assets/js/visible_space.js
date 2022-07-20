function makeVisibleSpaces() {
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
        span.appendChild(document.createTextNode(value.substring(found, found+1)));
        replacements.push(span);
        i = found+1;
      }
      if (i === 0) return console.log("no:",value);
      if (i < value.length) {
        replacements.push(document.createTextNode(value.substring(i, value.length)));
      }
      console.log(e, replacements);
      replacements.forEach(function(r) {
        e.parentNode.insertBefore(r, e);
      });
      e.parentNode.removeChild(e);
    } else {
      forEach(e.childNodes, visit);
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
