---
title: Cofree Collective Blog
subtitle: |
  [Looking for [Solomonʼs
  blog](https://cofree.coffee/~solomon/)?]{.nowrap}
subtitle: ""
---

Introduction
------------

Welcome to our new collective blog for the
`cofree.coffee`{style="font-weight: 700;"} programming community! Weʼre
a bunch of programmers who hang out on Matrix, talk about functional
programming, programming languages, and more. Particular topics we tend
to discuss include methods for doing parsing & testing as well as the
design and implementation of programming languages. We believe that
through collaboration we can bring out the best in each other, and with
such a wealth of knowledge in our community, weʼre excited to share some
things weʼve learned and discussed with you.

```haskell
import Prelude
import Effect.Console (log)

greet :: String -> String
greet name = "Hello,\n...long•· drumroll...\n" <> name <> "!"

main = const (log (greet "World")) (Tuple 'c' 1)


```

```javascript
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


```

```css
.visible-space[data-whitespace="0x20"]::before {
  content: "·";
  transform: scale(0.8);
  opacity: 0.4;
  position: absolute;
  color: #509e9e;
  text-shadow: none;
}

.visible-space[data-whitespace="0xa"]::before {
  content: "¶";
  transform: scale(0.7);
  opacity: 0.3;
  position: absolute;
  color: #509e9e;
  text-shadow: none;
}

pre, code {
  position: relative;
}


```

Typography test
---------------

[Welcome to our lovely "blog" here! ñéŪ ]{.main}[ᾍδης
ᾇβγδζσπ]{.greek} and
then `this.do(the_thing') ᾍδης ᾇβγδζσπ <= => >= => := ::`\
Math would be $\infty-co^ol$ too.
$$\forall \epsilon > 0, \exists N > 0, \forall n, m > N, \\ \left|f(n)/n - f(m)/m\right| < \epsilon.$$

$$\begin{align*}\frac{f(x_2) - f(x_1)}{x_2 - x_1} &= \frac{(m*x_2 + b) - (m*x_1 + b)}{x_2 - x_1} \\&= \frac{(m*x_2 - m*x_1) + \cancel{(b - b)}}{x_2 - x_1} \\&= \frac{m*\cancel{(x_2 - x_1)}}{\cancel{x_2 - x_1}} \\&= m\end{align*}$$
