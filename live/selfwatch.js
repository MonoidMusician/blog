"use strict";
{
  const hrefs = [...document.querySelectorAll("body script[src]")].map(x => x.src);
  ['', 'widgets.js', 'styles/bundled.css', ...hrefs].forEach(tgt => {
    let n = 0;
    const es = new EventSource(tgt+'?watch');
    es.onmessage = _ => {
      if (n++) {
        es.close();
        location.reload();
      }
    };
  });
}
