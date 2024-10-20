['', 'widgets.js', 'styles/bundled.css'].forEach(tgt => {
  let n = 0;
  const es = new EventSource(tgt+'?watch');
  es.onmessage = _ => {
    if (n++) {
      es.close();
      location.reload();
    }
  };
});
