((n) => {
  const es = new EventSource('?watch');
  es.onmessage = _ => {
    if (n++) {
      es.close();
      location.reload();
    }
  };
})(0);
