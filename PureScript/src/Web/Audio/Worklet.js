const loaded = new WeakMap();
export const _loadAudioWorkletNode = audioContext => urlOrSource => name => onSuccess => onError => () => {
  try {
    if (!loaded.has(audioContext)) loaded.set(audioContext, new Map());
    const wait = loaded.get(audioContext).get(urlOrSource) ??
      audioContext.audioWorklet.addModule(
        URL.canParse(urlOrSource) ? urlOrSource : URL.createObjectURL(new Blob([urlOrSource], { type: "text/javascript" }))
      );
    loaded.get(audioContext).set(urlOrSource, wait);
    wait.then(() => {
      const mkNode = options => () => {
        const node = new AudioWorkletNode(audioContext, name, options);
        const send = value => () => node.port.postMessage(value);
        const receive = cb => () => {
          const listener = (event) => cb(event.data)();
          node.port.addEventListener('message', listener);
          return () => node.port.removeEventListener('message', listener);
        };
        const destroy = () => node.port.close();
        return { node, send, receive, destroy };
      };
      onSuccess(mkNode)();
    }, e => onError(e)());
  } catch(e) { onError(e); }
};
