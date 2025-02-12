const loaded = new WeakMap();
export const _loadAudioWorkletNode = audioContext => url => name => onSuccess => onError => () => {
  try {
    if (!loaded.has(audioContext)) loaded.set(audioContext, new Map());
    const wait = loaded.get(audioContext).get(url) ??
      audioContext.audioWorklet.addModule(
        URL.canParse(url) ? url : URL.createObjectURL(new Blob([url], { type: "text/javascript" }))
      );
    loaded.get(audioContext).set(url, wait);
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
