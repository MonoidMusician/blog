export const _loadAudioWorkletNode = audioContext => url => name => onSuccess => onError => () => {
  audioContext.audioWorklet.addModule(url).then((r) => {
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
};
