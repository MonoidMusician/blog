export const _getPermissionStatusMIDI = options => onSuccess => onError => () => {
  navigator.permissions.query({ ...options, name: "midi" }).then(result => {
    onSuccess(result.state)();
  }, e => onError(e)());
};

export const _requestMIDI = options => onSuccess => onError => () => {
  navigator.requestMIDIAccess(options).then(access => {
    const inputs = Array.from(access.inputs.values());
    const outputs = Array.from(access.outputs.values());
    onSuccess({ inputs, outputs, access })();
  }, e => onError(e)());
};
export const access = access => () => {
  const inputs = Array.from(access.inputs.values());
  const outputs = Array.from(access.outputs.values());
  return { inputs, outputs };
};

export const open = midiPort => () => midiPort.open();
export const close = midiPort => () => midiPort.close();

export const onstatechange = midiPort => cb => () => {
  const listener = () => cb();
  midiPort.addEventListener('statechange', listener);
  return () => midiPort.removeEventListener('statechange', listener);
};
export const onmidimessage = midiInput => cb => () => {
  const listener = event => {
    cb(Array.from(event.data))();
  };
  midiInput.addEventListener('midimessage', listener);
  return () => midiInput.removeEventListener('midimessage', listener);
};
export const send = midiOutput => data => () => {
  midiOutput.send(data);
};
export const schedule = midiOutput => when => data => () => {
  midiOutput.send(data, when(performance.now()));
};

