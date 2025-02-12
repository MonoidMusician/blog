export const disConnect = dis => ({ src, outputIndex }) => ({ dest, inputIndex }) => () => {
  src[dis ? 'disconnect' : 'connect'](dest, outputIndex, inputIndex);
};

export const start = node => time => () => node.start(time);
export const startNow = node => () => node.startNow(time);
export const stop = node => time => () => node.stop(time);
export const stopNow = node => () => node.stopNow(time);
export const whenEnded = node => cb => () => node.addEventListener("ended", () => cb());

export const _eqAudioSrc = o1 => o2 => o1.src === o2.src && o1.outputIndex === o2.outputIndex;
export const _eqAudioDest = o1 => o2 => o1.dest === o2.dest && o1.inputIndex === o2.inputIndex;

export const _unsafeCreateNode = name => ctx => options => () => {
  // Flatten any nested records out, to work with `class FFI t (Record ...)`
  const newOptions = Object.fromEntries(Object.entries(options).flatMap(
    ([k, v]) =>
      typeof v === 'object' && [Object.prototype, null].includes(Object.getPrototypeOf(v))
        ? Object.entries(v) : [[k, v]]
  ));
  return new window[name](ctx, newOptions);
};

export const _setOscillatorType = node => options => () => {
  // This is not spec compliant: the OscillatorNode prioritizes a not-undefined
  // `periodicWave: PeriodicWave` property, over the `type` (whyyyy lol)
  if (options.type === 'custom') {
    node.setPeriodicWave(options.periodicWave);
  } else {
    node.type = options.type;
  }
};

export const createPeriodicWave = ctx => disableNormalization => real => imag =>
  new PeriodicWave(ctx, { real, imag, disableNormalization });
