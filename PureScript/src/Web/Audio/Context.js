export const _createAudioContext = options => () => new AudioContext(options);
export const close = ctx => () => ctx.close();
export const suspend = ctx => () => ctx.suspend();
export const resume = ctx => () => ctx.resume();
