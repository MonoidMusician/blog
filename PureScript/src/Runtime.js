export const _installSideChannel = name => cb => () => {
  if (typeof window.sideChannel === 'undefined') window.sideChannel = {};
  window.sideChannel[name] = cb;
};
export const _messageInABottle = name => msg => () => {
  window.sideChannel[name](msg)();
};
export const _specialCacheSet = name => value => () => {
  if (typeof window.specialCache === 'undefined') window.specialCache = {};
  window.specialCache[name] = value;
};
export const _specialCacheGet = name => () => {
  if (name in window.specialCache) return window.specialCache[name];
  throw new Error("Key not present in specialCache: " + name);
};
