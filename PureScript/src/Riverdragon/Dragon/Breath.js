export function unsafeSetProperty(el, name, val) {
  el[name] = val;
};
export function _setAttributeNS(ns, name, val, el) {
  el.setAttributeNS(ns, name, val);
};
export function _microtask(fn, ...args) {
  queueMicrotask(() => fn(...args));
};
