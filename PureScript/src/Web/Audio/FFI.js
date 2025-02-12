export const _unsafeGetProperty = obj => prop => () => obj[prop];
export const _unsafeSetProperty = obj => prop => val => () => { obj[prop] = val };
export const _undefined = undefined;
