// https://bugzilla.mozilla.org/show_bug.cgi?id=1308431
// export const cancelAndHoldAtTime = param => time => () => param.cancelAndHoldAtTime(time);
export const cancelScheduledValues = param => time => () => param.cancelScheduledValues(time);
export const exponentialRampToValueAtTime = param => value => time => () => param.exponentialRampToValueAtTime(value, time);
export const linearRampToValueAtTime = param => value => time => () => param.linearRampToValueAtTime(value, time);
export const setTargetAtTime = param => value => time => timeConstant => () => param.setTargetAtTime(value, time, timeConstant);
export const setValueAtTime = param => value => time => () => param.setValueAtTime(value, time);
export const setValueCurveAtTime = param => values => time => duration => () => param.setValueCurveAtTime(values, time, duration);

export const defaultValue = param => param.defaultValue;
export const maxValue = param => param.maxValue;
export const minValue = param => param.minValue;
export const currentValue = param => () => param.value;
