export const _devicePixelRatio = {
  now: () => window.devicePixelRatio,
  subscribe: cb => () => {
    let active = true;
    let rolling = () => {};
    const untilNext = cb => {
      const mediaQueried = window.matchMedia(`(resolution: ${window.devicePixelRatio}dppx)`);
      mediaQueried.addEventListener('change', cb);
      rolling = () => mediaQueried.removeEventListener('change', cb);
    };
    const onChange = () => {
      console.log('Changed!');
      rolling();
      if (!active) return;
      cb(window.devicePixelRatio)();
      untilNext(onChange);
    }
    untilNext(onChange);
    return () => { active=false; rolling() };
  },
};
