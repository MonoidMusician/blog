export const installSideChannel = cb => () => {
  window.sideChannel = cb;
};
export const messageInBottle = msg => () => {
  window.sideChannel(msg)();
};
