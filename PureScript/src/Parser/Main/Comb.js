export const messageInBottle = msg => () => {
  window.sideChannel(msg)();
};
export const installSideChannel = cb => () => {
  window.sideChannel = cb;
};
