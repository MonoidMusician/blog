type ResourceM<T> = (scope: Scope) => T;
type Scope = {
  putDestructor: (cb: () => void) => void,
  destroy: () => void,
  destroyed: () => boolean,
  waitDestroyed: Promise<void>,
  putWaiters: (priority: number, waiter: () => Promise<void>) => void,
  wait: Promise<void>,
};


