export type Maybe<T> = [] | [T];

export type Cb<T = void> = (value: T) => void;
export type Destr = Cb;
export function nocb() {}
export function consttrue() { return true }
export function constfalse() { return false }

export function freshId(): () => number {
  let id = 0;
  return () => id++;
}

export function cleanup<T>(...cbs: Cb<T>[]): Cb<T> {
  return function(value: T) {
    const saved = cbs;
    cbs = [];
    for (const cb of saved) cb(value);
  };
}
export function cleanupRunning<T>(cb: Cb<T>) {
  let running = true;
  return {
    cleanup: function(value: T) {
      const saved = cb;
      cb = nocb;
      saved(value);
      running = false;
    },
    running: () => running,
  };
}
export function rolling<T>(): (cb: Cb<T>, value: T) => void {
  let past: Cb<T> = nocb;
  return function(cb: Cb<T>, value: T) {
    const saved = past;
    let immediately: Maybe<T> = [];
    past = (newValue: T) => immediately = [newValue];
    saved(value);
    if (immediately.length) {
      past = nocb;
      cb(immediately[0]);
    } else {
      past = cb;
    }
  };
}

export function breakerResettable<T>(cb: Cb<T>) {
  let needsToRun = true;
  let me = {
    run: (value: T) => { if (needsToRun) cb(value); return me },
    trip: () => { needsToRun = false; return me },
    reset: () => { needsToRun = true; return me },
    running: () => needsToRun,
  };
  return me;
}
export function breaker<T>(cb: Cb<T>) {
  let me = {
    run: (value: T) => { cb(value); return me },
    trip: () => { cb = nocb; me.running = constfalse; return me },
    running: consttrue,
  };
  return me;
}

type Subscriptions<O> = {
  push: (value: O) => Destr,
  notify: (onValue: Cb<O>) => Subscriptions<O>,
  destroy: (onDestroy: Cb<O>) => void,
  size: () => number,
  running: () => boolean,
};

export function subscriptions<O>(): Subscriptions<O> {
  let ids = freshId();
  let listeners: { id: number, value: O }[] = [];
  let { cleanup: destroy, running } = cleanupRunning((onDestroy: Cb<O>) => {
    for (let i=0; i<listeners.length; i++)
      onDestroy(listeners[i].value);
    listeners = [];
  });
  let me = {
    notify: (onValue: Cb<O>) => {
      for (let i=0; i<listeners.length; i++)
        onValue(listeners[i].value);
      return me;
    },
    push: (value: O) => {
      if (!running()) return nocb;
      let id = ids();
      listeners.push({ id, value });
      return cleanup(() => {
        for (let i=0; i<listeners.length; i++) {
          if (listeners[i].id === id) {
            listeners.splice(i, 1);
            break;
          }
        }
      });
    },
    destroy,
    size: () => listeners.length,
    running,
  };
  return me;
}

// accumulator

// Only run it on the nth time.
export function threshold(n: number, cb: Cb): Cb {
  if (n < 1) { cb(); return nocb }
  let count = 0;
  return () => {
    if (!--count) {
      cb(); cb = nocb;
    }
  };
}

export function prealloc<A>(value: A): {
  get: () => A,
  set: (value: A) => void,
  swap: (value: A) => A,
} {
  return {
    get: () => value,
    set: next => { value = next },
    swap: next => { const prev = value; value = next; return prev }
  };
}

export function loading<R>(load: (isLoading: () => boolean) => R): R {
  let isLoading = true;
  const r = load(() => isLoading);
  isLoading = false;
  return r;
}

export function loadingBurst<A, R>(loader: (captureBurst: (value: A) => boolean) => ((burst: A[]) => R)): R {
  let bursts: A[] = [];
  const r = loading(isLoading =>
    loader((value: A) => {
      if (isLoading()) { bursts.push(value); return true }
      else return false;
    })
  );
  // FIXME GC
  return r(bursts);
}
