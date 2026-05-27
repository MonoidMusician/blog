export type Maybe<T> = [] | [T];

export type Cb<T = void> = (value: T) => void;
export type Destr = Cb;
export function nocb() {}
export function consttrue() { return true }
export function constfalse() { return false }

export function mintCounter(): () => number {
  let id = 0;
  return () => id++;
};

export function mintCleanup<T>(...cbs: Cb<T>[]): Cb<T> {
  return function(value: T) {
    const saved = cbs;
    cbs = [];
    for (const cb of saved) cb(value);
  };
};
export function mintCleanupRunning<T>(cb: Cb<T>) {
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
};
export function mintRolling<T>(): (cb: Cb<T>, value: T) => void {
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
};

export function mintBreakerResettable<T>(cb: Cb<T>) {
  let needsToRun = true;
  let me = {
    run: (value: T) => { if (needsToRun) cb(value); return me },
    trip: () => { needsToRun = false; return me },
    reset: () => { needsToRun = true; return me },
    running: () => needsToRun,
  };
  return me;
};
export function mintBreaker<T>(cb: Cb<T>) {
  let me = {
    run: (value: T) => { cb(value); return me },
    trip: () => { cb = nocb; me.running = constfalse; return me },
    running: consttrue,
  };
  return me;
};

export type Subscriptions<O> = {
  push(value: O): Destr,
  notify(onValue: Cb<O>): Subscriptions<O>,
  destroy(onDestroy: Cb<O>): void,
  size(): number,
  running(): boolean,
};

export function mintSubscriptions<O>(): Subscriptions<O> {
  let ids = mintCounter();
  let listeners: { id: number, value: O }[] = [];
  let { cleanup: destroy, running } = mintCleanupRunning((onDestroy: Cb<O>) => {
    const errors = [];
    for (let i=0; i<listeners.length; i++)
      try { onDestroy(listeners[i].value) }
      catch(e) { errors.push(e) }
    if (errors.length) Promise.reject(new AggregateError(errors, "Error(s) during subscription cleanup"));
    listeners = [];
  });
  let me = {
    notify: (onValue: Cb<O>) => {
      const errors = [];
      for (let i=0; i<listeners.length; i++)
        try { onValue(listeners[i].value) }
        catch(e) { errors.push(e) }
      if (errors.length) Promise.reject(new AggregateError(errors, "Error(s) during subscription callbacks"));
      return me;
    },
    push: (value: O) => {
      if (!running()) return nocb;
      let id = ids();
      listeners.push({ id, value });
      return mintCleanup(() => {
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
};

// accumulator

// Only run it on the nth time.
export function mintThreshold(n: number, cb: Cb): Cb {
  if (n < 1) { cb(); return nocb }
  let count = 0;
  return () => {
    if (!--count) {
      cb(); cb = nocb;
    }
  };
};

export function mintCell<A>(value: A): {
  get: () => A,
  set: (value: A) => void,
  swap: (value: A) => A,
} {
  return {
    get: () => value,
    set: next => { value = next },
    swap: next => { const prev = value; value = next; return prev }
  };
};

export function whileLoading<R>(load: (isLoading: () => boolean) => R): R {
  let isLoading = true;
  const r = load(() => isLoading);
  isLoading = false;
  return r;
};

export function gatherLoadingBurst<A, R>(loader: (captureBurst: (value: A) => boolean) => ((burst: A[]) => R)): R {
  let bursts: A[] = [];
  const r = whileLoading(isLoading =>
    loader((value: A) => {
      if (isLoading()) { bursts.push(value); return true }
      else return false;
    })
  );
  return { result: r(bursts), sideEffect: bursts = [] }.result;
};


export type MintMap<K, V> = ReturnType<typeof mintMap<K, V>>;
export function mintMap<K, V>(): {
  get(k: K, df: () => V): V,
  get(k: K, df?: () => V): V | undefined,
  set(k: K, v: V | undefined): void,
  reset(del?: (k: K, v: V) => void): void,
  keys(): K[],
};
export function mintMap<K, V>() {
  const storage = new Map<K, V>();
  return {
    get: (k: K, df?: () => V) => {
      if (storage.has(k)) return storage.get(k);
      if (df == undefined) return undefined;
      const v = df();
      storage.set(k, v);
      return v;
    },
    set: (k: K, v: V | undefined) =>
      v !== undefined ? storage.set(k, v) : storage.delete(k),
    reset: (onEach?: (k: K, v: V) => void) => {
      if (!onEach) return storage.clear();
      const saved = storage.entries();
      storage.clear();
      for (const [k, v] of saved)
        onEach(k, v);
    },
    keys: () => [...storage.keys()],
  };
};
