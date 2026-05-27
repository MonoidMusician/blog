// A Resource "Monad" that provides a Scope to track lifecycle things
// (destructors to run to finalize the scope, and waiters to run to load in
// stages and wait for everything to be available).
//
// It additionally provides extra ReaderT-style data, for passing around
// implicit context.
//
// It does not really work well with asynchronous programming (promises and
// callbacks), but see `Resource.saveScope` and `Resource.then`.

import { mintCleanup } from "./Bed";

// Scope keeps track of three categories: destructors, waiters, and extras.
export type Scope = {
  // Destructors can be added from anywhere
  addDestructor(cb: () => void): void,
  // Ask to destroy the scope
  destroy(): void,
  // Whether the scope has already been destroyed
  destroyed(): boolean,
  // Resolves when the scope is destroyed
  waitDestroyed: Promise<void>,

  // Add waiters that are interleaved by priority.
  addWaiters(waiters: Waiters): void,
  // Initiate the stages and wait for the result. Idempotent.
  wait(): Promise<void>,

  // Extra data in scope. Be careful around typing.
  extras: Map<any, any>;
};

// Waiters are indexed by priority.
export type Waiters = {
  [prio: number]: {
    // Sequential waiters: run in order
    seq: Array< () => Promise<void> >,
    // Sequential waiters: run in reverse order
    qes: Array< () => Promise<void> >,
    // Sequential waiters: run in order without waiting, after `seq` and `qes`
    par: Array< () => Promise<void> >,
  },
};
// Functions for waiters
export namespace Waiters {
  // Make sure a waiter is idempotent
  export function cache(cb: () => Promise<void>) {
    let cached: Promise<void>;
    return () => {
      if (!cached) cached = cb();
      return cached;
    };
  }
  export function cacheAll(waiters: Waiters): Waiters {
    return Object.fromEntries(Object.entries(waiters).map(([prio, cbs]) =>
      [prio, {
        seq: cbs.seq.map(Waiters.cache),
        qes: cbs.qes.map(Waiters.cache),
        par: cbs.par.map(Waiters.cache),
      }]
    ));
  }
  export function seq(prio: number, cb: () => Promise<void>) {
    return { [prio]: { seq: [cache(cb)], qes: [], par: [] } };
  }
  export function qes(prio: number, cb: () => Promise<void>) {
    return { [prio]: { seq: [], qes: [cache(cb)], par: [] } };
  }
  export function par(prio: number, cb: () => Promise<void>) {
    return { [prio]: { seq: [], qes: [], par: [cache(cb)] } };
  }
  export function addTo(w1: Waiters, w2: Waiters) {
    for (const prio in w2) {
      if (!w1[prio]) w1[prio] = { seq: [], qes: [], par: [] };
      w1[prio].seq.push(...w2[prio].seq);
      w1[prio].qes.push(...w2[prio].qes);
      w1[prio].par.push(...w2[prio].par);
    }
  }
  // Running drains it by priority. Waiters can be added while it is running,
  // and may set the priority back.
  export async function run(waiters: Waiters): Promise<void> {
    while (true) {
      const keys = Object.keys(waiters).map(prio => +prio).sort();
      if (!keys.length) return;
      const key = keys[0];
      const cbs = waiters[key];
      delete waiters[key];
      for (const cb of cbs.seq) await cb();
      for (const cb of cbs.qes.reverse()) await cb();
      await Promise.all(cbs.par.map(f => f()));
    }
  }
} // namespace Waiters

// An empty scope, does nothing. Mostly used for creating initial scopes.
export const noScope: Scope = {
  addDestructor: ()=>{},
  destroy: ()=>{},
  destroyed: ()=>false,
  waitDestroyed: Promise.any([]),
  addWaiters: ()=>{},
  wait: () => Promise.resolve(),
  extras: new Map(),
};

let _currentScope: Scope | null = null;

// Provide the scope for a single synchronous function call
export function withScope<Arg extends any[], R>(scope: Scope | null, body: (...arg: Arg) => R, ...arg: Arg): R {
  const saved = _currentScope;
  _currentScope = scope;
  const ret = body(...arg);
  _currentScope = saved;
  return ret;
};

// Get the scope that is currently provided
export function getScope(): Scope | null {
  return _currentScope;
};

// Make a scope inheriting from a parent scope.
export function mkSubscope(parent: Scope): Scope {
  if (parent.destroyed()) throw new Error("Parent scope already destroyed");
  let state = {
    destructors: [] as (()=>void)[],
    destroyed: false,
    waitDestroyed: Promise.withResolvers<void>(),
    waiters: {} as Waiters,
    waiting: Promise.withResolvers<void>(),
    waited: false,
  };
  let destroy = () => {
    state.destroyed = true;
    const destructors = state.destructors;
    state.destructors = [];
    const errors = [];
    for (let i=destructors.length-1; i>=0; i-=1)
      try { destructors[i]() }
      catch(e) { errors.push(e) }
    if (errors.length) {
      const message = "Error(s) during resource cleanup";
      const flattened = errors.flatMap(e =>
        (e instanceof AggregateError && e.message === message)
        ? e.errors : [e]
      );
      throw new AggregateError(flattened, message);
    }
  };
  parent.addDestructor(destroy);
  return {
    destroy: destroy,
    addDestructor: cb => {
      if (state.destroyed)
        throw new Error("Scope already destroyed");
      state.destructors.push(cb);
      return cb;
    },
    destroyed: () => state.destroyed,
    waitDestroyed: state.waitDestroyed.promise,
    addWaiters: (newWaiters: Waiters) => {
      // Because of interleaving, we cannot just add a single waiter to the
      // parent to proxy them, like we do with destructors. Instead, we make
      // sure they are idempotent, and add them to the parent as well.
      newWaiters = Waiters.cacheAll(newWaiters);
      Waiters.addTo(state.waiters, newWaiters);
      parent.addWaiters(newWaiters);
    },
    wait: () => Waiters.run(state.waiters).then(() => {
      state.waited = true;
      state.waiting.resolve();
    }),
    extras: parent.extras,
  };
};


// Inherit from multiple scopes at once. Monoid.
export function multiScope(scopes: Scope[]): Scope {
  scopes = _dedupBySet(scopes);
  if (scopes.length == 0) return noScope;
  if (scopes.length == 1) return scopes[0];
  return {
    addDestructor: cb => { for (const scope of scopes) scope.addDestructor(cb); },
    destroy: () => { for (const scope of scopes) scope.destroy(); },
    destroyed: () => { for (const scope of scopes) if (scope.destroyed()) return true; return false },
    waitDestroyed: Promise.all(scopes.map(scope => scope.waitDestroyed)).then(),
    addWaiters: (waiters: Waiters) => {
      waiters = Waiters.cacheAll(waiters);
      for (const scope of scopes) scope.addWaiters(waiters);
    },
    wait: () => Promise.all(scopes.map(scope => scope.wait())).then(),
    extras: new Map(scopes.flatMap(scope => Array.from(scope.extras.entries()))),
  };
};
// Keep order
function _dedupBySet<T>(items: T[]): T[] {
  const seen = new Set();
  return items.filter(item => {
    if ( seen.has(item)) return false;
    else seen.add(item); return true;
  })
}


export function start<Arg extends any[], R>(fn: (...arg: Arg) => R, ...arg: Arg): {
  result: R, scope: Scope, destroy: ()=>void, wait: ()=>Promise<void>
} {
  const scope = mkSubscope(_currentScope === null ? noScope : _currentScope);
  const result = withScope(scope, fn, ...arg);
  return { result: result, wait: scope.wait, scope: scope, destroy: scope.destroy };
};
export async function run<Arg extends any[], R>(fn: (...arg: Arg) => R, ...arg: Arg): Promise<{ result: R, scope: Scope, destroy: ()=>void }> {
  const r = start(fn, ...arg);
  await r.wait();
  return r;
};


export function bindScope<Arg extends any[], R>(scope: Scope | null, fn: (...arg: Arg) => R): (...arg: Arg) => R {
  return (...arg) => withScope(scope, fn, ...arg);
};
export function saveScope<Arg extends any[], R>(fn: (...arg: Arg) => R): (...arg: Arg) => R {
  return bindScope(getScope(), fn);
};
export function saveRevolvingScope<Arg extends any[], R>(fn: (...arg: Arg) => R): (...arg: Arg) => R {
  const revolving = oneSubScopeAtATime(getScope() ?? noScope);
  return (...arg) => withScope(revolving(), fn, ...arg);
};


export function oneSubScopeAtATime(parent: Scope): () => Scope {
  let destroy = ()=>{};
  return () => {
    const scope = mkSubscope(parent);
    destroy();
    destroy = scope.destroy;
    return scope;
  };
};

// (Try to?) save scope across an async pause.
export const then = <T>(x: Promise<T>): Promise<T> => {
  const scope = getScope();
  if (!scope) return x;
  x.then = (y, z, ...arg) => {
    return Object.getPrototypeOf(x).then.call(x,
      y && ((value: T) => withScope(scope, y, value)),
      z && ((reason: any) => withScope(scope, z, reason)),
      ...arg
    );
  };
  x.catch = (z, ...arg) => {
    return Object.getPrototypeOf(x).catch.call(x,
      z && ((reason: any) => withScope(scope, z, reason)),
      ...arg
    );
  };
  x.finally = (y, ...arg) => {
    return Object.getPrototypeOf(x).then.call(x,
      y && (() => withScope(scope, y)),
      ...arg
    );
  };
  return x;
};


// Helper for preserving type `T` in a `runner`: `yield*plz(promise)`,
// so it does not get lost in `any` soup
export const plz = function* plz<T>(promise: T):
  Generator<Promise<T>, Awaited<T>, Awaited<T>>
{
  return yield Promise.resolve(promise);
};
// Preserve the scope for every step of the runner
export function runner<Arg extends any[], R>(
  tracked: (...arg: Arg) => Generator<Promise<any>, R, any>
): (...arg: Arg) => Promise<R> {
  const scope = getScope()!;
  return (...arg) => {
    return then(new Promise<R>((complete, reject) => {
      const generating = tracked(...arg);
      let step = withScope(scope, () => generating.next());
      const pump = () => {
        if (step && step.done)
          complete(step.value);
        else if (!scope.destroyed())
          Promise.resolve(step.value).then(produced => {
            step = withScope(scope, () => generating.next(produced));
          }, reject);
      };
      pump();
    }));
  };
};



// Add and return a destructor: it must be in a scope. It is made idempotent.
export const addDestructor = (cb: ()=>void) => {
  cb = mintCleanup<void>(cb);
  getScope()!.addDestructor(cb);
  return cb;
};
// Add a destructor, or just return it. It is made idempotent either way.
export const tryAddDestructor = (cb: ()=>void) => {
  cb = mintCleanup<void>(cb);
  getScope()?.addDestructor(cb);
  return cb;
};
// Save the function to add a destructor
export const getAddDestructor = () => getScope()!.addDestructor;
// Get the function to destroy this scope
export const selfDestructor = () => getScope()!.destroy;
// Destroy this scope immediately
export const selfDestruct = () => getScope()!.destroy();

// Obtain a subscope of this scope
export const subScope = () => mkSubscope(getScope()!);

// Make a subscope that only lasts for the synchronous call
export function scoped<Arg extends any[], R>(body: (...arg: Arg) => R, ...arg: Arg): R {
  const scope = mkSubscope(getScope() ?? noScope);
  const ret = withScope(scope, body, ...arg);
  scope.destroy();
  return ret;
};
// Make a subscope
export function inSubScope<Arg extends any[], R>(body: (...arg: Arg) => R, ...arg: Arg): R {
  const scope = mkSubscope(getScope() ?? noScope);
  return withScope(scope, body, ...arg);
};
// Temporarily disable `addDestructor` for the subscope
export function impervious<Arg extends any[], R>(body: (...arg: Arg) => R, ...arg: Arg): R {
  const scope = mkSubscope(getScope() ?? noScope);
  scope.addDestructor = ()=>{};
  return withScope(scope, body, ...arg);
};

export function track<Arg extends any[], R extends { destroy: ()=>void }>(fn: (...arg: Arg) => R, ...arg: Arg): R {
  var ret = fn(...arg);
  addDestructor(ret.destroy);
  return ret;
};
export function unSub<Arg extends any[]>(fn: (...arg: Arg) => () => void, ...arg: Arg) {
  addDestructor(fn(...arg));
};

export function tryTrack<Arg extends any[], R extends { destroy: ()=>void }>(fn: (...arg: Arg) => R, ...arg: Arg) {
  var ret = fn(...arg);
  tryAddDestructor(ret.destroy);
  return ret;
};
export function tryUnSub<Arg extends any[]>(fn: (...arg: Arg) => () => void, ...arg: Arg) {
  tryAddDestructor(fn(...arg));
};



type Provider<V> = {
  provide: <Arg extends any[], R>(value: V, body: (...arg: Arg) => R, ...arg: Arg) => R,
  provided: () => V | undefined,
} & (() => V); // convenience function
// Mint a fresh `Provider`, using a unique `Symbol` so it is typesafe
export function mintProvider<V>(name?: string): Provider<V> {
  const key = Symbol(name);
  return provider<typeof key, V>(key, name);
};
// Provide a specific type, keyed by its class constructor
export function classProvider<T>(key: { prototype: T, name?: string }): Provider<T> {
  return provider<{ prototype: T }, T>(key, key.name);
};


// Through `extras: Map<any, any>` we can provide arbitrary data to callers
export function provide<K, V>(key: K): <Arg extends any[], R>(value: V, body: (...arg: Arg) => R, ...arg: Arg) => R {
  return <Arg extends any[], R>(value: V, body: (...arg: Arg) => R, ...arg: Arg): R => {
    const scope = getScope()!;
    const extras = new Map(scope.extras.entries());
    extras.set(key, value);
    return withScope<Arg, R>({ ...scope, extras }, body, ...arg);
  };
};
// Get what may have been provided
export function provided<K, V>(key: K): V | undefined {
  return getScope()?.extras.get(key);
};
export function provider<K, V>(key: K, name?: string): Provider<V> {
  const p = () => {
    const r = provided<K, V>(key);
    if (!r) throw new Error("" + (name ?? key) + "was not provided in this Resource scope");
    return r;
  };
  return Object.assign(p, {
    provide: provide<K, V>(key),
    provided: () => provided<K, V>(key),
  });
};

