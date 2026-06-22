// A Resource "Monad" that provides a Scope to track lifecycle things
// (destructors to run to finalize the scope, and waiters to run to load in
// stages and wait for everything to be available).
//
// It additionally provides extra ReaderT-style data, for passing around
// implicit context.
//
// It does not really work well with asynchronous programming (promises and
// callbacks), but see `Resource.saveScope` and `Resource.then`.
import { mintCleanup } from "./Bed.js";
// Functions for waiters
export var Waiters;
(function (Waiters) {
    // Make sure a waiter is idempotent
    function cache(cb) {
        let cached;
        return () => {
            if (!cached)
                cached = cb();
            return cached;
        };
    }
    Waiters.cache = cache;
    function cacheAll(waiters) {
        return Object.fromEntries(Object.entries(waiters).map(([prio, cbs]) => [prio, {
                seq: cbs.seq.map(Waiters.cache),
                qes: cbs.qes.map(Waiters.cache),
                par: cbs.par.map(Waiters.cache),
            }]));
    }
    Waiters.cacheAll = cacheAll;
    function seq(prio, cb) {
        return { [prio]: { seq: [cache(cb)], qes: [], par: [] } };
    }
    Waiters.seq = seq;
    function qes(prio, cb) {
        return { [prio]: { seq: [], qes: [cache(cb)], par: [] } };
    }
    Waiters.qes = qes;
    function par(prio, cb) {
        return { [prio]: { seq: [], qes: [], par: [cache(cb)] } };
    }
    Waiters.par = par;
    function addTo(w1, w2) {
        for (const prio in w2) {
            if (!w1[prio])
                w1[prio] = { seq: [], qes: [], par: [] };
            w1[prio].seq.push(...w2[prio].seq);
            w1[prio].qes.push(...w2[prio].qes);
            w1[prio].par.push(...w2[prio].par);
        }
    }
    Waiters.addTo = addTo;
    // Running drains it by priority. Waiters can be added while it is running,
    // and may set the priority back.
    async function run(waiters) {
        while (true) {
            const keys = Object.keys(waiters).map(prio => +prio).sort();
            if (!keys.length)
                return;
            const key = keys[0];
            const cbs = waiters[key];
            delete waiters[key];
            for (const cb of cbs.seq)
                await cb();
            for (const cb of cbs.qes.reverse())
                await cb();
            await Promise.all(cbs.par.map(f => f()));
        }
    }
    Waiters.run = run;
})(Waiters || (Waiters = {})); // namespace Waiters
// An empty scope, does nothing. Mostly used for creating initial scopes.
export const noScope = {
    addDestructor: () => { },
    destroy: () => { },
    destroyed: () => false,
    waitDestroyed: new Promise(() => { }),
    addWaiters: () => { },
    wait: () => Promise.resolve(),
    extras: new Map(),
};
let _currentScope = null;
// Provide the scope for a single synchronous function call
export function withScope(scope, body, ...arg) {
    const saved = _currentScope;
    _currentScope = scope;
    const ret = body(...arg);
    _currentScope = saved;
    return ret;
}
;
// Get the scope that is currently provided
export function getScope() {
    return _currentScope;
}
;
// Make a scope inheriting from a parent scope.
export function mkSubscope(parent) {
    if (parent.destroyed())
        throw new Error("Parent scope already destroyed");
    let state = {
        destructors: [],
        destroyed: false,
        waitDestroyed: Promise.withResolvers(),
        waiters: {},
        waiting: Promise.withResolvers(),
        waited: false,
    };
    let destroy = () => {
        state.destroyed = true;
        const destructors = state.destructors;
        state.destructors = [];
        const errors = [];
        for (let i = destructors.length - 1; i >= 0; i -= 1)
            try {
                destructors[i]();
            }
            catch (e) {
                errors.push(e);
            }
        if (errors.length) {
            const message = "Error(s) during resource cleanup";
            const flattened = errors.flatMap(e => (e instanceof AggregateError && e.message === message)
                ? e.errors : [e]);
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
        addWaiters: (newWaiters) => {
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
}
;
// Inherit from multiple scopes at once. Monoid.
export function multiScope(scopes) {
    scopes = _dedupBySet(scopes);
    if (scopes.length == 0)
        return noScope;
    if (scopes.length == 1)
        return scopes[0];
    return {
        addDestructor: cb => { for (const scope of scopes)
            scope.addDestructor(cb); },
        destroy: () => { for (const scope of scopes)
            scope.destroy(); },
        destroyed: () => { for (const scope of scopes)
            if (scope.destroyed())
                return true; return false; },
        waitDestroyed: Promise.all(scopes.map(scope => scope.waitDestroyed)).then(),
        addWaiters: (waiters) => {
            waiters = Waiters.cacheAll(waiters);
            for (const scope of scopes)
                scope.addWaiters(waiters);
        },
        wait: () => Promise.all(scopes.map(scope => scope.wait())).then(),
        extras: new Map(scopes.flatMap(scope => Array.from(scope.extras.entries()))),
    };
}
;
// Keep order
function _dedupBySet(items) {
    const seen = new Set();
    return items.filter(item => {
        if (seen.has(item))
            return false;
        else
            seen.add(item);
        return true;
    });
}
export function start(fn, ...arg) {
    const scope = mkSubscope(_currentScope === null ? noScope : _currentScope);
    const result = withScope(scope, fn, ...arg);
    return { result: result, wait: scope.wait, scope: scope, destroy: scope.destroy };
}
;
export async function run(fn, ...arg) {
    const r = start(fn, ...arg);
    await r.wait();
    return r;
}
;
export function bindScope(scope, fn) {
    return (...arg) => withScope(scope, fn, ...arg);
}
;
export function saveScope(fn) {
    return bindScope(getScope(), fn);
}
;
export function saveRevolvingScope(fn) {
    const revolving = oneSubScopeAtATime(getScope() ?? noScope);
    return (...arg) => withScope(revolving(), fn, ...arg);
}
;
export function oneSubScopeAtATime(parent) {
    let destroy = () => { };
    return () => {
        const scope = mkSubscope(parent);
        destroy();
        destroy = scope.destroy;
        return scope;
    };
}
;
// (Try to?) save scope across an async pause.
export const then = (x) => {
    const scope = getScope();
    if (!scope)
        return x;
    x.then = (y, z, ...arg) => {
        return Object.getPrototypeOf(x).then.call(x, y && ((value) => withScope(scope, y, value)), z && ((reason) => withScope(scope, z, reason)), ...arg);
    };
    x.catch = (z, ...arg) => {
        return Object.getPrototypeOf(x).catch.call(x, z && ((reason) => withScope(scope, z, reason)), ...arg);
    };
    x.finally = (y, ...arg) => {
        return Object.getPrototypeOf(x).then.call(x, y && (() => withScope(scope, y)), ...arg);
    };
    return x;
};
// Helper for preserving type `T` in a `runner`: `yield*plz(promise)`,
// so it does not get lost in `any` soup
export const plz = function* plz(promise) {
    return yield Promise.resolve(promise);
};
// Preserve the scope for every step of the runner
export function runner(tracked) {
    const scope = getScope();
    return (...arg) => {
        return then(new Promise((complete, reject) => {
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
}
;
// Add and return a destructor: it must be in a scope. It is made idempotent.
export const addDestructor = (cb) => {
    cb = mintCleanup(cb);
    getScope().addDestructor(cb);
    return cb;
};
// Add a destructor, or just return it. It is made idempotent either way.
export const tryAddDestructor = (cb) => {
    cb = mintCleanup(cb);
    getScope()?.addDestructor(cb);
    return cb;
};
// A destructor that uses a weak ref, so it does not run if
// the resource has already been garbage collected
export const tryWeakDestructor = (resource, destroy) => {
    const ref = new WeakRef(resource);
    return tryAddDestructor(() => {
        const resource = ref.deref();
        if (resource !== undefined)
            destroy(resource);
    });
};
// Save the function to add a destructor
export const getAddDestructor = () => getScope().addDestructor;
// Get the function to destroy this scope
export const selfDestructor = () => getScope().destroy;
// Destroy this scope immediately
export const selfDestruct = () => getScope().destroy();
// Obtain a subscope of this scope
export const subScope = () => mkSubscope(getScope());
// Make a subscope that only lasts for the synchronous call
export function scoped(body, ...arg) {
    const scope = mkSubscope(getScope() ?? noScope);
    const ret = withScope(scope, body, ...arg);
    scope.destroy();
    return ret;
}
;
// Make a subscope
export function inSubScope(body, ...arg) {
    const scope = mkSubscope(getScope() ?? noScope);
    return withScope(scope, body, ...arg);
}
;
// Temporarily disable `addDestructor` for the subscope
export function impervious(body, ...arg) {
    const scope = mkSubscope(getScope() ?? noScope);
    scope.addDestructor = () => { };
    return withScope(scope, body, ...arg);
}
;
export function track(fn, ...arg) {
    var ret = fn(...arg);
    addDestructor(ret.destroy);
    return ret;
}
;
export function unSub(fn, ...arg) {
    addDestructor(fn(...arg));
}
;
export function tryTrack(fn, ...arg) {
    var ret = fn(...arg);
    tryAddDestructor(ret.destroy);
    return ret;
}
;
export function tryUnSub(fn, ...arg) {
    tryAddDestructor(fn(...arg));
}
;
// Mint a fresh `Provider`, using a unique `Symbol` so it is typesafe
export function mintProvider(name) {
    const key = Symbol(name);
    return provider(key, name);
}
;
// Provide a specific type, keyed by its class constructor
export function classProvider(key) {
    return provider(key, key.name);
}
;
// Through `extras: Map<any, any>` we can provide arbitrary data to callers
export function provide(key) {
    return (value, body, ...arg) => {
        const scope = getScope();
        const extras = new Map(scope.extras.entries());
        extras.set(key, value);
        return withScope({ ...scope, extras }, body, ...arg);
    };
}
;
// Get what may have been provided
export function provided(key) {
    return getScope()?.extras.get(key);
}
;
export function provider(key, name) {
    const p = () => {
        const r = provided(key);
        if (!r)
            throw new Error("" + (name ?? key) + "was not provided in this Resource scope");
        return r;
    };
    return Object.assign(p, {
        provide: provide(key),
        provided: () => provided(key),
    });
}
;
