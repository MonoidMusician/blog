export function nocb() { }
export function consttrue() { return true; }
export function constfalse() { return false; }
export function mintCounter() {
    let id = 0;
    return () => id++;
}
;
export function mintCleanup(...cbs) {
    return function (value) {
        const saved = cbs;
        cbs = [];
        for (const cb of saved)
            cb(value);
    };
}
;
export function mintCleanupRunning(cb) {
    let running = true;
    return {
        cleanup: function (value) {
            const saved = cb;
            cb = nocb;
            saved(value);
            running = false;
        },
        running: () => running,
    };
}
;
export function mintRolling() {
    let past = nocb;
    return function (cb, value) {
        const saved = past;
        let immediately = [];
        past = (newValue) => immediately = [newValue];
        saved(value);
        if (immediately.length) {
            past = nocb;
            cb(immediately[0]);
        }
        else {
            past = cb;
        }
    };
}
;
export function mintBreakerResettable(cb) {
    let needsToRun = true;
    let me = {
        run: (value) => { if (needsToRun)
            cb(value); return me; },
        trip: () => { needsToRun = false; return me; },
        reset: () => { needsToRun = true; return me; },
        running: () => needsToRun,
    };
    return me;
}
;
export function mintBreaker(cb) {
    let me = {
        run: (value) => { cb(value); return me; },
        trip: () => { cb = nocb; me.running = constfalse; return me; },
        running: consttrue,
    };
    return me;
}
;
export function mintSubscriptions() {
    let ids = mintCounter();
    let listeners = [];
    let { cleanup: destroy, running } = mintCleanupRunning((onDestroy) => {
        const errors = [];
        for (let i = 0; i < listeners.length; i++)
            try {
                onDestroy(listeners[i].value);
            }
            catch (e) {
                errors.push(e);
            }
        if (errors.length)
            Promise.reject(new AggregateError(errors, "Error(s) during subscription cleanup"));
        listeners = [];
    });
    let me = {
        notify: (onValue) => {
            const errors = [];
            for (let i = 0; i < listeners.length; i++)
                try {
                    onValue(listeners[i].value);
                }
                catch (e) {
                    errors.push(e);
                }
            if (errors.length)
                Promise.reject(new AggregateError(errors, "Error(s) during subscription callbacks"));
            return me;
        },
        push: (value) => {
            if (!running())
                return nocb;
            let id = ids();
            listeners.push({ id, value });
            return mintCleanup(() => {
                for (let i = 0; i < listeners.length; i++) {
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
;
// accumulator
// Only run it on the nth time.
export function mintThreshold(n, cb) {
    if (n < 1) {
        cb();
        return nocb;
    }
    let count = 0;
    return () => {
        if (!--count) {
            cb();
            cb = nocb;
        }
    };
}
;
export function mintCell(value) {
    return {
        get: () => value,
        set: next => { value = next; },
        swap: next => { const prev = value; value = next; return prev; }
    };
}
;
export function whileLoading(load) {
    let isLoading = true;
    const r = load(() => isLoading);
    isLoading = false;
    return r;
}
;
export function gatherLoadingBurst(loader) {
    let bursts = [];
    const r = whileLoading(isLoading => loader((value) => {
        if (isLoading()) {
            bursts.push(value);
            return true;
        }
        else
            return false;
    }));
    return { result: r(bursts), sideEffect: bursts = [] }.result;
}
;
export function mintMap() {
    const storage = new Map();
    return {
        get: (k, df) => {
            if (storage.has(k))
                return storage.get(k);
            if (df == undefined)
                return undefined;
            const v = df();
            storage.set(k, v);
            return v;
        },
        set: (k, v) => v !== undefined ? storage.set(k, v) : storage.delete(k),
        reset: (onEach) => {
            if (!onEach)
                return storage.clear();
            const saved = storage.entries();
            storage.clear();
            for (const [k, v] of saved)
                onEach(k, v);
        },
        keys: () => [...storage.keys()],
    };
}
;
