// A fresher, more unified approach to Web APIs
// using ResourceM, Riverdragon, and Bed
//
// This is a creative and artistic exercise, a way to make JavaScript a bit
// friendlier and to plan a design for future languages
//
// One of my complaints is that finding out how to use APIs often involves
// looking up twenty named objects and their methods and their inheritance
// chain and their defined events and how their events relate to their lifecycle
// and their members and ... itʼs a whole mess. WebRTC has ~35 object classes!
//
// What if we could simplify it?
//
// Philosophy of design:
//
// - https://blog.veritates.love/resource_acquisition.html
// - https://blog.veritates.love/riverdragon_implementation.html
// - https://blog.veritates.love/reformed_oop.html
//
// Each API is exposed as a function to instantiate or acquire a resource. This
// instance is represented as a record of members and methods, to capture static
// information about the resource, streams/stores of information coming out of
// the resource, and methods to modify the resource and perform actions.
// Sometimes the underlying object is exposed, but the other methods are plain
// closures and do not require calling with `this`.
//
// Each resource keeps track of its destructor in the ambient `Scope` from the
// Resource “monad”, so while they do have defined lifetimes, this is not tied
// to a notion of “object” or class specifically, and related objects are
// naturally aggregated together. It is compositional. (Event streams for close
// events are not closed on destroy.)
//
// Scopes can also keep track of other data, like AudioContexts. Care must be
// taken when using scopes in asynchronous contexts, especially promises, which
// will not preserve scope across `.then()`/`await`. Riverdragon streams are
// already integrated with resource management and do save the scope from the
// time they are subscribed to when the subscriptionʼs callback is invoked.
//
// Having responsive event streams allows getting an initial value immediately
// on subscription *and* automatically listening for the right events to update
// its value, allowing it to seamlessly combine with other reactive values.
//
// The unique bonus of Riverdragon is to synchronize updates across the whole
// FRP graph, so that when two streams are updated from the same atomic event,
// they do not result in duplicate events down the line. For example, deriving
// the magnitude of a joystick from X and Y axis positions will result in only
// one update per polling cycle, preventing the kind of glitchy transient value
// (which might depend on subscription order) in most other frameworks.
//
// Modeling events and object lifecycles through event streams also clarifies
// the lifecycle of objects and how they update. Time can be modeled in types,
// or at least the progression of an objectʼs lifecycle from opening to usable
// and what sub-resources it may spawn access to. Event streams can signal their
// end if they know they will never happen: a close event only occurs once, for
// example, which can be gleaned from a `.readyState` for some objects.
//
// Furthermore, sticking to plain records clarifies which IDL properties are
// readonly-as-in-immutable or readonly as in controlled by outside events.
// There are also some weird issues around whether the interface returns
// consistent objects (and just updates them) or creates new objects on each
// request, which are at least less relevant here. And this allows much more
// flexibility in emulating interfaces and proxying them remotely, perhaps.
//
// There are limitations to this approach, records-of-closures, but for these
// kinds of APIs, it works really well. One limitation that is clear here is
// that the underlying JavaScript object often needs to be exposed too, so that
// it can be transferred across structured clone algorithms (like MessagePorts).
// It is not really suitable for a DOM tree or other places where binary methods
// (e.g. `.appendChild`) are required, for similar reasons. It obviously
// requires a garbage collector, too, but that is okay.
//
// The top-level of this module should be pure and not rely on any APIs to be
// available to load. It should only matter which things are actually used.
//
// Library:
//
// A lot of it is just stitching together weird combinations of event listeners
// with dynamic properties and lifecycle methods. Like, I hope you see that it
// takes a lot of work, knowledge, and docs+spec reading to see how to combine
// the information from each API into coherent event streams and behaviors (look
// at how many weird events are on the base `Window`!), and even then, there are
// holes (e.g. no Gamepad update events, no event for when a Lock gets stolen).
//
// Not all APIs are complete, so it may not collaborate well with consumers not
// using this interface. Examples:
// - A lot of state that you would like to be a `Store` (available on demand) is
//   only available after an event occurs (e.g. keyboard state or mouse
//   position), but this is for good reason (security/privacy) and is not
//   specific to the web platform even
// - `MediaSession.setActionHandler` is exclusive, only one handler at a time,
//   unless subscribing through this Riverdragon-ified API
// - `localStorage`/`sessionStorage` do not send events to the current tab, so
//   JavaScript setting it naïvely will not result in updates
//
// You could monkey patch the methods to add these notifications, but that is
// messy and frowned upon.
//
// Cross-process/cross-computer communication is also messy (Workers,
// WebSockets, and so on), since the event streams do not come with any
// semantics, not for remote procedure calls or transferred resource lifetimes
// or anything. You are lucky to get closing and sometimes error semantics.
//
////////////////////////////////////////////////////////////////////////////////
import * as Bed from "./Bed.js";
import * as Resource from "./Resource.js";
import { Stream } from "./Riverdragon.js";
export const unreachable = (_why) => {
    throw new Error("Unreachable case");
};
// Lazy initialiation: run the function only once, the first time it is called.
export const _lazy = (mk) => {
    let inited = false;
    let value;
    mk = Resource.saveScope(mk);
    return (force) => {
        if (force || !inited)
            value = mk();
        return value;
    };
};
export const _lazyKeyed = (mk) => {
    const inited = new Map();
    mk = Resource.saveScope(mk);
    return (key, force) => {
        if (force || !inited.has(key))
            inited.set(key, mk(key));
        return inited.get(key);
    };
};
export const _jsonEq = (x, y) => JSON.stringify(x) === JSON.stringify(y);
// Mint an abort signal that is destroyed by the current `Scope`
export function mintAbortSignal() {
    if (Resource.getScope()?.destroyed())
        return AbortSignal.abort();
    const compt = new AbortController();
    Resource.addDestructor(() => compt.abort());
    return compt.signal;
}
;
// Mint an abort controller that can destroy the current `Scope`
export function mintAbortController() {
    const compt = new AbortController();
    Resource.addDestructor(() => compt.abort());
    compt.signal.addEventListener("abort", Resource.selfDestructor(), { once: true });
    return compt;
}
;
export const { stream: repollRequests, send: repollNow } = Stream.createRiver();
export function repollable(which, ...extra) {
    which !== undefined && repollableSpecifiers.add(which);
    return Stream.oneStream([
        // Run once to initialize the Store (hopefully)
        Stream.poke(undefined),
        // And listen for specified repolls or all repolls
        !which ? repollRequests : repollRequests
            .filter(requested => requested === undefined || requested === which),
        ...extra,
    ]);
}
;
export const repollableSpecifiers = new Set();
////////////////////////////////////////////////////////////////////////////////
// Interface for timer-like functions on the event loop. Set timers, delay
// streams, run a buffer to synchronize events (e.g. to the next animation
// frame), and so on. All is cleaned up when the current scope is destroy.
////////////////////////////////////////////////////////////////////////////////
export var Timer;
(function (Timer) {
    // API for callbacks that run once
    function _once(register, unregister) {
        const callback = (yes, no) => {
            const token = register(() => {
                yes();
                yes = no = Bed.nocb;
            });
            return Resource.tryAddDestructor(() => {
                no?.();
                yes = no = Bed.nocb;
                unregister?.(token);
            });
        };
        return {
            lake: Stream.makeLake((cb, runDry) => callback(() => { cb(); runDry(); }, runDry)),
            callback,
            mint: (cb, ...arg) => {
                const stream = Stream.createRiver();
                const cancel = callback(Resource.saveScope(() => {
                    stream.send(cb(...arg));
                    stream.destroy();
                }), stream.destroy);
                return {
                    cancel,
                    stream: stream.stream,
                    promise: stream.stream.nextValue(),
                };
            },
            // Requires its delay function to be predictable, especially if the result
            // is used as a river.
            delay: (upstream) => upstream.delay(callback),
        };
    }
    Timer._once = _once;
    ;
    // API for callbacks that loop
    function _loop(register, unregister, simulate) {
        const callback = (yes, no) => {
            let run = () => {
                yes();
                if (simulate && run)
                    token = register(run);
            };
            let token = register(run);
            return Resource.tryAddDestructor(() => {
                no?.();
                yes = Bed.nocb;
                no = run = undefined;
                unregister?.(token);
                token = undefined;
            });
        };
        return {
            lake: Stream.makeLake(callback),
            callback,
            mint: (cb, ...arg) => {
                const stream = Stream.createRiver();
                const cancel = callback(() => {
                    stream.send(cb(...arg));
                }, stream.destroy);
                return {
                    cancel,
                    stream: stream.stream,
                    next: () => stream.stream.nextValue(),
                };
            },
        };
    }
    Timer._loop = _loop;
    ;
    // queueMicrotask, the same queue that is used for resolving Promise callbacks
    Timer.microtask = _once(queueMicrotask);
    // setImmediate/clearImmediate
    // export const immediate = _once(setImmediate, clearImmediate);
    // setTimeout/clearTimeout
    Timer.timeout = (ms) => _once(cb => setTimeout(cb, ms), clearTimeout);
    // setInterval/clearInterval
    Timer.interval = (ms) => _loop(cb => setInterval(cb, ms), clearInterval);
    // requestAnimationFrame/cancelAnimationFrame
    Timer.animationFrame = _once(requestAnimationFrame, cancelAnimationFrame);
    // requestAnimationFrame loop
    Timer.animationLoop = _loop(requestAnimationFrame, cancelAnimationFrame, true);
    // A buffered delayer is useful for making sure DOM mutations happen during
    // animation frames, for example.
    //
    // Only one timer will be active at a time, then it will enter draining mode
    // and resolve all of the queued events synchronously, up to a time limit set
    // by `setDrainTimeout`. Any events that are queued while draining will be
    // included synchronously, in queue order but without further delay. Events
    // are always run after one or two delays: they never miss two drain cycles.
    // Queued events are discarded upon `.destroy()`: use `.drain()` first if
    // you want to pick them up.
    //
    // Probably best used at the consumer end: `.buffering(stream).subscrbe(...)`
    function mkBufferedDelayer(delayFn) {
        if (!delayFn)
            delayFn = Timer.animationFrame.callback;
        let postponed = [];
        let pending = [];
        let state = ["idle"];
        let currentDrainTimeout = null;
        const drain = () => {
            if (state[0] === "destroyed")
                return;
            if (state[0] === "draining")
                return;
            if (currentDrainTimeout === null) {
                state = ["draining", null];
                while (pending.length || postponed.length) {
                    const saved = postponed.concat(pending);
                    postponed = [];
                    pending = [];
                    for (const work of saved)
                        work();
                }
            }
            else {
                const drainStart = performance.now();
                state = ["draining", { start: drainStart, timeout: currentDrainTimeout }];
                while (pending.length || postponed.length) {
                    const saved = pending;
                    pending = [];
                    for (const work of postponed)
                        work();
                    postponed = [];
                    for (const work of saved) {
                        if (performance.now() - drainStart < currentDrainTimeout)
                            work();
                        else
                            postponed.push(work);
                    }
                    if (postponed.length)
                        break;
                }
            }
            state = ["idle"];
        };
        const buffering = (upstream) => upstream.delay(cb => {
            switch (state[0]) {
                case "destroyed": return Bed.nocb;
                case "idle":
                    const canceler = delayFn(drain);
                    state = ["requested", canceler];
            }
            pending.push(() => cb()); // let cb() mutate
            return () => { cb = Bed.nocb; };
        });
        return {
            buffering, drain,
            draining: () => state[0] === "draining",
            destroy: Resource.tryAddDestructor(() => {
                if (state[0] === "requested")
                    state[1](); // cancel the request
                state = ["destroyed"];
            }),
            setDrainTimeout: (timeout) => {
                let prev = currentDrainTimeout;
                currentDrainTimeout = timeout;
                return prev;
            },
        };
    }
    Timer.mkBufferedDelayer = mkBufferedDelayer;
    ;
    // A default animation frame queue
    Timer.globalAnimFrame = mkBufferedDelayer();
})(Timer || (Timer = {})); // namespace Timers
////////////////////////////////////////////////////////////////////////////////
// Interfaces to the DOM (Document Object Model): listeners, content loading,
// mutation observers, intersection observers?
////////////////////////////////////////////////////////////////////////////////
export var DOM;
(function (DOM) {
    // addEventListener/removeEventListener
    // inference does not work well ...
    function mintListener(target, type, options) {
        const stream = Stream.createRiver();
        const listener = (ev) => {
            stream.send(ev);
            if (options?.once)
                destroy();
        };
        target.addEventListener(type, listener, options);
        const destroy = Bed.mintCleanup(() => {
            target.removeEventListener(type, listener, options);
            stream.destroy();
        });
        return { stream: stream.stream, destroy, next: () => stream.stream.nextValue() };
    }
    DOM.mintListener = mintListener;
    ;
    // Forces { once: true }, so it only listens for one event
    function mintListener1(target, type, options) {
        const stream = Stream.createRiver();
        const listener = (ev) => {
            stream.send(ev);
            destroy();
        };
        options = { ...(options ?? {}), once: true };
        target.addEventListener(type, listener, options);
        const destroy = Bed.mintCleanup(() => {
            target.removeEventListener(type, listener, options);
            stream.destroy();
        });
        return { stream: stream.stream, destroy, promise: stream.stream.nextValue() };
    }
    DOM.mintListener1 = mintListener1;
    ;
    // memoized River: all subscriptions will be the same, but the upstream
    // subscription is active only when there are subscribers
    function listener(target, type, options) {
        return Stream.makeLake((cb, runDry) => {
            const listener = (ev) => {
                cb(ev);
                if (options?.once)
                    runDry();
            };
            target.addEventListener(type, listener, options);
            return Bed.mintCleanup(() => {
                target.removeEventListener(type, listener, options);
            });
        }).memoize();
    }
    DOM.listener = listener;
    ;
    // This has much better typing behavior, if you know what the
    // event mapping type for the class is
    function listenTo(target) {
        return {
            listener: (type, options) => listener(target, type, options),
            mintListener: (type, options) => mintListener(target, type, options),
            mintListener1: (type, options) => mintListener1(target, type, options),
        };
    }
    DOM.listenTo = listenTo;
    ;
    DOM.windowEvents = _lazy(() => listenTo(window));
    DOM.documentEvents = _lazy(() => listenTo(window.document));
    DOM.rootEvents = _lazy(() => listenTo(window.document.documentElement));
    DOM.bodyEvents = _lazy(() => listenTo(window.document.body));
    // export function eventCycle
    // A pair of events to toggle a state, with a method to check it at the start
    function eventToggle(target, check, on, off, options) {
        return Stream.oneStream([
            Stream.poke(undefined).map(check),
            DOM.listener(target, on, options).map(() => false),
            DOM.listener(target, off, options).map(() => true),
        ]);
    }
    DOM.eventToggle = eventToggle;
    ;
    // `DOMContentLoaded` unless `document.readyState === 'complete'` already
    function contentLoaded() {
        // https://developer.mozilla.org/en-US/docs/Web/API/Document/readyState
        // 'loading', 'interactive', 'complete'
        //
        // interactive
        //   The document has finished loading and the document has been parsed but
        //   sub-resources such as scripts, images, stylesheets and frames are still
        //   loading. The state indicates that the DOMContentLoaded event is about
        //   to fire.
        if (window.document.readyState === 'complete') {
            return {
                destroy: Bed.nocb,
                stream: Stream.bursting([undefined]),
                promise: Promise.resolve(undefined),
            };
        }
        else {
            // https://developer.mozilla.org/en-US/docs/Web/API/Document/DOMContentLoaded_event
            // The DOMContentLoaded event fires when the HTML document has been
            // completely parsed, and all deferred scripts (<script defer src="…"> and
            // <script type="module">) have downloaded and executed. It doesnʼt wait
            // for other things like images, subframes, and async scripts to finish
            // loading.
            //
            // DOMContentLoaded does not wait for stylesheets to load, however
            // deferred scripts do wait for stylesheets, and the DOMContentLoaded
            // event is queued after deferred scripts. Also, scripts which arenʼt
            // deferred or async (e.g. <script>) will wait for already-parsed
            // stylesheets to load.
            //
            // A different event, load, should be used only to detect a fully-loaded
            // page. It is a common mistake to use load where DOMContentLoaded would
            // be more appropriate.
            return mintListener1(window, "DOMContentLoaded");
        }
    }
    DOM.contentLoaded = contentLoaded;
    ;
    // `window.onload` unless `document.readyState === 'complete'` already
    function fullyLoaded() {
        if (window.document.readyState === 'complete') {
            return {
                destroy: Bed.nocb,
                stream: Stream.bursting([undefined]),
                promise: Promise.resolve(undefined),
            };
        }
        else {
            return mintListener1(window, "load");
        }
    }
    DOM.fullyLoaded = fullyLoaded;
    ;
    // Track focus on or within a specific element, using the focus/blur events,
    // or the focusin/focusout events, and querying `document.activeElement`
    function focus(target, where = "on") {
        const check = where === "within"
            ? () => target.contains(window.document.activeElement)
            : () => window.document.activeElement === target;
        return eventToggle(target, check, where === "within" ? "focusout" : "blur", where === "within" ? "focusin" : "focus", { passive: true });
    }
    DOM.focus = focus;
    ;
    // Track hover on a specific element, using mouse or pointer enter and leave
    // events, and the `target.matches(":hover")` CSS query
    function hover(target, type = "pointer") {
        return eventToggle(target, () => target.matches(":hover"), type + "enter", type + "leave", { passive: true });
    }
    DOM.hover = hover;
    ;
})(DOM || (DOM = {})); // namespace DOM
////////////////////////////////////////////////////////////////////////////////
// Interfaces for user input, taking care to keep track of state persistently
// where possible. Mouse, touch, keyboard, modifiers. Gamepad input. See
// below for MIDI input, which is big enough to warrant its own.
////////////////////////////////////////////////////////////////////////////////
export var Input;
(function (Input) {
    let _globals = null;
    function globals() {
        if (_globals === null)
            _globals = mkGlobals();
        return _globals;
    }
    Input.globals = globals;
    ;
    function mkGlobals() {
        return Resource.withScope(Resource.mkSubscope(Resource.noScope), () => {
            const mouse = Stream.createRiverStore();
            const mouseEvents = [
                "auxclick", "click", "contextmenu", "dblclick",
                "mousedown", "mouseenter", "mouseleave", "mousemove",
                "mouseout", "mouseover", "mouseup",
            ];
            for (const mouseEvent of mouseEvents)
                DOM.windowEvents().mintListener(mouseEvent, { passive: true }).stream.subscribe(mouse.send);
            // TODO: wheel event, correlate with scroll?
            const touch = Stream.createRiverStore();
            const touchEvents = [
                "touchcancel", "touchend", "touchmove", "touchstart",
            ];
            for (const touchEvent of touchEvents)
                DOM.windowEvents().mintListener(touchEvent, { passive: true }).stream.subscribe(touch.send);
            const pointer = Stream.createRiverStore();
            const pointerEvents = [
                "pointercancel", "pointerdown", "pointerenter", "pointerleave",
                "pointermove", "pointerout", "pointerover", "pointerup",
                // "pointerrawupdate"
            ];
            for (const pointerEvent of pointerEvents)
                DOM.windowEvents().mintListener(pointerEvent, { passive: true }).stream.subscribe(pointer.send);
            const pointerEventType = pointer.stream.map(ev => ({ key: ev.pointerType, value: ev })).mailbox();
            const keyboard = Stream.createRiverStore();
            // Map of KeyboardEvent.code to KeyboardEvent.key, at time of keydown
            const knownKeys = new Map();
            const isComposing = Stream.createStore(false);
            const keyboardEvents = [
                "keydown", "keypress", "keyup",
            ];
            for (const keyboardEvent of keyboardEvents)
                DOM.windowEvents().mintListener(keyboardEvent, { passive: true }).stream.subscribe(ev => {
                    if (ev.isComposing !== isComposing.current())
                        isComposing.send(ev.isComposing);
                    if (ev.type === "keydown")
                        knownKeys.set(ev.code, ev.key);
                    else if (ev.type === "keyup")
                        knownKeys.delete(ev.code);
                    keyboard.send(ev);
                });
            // Track composition state
            DOM.windowEvents().mintListener("compositionstart", { passive: true })
                .stream.subscribe(() => isComposing.send(true));
            DOM.windowEvents().mintListener("compositionend", { passive: true })
                .stream.subscribe(() => isComposing.send(false));
            // Browser/OS-shortcuts, like Alt+Tab, would leave dangling keys.
            // This is probably the best we can do.
            DOM.windowEvents().mintListener("blur", { passive: true })
                .stream.subscribe(() => knownKeys.clear());
            const modifiers = Stream.createRiverStore();
            const currentModifiers = ev => {
                if (ev instanceof KeyboardEvent)
                    return {
                        altGraph: ev.getModifierState("AltGraph"),
                        capsLock: ev.getModifierState("CapsLock"),
                        fn: ev.getModifierState("Fn"),
                        fnLock: ev.getModifierState("FnLock"),
                        hyper: ev.getModifierState("Hyper"),
                        numLock: ev.getModifierState("NumLock"),
                        scrollLock: ev.getModifierState("ScrollLock"),
                        super: ev.getModifierState("Super"),
                        symbol: ev.getModifierState("Symbol"),
                        symbolLock: ev.getModifierState("SymbolLock"),
                    };
            };
            const noModifiers = {
                altGraph: false,
                capsLock: false,
                fn: false,
                fnLock: false,
                hyper: false,
                numLock: false,
                scrollLock: false,
                super: false,
                symbol: false,
                symbolLock: false,
            };
            Stream.oneStream([
                mouse.stream, touch.stream, keyboard.stream
            ]).subscribe(ev => {
                let was = modifiers.current();
                let mods = currentModifiers(ev) ?? was?.other;
                if (!was
                    || was.altKey !== ev.altKey
                    || was.ctrlKey !== ev.ctrlKey
                    || was.metaKey !== ev.metaKey
                    || was.shiftKey !== ev.shiftKey
                    || (was.other !== mods && !_jsonEq(was.other, mods)))
                    modifiers.send({
                        altKey: ev.altKey,
                        ctrlKey: ev.ctrlKey,
                        metaKey: ev.metaKey,
                        shiftKey: ev.shiftKey,
                        other: mods ?? noModifiers
                    });
            });
            return {
                _destroy: Resource.selfDestructor(),
                mouse: {
                    stream: mouse.stream,
                    current: mouse.current,
                    buttons: mouse.stream.map(ev => ev.buttons).dedup().unsafeRiver(),
                },
                touch: {
                    stream: touch.stream,
                    current: touch.current,
                    touches: touch.stream.map(ev => Array.from(ev.touches)).memoize(),
                },
                pointer: {
                    stream: pointer.stream,
                    current: pointer.current,
                    byType: (type) => pointerEventType(type),
                },
                keyboard: {
                    stream: keyboard.stream,
                    current: keyboard.current,
                    knownKeys,
                    isComposing: isComposing.stream,
                },
                modifiers: {
                    stream: modifiers.stream,
                    current: modifiers.current,
                    altKey: modifiers.stream.map(m => m.altKey).dedup().store(),
                    ctrlKey: modifiers.stream.map(m => m.ctrlKey).dedup().store(),
                    metaKey: modifiers.stream.map(m => m.metaKey).dedup().store(),
                    shiftKey: modifiers.stream.map(m => m.shiftKey).dedup().store(),
                },
            };
        });
    }
    Input.mkGlobals = mkGlobals;
    ;
    // The live view of what gamepads are available
    Input.gamepads = Stream.oneStream([
        repollable("gamepads").map(() => [...window.navigator.getGamepads()]),
        // `.getGamepads()` updates *after* the events are processed
        // so we simulate the updates ourselves before they happen
        // (at least it is idempotent...)
        DOM.windowEvents().listener("gamepadconnected").map(ev => {
            return Object.assign([...window.navigator.getGamepads()], { [ev.gamepad.index]: ev.gamepad });
        }),
        DOM.windowEvents().listener("gamepaddisconnected").map(ev => {
            const after = Object.assign([...window.navigator.getGamepads()], { [ev.gamepad.index]: null });
            if (after[after.length - 1] === null)
                after.pop();
            return after;
        }),
    ]).memoize();
    function gamepad(device, polling) {
        if (!polling)
            polling = Timer.animationLoop.lake;
        let lastTimestamp = -Infinity;
        const polled = Stream.oneStream([
            Stream.poke(undefined), polling
        ]).map(() => {
            // actually poll the device! devices do not update if this is not called :-/
            window.navigator.getGamepads();
            return device;
        }).filter(d => ({ u: d.timestamp !== lastTimestamp, e: lastTimestamp = d.timestamp }.u)).memoize();
        return {
            stream: polled,
            axes: device.axes.map((_, i) => polled.map(_ => device.axes[i])),
            buttons: device.buttons.map((_, i) => polled.map(_ => device.buttons[i])),
        };
    }
    Input.gamepad = gamepad;
    ;
})(Input || (Input = {})); // namespace Input
////////////////////////////////////////////////////////////////////////////////
// The MIDI interface is rather complicated, mostly because it exposes raw
// message events and keeping track of the state of keys is complicated.
// It also does not expose input/output pairs, just individual ports, so you
// will have to aggregate that yourself.
////////////////////////////////////////////////////////////////////////////////
export var MIDI;
(function (MIDI) {
    // TODO: https://github.com/WebAudio/web-midi-api/issues/231
    function requestMIDI(options) {
        return window.navigator.requestMIDIAccess(options)
            .then(Resource.saveScope((access) => {
            const updates = DOM.listener(access, "statechange");
            const andUpdates = Stream.oneStream([repollable("midi"), updates]);
            const eqKeys = (last, next) => last.size === next.size && _jsonEq([...last.keys()], [...next.keys()]);
            const inputs = andUpdates.map(() => new Map(access.inputs.entries())).dedup(eqKeys).unsafeRiver();
            const outputs = andUpdates.map(() => new Map(access.outputs.entries())).dedup(eqKeys).unsafeRiver();
            return {
                access,
                sysexEnabled: access.sysexEnabled,
                inputs,
                outputs,
            };
        }));
    }
    MIDI.requestMIDI = requestMIDI;
    ;
    function MIDIPort(port) {
        const updates = DOM.listener(port, "statechange");
        const andUpdates = Stream.oneStream([repollable("midi"), updates]);
        return {
            port: port,
            connected: andUpdates.map(() => port.state === "connected"),
        };
    }
    MIDI.MIDIPort = MIDIPort;
    ;
    // Handle the nibbles:
    //   0b1001, 0b1000, 0b1010, 0b1101
    function parseMIDINoteEvent(data) {
        if (!data)
            return;
        if (data.length === 3 && data[2] < 128 && data[3] < 128) {
            let parsed;
            // 4 bits for the channel message type: 0b1001, 0b1000, 0b1010
            // 4 bits for the channel number
            // one byte for the key (note) number, <128
            // one byte for the velocity/pressure, <128
            const note = {
                key: data[2],
                channel: data[0] & 0x0F,
            };
            switch (data[0] >> 4) {
                case 0b1001: // 0x9, Note On
                    // sometimes velocity 0 indicates note off
                    parsed = { type: "velocity", pressed: data[3] > 0, velocity: data[3] };
                    break;
                case 0b1000: // 0x8, Note Off
                    parsed = { type: "velocity", pressed: false, velocity: data[3] };
                    break;
                case 0b1010: // 0xA, Polyphonic Key Pressure (Aftertouch)
                    parsed = { type: "aftertouch", aftertouch: data[3] };
                    break;
                default: return;
            }
            return { note, event: parsed };
        }
        // Channel Pressure (Aftertouch)
        if (data.length === 2 && (data[0] >> 4) === 0b1101 && data[2] < 128) {
            return {
                note: { channel: data[0] & 0x0F, key: undefined },
                event: { type: "aftertouch", aftertouch: data[1] },
            };
        }
    }
    MIDI.parseMIDINoteEvent = parseMIDINoteEvent;
    ;
    function mintMIDINoteMap() {
        const updates = Stream.createRiver();
        const channelNoteMap = () => {
            const storage = Bed.mintMap();
            return {
                storage,
                get: (note) => storage.get(note.channel, Bed.mintMap).get(note.key),
                set: (note, state) => storage.get(note.channel, Bed.mintMap).set(note.key, state),
                reset: Resource.tryWeakDestructor(storage, storage => storage.reset((_, v) => v.reset())),
            };
        };
        const noteState = channelNoteMap();
        const noteStreams = channelNoteMap();
        const notePresses = channelNoteMap();
        const aftertouchStreams = channelNoteMap();
        const getValue = (get) => {
            return [{ static: [], dynamic: () => { const v = get(); return v !== undefined ? [v] : []; } }];
        };
        const mailboxes = {
            noteEvents: updates.stream
                .mailbox(noteStreams),
            presses: updates.stream
                .filter(({ value: { event } }) => event.type === "velocity")
                .map(({ key, value }) => ({ key, value: value.state }))
                .mailbox(notePresses, note => getValue(() => noteState.get(note))),
            aftertouch: updates.stream
                .filter(({ value: { event } }) => event.type === "aftertouch")
                .map(({ key, value }) => ({ key, value: value.state.aftertouch }))
                .mailbox(aftertouchStreams, note => getValue(() => noteState.get(note)?.aftertouch)),
        };
        const allEvents = updates.stream
            .map(({ key, value }) => ({ ...key, ...value }))
            .instantiate().stream;
        const allNotes = allEvents
            .map(event => ({ ...event, live: liveNote(event) }))
            .memoize();
        const channelEvents = updates.stream
            .map(({ key, value }) => ({ key: key.channel, value: { ...key, ...value } }))
            .mailbox();
        const channelNotes = _lazyKeyed((channel) => channelEvents(channel)
            .map(event => ({ ...event, live: liveNote(event) }))
            .memoize());
        const currentAftertouch = Resource.saveScope((note) => {
            return mailboxes.aftertouch(note).whileDefined().instantiate().stream;
        });
        const unpressed = (state) => ({ ...state, release: Stream.empty, aftertouch: Stream.empty });
        const liveNote = Resource.saveScope((note) => {
            return mailboxes.presses(note).map(state => {
                if (!state.pressed)
                    return unpressed(state);
                const aftertouch = currentAftertouch(note);
                return {
                    ...state,
                    aftertouch,
                    release: mailboxes.presses(note)
                        .filter(state => !state.pressed)
                        .map(unpressed)
                        .limitTo(1).instantiate().stream,
                };
            });
        });
        const send = (note, event, DOMevent) => {
            let state = noteState.get(note);
            if (event.type === "velocity") {
                const wasPressed = state?.pressed;
                state = {
                    pressed: event.pressed && event.velocity > 0,
                    velocity: event.velocity,
                    aftertouch: undefined,
                    event: DOMevent,
                };
                if (!wasPressed && !state.pressed)
                    return;
            }
            else if (state && event.type === "aftertouch") {
                state = { ...state, aftertouch: event.aftertouch, event: DOMevent };
            }
            else
                return;
            state.timestamp = state.event?.timeStamp;
            noteState.set(note, state.pressed ? state : undefined);
            updates.send({ key: note, value: { event, state } });
            return state;
        };
        const chooseChannel = (fn) => (channel = 0) => (key) => fn({ channel: channel ?? 0, key });
        return {
            send,
            fromWire: parseMIDINoteEvent,
            parse: (DOMevent) => {
                const { note, event } = parseMIDINoteEvent(DOMevent.data) ?? { event: undefined };
                if (!event)
                    return;
                if (note.key !== undefined)
                    return send(note, event, DOMevent);
                for (const key of noteState.storage.get(note.channel)?.keys() ?? [])
                    send({ channel: note.channel, key }, event, DOMevent);
            },
            disconnect: () => {
                // Release all notes
                for (const channel of noteState.storage.keys())
                    for (const key of noteState.storage.get(channel)?.keys() ?? [])
                        send({ channel, key }, { type: "velocity", pressed: false, velocity: 0 });
            },
            reconnect: () => { },
            // Streams
            allEvents,
            ...mailboxes,
            allNotes,
            liveNote,
            currentAftertouch,
            selectChannel: (channel) => ({
                allEvents: channelEvents(channel),
                noteEvents: chooseChannel(mailboxes.noteEvents)(channel),
                presses: chooseChannel(mailboxes.presses)(channel),
                aftertouch: chooseChannel(mailboxes.aftertouch)(channel),
                allNotes: channelNotes(channel),
                liveNote: chooseChannel(liveNote)(channel),
                currentAftertouch: chooseChannel(currentAftertouch)(channel),
            }),
        };
    }
    MIDI.mintMIDINoteMap = mintMIDINoteMap;
    ;
    function MIDIInput(port) {
        const iface = MIDIPort(port);
        const receive = DOM.listener(port, "midimessage");
        const noteMap = mintMIDINoteMap();
        receive.subscribe(noteMap.parse);
        iface.connected.filter(c => !c).subscribe(noteMap.disconnect);
        const getValue = (get) => {
            return [{ static: [], dynamic: () => { const v = get(); return v !== undefined ? [v] : []; } }];
        };
        const pitchBendValues = new Map();
        const pitchBendEvents = receive.mapArray(({ data }) => {
            if (data && data.length === 3 && (data[0] >> 4) === 0b1110) {
                if (data[1] < 128 && data[2] < 128) {
                    const channel = data[0] & 0x0F;
                    const integer = data[1] /* 7 low bits */ | (data[2] << 7) /* 7 high bits */;
                    const center = 0x2000;
                    const pitchBend = (integer - center) / center;
                    return [{ key: channel, value: pitchBend }];
                }
            }
        });
        pitchBendEvents.subscribe(({ key, value }) => pitchBendValues.set(key, value));
        const pitchBend = pitchBendEvents
            .mailbox(undefined, channel => getValue(() => pitchBendValues.get(channel)));
        const byByte = _lazyKeyed(receive
            .mapArray(ev => ev.data ? [{ key: ev.data[0], value: ev }] : [])
            .mailbox());
        const byNibble = _lazyKeyed(receive
            .mapArray(ev => ev.data ? [{ key: ev.data[0] >> 4, value: ev }] : [])
            .mailbox());
        return {
            ...iface,
            receive,
            notes: noteMap,
            liveNote: noteMap.liveNote,
            pitchBend,
            byByte, byNibble,
        };
    }
    MIDI.MIDIInput = MIDIInput;
    ;
    function MIDIOutput(port) {
        return {
            ...MIDIPort(port),
            send: port.send.bind(port),
        };
    }
    MIDI.MIDIOutput = MIDIOutput;
    ;
})(MIDI || (MIDI = {})); // namespace MIDI
////////////////////////////////////////////////////////////////////////////////
// Basic network requests: fetch API, WebSockets, EventSources
// (Communication with servers)
////////////////////////////////////////////////////////////////////////////////
export var Network;
(function (Network) {
    // Load a promise that can set the settled state to success or failed.
    // Promise errors also result in a failed state, but it is not typesafe.
    function loadingAdv(promise, cancel) {
        return Resource.inSubScope(() => {
            const destroy = Resource.selfDestructor();
            if (cancel)
                Resource.addDestructor(() => cancel.abort());
            // If it is cancelable, the canceling should come through `promise`
            const store = cancel ? Resource.impervious((Stream.createStore), { status: "loading", cancel: destroy }) : Stream.createStore({ status: "loading", cancel: destroy });
            promise.then(settled => {
                store.send(settled);
                store.destroy();
            }, err => {
                store.send({ status: "failed", error: err });
                store.destroy();
            });
            return {
                status: store.current,
                promise,
                stream: store.stream,
            };
        });
    }
    Network.loadingAdv = loadingAdv;
    ;
    function loading(promise, cancel) {
        return loadingAdv(promise.then(data => ({ status: "success", data })), cancel);
    }
    Network.loading = loading;
    ;
    // General fetch function with an auxiliary callback to handle the request
    // body. Can modify the request before sending. A local `AbortSignal` is
    // always added.
    function fetchAdv(input, init, 
    // obtain specific data from the request
    onResponse, 
    // set headers and such
    modifyRequest) {
        return Resource.inSubScope(() => {
            const destroy = Resource.selfDestructor();
            const abortController = mintAbortController();
            const req = new Request(input, init);
            modifyRequest?.(req);
            const response = window.fetch(req, {
                signal: req.signal ? AbortSignal.any([
                    req.signal,
                    abortController.signal,
                ]) : abortController.signal,
            });
            const promise = response
                .then(async (res) => {
                try {
                    if (res.ok) {
                        const r = await onResponse(res);
                        if (r.status === "failed")
                            return r;
                        return {
                            statusCode: res.status,
                            statusText: res.statusText,
                            res: res,
                            headers: res.headers.get.bind(res.headers),
                            mime: res.headers.get("content-type"),
                            ...r,
                            status: "success",
                        };
                    }
                    else {
                        return Promise.resolve({
                            status: "failed", error: {
                                statusCode: res.status, statusText: res.statusText,
                                res: res,
                            }
                        });
                    }
                }
                catch (error) {
                    return { status: "failed", error: error };
                }
            });
            return {
                response,
                ...loadingAdv(promise, abortController),
                cancel: destroy,
            };
        });
    }
    Network.fetchAdv = fetchAdv;
    ;
    // Fetch text body
    function fetchText(input, init) {
        return fetchAdv(input, init, async (res) => ({ status: "success", text: await res.text() }));
    }
    Network.fetchText = fetchText;
    ;
    // Fetch and decode json, setting MIME type `application/json`
    function fetchJson(input, init) {
        return fetchAdv(input, init, async (res) => ({ status: "success", json: await res.json() }), ({ headers }) => headers.set("Accept", "application/json"));
    }
    Network.fetchJson = fetchJson;
    ;
    function fetchBlob(input, init) {
        return fetchAdv(input, init, async (res) => ({ status: "success", blob: await res.blob() }));
    }
    Network.fetchBlob = fetchBlob;
    ;
    function fetchBytes(input, init) {
        return fetchAdv(input, init, async (res) => ({ status: "success", bytes: await res.bytes() }));
    }
    Network.fetchBytes = fetchBytes;
    ;
    function fetchBuffer(input, init) {
        return fetchAdv(input, init, async (res) => ({ status: "success", buffer: await res.arrayBuffer() }));
    }
    Network.fetchBuffer = fetchBuffer;
    ;
    // Fetch and parse XML/HTML with a set MIME type
    function _fetchXML(preferredMIME) {
        return (input, init) => fetchAdv(input, init, async (res) => ({ status: "success", xml: new DOMParser().parseFromString(await res.text(), res.headers.get("Content-Type") ?? preferredMIME) }), ({ headers }) => headers.set("Accept", preferredMIME));
    }
    Network._fetchXML = _fetchXML;
    ;
    Network.fetchXML = _fetchXML("application/xml");
    Network.fetchHTML = _fetchXML("text/html");
    Network.fetchXHML = _fetchXML("application/xhtml+xml");
    Network.fetchSVG = _fetchXML("image/svg+xml");
    // Create a websocket, with `send` and `receive`, reactive `status`, and
    // `close` capabilities.
    function websocket(url, options) {
        const socket = new WebSocket(url, options.protocols);
        if (options.binaryType)
            socket.binaryType = options.binaryType;
        Resource.addDestructor(() => socket.close());
        const getStatus = () => {
            switch (socket.readyState) {
                case socket.CONNECTING: return "connecting";
                case socket.OPEN: return "open";
                case socket.CLOSING: return "closing";
                case socket.CLOSED: return "closed";
            }
            throw new Error("Unrecognized WebSocket.readyState");
        };
        const events = DOM.listenTo(socket);
        const onOpen = events.mintListener1("open").promise;
        const onError = events.mintListener1("error").promise;
        const onClose = Resource.impervious(() => events.mintListener1("close").promise);
        const status = Stream.makeLake((cb, runDry) => {
            let subbed = true;
            const poke = () => {
                if (!subbed)
                    return;
                const s = getStatus();
                cb(s);
                if (s === "closed")
                    runDry();
                return s;
            };
            const s0 = poke();
            if (s0 !== "closed") {
                if (s0 === "connecting")
                    onOpen.then(poke);
                onError.then(poke);
                onClose.then(poke);
            }
            return () => subbed = false;
        }).instantiate().stream;
        return {
            socket,
            status: { stream: status, current: getStatus },
            onOpen, onError, onClose,
            send: socket.send.bind(socket),
            receive: DOM.listener(socket, "message"),
            close: socket.close.bind(socket),
        };
    }
    Network.websocket = websocket;
    ;
    // An `EventSource` is an ancient API for streaming from server to client,
    // more commonly called server-sent events (SSE) outside of this API
    function eventSource(url, options) {
        const source = new EventSource(url, options);
        Resource.addDestructor(() => source.close());
        const getStatus = () => {
            switch (source.readyState) {
                case source.CONNECTING: return "connecting";
                case source.OPEN: return "open";
                case source.CLOSED: return "closed";
            }
            throw new Error("Unrecognized EventSource.readyState");
        };
        const events = DOM.listenTo(source);
        const onOpen = events.mintListener1("open").promise;
        const onError = events.mintListener1("error").promise;
        const status = Stream.makeLake((cb, runDry) => {
            let subbed = true;
            const poke = () => {
                if (!subbed)
                    return;
                const s = getStatus();
                cb(s);
                if (s === "closed")
                    runDry();
                return s;
            };
            if (poke() !== "closed") {
                onOpen.then(poke);
                onError.then(poke);
                // FIXME: close???
            }
            return () => subbed = false;
        }).instantiate().stream;
        return {
            source,
            status: { stream: status, current: getStatus },
            onOpen, onError,
            receive: DOM.listener(source, "message"),
            receiveType: _lazyKeyed((eventType) => DOM.listener(source, eventType)),
            close: source.close.bind(source),
        };
    }
    Network.eventSource = eventSource;
    ;
})(Network || (Network = {})); // namespace Network
////////////////////////////////////////////////////////////////////////////////
// APIs covered: localStorage, sessionStorage, window.onstorage
////////////////////////////////////////////////////////////////////////////////
export var Storage;
(function (Storage) {
    // Create an interface for `localStorage` or `sessionStorage`: the function is
    // not pure but creates a distinct interface each time, which shows up as
    // events on `.loopback` but not `.stream`. Events are synchronized between
    // tabs for `localStorage` and `sessionStorage` via the `"storage"` event
    // on `window`.
    function storageInterface(storage, global) {
        const mkId = Bed.mintCounter();
        // Split crosstalk into a mailbox for specific keys and one event for
        // global deletion, from the storage event
        const crosstalk = Stream.createRiver();
        const crosstalkFor = crosstalk.stream.mailbox();
        let crossdelete = Stream.empty;
        if (global) {
            const globalId = mkId();
            const crossdeletion = Stream.createRiver();
            crossdelete = crossdeletion.stream;
            DOM.windowEvents().listener("storage").subscribe((ev) => {
                if (ev.storageArea !== storage)
                    return;
                if (ev.key)
                    crosstalk.send({ key: ev.key, value: { id: globalId, value: ev.newValue } });
                else
                    crossdeletion.send(null);
            });
        }
        return key => {
            // Create a unique id for this instance of the interface
            const id = mkId();
            // We may miss events (from interactions outside of this API on this tab)
            // so make it repollable
            const base = repollable("storage").map(() => storage.getItem(key));
            // These are the events we care about, which are filtered into loopback
            // and non-loopback
            const talkBack = Stream.oneStream([
                crossdelete,
                crosstalkFor(key),
            ]).dedup().unsafeRiver();
            const loopback = Stream.oneStream([
                base,
                talkBack.map(ev => ev === null ? null : ev.value)
            ]);
            const stream = Stream.oneStream([
                base,
                talkBack
                    .filter(ev => ev?.id !== id)
                    .map(ev => ev === null ? null : ev.value)
            ]);
            return {
                loopback,
                stream,
                current: () => storage.getItem(key),
                send: value => {
                    if (value != null)
                        storage.setItem(key, value);
                    else
                        storage.removeItem(key);
                    crosstalk.send({ key, value: { id, value } });
                },
            };
        };
    }
    Storage.storageInterface = storageInterface;
    ;
    Storage.localStorage = _lazy(() => storageInterface(window.localStorage, true));
    Storage.sessionStorage = _lazy(() => storageInterface(window.sessionStorage, true));
})(Storage || (Storage = {})); // namespace Storage
////////////////////////////////////////////////////////////////////////////////
// Interfaces to WebAudio
////////////////////////////////////////////////////////////////////////////////
export var Audio;
(function (Audio) {
    Audio.ctx = Resource.classProvider(AudioContext);
    const loaded = new WeakMap();
    // Load and cache an audio worklet by its URL or source. Tracked in a weak
    // map, indexed by `AudioContext` and then the URL/source string.
    async function loadAudioWorkletNode(urlOrSource, name) {
        const audioContext = Audio.ctx();
        const scope = Resource.getScope();
        if (!loaded.has(audioContext))
            loaded.set(audioContext, new Map());
        // Load it once, across all requests
        urlOrSource = String(urlOrSource);
        let blob = undefined;
        const wait = 
        // Get an existing promise, or true if it was fully loaded
        loaded.get(audioContext).get(urlOrSource) ??
            // Otherwise ask the audio context to load it
            audioContext.audioWorklet.addModule(URL.canParse(urlOrSource) ? urlOrSource :
                // Make source code into a blob URL
                (blob = URL.createObjectURL(new Blob([urlOrSource], { type: "text/javascript" })))).then(() => {
                loaded.get(audioContext).set(urlOrSource, true);
                if (blob)
                    URL.revokeObjectURL(blob);
            });
        // Set it for others to use
        loaded.get(audioContext).set(urlOrSource, wait);
        // Avoid the microtask delay for loaded worklets
        if (wait !== true)
            await wait;
        return options => Resource.withScope(scope, () => {
            const node = new AudioWorkletNode(audioContext, name, options);
            // TODO: use Workers.Port?
            const send = (message) => node.port.postMessage(message);
            const receive = Stream.makeLake(cb => () => {
                const listener = (event) => cb(event.data);
                node.port.addEventListener('message', listener);
                return () => node.port.removeEventListener('message', listener);
            }).unsafeRiver();
            const destroy = Resource.tryAddDestructor(() => node.port.close());
            return { node, send, receive, destroy };
        });
    }
    Audio.loadAudioWorkletNode = loadAudioWorkletNode;
    ;
    Audio.eqSrc = (s1) => (s2) => s1.src === s2.src && s1.idx === s2.idx;
    Audio.eqDest = (d1) => (d2) => d1.dest === d2.dest && d1.idx === d2.idx;
    // Idempotent.
    function disConnect(src, dest, dis = true) {
        if (dest.dest instanceof AudioParam)
            src.src[dis ? 'disconnect' : 'connect'](dest.dest, src.idx);
        else
            src.src[dis ? 'disconnect' : 'connect'](dest.dest, src.idx, dest.idx);
    }
    Audio.disConnect = disConnect;
    ;
    // Idempotent.
    function connect(src, dest) {
        disConnect(src, dest, false);
        return Resource.tryAddDestructor(() => disConnect(src, dest, true));
    }
    Audio.connect = connect;
    ;
    // Continuously connect only the specified audio sources to the destination.
    function connecting(srcs, dest) {
        let active = [];
        return srcs.subscribe(current => {
            if (current === active)
                return;
            const added = current.filter(s1 => !active.some(Audio.eqSrc(s1)));
            const removed = active.filter(s1 => !current.some(Audio.eqSrc(s1)));
            for (const src of added)
                disConnect(src, dest, true);
            for (const src of removed)
                disConnect(src, dest, false);
            active = current;
        });
    }
    Audio.connecting = connecting;
    ;
    // Accumulate all audio sources, by creating a gain node to connect to.
    function acc(srcs) {
        const virtualSrc = Audio.ctx().createGain();
        srcs.subscribe(upstream => connect(upstream, { dest: virtualSrc, idx: 0 }));
        return { src: virtualSrc, idx: 0 };
    }
    Audio.acc = acc;
    ;
    // Lazy audio source.
    function lazy(src) {
        return Audio.acc(Stream.fromPromise(src));
    }
    Audio.lazy = lazy;
    ;
})(Audio || (Audio = {})); // namespace Audio
////////////////////////////////////////////////////////////////////////////////
// Low-level media codecs are pretty hairy. Right now this just helps with the
// lifecycle of the WebCodec API. Parsing containers (like MP4 or MKV) to
// obtain the codec metadata and find frames is out of scope.
////////////////////////////////////////////////////////////////////////////////
export var Codecs;
(function (Codecs) {
    // A promise-based API for a decoder/encoder. Retries queued frames if they
    // do not seem to be directly responsible for the error, by recreating
    // the decoder and re-queueing them. This only works if the raw frames are
    // still open.
    function _promiseTranscoder(config, transcoderCls) {
        // (Re)creating the transcoder should be a simple function, no?
        const mkprocessor = (config, output, error) => {
            // Create the processor with the callbacks
            const processor = new transcoderCls({ output, error });
            // And set the config
            processor.configure(config);
            // console.debug(processor, transcoderCls.isConfigSupported(config));
            // Reset the per-transcoder statistics
            lastFrameTimestamp = undefined;
            frame = 0;
            return {
                process: processor.decode
                    ? processor.decode.bind(processor)
                    : processor.encode.bind(processor),
                close: () => processor.close(),
            };
        };
        //
        // The primary queue of callbacks for each frame, kept in decoding order
        // and indexed by `frame` (decoding order index).
        let queue = new Map();
        // A backwards queue of additional frame queues, accessed with `.unshift()`
        // and `.pop()`, which queues a `.refresh()` between frame queues.
        let whenSettled = [];
        //
        // The timestamp of the last frame put onto the queue, or `undefined` if none
        // (reset to `undefined` by `refresh()`).
        let lastFrameTimestamp = undefined;
        // The index of frame being processed through this transcoder instance
        // (reset to zero by `refresh()`).
        let frame = 0;
        ////////////////////////////////////////////////////////////////////////////
        // We can “refresh” the transcoder: closing the old one and creating a new
        // one from scratch
        const refresh = () => {
            if (!frame)
                return; // we just refreshed it
            try {
                transcoder?.close();
            }
            catch (e) {
                console.warn(e);
            }
            transcoder = mkprocessor(config, onoutput, onerror);
        };
        // This is always our `output` callback for each transcoder
        const onoutput = (outputFrame) => {
            // Pop from the queue
            const [key, cb] = queue.entries().next().value;
            queue.delete(key);
            // We have successfully processed the immediate queue,
            // so continue on with the queue that is waiting for
            // it to be settled
            if (!queue.size && whenSettled.length) {
                queue = whenSettled.pop();
                refresh();
            }
            // Return the frame output!
            cb(outputFrame);
        };
        // This is always our `error` callback for each transcoder
        const onerror = (error) => {
            // Burn the whole queue
            const burned = queue.values();
            queue = new Map();
            // Create a new transcoder
            transcoder = mkprocessor(config, onoutput, onerror);
            // Process all the callbacks: only the first one gets a real error,
            // because the error came from it, while the rest get error `undefined`.
            for (const cb of burned) {
                try {
                    cb(undefined, error);
                }
                catch { }
                error = undefined;
            }
            // If no frames needed/wanted to be recreated, advance to the next queue
            if (!queue.size)
                queue = whenSettled.pop() ?? new Map();
            // Unknown error, not associated with any frame.
            if (error)
                console.error(error);
        };
        // Create one transcoder immediately. This will be reset with a new
        // instance from mkprocessor on error or on refresh, with the same arguments
        let transcoder = mkprocessor(config, onoutput, onerror);
        // Pass the encoded frame into the transcoder and leave a callback to resolve
        // with the decoded or encoded output frame
        const processit = (data, keepOpen) => new Promise((resolve, reject) => {
            (whenSettled[0] ?? queue).set(frame++, (frameOrUndef, errorOrUndef) => {
                if (frameOrUndef) {
                    resolve(frameOrUndef);
                }
                else if (errorOrUndef) {
                    reject(errorOrUndef);
                }
                else {
                    processit(data, keepOpen); // retry
                }
            });
            lastFrameTimestamp = data.timestamp;
            try {
                transcoder.process(data);
                if (!keepOpen)
                    data.close?.();
            }
            catch (err) {
                reject(err);
            }
        });
        // Prime with ignored frames as necessary
        processit.prime = (datas, keepOpen) => {
            let i = datas.length;
            // Back up until we have just seen a frame
            while (i-- > 0)
                if (datas[i].timestamp === lastFrameTimestamp)
                    break;
            // console.debug('needed priming', datas.length - i - 1, 'given priming', datas.length, lastFrameTimestamp, datas.map(d=>d.timestamp));
            // Resume decoding from there, discarding the decoded frames
            while (++i < datas.length)
                processit(datas[i], keepOpen).then(decoded => decoded.close?.());
        };
        // Queue recreating the transcoder
        processit.refresh = () => {
            if (queue.size) {
                whenSettled.unshift(new Map());
            }
            else {
                refresh();
            }
        };
        return {
            process: processit,
            prime: processit.prime,
            refresh: processit.refresh,
        };
    }
    Codecs._promiseTranscoder = _promiseTranscoder;
    ;
    function audioDecoder(config) {
        return _promiseTranscoder(config, AudioDecoder);
    }
    Codecs.audioDecoder = audioDecoder;
    ;
    function videoDecoder(config) {
        return _promiseTranscoder(config, VideoDecoder);
    }
    Codecs.videoDecoder = videoDecoder;
    ;
    function audioEncoder(config) {
        return _promiseTranscoder(config, AudioEncoder);
    }
    Codecs.audioEncoder = audioEncoder;
    ;
    function videoEncoder(config) {
        return _promiseTranscoder(config, VideoEncoder);
    }
    Codecs.videoEncoder = videoEncoder;
    ;
    // export function lruCache<I, O>(decoder: Transcoder<I, O>): {
    //   decoder: Transcoder<I, { use: () => O, uncache: Bed.Destr }>,
    //   destroy: Bed.Destr,
    //   check: () => { [k in "inputs" | "outputs"]: { allowed: number, current: number, active: number } }
    // } {
    //   return undefined as any;
    // }
})(Codecs || (Codecs = {})); // namespace Codecs
////////////////////////////////////////////////////////////////////////////////
export var Workers;
(function (Workers) {
    // A Remote Procedure Call sends an event through the channel and waits for
    // a matching response. Easy to write with the stream methods we have access to.
    function RPC(iface, recognize) {
        return {
            ...iface,
            request: (...arg) => {
                iface.send(...arg);
                return iface.receive
                    .mapArray(recognize(...arg))
                    .limitTo(1).nextValue();
            },
        };
    }
    Workers.RPC = RPC;
    ;
    // One `Stream` will be in control of the `send` interface at a time,
    // and can be revoked individually or collectively.
    function mintController(send) {
        const scope = Resource.subScope();
        return {
            control: Resource.saveRevolvingScope(controller => {
                if (!scope.destroyed())
                    return controller.subscribe(send);
                return Bed.nocb;
            }),
            destroy: scope.destroy,
        };
    }
    Workers.mintController = mintController;
    ;
    function elongate(channel1, channel2) {
        return {
            send: channel1.send,
            receive: channel2.receive,
            disconnect: channel1.receive.subscribe(channel2.send),
        };
    }
    Workers.elongate = elongate;
    ;
    function interconnect(channel1, channel2) {
        const d1 = channel1.receive.subscribe(channel2.send);
        const d2 = channel2.receive.subscribe(channel1.send);
        return () => { d1(); d2(); };
    }
    Workers.interconnect = interconnect;
    ;
    // Web Worker. You may want to initialize this under `Resource.impervious`
    // to provide your own graceful shutdown instead of `Worker.terminate()`.
    function Worker(...args) {
        const worker = new window.Worker(...args);
        Resource.tryAddDestructor(() => worker.terminate());
        return {
            worker,
            send: worker.postMessage.bind(worker),
            errors: DOM.listener(worker, "error"),
            receive: DOM.listener(worker, "message"),
        };
    }
    Workers.Worker = Worker;
    ;
    // A named broadcast channel, for communication across tabs per origin.
    // (The same name always returns an equivalent channel.)
    function BroadcastAs(name) {
        const port = new BroadcastChannel(name);
        Resource.tryAddDestructor(port.close.bind(port));
        const stream = DOM.listener(port, "message");
        const errors = DOM.listener(port, "messageerror");
        const receive = new Stream({
            ...stream,
            _subscribe: cbs => {
                return stream._subscribe(cbs);
            },
        });
        return {
            port,
            send: port.postMessage.bind(port),
            receive,
            errors,
        };
    }
    Workers.BroadcastAs = BroadcastAs;
    ;
    // Message Port, used for some types of Workers other contexts like iframes
    function Port(port) {
        const events = DOM.listenTo(port);
        const stream = events.listener("message");
        const errors = events.listener("messageerror");
        const receive = new Stream({
            ...stream,
            _subscribe: cbs => {
                port.start();
                return stream._subscribe(cbs);
            },
        });
        return {
            port,
            send: port.postMessage.bind(port),
            receive,
            errors,
            // "close" is not in MessagePortEventMap or MDN, but it is in the spec
            onClose: Resource.impervious(() => DOM.mintListener1(port, "close").promise),
            close: Resource.tryAddDestructor(port.close.bind(port)),
        };
    }
    Workers.Port = Port;
    ;
    function PortPair() {
        let channel = new MessageChannel();
        return {
            ports: [
                _lazy(() => Port(channel.port1)),
                _lazy(() => Port(channel.port2)),
            ],
            close: () => {
                // FIXME?
                try {
                    channel.port1.close();
                }
                catch { }
                try {
                    channel.port2.close();
                }
                catch { }
            },
        };
    }
    Workers.PortPair = PortPair;
    ;
    // A named Lock per origin. Perform work while holding the `Lock`
    // (although it can be stolen, and there does not appear to be any event
    // for that...).
    //
    // honestly, `navigator.locks.request` is pretty straightforward already
    function Lock(name, options, work) {
        const scopeSignal = mintAbortSignal();
        options = { ...options, signal: options.signal ? AbortSignal.any([options.signal, scopeSignal]) : scopeSignal };
        return Promise.resolve(window.navigator.locks.request(name, options, work));
    }
    Workers.Lock = Lock;
    ;
})(Workers || (Workers = {})); // namespace Workers
// I'm not sure that Document vs Window vs Navigator is always a coherent
// distinction, but might as well keep it for familiarity, so things are
// easier to find from standard references.
///////////////////////////////////////////////////////////////////////////////
export var Window;
(function (Window) {
    Window.devicePixelRatio = {
        current: () => window.devicePixelRatio,
        stream: Stream.makeLake(emit => {
            let active = true;
            let rolling = () => { };
            const untilNext = (cb) => {
                const mediaQueried = window.matchMedia(`(resolution: ${window.devicePixelRatio}dppx)`);
                mediaQueried.addEventListener('change', cb);
                rolling = () => mediaQueried.removeEventListener('change', cb);
            };
            const onChange = () => {
                rolling();
                if (!active)
                    return;
                emit(window.devicePixelRatio);
                untilNext(onChange);
            };
            untilNext(onChange);
            return () => { active = false; rolling(); };
        }).memoize(),
    };
})(Window || (Window = {})); // namespace Window
///////////////////////////////////////////////////////////////////////////////
export var Document;
(function (Document) {
    Document.fullscreenElement = {
        request: (target, options) => target.requestFullscreen(options),
        errors: DOM.documentEvents().listener("fullscreenerror"),
        stream: Stream.oneStream([
            Stream.poke(undefined),
            DOM.documentEvents().listener("fullscreenchange")
        ]).map(() => window.document.fullscreenElement).memoize(),
    };
})(Document || (Document = {})); // namespace Document
////////////////////////////////////////////////////////////////////////////////
export var Navigator;
(function (Navigator) {
    // A weak hint for whether the device is online (different across browsers)
    Navigator.onlineHint = Stream.oneStream([
        repollable("onlineHint"),
        DOM.listener(window, "online"),
        DOM.listener(window, "offline"),
    ]).map(() => window.navigator.onLine).memoize();
    const _WakeLocks = new Set();
    Navigator.wakeLock = {
        // Request a wake lock, register it as a resource to destroy and return that
        // destructor. Also weakly save it in a registry.
        request: async () => {
            const lock = await Resource.then(window.navigator.wakeLock.request());
            // Clean up stale weak refs
            for (const other of [..._WakeLocks])
                if (!other.deref())
                    _WakeLocks.delete(other);
            // Add this weak ref, until it is released
            const weakref = new WeakRef(lock);
            _WakeLocks.add(weakref);
            lock.addEventListener("release", () => _WakeLocks.delete(weakref));
            const destroy = Resource.tryAddDestructor(() => weakref.deref()?.release());
            // Return the destructor: no need for `WakeLockSentinel` interface
            return destroy;
        },
        // This can only release locks we know about from `wakeLock.request`
        releaseAll: (() => {
            for (const weakref of [..._WakeLocks])
                weakref.deref()?.release();
            _WakeLocks.clear();
        }),
    };
    // A live view of the available media devices, listening for `devicechange`
    // events. It will ignore extra events if they occur while an enumeration
    // is in progress, for debouncing purposes.
    //
    // TODO: add synchronous store behavior when at least one is active?
    Navigator.mediaDevices = Stream.oneStream([
        repollable("mediaDevices"),
        DOM.listener(window.navigator.mediaDevices, "devicechange"),
    ]).mapBlocking(() => Stream.fromPromise(window.navigator.mediaDevices.enumerateDevices())).memoize();
    // The weird `setActionHandler` interface does not broadcast like
    // `.addEventListener()`, it acts like `.onevent=` instead, so this caches the
    // rivers while still respecting that it is set to null when there are no
    // active subscribers.
    Navigator.actionHandler = _lazyKeyed((action) => {
        return Stream.makeLake(cb => {
            window.navigator.mediaSession.setActionHandler(action, cb);
            return () => window.navigator.mediaSession.setActionHandler(action, null);
        }).memoize();
    });
    // playbackState (status?)
    // setPositionState (media playback position)
    // userActivation
})(Navigator || (Navigator = {})); // namespace Navigator
///////////////////////////////////////////////////////////////////////////////
(function (DOM) {
    var Observer;
    (function (Observer) {
        function mutationObserver(startObserving) {
            const events = Stream.createRiver();
            const observer = new MutationObserver(events.send);
            let observing = new Set();
            const observers = {
                current: () => [...observing].flatMap(entry => {
                    const target = entry.target.deref();
                    if (target)
                        return [{ target, options: entry.options }];
                    observing.delete(entry);
                    return [];
                }),
                register: (target, options) => {
                    const entry = { target: new WeakRef(target), options };
                    observing.add(entry);
                    observer.observe(target, options);
                    return Resource.tryAddDestructor(() => {
                        observing.delete(entry);
                        observers.reinit();
                    });
                },
                reinit: () => {
                    const buffered = observer.takeRecords();
                    if (buffered.length)
                        events.send(buffered);
                    observer.disconnect();
                    for (const { target, options } of observers.current())
                        observer.observe(target, options);
                },
                clear: (suppress = false) => {
                    const buffered = observer.takeRecords();
                    if (!suppress && buffered.length)
                        events.send(buffered);
                    observer.disconnect();
                    observing = new Set();
                },
            };
            if (startObserving)
                for (const entry of startObserving)
                    observers.register(entry.target, entry.options);
            return {
                aggregated: events.stream,
                stream: events.stream.mapArray(many => many),
                drain: (suppress = false) => {
                    const buffered = observer.takeRecords();
                    if (!suppress && buffered.length)
                        events.send(buffered);
                    return buffered;
                },
                observers,
                destroy: Resource.tryAddDestructor(() => {
                    observer.disconnect();
                    observing = new Set();
                }),
            };
        }
        Observer.mutationObserver = mutationObserver;
        ;
        function textContent(target) {
            const observer = mutationObserver();
            observer.observers.register(target, {
                subtree: true,
                characterData: true,
            });
            return observer.aggregated
                .startWith(undefined).dynamic()
                .map(() => target.textContent);
        }
        Observer.textContent = textContent;
        ;
        function directAttributes(target, select) {
            const observer = mutationObserver();
            observer.observers.register(target, {
                subtree: false,
                attributes: true,
                attributeFilter: select,
                attributeOldValue: true,
            });
            return observer.aggregated.map(updates => {
                const changes = new Map();
                for (const update of updates) {
                    const attr = update.attributeName;
                    if (attr === null || update.target !== target || changes.has(attr))
                        continue;
                    changes.set(attr, { prev: update.oldValue, next: target.getAttribute(attr) });
                }
                return changes;
            });
        }
        Observer.directAttributes = directAttributes;
        ;
        function attribute(target, name) {
            const observer = mutationObserver();
            observer.observers.register(target, {
                attributes: true, attributeFilter: [name],
            });
            return observer.stream
                .startWith(undefined).dynamic()
                .map(() => target.getAttribute(name));
        }
        Observer.attribute = attribute;
        ;
        function intersection(options, startObserving) {
            const events = Stream.createRiver();
            const observer = new IntersectionObserver(events.send, options);
            const observers = {
                register: (target) => {
                    observer.observe(target);
                    const retained = new WeakRef(target);
                    return Resource.tryAddDestructor(() => {
                        const target = retained.deref();
                        if (target)
                            observer.unobserve(target);
                    });
                },
                clear: (suppress = false) => {
                    const buffered = observer.takeRecords();
                    if (!suppress && buffered.length)
                        events.send(buffered);
                    observer.disconnect();
                },
            };
            if (startObserving)
                for (const target of startObserving)
                    observers.register(target);
            return {
                aggregated: events.stream,
                stream: events.stream.mapArray(many => many),
                observer,
                observers,
                destroy: Resource.tryAddDestructor(() => observer.disconnect()),
            };
        }
        Observer.intersection = intersection;
        ;
    })(Observer = DOM.Observer || (DOM.Observer = {}));
})(DOM || (DOM = {})); // namespace DOM.Observer
///////////////////////////////////////////////////////////////////////////////
(function (DOM) {
    var VDOM;
    (function (VDOM) {
        function setAttribute(target, name, value) {
            if (typeof value === "string" || typeof value === "number") {
                target.setAttribute(name, String(value));
            }
            else if (value === null) {
                target.removeAttribute(name);
            }
            else {
                value.subscribe(v => setAttribute(target, name, v));
            }
        }
        VDOM.setAttribute = setAttribute;
        ;
        function $text(values) {
            const node = window.document.createTextNode("");
            values.subscribe(text => node.nodeValue = String(text));
            return node;
        }
        VDOM.$text = $text;
        ;
        function $HTML(type, attrs, ...children) {
            const node = window.document.createElement(type);
            for (const [k, v] of Object.entries(attrs ?? {}))
                setAttribute(node, k, v);
            for (const child of children.flatMap(liveChild))
                node.appendChild(child);
            return node;
        }
        VDOM.$HTML = $HTML;
        ;
        function $SVG(type, attrs, ...children) {
            const node = window.document.createElementNS(VDOM.NS.SVG, type);
            for (const [k, v] of Object.entries(attrs ?? {}))
                setAttribute(node, k, v);
            for (const child of children.flatMap(liveChild))
                node.appendChild(child);
            return node;
        }
        VDOM.$SVG = $SVG;
        ;
        function liveChild(child) {
            if (child === null || child === undefined)
                return [];
            if (typeof child === 'string' || typeof child === 'number')
                return [$text(Stream.pure(child))];
            if (child instanceof Node)
                return [child];
            return child.flatMap(liveChild);
        }
        VDOM.liveChild = liveChild;
        ;
        VDOM.NS = {
            SVG: "http://www.w3.org/2000/svg",
            XHTML: "http://www.w3.org/1999/xhtml",
            HTML: "http://www.w3.org/1999/xhtml",
            XLink: "http://www.w3.org/1999/xlink",
            XML: "http://www.w3.org/XML/1998/namespace",
            XMLNS: "http://www.w3.org/2000/xmlns/",
        };
    })(VDOM = DOM.VDOM || (DOM.VDOM = {}));
})(DOM || (DOM = {})); // namespace DOM.VDOM
// To add:
// - touch events (WIP)
// - mouse drag (+ drag n drop API?)
// - pointer events, pointer capture
// - scroll position (scroll snap?)
// - text cursor
// Research
// - https://patrickhlauke.github.io/touch/
// - https://kenneth.io/post/detecting-multi-touch-trackpad-gestures-in-javascript
