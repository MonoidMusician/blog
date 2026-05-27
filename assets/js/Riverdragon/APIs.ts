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

import * as Bed from "./Bed";
import * as Resource from "./Resource";
import { Flowing, Lake, River, Stream, StreamBurstBehavior } from "./Riverdragon";

export const unreachable = (_why: never) => {
  throw new Error("Unreachable case");
};

export type MaybePromise<T> = T | Promise<T>;

// Lazy initialiation: run the function only once, the first time it is called.
export const _lazy = <R>(mk: () => R): (() => R) => {
  let inited = false;
  let value: R;
  mk = Resource.saveScope(mk);
  return () => {
    if (!inited) value = mk();
    return value;
  };
};
export const _lazyKeyed = <K, V>(mk: (key: K) => V): (key: K) => V => {
  const inited = new Map<K, V>();
  mk = Resource.saveScope(mk);
  return key => {
    if (!inited.has(key)) inited.set(key, mk(key));
    return inited.get(key)!;
  };
};

export const _jsonEq = <T>(x: T, y: T) => JSON.stringify(x) === JSON.stringify(y);

// Mint an abort signal that is destroyed by the current `Scope`
export function mintAbortSignal() {
  if (Resource.getScope()?.destroyed())
    return AbortSignal.abort();

  const compt = new AbortController();
  Resource.addDestructor(() => compt.abort());
  return compt.signal;
};
// Mint an abort controller that can destroy the current `Scope`
export function mintAbortController() {
  const compt = new AbortController();
  Resource.addDestructor(() => compt.abort());
  compt.signal.addEventListener("abort", Resource.selfDestructor(), { once: true });
  return compt;
};


// Sometimes you just need to re-poll some things, so this is a global place
// to re-poll everything that has opted in, or specific things
//
// Currently used keys:
// - "gamepads", to poll connected gamepads
// - "midi", to poll connected MIDI devices
// - "storage", to poll `localStorage`/`sessionStorage`
// - "onlineHint", to poll `navigator.onLine`
// - "mediaDevices", to poll `navigator.mediaDevices.enumerateDevices()`
// - "rtc", to poll WebRTC state, especially negotiation information
//
// Use like `repollable("name").map(() => getThingy()).dedup(eq).unsafeRiver()`
type MightSpecify = string | symbol | undefined | void;
export const { stream: repollRequests, send: repollNow } =
  Stream.createRiver<MightSpecify>();
export function repollable(which?: MightSpecify, ...extra: River<MightSpecify>[]): River<MightSpecify> {
  which !== undefined && repollableSpecifiers.add(which);
  return Stream.oneStream([
    // Run once to initialize the Store (hopefully)
    Stream.poke(undefined),
    // And listen for specified repolls or all repolls
    !which ? repollRequests : repollRequests
      .filter(requested => requested === undefined || requested === which),
    ...extra,
  ])
};
export const repollableSpecifiers: Set<string | symbol> = new Set();



////////////////////////////////////////////////////////////////////////////////
// Interface for timer-like functions on the event loop. Set timers, delay
// streams, run a buffer to synchronize events (e.g. to the next animation
// frame), and so on. All is cleaned up when the current scope is destroy.
////////////////////////////////////////////////////////////////////////////////
export namespace Timer {
  // API for callbacks that run once
  export function _once<Tok>(register: (cb: ()=>void) => Tok, unregister?: (tok: Tok) => void) {
    const callback = (yes: Bed.Cb, no?: Bed.Destr): Bed.Destr => {
      const token: Tok = register(() => {
        yes();
        yes = no = Bed.nocb;
      });
      return Resource.tryAddDestructor(() => {
        no?.(); yes = no = Bed.nocb;
        unregister?.(token);
      });
    };
    return {
      lake: Stream.makeLake<void>((cb, runDry) =>
        callback(() => { cb(); runDry() }, runDry)),
      callback,
      mint: <Arg extends any[], T>(cb: (...arg: Arg) => T, ...arg: Arg) => {
        const stream = Stream.createRiver<T>();
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
      delay: <T>(upstream: Stream<T>): Lake<T> =>
        upstream.delay(callback),
    };
  };

  // API for callbacks that loop
  export function _loop<Tok>(register: (cb: ()=>void) => Tok, unregister?: (tok: Tok) => void, simulate?: boolean) {
    const callback = (yes: Bed.Cb, no?: Bed.Destr): Bed.Destr => {
      let run: Bed.Cb | undefined = () => {
        yes();
        if (simulate && run)
          token = register(run);
      };
      let token: Tok = register(run);
      return Resource.tryAddDestructor(() => {
        no?.(); yes = Bed.nocb; no = run = undefined;
        unregister?.(token); token = undefined as Tok;
      });
    };
    return {
      lake: Stream.makeLake<void>(callback),
      callback,
      mint: <Arg extends any[], T>(cb: (...arg: Arg) => T, ...arg: Arg) => {
        const stream = Stream.createRiver<T>();
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
  };

  // queueMicrotask, the same queue that is used for resolving Promise callbacks
  export const microtask = _once(queueMicrotask);

  // setImmediate/clearImmediate
  export const immediate = _once(setImmediate, clearImmediate);

  // setTimeout/clearTimeout
  export const timeout = (ms?: number) => _once(cb => setTimeout(cb, ms), clearTimeout);

  // setInterval/clearInterval
  export const interval = (ms?: number) => _loop(cb => setInterval(cb, ms), clearInterval);

  // requestAnimationFrame/cancelAnimationFrame
  export const animationFrame = _once(requestAnimationFrame, cancelAnimationFrame);

  // requestAnimationFrame loop
  export const animationLoop = _loop(requestAnimationFrame, cancelAnimationFrame, true);

  export type BufferedDelayer = {
    buffering: <T>(stream: Stream<T>) => Lake<T>,
    drain: Bed.Cb,
    draining: () => boolean,
    destroy: Bed.Destr,
    setDrainTimeout: (ms: number | null) => number | null,
  };

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
  export function mkBufferedDelayer(delayFn?: (cb: Bed.Cb) => Bed.Destr): BufferedDelayer {
    if (!delayFn) delayFn = animationFrame.callback;

    type AFBState =
      | ["idle"]
      | ["requested", Bed.Cb]
      | ["draining", { start: DOMHighResTimeStamp, timeout: number } | null]
      | ["destroyed"]
      ;
    let postponed: Bed.Cb[] = [];
    let pending: Bed.Cb[] = [];
    let state: AFBState = ["idle"];
    let currentDrainTimeout: number | null = null;

    const drain = () => {
      if (state[0] === "destroyed") return;
      if (state[0] === "draining") return;

      if (currentDrainTimeout === null) {
        state = ["draining", null] as AFBState;
        while (pending.length || postponed.length) {
          const saved = postponed.concat(pending);
          postponed = [];
          pending = [];
          for (const work of saved) work();
        }
      } else {
        const drainStart = performance.now();
        state = ["draining", { start: drainStart, timeout: currentDrainTimeout }] as AFBState;
        while (pending.length || postponed.length) {
          const saved = pending;
          pending = [];
          for (const work of postponed) work();
          postponed = [];
          for (const work of saved) {
            if (performance.now() - drainStart < currentDrainTimeout)
              work(); else postponed.push(work);
          }
          if (postponed.length) break;
        }
      }
      state = ["idle"];
    };
    const buffering = <T>(upstream: Stream<T>): Lake<T> =>
      upstream.delay(cb => {
        switch (state[0]) {
          case "destroyed": return Bed.nocb;
          case "idle":
            const canceler = delayFn(drain);
            state = ["requested", canceler];
        }
        pending.push(() => cb()); // let cb() mutate
        return () => { cb = Bed.nocb };
      });
    return {
      buffering, drain,
      draining: () => state[0] === "draining",
      destroy: Resource.tryAddDestructor(() => {
        if (state[0] === "requested") state[1](); // cancel the request
        state = ["destroyed"];
      }),
      setDrainTimeout: (timeout: number | null) => {
        let prev = currentDrainTimeout;
        currentDrainTimeout = timeout;
        return prev;
      },
    };
  };

  // A default animation frame queue
  export const globalAnimFrame = mkBufferedDelayer();
} // namespace Timers



////////////////////////////////////////////////////////////////////////////////
// Interfaces to the DOM (Document Object Model): listeners, content loading,
// mutation observers, intersection observers?
////////////////////////////////////////////////////////////////////////////////
export namespace DOM {
  export type HasEventListener<K extends string, V> = {
    addEventListener(type: K, listener: (ev: V) => void, options?: AddEventListenerOptions): void;
    removeEventListener(type: K, listener: (ev: V) => void, options?: AddEventListenerOptions): void;
  };
  // This only gets us as far as a union of all the possible event types,
  // not the one specific to `type: K`
  export type ListenerType<T, K extends string> =
    T extends {
      addEventListener(type: K, listener: infer L, options?: boolean | AddEventListenerOptions): void;
      addEventListener(type: string, listener: EventListener | EventListenerObject, options?: boolean | AddEventListenerOptions): void;
    } ? L extends (ev: infer V) => any ? V : any : any;

  // addEventListener/removeEventListener
  // inference does not work well ...
  export function mintListener<T extends HasEventListener<K, V>, K extends string, V = ListenerType<T, K>>(target: T, type: K, options?: AddEventListenerOptions) {
    const stream = Stream.createRiver<V>();
    const listener = (ev: V) => {
      stream.send(ev);
      if (options?.once) destroy();
    };
    target.addEventListener(type, listener, options);
    const destroy = Bed.mintCleanup<void>(() => {
      target.removeEventListener(type, listener, options);
      stream.destroy();
    });
    return { stream: stream.stream, destroy, next: () => stream.stream.nextValue() };
  };
  // Forces { once: true }, so it only listens for one event
  export function mintListener1<T extends HasEventListener<K, V>, K extends string, V>(target: T, type: K, options?: AddEventListenerOptions) {
    const stream = Stream.createRiver<V>();
    const listener = (ev: V) => {
      stream.send(ev);
      destroy();
    };
    options = { ...(options??{}), once: true };
    target.addEventListener(type, listener, options);
    const destroy = Bed.mintCleanup<void>(() => {
      target.removeEventListener(type, listener, options);
      stream.destroy();
    });
    return { stream: stream.stream, destroy, promise: stream.stream.nextValue() };
  };
  // memoized River: all subscriptions will be the same, but the upstream
  // subscription is active only when there are subscribers
  export function listener<T extends HasEventListener<K, V>, K extends string, V>(target: T, type: K, options?: AddEventListenerOptions): River<V> {
    return Stream.makeLake<V>((cb, runDry) => {
      const listener = (ev: V) => {
        cb(ev);
        if (options?.once) runDry();
      };
      target.addEventListener(type, listener, options);
      return Bed.mintCleanup<void>(() => {
        target.removeEventListener(type, listener, options);
      });
    }).memoize();
  };

  // This has much better typing behavior, if you know what the
  // event mapping type for the class is
  export function listenTo<
    EventMap extends Record<string, any>,
    T extends HasEventListener<keyof EventMap & string, EventMap[string]>
      = HasEventListener<keyof EventMap & string, EventMap[string]>
  >(target: T): {
    listener: <K extends keyof EventMap & string>
      (type: K, options?: AddEventListenerOptions) =>
        River<EventMap[K]>,
    mintListener: <K extends keyof EventMap & string>
      (type: K, options?: AddEventListenerOptions) =>
        { stream: River<EventMap[K]>, destroy: Bed.Destr, next: () => Promise<EventMap[K]> },
    mintListener1: <K extends keyof EventMap & string>
      (type: K, options?: AddEventListenerOptions) =>
        { stream: River<EventMap[K]>, destroy: Bed.Destr, promise: Promise<EventMap[K]> },
  } {
    type Opts = AddEventListenerOptions;
    return {
      listener:
        <K extends keyof EventMap & string>(type: K, options?: Opts) =>
          listener<T, keyof EventMap & string, EventMap[string]>
            (target, type, options) as River<EventMap[K]>,
        mintListener:
        <K extends keyof EventMap & string>(type: K, options?: Opts) =>
          mintListener<T, keyof EventMap & string, EventMap[string]>
            (target, type, options) as
              { stream: River<EventMap[K]>, destroy: Bed.Destr, next: () => Promise<EventMap[K]> },
      mintListener1:
        <K extends keyof EventMap & string>(type: K, options?: Opts) =>
          mintListener1<T, keyof EventMap & string, EventMap[string]>
            (target, type, options) as
              { stream: River<EventMap[K]>, destroy: Bed.Destr, promise: Promise<EventMap[K]> },
    };
  };

  export const windowEvents = listenTo<WindowEventMap, Window>(window);


  // `DOMContentLoaded` unless `document.readyState === 'complete'` already
  export function contentLoaded(): { stream: River<Event | undefined>, promise: Promise<Event | undefined>, destroy: Bed.Cb } {
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
    } else {
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
  };
  // `window.onload` unless `document.readyState === 'complete'` already
  export function fullyLoaded(): { stream: River<Event | undefined>, promise: Promise<Event | undefined>, destroy: Bed.Cb } {
    if (window.document.readyState === 'complete') {
      return {
        destroy: Bed.nocb,
        stream: Stream.bursting([undefined]),
        promise: Promise.resolve(undefined),
      };
    } else {
      return mintListener1(window, "load");
    }
  };
} // namespace DOM



////////////////////////////////////////////////////////////////////////////////
// Interfaces for user input, taking care to keep track of state persistently
// where possible. Mouse, touch, keyboard, modifiers. Gamepad input. See
// below for MIDI input, which is big enough to warrant its own.
////////////////////////////////////////////////////////////////////////////////
export namespace Input {
  // Global state of mouse, touch events, keyboard, modifiers. This is loaded
  // only when requested by `API.DOM.globals()`, so it may miss events if not
  // requested immediately.
  export type Globals = ReturnType<typeof mkGlobals>;
  export type ModifierKeys = {
    altKey: boolean,
    ctrlKey: boolean,
    metaKey: boolean,
    shiftKey: boolean,
  };
  let _globals: Globals | null = null;
  export function globals(): Globals {
    if (_globals === null) _globals = mkGlobals();
    return _globals;
  };
  export function mkGlobals() {
    return Resource.withScope(Resource.mkSubscope(Resource.noScope), () => {
      const mouse = Stream.createRiverStore<MouseEvent>();
      const mouseEvents = [
        "auxclick", "click", "contextmenu", "dblclick",
        "mousedown", "mouseenter", "mouseleave", "mousemove",
        "mouseout", "mouseover", "mouseup",
      ] as const;
      for (const mouseEvent of mouseEvents)
        DOM.windowEvents.mintListener(mouseEvent).stream.subscribe(mouse.send);

      const touch = Stream.createRiverStore<TouchEvent>();
      const touchEvents = [
        "touchcancel", "touchend", "touchmove", "touchstart",
      ] as const;
      for (const touchEvent of touchEvents)
        DOM.windowEvents.mintListener(touchEvent).stream.subscribe(touch.send);

      const keyboard = Stream.createRiverStore<KeyboardEvent>();
      const keyboardEvents = [
        "keydown", "keypress", "keyup",
      ] as const;
      for (const keyboardEvent of keyboardEvents)
        DOM.windowEvents.mintListener(keyboardEvent).stream.subscribe(keyboard.send);

      const modifiers = Stream.createRiverStore<ModifierKeys>();

      Stream.oneStream<ModifierKeys>([
        mouse.stream, touch.stream, keyboard.stream
      ]).subscribe(ev => {
        let was = modifiers.current();
        if (
          !was
          || was.altKey !== ev.altKey
          || was.ctrlKey !== ev.ctrlKey
          || was.metaKey !== ev.metaKey
          || was.shiftKey !== ev.shiftKey
        ) modifiers.send(ev);
      });

      return {
        _destroy: Resource.selfDestructor(),
        mouse: {
          stream: mouse.stream,
          current: mouse.current,
        },
        touch: {
          stream: touch.stream,
          current: touch.current,
        },
        keyboard: {
          stream: keyboard.stream,
          current: keyboard.current,
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
  };

  // The live view of what gamepads are available
  export const gamepads: River<Array<Gamepad | null>> =
    Stream.oneStream([
      repollable("gamepads").map(() => [...window.navigator.getGamepads()]),
      // `.getGamepads()` updates *after* the events are processed
      // so we simulate the updates ourselves before they happen
      // (at least it is idempotent...)
      DOM.windowEvents.listener("gamepadconnected").map(ev => {
        return Object.assign([...window.navigator.getGamepads()], { [ev.gamepad.index]: ev.gamepad });
      }),
      DOM.windowEvents.listener("gamepaddisconnected").map(ev => {
        const after = Object.assign([...window.navigator.getGamepads()], { [ev.gamepad.index]: null });
        if (after[after.length-1] === null) after.pop();
        return after;
      }),
    ]).memoize();

  // Split a gamepad into useful streams based on a polling function, or on
  // every animation frame if not specified.
  export function gamepad(device: Gamepad, polling?: Stream<any>): {
    stream: River<Gamepad>,
    axes: Array<River<number>>,
    buttons: Array<River<GamepadButton>>,
  };
  export function gamepad(device: Gamepad, polling?: Stream<any>) {
    if (!polling) polling = Timer.animationLoop.lake;

    let lastTimestamp: DOMHighResTimeStamp = -Infinity;
    const polled = Stream.oneStream([
      Stream.poke(undefined), polling
    ]).map(() => {
      // actually poll the device! devices do not update if this is not called :-/
      window.navigator.getGamepads();
      return device;
    }).filter(d =>
      ({ u: d.timestamp !== lastTimestamp, e: lastTimestamp = d.timestamp }.u)
    ).memoize();

    return {
      stream: polled,
      axes: device.axes.map((_, i) => polled.map(_ => device.axes[i])),
      buttons: device.buttons.map((_, i) => polled.map(_ => device.buttons[i])),
    };
  };
} // namespace Input



////////////////////////////////////////////////////////////////////////////////
// The MIDI interface is rather complicated, mostly because it exposes raw
// message events and keeping track of the state of keys is complicated.
// It also does not expose input/output pairs, just individual ports, so you
// will have to aggregate that yourself.
////////////////////////////////////////////////////////////////////////////////
export namespace MIDI {
  // TODO: memoize or something
  // TODO: expose query for permissions
  export function requestMIDI(options?: MIDIOptions): Promise<{
    access: MIDIAccess,
    sysexEnabled: boolean,
    inputs: River<Map<string, MIDIInput>>,
    outputs: River<Map<string, MIDIOutput>>,
  }>;
  // TODO: https://github.com/WebAudio/web-midi-api/issues/231
  export function requestMIDI(options?: MIDIOptions) {
    return window.navigator.requestMIDIAccess(options)
      .then(Resource.saveScope((access: MIDIAccess) => {
        const updates = DOM.listener<MIDIAccess, "statechange", MIDIConnectionEvent>(access, "statechange");
        const andUpdates = Stream.oneStream<any>([ repollable("midi"), updates ]);

        const eqKeys = <V>(last: Map<string, V>, next: Map<string, V>) =>
          last.size === next.size && _jsonEq<string[]>([...last.keys()], [...next.keys()]);

        const inputs = andUpdates.map(() =>
          new Map((access.inputs as Map<string, MIDIInput>).entries())
        ).dedup(eqKeys).unsafeRiver();
        const outputs = andUpdates.map(() =>
          new Map((access.outputs as Map<string, MIDIOutput>).entries())
        ).dedup(eqKeys).unsafeRiver();

        return {
          access,
          sysexEnabled: access.sysexEnabled,
          inputs,
          outputs,
        };
      }));
  };

  export function MIDIPort(port: MIDIPort) {
    const updates = DOM.listener<MIDIPort, "statechange", MIDIConnectionEvent>(port, "statechange");
    const andUpdates = Stream.oneStream<any>([ repollable("midi"), updates ]);

    return {
      port: port,
      connected: andUpdates.map(() => port.state === "connected"),
    };
  };

  // Handle the nibbles:
  //   0b1001, 0b1000, 0b1010, 0b1101
  export function parseMIDINoteEvent(data: number[] | Uint8Array | null | undefined): {
    note: MIDINote | MIDIChannel,
    event: MIDINoteEvent
  } | undefined {
    if (!data) return;
    if (data.length === 3 && data[2] < 128 && data[3] < 128) {
      let parsed: MIDINoteEvent;

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
  };

  export type MIDINote = {
    channel: number, // 0–15
    key: number, // 0–127, 21=A0 through G9=127
  };
  export type MIDIChannel = {
    channel: number,
    key?: undefined,
  };

  export type MIDINoteEvent =
    | { type: "velocity", pressed: boolean, velocity: number, event?: MIDIMessageEvent } // 0 implies off
    | { type: "aftertouch", aftertouch: number, event?: MIDIMessageEvent };
  export type MIDINoteState = {
    pressed: boolean, // <= (velocity > 0)
    velocity: number, // 0-127
    aftertouch: number | undefined,
    timeStamp?: number,
    event?: MIDIMessageEvent,
  };
  export type MIDINoteUpdate = { event: MIDINoteEvent, state: MIDINoteState };

  // A live view of this note event (press or release) until its next release
  export type MIDINoteLive = {
    pressed: boolean, // <= (velocity > 0)
    velocity: number, // 0-127
    timeStamp?: number,
    event?: MIDIMessageEvent,
    release: River<MIDINoteLive>, // empty if pressed == false, limit 1
    aftertouch: River<number>, // empty if pressed == false, ends when released
  };
  // Keep track of all the notes that are currently playing on the MIDIInput,
  // so they do not get dropped for lack of subscribers. Provide convenient
  // (and efficient?) ways to slice the events by channel and by note.
  export type MIDINoteMap = {
    // Update the state, returning the state if it makes sense
    // (off events if it was on, and aftertouch only while it is on)
    send(note: MIDINote, event: MIDINoteEvent, DOMevent?: MIDIMessageEvent): MIDINoteState | undefined,
    // Merely parse an event (pure function)
    fromWire(data: number[] | Uint8Array | null | undefined):
      { note: MIDINote | MIDIChannel, event: MIDINoteEvent } | undefined,
    // Parse a message event and update the state
    parse(DOMevent: MIDIMessageEvent):
      MIDINoteState | undefined,

    // All events, for all channels and all notes
    allEvents: River<MIDINote & MIDINoteUpdate>,
    // All updates for a specific note
    noteEvents(note: MIDINote): River<MIDINoteUpdate>,
    // On and off events for a specific note
    presses(note: MIDINote): River<MIDINoteState>,
    // Aftertouch for a specific note
    aftertouch(note: MIDINote): River<number | undefined>,

    // A live view of each note: on and off events
    allNotes: River<MIDINote & MIDINoteUpdate & { live: River<MIDINoteLive> }>,
    liveNote(note: MIDINote): River<MIDINoteLive>,
    currentAftertouch(note: MIDINote): River<number>,

    selectChannel(channel?: number): {
      // All events, for all notes on this channel
      allEvents: River<MIDINote & MIDINoteUpdate>,
      noteEvents(key: number): River<MIDINoteUpdate>,
      presses(key: number): River<MIDINoteState>,
      aftertouch(key: number): River<number | undefined>,

      allNotes: River<MIDINote & MIDINoteUpdate & { live: River<MIDINoteLive> }>,
      liveNote(key: number): River<MIDINoteLive>,
      currentAftertouch(key: number): River<number>,
    },

    disconnect(): void,
    reconnect(): void,
  };

  export function mintMIDINoteMap(): MIDINoteMap {
    const updates = Stream.createRiver<{ key: MIDINote, value: MIDINoteUpdate }>();

    const channelNoteMap = <T>() => {
      const storage = Bed.mintMap<number, Bed.MintMap<number, T>>();
      return {
        storage,
        get: (note: MIDINote) =>
          storage.get(note.channel, Bed.mintMap).get(note.key),
        set: (note: MIDINote, state: T | undefined) =>
          storage.get(note.channel, Bed.mintMap).set(note.key, state),
        reset: Resource.tryAddDestructor(() => storage.reset((_, v) => v.reset())),
      };
    };
    const noteState = channelNoteMap<MIDINoteState>();
    const noteStreams = channelNoteMap<Chan<MIDINoteUpdate>>();
    const notePresses = channelNoteMap<Chan<MIDINoteState>>();
    const aftertouchStreams = channelNoteMap<Chan<number | undefined>>();

    type Chan<T> = {
      send: (state: T) => void,
      stream: River<T>,
    };
    const getValue = <T>(get: () => T | undefined): StreamBurstBehavior<T> => {
      return [{ static: [], dynamic: () => { const v=get(); return v!==undefined?[v]:[] } }];
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
      .instantiate().stream
    const allNotes = allEvents
      .map(event => ({ ...event, live: liveNote(event) }))
      .memoize();
    const channelEvents = updates.stream
      .map(({ key, value }) => ({ key: key.channel, value: { ...key, ...value } }))
      .mailbox();
    const channelNotes = _lazyKeyed((channel: number) => channelEvents(channel)
      .map(event => ({ ...event, live: liveNote(event) }))
      .memoize());

    const currentAftertouch = Resource.saveScope((note: MIDINote): River<number> => {
      return mailboxes.aftertouch(note).whileDefined().instantiate().stream;
    });
    const unpressed = (state: MIDINoteState) =>
      ({ ...state, release: Stream.empty, aftertouch: Stream.empty });
    const liveNote = Resource.saveScope((note: MIDINote): River<MIDINoteLive> => {
      return mailboxes.presses(note).map(state => {
        if (!state.pressed) return unpressed(state);
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

    const send = (note: MIDINote, event: MIDINoteEvent, DOMevent?: MIDIMessageEvent): MIDINoteState | undefined => {
      let state = noteState.get(note);
      if (event.type === "velocity") {
        const wasPressed = state?.pressed;
        state = {
          pressed: event.pressed && event.velocity > 0,
          velocity: event.velocity,
          aftertouch: undefined,
          event: DOMevent,
        };
        if (!wasPressed && !state.pressed) return;
      } else if (state && event.type === "aftertouch") {
        state = { ...state, aftertouch: event.aftertouch, event: DOMevent };
      } else return;

      state.timeStamp = state.event?.timeStamp;

      noteState.set(note, state.pressed ? state : undefined);
      updates.send({ key: note, value: { event, state } });
      return state;
    };

    const chooseChannel = <T>(fn: (note: MIDINote) => T) =>
      (channel: number = 0) => (key: number) => fn({ channel: channel ?? 0, key });

    return {
      send,
      fromWire: parseMIDINoteEvent,
      parse: (DOMevent: MIDIMessageEvent) => {
        const { note, event } = parseMIDINoteEvent(DOMevent.data) ?? { event: undefined };
        if (!event) return;
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
      reconnect: () => {},

      // Streams
      allEvents,
      ...mailboxes,
      allNotes,
      liveNote,
      currentAftertouch,

      selectChannel: (channel: number) => ({
        allEvents: channelEvents(channel),
        noteEvents: chooseChannel(mailboxes.noteEvents)(channel),
        presses: chooseChannel(mailboxes.presses)(channel),
        aftertouch: chooseChannel(mailboxes.aftertouch)(channel),

        allNotes: channelNotes(channel),
        liveNote: chooseChannel(liveNote)(channel),
        currentAftertouch: chooseChannel(currentAftertouch)(channel),
      }),
    };
  };
  export function MIDIInput(port: MIDIInput) {
    const iface = MIDIPort(port);

    const receive = DOM.listener<MIDIInput, "midimessage", MIDIMessageEvent>(port, "midimessage");
    const noteMap = mintMIDINoteMap();

    receive.subscribe(noteMap.parse);
    iface.connected.filter(c => !c).subscribe(noteMap.disconnect);

    const getValue = <T>(get: () => T | undefined): StreamBurstBehavior<T> => {
      return [{ static: [], dynamic: () => { const v=get(); return v!==undefined?[v]:[] } }];
    };

    const pitchBendValues = new Map<number, number>();
    const pitchBendEvents =
      receive.mapArray(({ data }) => {
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
    const pitchBend: (channel: number) => River<number> = pitchBendEvents
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
  };
  export function MIDIOutput(port: MIDIOutput) {
    return {
      ...MIDIPort(port),
      send: port.send.bind(port),
    };
  };
} // namespace MIDI



////////////////////////////////////////////////////////////////////////////////
// Basic network requests: fetch API, WebSockets, EventSources
// (Communication with servers)
////////////////////////////////////////////////////////////////////////////////
export namespace Network {
  // The RemoteData pattern. Data can be idle, not loaded.
  // https://pursuit.purescript.org/packages/purescript-remotedata/5.0.0/docs/Network.RemoteData#t:RemoteData
  export type RemoteData<dat extends {}, err = any> =
    | { status: "idle" }
    | RemoteLoad<dat, err>;
  // A load request can be in progress (and hopefully cancelable) or settled.
  export type RemoteLoad<dat extends {}, err = any> =
    | { status: "loading", cancel?: Bed.Destr }
    | RemoteSettled<dat, err>;
  // Data settles into an error or success
  export type RemoteSettled<dat extends {}, err = any> =
    | { status: "failed", error: err }
    | { status: "success" } & dat
    ;

  // Load a promise that can set the settled state to success or failed.
  // Promise errors also result in a failed state, but it is not typesafe.
  export function loadingAdv<dat extends {}, err = any>(promise: Promise<RemoteSettled<dat, err>>, cancel?: AbortController) {
    return Resource.inSubScope(() => {
      const destroy = Resource.selfDestructor();
      if (cancel) Resource.addDestructor(() => cancel.abort());
      // If it is cancelable, the canceling should come through `promise`
      const store = cancel ? Resource.impervious(
        Stream.createStore<RemoteLoad<dat, err>>,
        { status: "loading", cancel: destroy }
      ) : Stream.createStore<RemoteLoad<dat, err>>({ status: "loading", cancel: destroy });
      promise.then(
        settled => {
          store.send(settled);
          store.destroy();
        },
        err => {
          store.send({ status: "failed", error: err });
          store.destroy();
        },
      );
      return {
        status: store.current,
        promise,
        stream: store.stream,
      };
    });
  };
  export function loading<dat extends {}>(promise: Promise<dat>, cancel?: AbortController) {
    return loadingAdv<dat>(promise.then(data => ({ status: "success", data })), cancel);
  };

  export type FetchError = undefined | Error | {
    statusCode: number;
    statusText: string;
    res: Response;
  };
  export type FetchOk<dat = {}> = {
    statusCode: number;
    statusText: string;
    headers: (name: string) => string | null,
      // The MIME type from the Content-Type header
      mime: string | null;
    res: Response;
  } & dat;

  // General fetch function with an auxiliary callback to handle the request
  // body. Can modify the request before sending. A local `AbortSignal` is
  // always added.
  export function fetchAdv<dat extends {}>(
    input: URL | string | Request,
    init: RequestInit | undefined,
    // obtain specific data from the request
    onResponse: (res: Response) => Promise<RemoteSettled<dat, FetchError>>,
    // set headers and such
    modifyRequest?: (req: Request) => void,
  ) {
    return Resource.inSubScope(() => {
      const destroy = Resource.selfDestructor();
      const abortController = mintAbortController()

      const req = new Request(input, init);
      modifyRequest?.(req);
      const response = window.fetch(req, {
        signal: req.signal ? AbortSignal.any([
          req.signal,
          abortController.signal,
        ]) : abortController.signal,
      });
      type FetchSettled = RemoteSettled<FetchOk<dat>, FetchError>;
      const promise: Promise<FetchSettled> = response
        .then<FetchSettled>(async res => {
          try {
            if (res.ok) {
              const r = await onResponse(res)
              if (r.status === "failed") return r;
              return {
                statusCode: res.status,
                statusText: res.statusText,
                res: res,
                headers: res.headers.get.bind(res.headers),
                mime: res.headers.get("content-type"),
                ...r,
                status: "success",
              };
            } else {
              return Promise.resolve<FetchSettled>({ status: "failed", error: {
                statusCode: res.status, statusText: res.statusText,
                res: res,
              } });
            }
          } catch(error) {
            return { status: "failed", error: error as FetchError };
          }
        });

      return {
        response,
        ...loadingAdv(promise, abortController),
        cancel: destroy,
      };
    });
  };

  // Fetch text body
  export function fetchText(input: URL | string | Request, init?: RequestInit) {
    return fetchAdv<{ text: string }>(input, init,
      async res => ({ status: "success", text: await res.text() })
    );
  };
  // Fetch and decode json, setting MIME type `application/json`
  export function fetchJson(input: URL | string | Request, init?: RequestInit) {
    return fetchAdv<{ json: any }>(input, init,
      async res => ({ status: "success", json: await res.json() }),
      ({ headers }) => headers.set("Accept", "application/json"),
    );
  };
  export function fetchBlob(input: URL | string | Request, init?: RequestInit) {
    return fetchAdv<{ blob: Blob }>(input, init,
      async res => ({ status: "success", blob: await res.blob() })
    );
  };
  export function fetchBytes(input: URL | string | Request, init?: RequestInit) {
    return fetchAdv<{ bytes: Uint8Array }>(input, init,
      async res => ({ status: "success", bytes: await res.bytes() })
    );
  };
  export function fetchBuffer(input: URL | string | Request, init?: RequestInit) {
    return fetchAdv<{ buffer: ArrayBuffer }>(input, init,
      async res => ({ status: "success", buffer: await res.arrayBuffer() })
    );
  };

  // Fetch and parse XML/HTML with a set MIME type
  export function _fetchXML(preferredMIME: DOMParserSupportedType) {
    return (input: URL | string | Request, init: RequestInit | undefined) =>
      fetchAdv<{ xml: XMLDocument }>(input, init,
        async res => ({ status: "success", xml: new DOMParser().parseFromString(await res.text(), res.headers.get("Content-Type") as DOMParserSupportedType ?? preferredMIME) }),
        ({ headers }) => headers.set("Accept", preferredMIME),
      );
  };
  export const fetchXML = _fetchXML("application/xml");
  export const fetchHTML = _fetchXML("text/html");
  export const fetchXHML = _fetchXML("application/xhtml+xml");
  export const fetchSVG = _fetchXML("image/svg+xml");



  export type WebSocketStatus = "connecting" | "open" | "closing" | "closed";
  export type ForBinaryType<binaryType extends BinaryType> = {
    "blob": Blob,
    "arraybuffer": ArrayBuffer,
  }[binaryType];

  // Create a websocket, with `send` and `receive`, reactive `status`, and
  // `close` capabilities.
  export function websocket<binaryType extends BinaryType = BinaryType>(
    url: string | URL,
    options: {
      protocols?: string | string[],
      binaryType?: binaryType
    },
  ) {
    const socket = new WebSocket(url, options.protocols);
    if (options.binaryType) socket.binaryType = options.binaryType;

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

    const events = DOM.listenTo<WebSocketEventMap>(socket);
    const onOpen = events.mintListener1("open").promise;
    const onError = events.mintListener1("error").promise;
    const onClose = Resource.impervious(() => events.mintListener1("close").promise);

    const status = Stream.makeLake<WebSocketStatus>((cb, runDry) => {
      let subbed = true;
      const poke = () => {
        if (!subbed) return;
        const s = getStatus();
        cb(s);
        if (s === "closed") runDry();
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
      receive: DOM.listener<WebSocket, "message", MessageEvent<string | ForBinaryType<binaryType>>>(socket, "message"),
      close: socket.close.bind(socket),
    };
  };

  // An `EventSource` is an ancient API for streaming from server to client,
  // more commonly called server-sent events (SSE) outside of this API
  export function eventSource(url: string | URL, options: EventSourceInit) {
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

    const events = DOM.listenTo<EventSourceEventMap, EventSource>(source);
    const onOpen = events.mintListener1("open").promise;
    const onError = events.mintListener1("error").promise;

    const status = Stream.makeLake<ReturnType<typeof getStatus>>((cb, runDry) => {
      let subbed = true;
      const poke = () => {
        if (!subbed) return;
        const s = getStatus();
        cb(s);
        if (s === "closed") runDry();
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
      receive: DOM.listener<EventSource, "message", MessageEvent<string>>(source, "message"),
      receiveType: _lazyKeyed((eventType: string) => DOM.listener<EventSource, string, MessageEvent<string>>(source, eventType)),
      close: source.close.bind(source),
    };
  };
} // namespace Network



////////////////////////////////////////////////////////////////////////////////
// APIs covered: localStorage, sessionStorage, window.onstorage
////////////////////////////////////////////////////////////////////////////////
export namespace Storage {
  // Create an interface for `localStorage` or `sessionStorage`: the function is
  // not pure but creates a distinct interface each time, which shows up as
  // events on `.loopback` but not `.stream`. Events are synchronized between
  // tabs for `localStorage` and `sessionStorage` via the `"storage"` event
  // on `window`.
  export function storageInterface(storage: Storage, global?: boolean): (
    (key: string) => {
      loopback: River<string | null>,
      stream: River<string | null>,
      current: () => string | null,
      send: (value: string | null) => void,
    }
  ) {
    const mkId = Bed.mintCounter();
    // Split crosstalk into a mailbox for specific keys and one event for
    // global deletion, from the storage event
    const crosstalk = Stream.createRiver<{ key: string, value: { id: number, value: string | null } }>();
    const crosstalkFor = crosstalk.stream.mailbox();
    let crossdelete: River<null> = Stream.empty;

    if (global) {
      const globalId = mkId();
      const crossdeletion = Stream.createRiver<null>();
      crossdelete = crossdeletion.stream;
      DOM.windowEvents.listener("storage").subscribe((ev: StorageEvent) => {
        if (ev.storageArea !== storage) return;
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
  };

  export let localStorage = _lazy(() => storageInterface(window.localStorage, true));
  export let sessionStorage = _lazy(() => storageInterface(window.sessionStorage, true));
} // namespace Storage



////////////////////////////////////////////////////////////////////////////////
// Interfaces to WebAudio
////////////////////////////////////////////////////////////////////////////////
export namespace Audio {
  export const ctx = Resource.classProvider(AudioContext);

  // The interface to a worklet, with types for input and output recommended.
  export type CreatedWorklet<I=any, O=any> = {
    node: AudioNode,
    send: (message: I) => void,
    receive: River<O>,
    destroy: Bed.Destr,
  };

  const loaded = new WeakMap<AudioContext, Map<string, Promise<void> | true>>();

  // Load and cache an audio worklet by its URL or source. Tracked in a weak
  // map, indexed by `AudioContext` and then the URL/source string.
  export async function loadAudioWorkletNode<I, O>(
    urlOrSource: string | URL, name: string
  ): Promise<
    (options: AudioNodeOptions) => CreatedWorklet<I, O>
  > {
    const audioContext = Audio.ctx();
    const scope = Resource.getScope()!;

    if (!loaded.has(audioContext)) loaded.set(audioContext, new Map());

    // Load it once, across all requests
    urlOrSource = String(urlOrSource);
    let blob: string | undefined = undefined;
    const wait: Promise<void> | true =
      // Get an existing promise, or true if it was fully loaded
      loaded.get(audioContext)!.get(urlOrSource) ??
        // Otherwise ask the audio context to load it
        audioContext.audioWorklet.addModule(
          URL.canParse(urlOrSource) ? urlOrSource :
            // Make source code into a blob URL
            (blob = URL.createObjectURL(new Blob([urlOrSource], { type: "text/javascript" })))
        ).then(() => {
          loaded.get(audioContext)!.set(urlOrSource, true)
          if (blob) URL.revokeObjectURL(blob);
        });
    // Set it for others to use
    loaded.get(audioContext)!.set(urlOrSource, wait);
    // Avoid the microtask delay for loaded worklets
    if (wait !== true) await wait;

    return options => Resource.withScope(scope, () => {
      const node = new AudioWorkletNode(audioContext, name, options);
      // TODO: use Workers.Port?
      const send = (message: I) => node.port.postMessage(message);
      const receive = Stream.makeLake<O>(cb => () => {
        const listener = (event: MessageEvent<O>) => cb(event.data);
        node.port.addEventListener('message', listener);
        return () => node.port.removeEventListener('message', listener);
      }).unsafeRiver();
      const destroy = Resource.tryAddDestructor(() => node.port.close());
      return { node, send, receive, destroy };
    });
  };

  // `River<Audio[]>` is the type of reactive audio. In particular, it consists
  // of *sources* for audio, which can be plugged into the next node or output.
  export type Audio = AudioSrc;
  // An audio source is an output port of a node.
  export type AudioSrc = { src: AudioNode, idx: number };
  // An audio destination is an input port of a node or the input to an
  // audio parameter of a node.
  export type AudioDest = { dest: AudioNode, idx: number } | { dest: AudioParam, idx?: undefined };

  export const eqSrc = (s1: AudioSrc) => (s2: AudioSrc) =>
    s1.src === s2.src && s1.idx === s2.idx;
  export const eqDest = (d1: AudioDest) => (d2: AudioDest) =>
    d1.dest === d2.dest && d1.idx === d2.idx;

  // Idempotent.
  export function disConnect(src: AudioSrc, dest: AudioDest, dis = true) {
    if (dest.dest instanceof AudioParam)
      src.src[dis ? 'disconnect' : 'connect'](dest.dest, src.idx);
    else
      src.src[dis ? 'disconnect' : 'connect'](dest.dest, src.idx, dest.idx);
  };
  // Idempotent.
  export function connect(src: AudioSrc, dest: AudioDest): Bed.Destr {
    disConnect(src, dest, false);
    return Resource.tryAddDestructor(() => disConnect(src, dest, true));
  };
  // Continuously connect only the specified audio sources to the destination.
  export function connecting(
    srcs: Stream<AudioSrc[]>, dest: AudioDest
  ): Bed.Destr {
    let active: AudioSrc[] = [];
    return srcs.subscribe(current => {
      if (current === active) return;
      const added = current.filter(s1 =>
        !active.some(eqSrc(s1))
      );
      const removed = active.filter(s1 =>
        !current.some(eqSrc(s1))
      );
      for (const src of added) disConnect(src, dest, true);
      for (const src of removed) disConnect(src, dest, false);
      active = current;
    });
  };

  // Accumulate all audio sources, by creating a gain node to connect to.
  export function acc(srcs: Stream<Audio>): Audio {
    const virtualSrc = Audio.ctx().createGain();
    srcs.subscribe(upstream => connect(upstream, { dest: virtualSrc, idx: 0 }));
    return { src: virtualSrc, idx: 0 };
  };
  // Lazy audio source.
  export function lazy(src: Promise<Audio>): Audio {
    return Audio.acc(Stream.fromPromise(src));
  };
} // namespace Audio



////////////////////////////////////////////////////////////////////////////////
// Low-level media codecs are pretty hairy. Right now this just helps with the
// lifecycle of the WebCodec API. Parsing containers (like MP4 or MKV) to
// obtain the codec metadata and find frames is out of scope.
////////////////////////////////////////////////////////////////////////////////
export namespace Codecs {
  // A promise-based API for a decoder or encoder. Methods are documented in the
  // chronological order that you may want to call them in.
  export type Transcoder<I, O> = {
    // Queue a refresh of the transcoder (switching to a fresh instance), so that
    // any internal state is decimated. This can be done to *revert* audio
    // priming, which is useful for processing the initial frame, for instance.
    // Otherwise audio echoes from the last decoded frame will leak into it.
    refresh: () => void,

    // Prime the *decoder* by sending in frames as needed and immediately closing
    // their results. If one of the frames was the last input to the decoder,
    // then it and previous frames are discarded: they are already primed.
    //
    // This is useful when skipping around an audio stream, so
    // that frames can be decoded (almost?) exactly despite the decoder
    // maintaining various audio coefficients between frames.
    //
    // For AAC audio (and probably most codecs), one frame of priming is enough.
    // So either you are processing the first frame and calling `.refresh()`
    // or you are processing a later frame and priming with the previous frame.
    //
    // Or this can be useful for video streams, which need to restart from an
    // IDR frame (ideally) or I frame (at least).
    //
    // (I frames contain a full encoded picture, so it will decode correctly on
    // its own, while IDR frames additionally guarantee that no prior data from
    // a video stream is necessary to decode them. Without this guarantee,
    // later B and P frames in the stream may give decoding errors due to
    // referencing data from frames that the encoder has not seen.)
    //
    // This is not useful for *encoders*, where the discarded frames will mean
    // that the following encoded frames will not decode correctly.
    prime: (frames: I[], keepOpen?: boolean) => void,

    // Decode/encode a frame and wait for the output to be available. Be careful
    // to either `.close()` the raw frame when it is no longer needed or make sure
    // the `Promise` is not accessible so `O` can be garbage collected, since
    // the `Promise` itself retains the data after it resolves.
    //
    // The API for transcoders is traditionally done asynchronously, returning
    // output frames without reference to the input frame that sparked it. That
    // makes sense for an optimized C API but is egregiously difficult to use
    // from JavaScript.  It is much easier to recreate the classic API from this
    // Promise-based API by calling `.then()` on each Promise, since they
    // resolve in order.
    process: (frame: I, keepOpen?: boolean) => Promise<O>,
  };

  // Abstract over `AudioDecoder`, `AudioEncoder`, `VideoDecoder`, and `VideoEncoder`
  export type _WebTranscoderClass<config, error, I, O> = {
    new(callbacks: { output: Bed.Cb<O>, error: Bed.Cb<error> }): _WebTranscoder<config, I>;
    prototype: _WebTranscoder<config, I>;
  };
  export type _WebTranscoder<config, I> = {
    configure(config: config): void;
    flush(): Promise<void>;
    reset(): void;
    close(): void;
  } & ({ decode(chunk: I): void; encode?: undefined } | { encode(chunk: I): void; decode?: undefined });

  // Abstract over `EncodedAudioChunk`, `EncodedVideoChunk`, `AudioData`, and `VideoFrame`
  export type FrameLike = {
    timestamp: number; // used for priming, the semantics do not matter
  };
  export type FrameResource = FrameLike & {
    close?(): void;
  };

  // A promise-based API for a decoder/encoder. Retries queued frames if they
  // do not seem to be directly responsible for the error, by recreating
  // the decoder and re-queueing them. This only works if the raw frames are
  // still open.
  export function _promiseTranscoder<
    config, error, I extends FrameResource, O extends FrameResource
  >(config: config, transcoderCls: _WebTranscoderClass<config, error, I, O>): Transcoder<I, O> {
    // (Re)creating the transcoder should be a simple function, no?
    const mkprocessor = (config: config, output: Bed.Cb<O | undefined>, error: Bed.Cb<error | undefined>) => {
      // Create the processor with the callbacks
      const processor: _WebTranscoder<config, I> = new transcoderCls({ output, error });
      // And set the config
      processor.configure(config);
      // console.debug(processor, transcoderCls.isConfigSupported(config));

      // Reset the per-transcoder statistics
      lastFrameTimestamp = undefined;
      frame = 0;

      return {
        process: processor.decode
          ? processor.decode.bind(processor)
          : processor.encode.bind(processor)
          ,
        close: () => processor.close(),
      };
    };

    ////////////////////////////////////////////////////////////////////////////
    // The type of the frame callbacks that are kept in the queues. The error
    // will be undefined for frames after the one that error.
    type FrameCb = (frameOrUndef: O | undefined, errorOrUndef?: error | undefined) => void;
    //
    // The primary queue of callbacks for each frame, kept in decoding order
    // and indexed by `frame` (decoding order index).
    let queue = new Map<number, FrameCb>();
    // A backwards queue of additional frame queues, accessed with `.unshift()`
    // and `.pop()`, which queues a `.refresh()` between frame queues.
    let whenSettled: (typeof queue)[] = [];
    //
    // The timestamp of the last frame put onto the queue, or `undefined` if none
    // (reset to `undefined` by `refresh()`).
    let lastFrameTimestamp: number | undefined = undefined;
    // The index of frame being processed through this transcoder instance
    // (reset to zero by `refresh()`).
    let frame = 0;
    ////////////////////////////////////////////////////////////////////////////

    // We can “refresh” the transcoder: closing the old one and creating a new
    // one from scratch
    const refresh = () => {
      if (!frame) return; // we just refreshed it
      try { transcoder?.close(); } catch(e) { console.warn(e) }
      transcoder = mkprocessor(config, onoutput, onerror);
    };
    // This is always our `output` callback for each transcoder
    const onoutput = (outputFrame: O | undefined) => {
      // Pop from the queue
      const [key, cb] = queue.entries().next().value!;
      queue.delete(key);
      // We have successfully processed the immediate queue,
      // so continue on with the queue that is waiting for
      // it to be settled
      if (!queue.size && whenSettled.length) {
        queue = whenSettled.pop()!;
        refresh();
      }
      // Return the frame output!
      cb(outputFrame);
    };
    // This is always our `error` callback for each transcoder
    const onerror = (error: error | undefined) => {
      // Burn the whole queue
      const burned = queue.values();
      queue = new Map();

      // Create a new transcoder
      transcoder = mkprocessor(config, onoutput, onerror);

      // Process all the callbacks: only the first one gets a real error,
      // because the error came from it, while the rest get error `undefined`.
      for (const cb of burned) {
        try { cb(undefined, error); } catch {}
        error = undefined;
      }

      // If no frames needed/wanted to be recreated, advance to the next queue
      if (!queue.size) queue = whenSettled.pop() ?? new Map();

      // Unknown error, not associated with any frame.
      if (error) console.error(error);
    };

    // Create one transcoder immediately. This will be reset with a new
    // instance from mkprocessor on error or on refresh, with the same arguments
    let transcoder = mkprocessor(config, onoutput, onerror);

    // Pass the encoded frame into the transcoder and leave a callback to resolve
    // with the decoded or encoded output frame
    const processit = (data: I, keepOpen?: boolean) => new Promise<O>((resolve, reject) => {
      (whenSettled[0] ?? queue).set(frame++, (frameOrUndef, errorOrUndef) => {
        if (frameOrUndef) {
          resolve(frameOrUndef);
        } else if (errorOrUndef) {
          reject(errorOrUndef);
        } else {
          processit(data, keepOpen); // retry
        }
      });
      lastFrameTimestamp = data.timestamp;
      try {
        transcoder.process(data);
        if (!keepOpen) data.close?.();
      } catch(err) {
        reject(err);
      }
    });
    // Prime with ignored frames as necessary
    processit.prime = (datas: I[], keepOpen?: boolean) => {
      let i = datas.length;
      // Back up until we have just seen a frame
      while (i-- > 0)
        if (datas[i].timestamp === lastFrameTimestamp) break;
      // console.debug('needed priming', datas.length - i - 1, 'given priming', datas.length, lastFrameTimestamp, datas.map(d=>d.timestamp));
      // Resume decoding from there, discarding the decoded frames
      while (++i < datas.length)
        processit(datas[i], keepOpen).then(decoded => decoded.close?.());
    };
    // Queue recreating the transcoder
    processit.refresh = () => {
      if (queue.size) {
        whenSettled.unshift(new Map());
      } else {
        refresh();
      }
    };

    return {
      process: processit,
      prime: processit.prime,
      refresh: processit.refresh,
    };
  };

  export function audioDecoder(config: AudioDecoderConfig): Transcoder<EncodedAudioChunk, AudioData> {
    return _promiseTranscoder(config, AudioDecoder);
  };
  export function videoDecoder(config: VideoDecoderConfig): Transcoder<EncodedVideoChunk, VideoFrame> {
    return _promiseTranscoder(config, VideoDecoder);
  };

  export function audioEncoder(config: AudioEncoderConfig): Transcoder<AudioData, EncodedAudioChunk> {
    return _promiseTranscoder(config, AudioEncoder);
  };
  export function videoEncoder(config: VideoEncoderConfig): Transcoder<VideoFrame, EncodedVideoChunk> {
    return _promiseTranscoder(config, VideoEncoder);
  };

  // export function lruCache<I, O>(decoder: Transcoder<I, O>): {
  //   decoder: Transcoder<I, { use: () => O, uncache: Bed.Destr }>,
  //   destroy: Bed.Destr,
  //   check: () => { [k in "inputs" | "outputs"]: { allowed: number, current: number, active: number } }
  // } {
  //   return undefined as any;
  // }
} // namespace Codecs



////////////////////////////////////////////////////////////////////////////////
export namespace Workers {
  export type Channel<Is extends any[], O> = {
    send: (...arg: Is) => void,
    receive: River<O>,
  };

  // A Remote Procedure Call sends an event through the channel and waits for
  // a matching response. Easy to write with the stream methods we have access to.
  export function RPC<
    Is extends [], O, R, iface extends Channel<Is, O> = Channel<Is, O>
  >(
    iface: iface, recognize: (...arg: Is) => (res: O) => [] | [R]
  ): iface & { request: (...arg: Is) => Promise<R> } {
    return {
      ...iface,
      request: (...arg) => {
        iface.send(...arg);
        return iface.receive
          .mapArray(recognize(...arg))
          .limitTo(1).nextValue();
      },
    };
  };

  // One `Stream` will be in control of the `send` interface at a time,
  // and can be revoked individually or collectively.
  export function mintController<T>(send: (value: T) => void):
    { control: (controller: Stream<T>) => Bed.Destr, destroy: Bed.Destr }
  {
    const scope = Resource.subScope();
    return {
      control: Resource.saveRevolvingScope(controller => {
        if (!scope.destroyed()) return controller.subscribe(send);
        return Bed.nocb;
      }),
      destroy: scope.destroy,
    };
  };

  export function elongate<I extends any[], X, O>(
    channel1: Channel<I, X>,
    channel2: Channel<[X], O>,
  ): Channel<I, O> & { disconnect: Bed.Destr } {
    return {
      send: channel1.send,
      receive: channel2.receive,
      disconnect: channel1.receive.subscribe(channel2.send),
    };
  };
  export function interconnect<X, Y>(
    channel1: Channel<[Y], X>,
    channel2: Channel<[X], Y>,
  ): Bed.Destr {
    const d1 = channel1.receive.subscribe(channel2.send);
    const d2 = channel2.receive.subscribe(channel1.send);
    return () => { d1(); d2() }
  };

  // Web Worker. You may want to initialize this under `Resource.impervious`
  // to provide your own graceful shutdown instead of `Worker.terminate()`.
  export function Worker(...args: ConstructorParameters<typeof window.Worker>) {
    const worker = new window.Worker(...args);

    Resource.tryAddDestructor(() => worker.terminate());

    return {
      worker,
      send: worker.postMessage.bind(worker),
      errors: DOM.listener<Worker, "error", Event>(worker, "error"),
      receive: DOM.listener<Worker, "message", MessageEvent>(worker, "message"),
    };
  };

  // A named broadcast channel, for communication across tabs per origin.
  // (The same name always returns an equivalent channel.)
  export function BroadcastAs(name: string) {
    const port = new BroadcastChannel(name);
    Resource.tryAddDestructor(port.close.bind(port));
    const stream = DOM.listener<BroadcastChannel, "message", MessageEvent>(port, "message");
    const errors = DOM.listener<BroadcastChannel, "messageerror", MessageEvent>(port, "messageerror");
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
  };

  // Message Port, used for some types of Workers other contexts like iframes
  export function Port(port: MessagePort) {
    const events = DOM.listenTo<MessagePortEventMap, MessagePort>(port);
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
      onClose: Resource.impervious(() => DOM.mintListener1<MessagePort, string, Event>(port, "close").promise),
      close: Resource.tryAddDestructor(port.close.bind(port)),
    };
  };

  export function PortPair() {
    let channel = new MessageChannel();
    return {
      ports: [
        _lazy(() => Port(channel.port1)),
        _lazy(() => Port(channel.port2)),
      ],
      close: () => {
        // FIXME?
        try { channel.port1.close(); } catch {}
        try { channel.port2.close(); } catch {}
      },
    };
  };

  // A named Lock per origin. Perform work while holding the `Lock`
  // (although it can be stolen, and there does not appear to be any event
  // for that...).
  //
  // honestly, `navigator.locks.request` is pretty straightforward already
  export function Lock<T>(name: string, options: LockOptions, work: (lock: Lock | null) => MaybePromise<T>): Promise<T> {
    const scopeSignal = mintAbortSignal();
    options = { ...options, signal: options.signal ? AbortSignal.any([ options.signal, scopeSignal ]) : scopeSignal };
    return window.navigator.locks.request(name, options, work);
  };
} // namespace Workers



////////////////////////////////////////////////////////////////////////////////
export namespace Navigator {
  // A weak hint for whether the device is online (different across browsers)
  export const onlineHint =
    Stream.oneStream([
      repollable("onlineHint"),
      DOM.listener(window, "online"),
      DOM.listener(window, "offline"),
    ]).map(() => window.navigator.onLine).memoize();

  const _WakeLocks = new Set<WeakRef<WakeLockSentinel>>();
  export const wakeLock = {
    // Request a wake lock, register it as a resource to destroy and return that
    // destructor. Also weakly save it in a registry.
    request: async () => {
      const lock = await Resource.then(window.navigator.wakeLock.request());

      const destroy = Resource.tryAddDestructor(lock.release.bind(lock));

      // Clean up stale weak refs
      for (const other of [..._WakeLocks])
        if (!other.deref()) _WakeLocks.delete(other);

      // Add this weak ref, until it is released
      const weakref = new WeakRef(lock);
      _WakeLocks.add(weakref);
      lock.addEventListener("release", () => _WakeLocks.delete(weakref));

      // Return the destructor: no need for `WakeLockSentinel` interface
      return destroy as Bed.Destr;
    },
    // This can only release locks we know about from `wakeLock.request`
    releaseAll: (() => {
      for (const weakref of [..._WakeLocks])
        weakref.deref()?.release();
      _WakeLocks.clear();
    }) as Bed.Destr,
  };

  // A live view of the available media devices, listening for `devicechange`
  // events. It will ignore extra events if they occur while an enumeration
  // is in progress, for debouncing purposes.
  //
  // TODO: add synchronous store behavior when at least one is active?
  export const mediaDevices: River<MediaDeviceInfo[]> =
    Stream.oneStream([
      repollable("mediaDevices"),
      DOM.listener(window.navigator.mediaDevices, "devicechange" as const),
    ]).mapBlocking(() =>
      Stream.fromPromise(window.navigator.mediaDevices.enumerateDevices())
    ).memoize();

  // The weird `setActionHandler` interface does not broadcast like
  // `.addEventListener()`, it acts like `.onevent=` instead, so this caches the
  // rivers while still respecting that it is set to null when there are no
  // active subscribers.
  export const actionHandler: (action: MediaSessionAction) => River<MediaSessionActionDetails> =
    _lazyKeyed((action: MediaSessionAction) => {
      return Stream.makeLake<MediaSessionActionDetails>(cb => {
        window.navigator.mediaSession.setActionHandler(action, cb);
        return () => window.navigator.mediaSession.setActionHandler(action, null);
      }).memoize();
    });

  // playbackState (status?)
  // setPositionState (media playback position)
  // userActivation
} // namespace Navigator
