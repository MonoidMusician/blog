import { Cb, Destr, Maybe, mintCounter, mintCleanup, mintThreshold } from "./Bed.js";
import * as Bed from "./Bed.js";
import * as Resource from "./Resource.js";

export type Id = number;

export type StreamBurstBehavior<T> = {
  static: T[],
  dynamic?: (() => T[]) | undefined
}[];
export type StreamCallbacks<T> = {
  receive(value: T): void,
  commit(source: Id): void,
  runDry(): void,
};
export type StreamSubscription<T> = {
  burst: T[],
  sources: Set<Id>,
  unsubscribe(): void,
};

export type Flowing = "Flowing" | "NotFlowing";

export class Stream<T, flow extends Flowing = Flowing> implements AsyncIterator<T, undefined, void> {
  _flow: (numquam: never) => flow = (numquam) => numquam;
  flowing: boolean;
  _burstBehavior: StreamBurstBehavior<T>;
  _subscribe: (callbacks: StreamCallbacks<T>) => StreamSubscription<T>;

  static mkId = mintCounter();

  constructor(copy: {
    flowing: boolean;
    _burstBehavior: StreamBurstBehavior<T>;
    _subscribe: (callbacks: StreamCallbacks<T>) => StreamSubscription<T>;
  }) {
    this.flowing = copy.flowing;
    this._burstBehavior = copy._burstBehavior;
    this._subscribe = copy._subscribe;
  }

  subscribe(receive: (value: T) => void, runDry?: () => void): Destr {
    let unsubscriber = Bed.mintRolling<void>();
    let hasLoaded = false;
    let isDestroyed = false;
    let destroy = Resource.tryAddDestructor(() => {
      isDestroyed = true;
      receive = Bed.nocb;
      runDry = Bed.nocb;
      destroy = Bed.nocb;
      unsubscriber(Bed.nocb);
    });
    let { unsubscribe, burst } = this._subscribe({
      receive: Resource.saveScope(value => {
        if (!isDestroyed) receive(value);
      }),
      commit: Bed.nocb,
      runDry: Resource.saveScope(() => {
        destroy();
        if (hasLoaded) runDry?.(); // otherwise queue it after `burst`
      }),
    });
    unsubscriber(unsubscribe);
    for (const v of burst) receive(v);
    burst = [];
    hasLoaded = true;
    if (isDestroyed) runDry?.();
    return destroy;
  }
  subscribe1(receive: (value: T) => void, runDry?: () => void): Destr {
    return this.subscribe(Resource.saveRevolvingScope(receive), runDry);
  }
  async next(): Promise<{ value: T, done: false } | { value: undefined, done: true }> {
    return await new Promise(resolve => {
      const unsubscriber = Bed.mintRolling<void>();
      this.subscribe((value: T) => {
        resolve({ value, done: false });
        unsubscriber(Bed.nocb);
      }, () => resolve({ value: undefined, done: true }));
    });
  }
  async nextValue(): Promise<T> {
    const got = await this.next();
    if (!got.done) return got.value;
    return await new Promise(()=>{});
  }
  [Symbol.asyncIterator]() {
    if (this.flowing) return this;
    const { send, destroy, stream } = Stream.createRiver();
    this.subscribe(send, destroy);
    return stream;
  }

  static makeLake<T>(subscribe: (emit: (value: T) => void, runDry: () => void) => Destr): Lake<T> {
    return new Stream<T, "NotFlowing">({
      flowing: false,
      _burstBehavior: [{ static: [], dynamic: () => {
        const burst: T[] = [];
        const unsubscribeImmediately = subscribe(value => burst.push(value), Bed.nocb);
        unsubscribeImmediately();
        return burst;
      } }],
      _subscribe: (cbs: StreamCallbacks<T>) => {
        const id = Stream.mkId();
        const selfSource = new Set([id]);
        return Bed.gatherLoadingBurst<T, StreamSubscription<T>>(captureBurst => {
          const unsubscribe = Resource.start(Resource.unSub, subscribe, (value: T) => {
            if (!captureBurst(value)) {
              cbs.receive(value);
              cbs.commit(id);
            }
          }, cbs.runDry).destroy;
          return (burst: T[]) => ({ burst, sources: selfSource, unsubscribe });
        });
      },
    });
  }

  static createRiver<T>(burstBehavior?: StreamBurstBehavior<T>, beforeBroadcast?: Cb<T>): {
    send: Cb<T>, stream: River<T>, destroy: Destr
  } {
    const id = Stream.mkId();
    const prox = this._createProxy(burstBehavior, beforeBroadcast);
    return {
      send: (value: T) => { prox.send(value); prox.commit(id) },
      stream: prox.stream(new Set([id])),
      destroy: prox.destroy,
    };
  }
  static _createProxy<T>(
    burstBehavior?: StreamBurstBehavior<T>,
    beforeBroadcast?: Cb<T>,
  ): {
    send: Cb<T>, commit: Cb<Id>,
    stream: (sources: Set<Id>) => River<T>,
    destroy: Destr, subscribers: () => number,
  } {
    const { push, notify, destroy, running, size } = Bed.mintSubscriptions<StreamCallbacks<T>>();
    return {
      send: (value: T) => {
        beforeBroadcast?.(value);
        notify(({ receive }) => receive(value));
      },
      commit: id => {
        notify(({ commit }) => commit(id));
      },
      stream: sources => new Stream({
        flowing: true,
        _burstBehavior: burstBehavior ?? [],
        _subscribe: cbs => {
          const burst = (burstBehavior??[]).flatMap(b => b.static.concat(b.dynamic?.() ?? []));
          const unsubscribe = running() ? push(cbs) : (cbs.runDry(), Bed.nocb);
          return { burst, sources, unsubscribe };
        },
      }),
      destroy: Resource.tryAddDestructor(() =>
        destroy(({ runDry }) => runDry())),
      subscribers: size,
    };
  }
  memoize(): River<T> {
    const prox = Stream._createProxy<T>();
    let stream: River<T> = Stream.empty;
    let release: Bed.Destr = Bed.nocb;
    return new Stream({
      flowing: true,
      _burstBehavior: this._burstBehavior,
      _subscribe: cbs => {
        let burst: T[];
        if (prox.subscribers() === 0) {
          const ret = this._subscribe({
            receive: prox.send,
            commit: prox.commit,
            runDry: prox.destroy,
          });
          stream = prox.stream(ret.sources);
          release = ret.unsubscribe;
          burst = ret.burst;
        } else {
          burst = this.burst();
        }
        const { sources, unsubscribe } = stream._subscribe(cbs);
        return {
          burst,
          sources,
          unsubscribe: () => {
            unsubscribe();
            if (prox.subscribers() === 0) {
              release();
              stream = Stream.empty;
              release = Bed.nocb;
            }
          },
        };
      },
    });
  }

  static createStore<T>(initialValue: T) {
    return Object.assign(Stream.createRiver<T>(
      [{ static: [], dynamic: () => [initialValue] }],
      (pushed: T) => initialValue = pushed,
    ), { current: () => initialValue });
  }

  static createRiverStore<T>() {
    let hasValue = false;
    let value: T;
    return Object.assign(Stream.createRiver<T>(
      [{ static: [], dynamic: () => hasValue ? [value] : [] }],
      (pushed: T) => {value = pushed; hasValue = true},
    ), { current: () => (hasValue ? value : undefined) as T | undefined });
  }

  unsafeRiver(): River<T> {
    const stream = new Stream<T, "Flowing">(this);
    stream.flowing = true;
    return stream;
  }
  instantiate(): { burst: T[], stream: River<T>, destroy: Bed.Cb } {
    const prox = Stream._createProxy<T>(this._burstBehavior);
    const sub = this._subscribe({
      receive: prox.send,
      commit: prox.commit,
      runDry: prox.destroy,
    });
    return {
      burst: sub.burst,
      stream: prox.stream(sub.sources),
      destroy: () => { prox.destroy(); sub.unsubscribe(); },
    };
  }
  store(...arg: [] | [T]): { burst: T[], stream: River<T>, destroy: Bed.Cb } {
    let hasValue = !!arg.length;
    let value: T;
    if (arg.length) value = arg[0];
    const prox = Stream._createProxy(
      [{ static: [], dynamic: () => hasValue ? [value] : [] }],
      (newValue) => { value = newValue; hasValue = true },
    );
    const sub = this._subscribe({
      receive: prox.send,
      commit: prox.commit,
      runDry: prox.destroy,
    });
    return {
      burst: sub.burst,
      stream: prox.stream(sub.sources),
      destroy: () => { prox.destroy(); sub.unsubscribe(); },
    };
  }

  burst(): T[] {
    return (this._burstBehavior??[]).flatMap(b =>
      b.static.concat(b.dynamic?.() ?? []));
  }
  dynamic(): Stream<T, flow> {
    return new Stream({
      ...this,
      _burstBehavior: [{ static: [], dynamic: () => this.burst() }],
    });
  }

  static bursting = <T, flow extends Flowing = Flowing>(values: T[], dynamic?: () => T[]): Stream<T, flow> =>
    new Stream({
      flowing: true,
      _burstBehavior: values.length || dynamic ? [{ static: values, dynamic }] : [],
      _subscribe: () => ({ burst: values, sources: new Set(), unsubscribe: Bed.nocb })
    });
  static pure = <T>(value: T): River<T> => Stream.bursting([ value ]);
  static poke = <T>(value: T): River<T> => Stream.bursting([], () => [ value ]);
  static empty: River<never> = Stream.bursting([]);

  static fromPromise<T>(promise: Promise<T>): River<T> {
    return Stream.makeLake<T>((emit, runDry) => {
      let running = true;
      promise
        .then(v => {if (running) emit(v)})
        .finally(() => {if (running) runDry()})
        ;
      return () => running = false;
    }).unsafeRiver();
  }

  dam<flow extends Flowing>(): Stream<T, flow> {
    return new Stream({ ...this, flowing: false });
  }

  static oneStream<T, flow extends Flowing = Flowing>(streams: Stream<T, flow>[]): Stream<T, flow> {
    streams = streams.filter(s => s !== Stream.empty as Stream<T>);
    if (!streams.length) return Stream.empty as Stream<T> as Stream<T, flow>;
    if (streams.length === 1) return streams[0];
    return new Stream({
      flowing: streams.every(s => s.flowing),
      _burstBehavior: streams.flatMap(s => s._burstBehavior),
      _subscribe: cbs => {
        const runDry = mintThreshold(streams.length, cbs.runDry);
        const unsubs: Destr[] = [];
        const result: StreamSubscription<T> = {
          burst: [],
          sources: new Set(),
          unsubscribe: mintCleanup<void>(() => { for (const unsub of unsubs) unsub() }),
        };
        result.burst = streams.flatMap(s => {
          const sub = s._subscribe({
            ...cbs, runDry: mintCleanup(runDry),
          });
          for (const id of sub.sources) sub.sources.add(id);
          unsubs.push(sub.unsubscribe);
          return sub.burst;
        });
        return result;
      }
    });
  }

  static combineStreams<A, B, C, flow extends Flowing = Flowing>(
    logic: [false | ((value0: A) => boolean), false | ((value1: B) => boolean)],
    combine: (value0: A, value1: B) => C,
    ...streams: [Stream<A, flow>, Stream<B, flow>]
  ): Stream<C, flow> {
    let _burstBehavior = streams[0]._burstBehavior.flatMap(burst0 =>
      streams[1]._burstBehavior.map(burst1 => {
        return {
          static: burst0.static.flatMap(value0 =>
            burst1.static.map(value1 => combine(value0, value1))
          ),
          dynamic: (!burst0.dynamic && !burst1.dynamic) ? undefined : () =>
            (burst0.static.concat(burst0.dynamic?.() ?? [])).flatMap(value0 =>
              (burst1.static.concat(burst1.dynamic?.() ?? [])).map(value1 => combine(value0, value1))
            ),
        };
      })
    );
    return new Stream({
      flowing: streams.every(s => s.flowing),
      _burstBehavior,
      _subscribe: cbs => {
        const runDry = mintThreshold(2, cbs.runDry);
        const lastValues: [Maybe<A>, Maybe<B>] = [[], []];
        let needsPush = false;
        let sourcesR = new Set<Id>();

        let commit = (id: Id, shouldPush: boolean) => {
          if (shouldPush) {
            if (lastValues[0]?.length && lastValues[1]?.length) {
              cbs.receive(combine(lastValues[0][0], lastValues[1][0]));
            } else {} // have not received a value on both sides
          }
          cbs.commit(id);
        };
        let { cbL, commitL, cbR, commitR } = {
          cbL: (value0: A) => {
            lastValues[0][0] = value0;
            needsPush = true;
          },
          commitL: (id: Id) => {
            const last = needsPush ? lastValues[0] : undefined;
            const handle = ({ shouldCommit, shouldPush }: { shouldCommit: boolean, shouldPush: boolean }) => {
              if (shouldCommit) commit(id, shouldPush);
            };
            if (logic[0] === false) {
              return handle({ shouldCommit: false, shouldPush: false });
            } else if (logic[1] === false) {
              return handle({ shouldCommit: true, shouldPush: !!last?.length && logic[0](last[0]) });
            } else {
              return handle({ shouldCommit: !sourcesR.has(id), shouldPush: !!last?.length && logic[0](last[0]) })
            }
          },

          cbR: (value1: B) => {
            lastValues[1][0] = value1;
            needsPush = true;
          },
          commitR: (id: Id) => {
            const last = needsPush ? lastValues[1] : undefined;
            const handle = ({ shouldCommit, shouldPush }: { shouldCommit: boolean, shouldPush: boolean }) => {
              if (shouldCommit) commit(id, shouldPush);
            };
            if (logic[1] === false) {
              return handle({ shouldCommit: false, shouldPush: false });
            } else {
              return handle({ shouldCommit: true, shouldPush: !!last?.length && logic[1](last[0]) })
            }
          },
        };

        let sub0 = streams[0]._subscribe({
          receive: cbL, commit: commitL,
          runDry: mintCleanup(runDry),
        });
        let sub1 = streams[1]._subscribe({
          receive: cbR, commit: commitR,
          runDry: mintCleanup(runDry),
        });
        sourcesR = sub1.sources;
        if (sub0.burst.length) lastValues[0] = [sub0.burst[sub0.burst.length - 1]];
        if (sub1.burst.length) lastValues[1] = [sub1.burst[sub1.burst.length - 1]];
        return {
          burst: sub0.burst.flatMap(value0 =>
            sub1.burst.map(value1 => combine(value0, value1))
          ),
          sources: new Set([
            ...(logic[0] !== false ? sub0.sources : []),
            ...(logic[1] !== false ? sub1.sources : []),
          ]),
          unsubscribe: mintCleanup<void>(sub0.unsubscribe, sub1.unsubscribe),
        };
      }
    })
  }


  latestStream<R, flow extends Flowing = Flowing>(
    mkStream: (event: T) => Stream<R, flow>
  ): Lake<R> {
    return Stream.makeLake((cb, runDry) => {
      let noMore = runDry;
      return this.subscribe(Resource.saveRevolvingScope((v: T) =>
        mkStream(v).subscribe(cb, noMore = mintThreshold(2, runDry))
      ), () => noMore())
    });
  }

  allStreams<R, flow extends Flowing = Flowing>(
    mkStream: (event: T) => Stream<R, flow>
  ): Lake<R> {
    return Stream.makeLake((cb, runDry) => {
      let running = 0;
      let activeStream = () => {
        running++;
        return () => --running || runDry();
      };
      return this.subscribe(Resource.saveScope((v: T) =>
        mkStream(v).subscribe(cb, activeStream())
      ), activeStream())
    });
  }

  // While the inner stream has not run dry, the outer stream will not
  // spawn a new inner stream
  mapBlocking<R, flow extends Flowing = Flowing>(
    mkStream: (event: T) => Stream<R, flow>
  ): Lake<R> {
    return Stream.makeLake((cb, runDry) => {
      let outer = true;
      let inner = false;
      return this.subscribe(Resource.saveScope((v: T) => {
        if (inner) return; // ignore
        inner = true;
        return mkStream(v).subscribe(cb, () => {
          inner = false; // reactivate
          if (!outer) runDry();
        });
      }), () => {
        outer = false;
        if (!inner) runDry();
      })
    });
  }

  // Must be pure, or use `.dynamic().map(fn)`
  map<B>(fn: (value: T) => B): Stream<B, flow> {
    const { _subscribe } = this;
    return new Stream({
      flowing: this.flowing,
      _burstBehavior: this._burstBehavior.map(b =>
        ({ static: b.static.map(fn), dynamic: !b.dynamic ? undefined : () => b.dynamic?.().map(fn) ?? [] })
      ),
      _subscribe: cbs => {
        const ret = _subscribe({ ...cbs, receive: value => cbs.receive(fn(value)) });
        return {
          ...ret, burst: ret.burst.map(fn),
        };
      },
    });
  }
  mapArray<B>(fn: (value: T) => B[] | undefined): Stream<B, flow> {
    const f = (value: T) => fn(value) ?? [];
    return new Stream({
      flowing: this.flowing,
      _burstBehavior: this._burstBehavior.map(b =>
        ({ static: b.static.flatMap(f), dynamic: !b.dynamic ? undefined : () => b.dynamic?.().flatMap(f) ?? [] })
      ),
      _subscribe: cbs => {
        const ret = this._subscribe({ ...cbs, receive: value => f(value).forEach(cbs.receive) });
        return {
          ...ret, burst: ret.burst.flatMap(f),
        };
      },
    });
  }
  filter(fn: (value: T) => boolean): Stream<T, flow> {
    return new Stream({
      flowing: this.flowing,
      _burstBehavior: this._burstBehavior.map(b =>
        ({ static: b.static.filter(fn), dynamic: !b.dynamic ? undefined : () => b.dynamic?.().filter(fn) ?? [] })
      ),
      _subscribe: cbs => {
        const ret = this._subscribe({ ...cbs, receive: value => fn(value) && cbs.receive(value) });
        return {
          ...ret, burst: ret.burst.filter(fn),
        };
      },
    });
  }
  drop(n: number): Lake<T> {
    // TODO: static burst
    return Stream.makeLake((emit, runDry) => {
      return this.subscribe(value => {
        if (n > 0) n -= 1;
        else emit(value);
      }, runDry);
    });
  }
  takeWhile(fn: (value: T) => boolean): Lake<T> {
    // TODO: static burst
    return Stream.makeLake((emit, runDry) => {
      return this.subscribe(value => {
        if (fn(value)) emit(value);
        else runDry();
      }, runDry);
    });
  }
  whileDefined<T>(this: Stream<T | null | undefined, flow>): Lake<T> {
    return this
      .takeWhile(value => value !== null && value !== undefined)
      .mapArray(value => value !== null && value !== undefined ? [value] : []);
  }
  EOS(): Stream<undefined, flow> {
    return new Stream({
      ...Stream.makeLake((emit, runDry) => {
        return this.subscribe(Bed.nocb, () => {
          emit(undefined); runDry();
        });
      }),
      flowing: this.flowing,
    });
  }
  startWith<extra = never>(...events: (T | extra)[]): Stream<T | extra, flow> {
    return Stream.oneStream<T | extra, flow>([
      Stream.bursting(events),
      this,
    ]);
  }

  statefulStream<S, R>(start: S, accum: (state: S, value: T) => { state: S, emit: R[] }): Lake<R> {
    return Stream.makeLake((emit, runDry) => {
      let current = start;
      return this.subscribe((value: T) => {
        let step = accum(current, value);
        current = step.state;
        step.emit.forEach(emit);
      }, runDry);
    });
  }

  withLast(): Lake<{ next: T } | { last: T, next: T }> {
    return this.statefulStream([] as T[], (state, next) => {
      const emit = [state.length ? { last: state[0], next } : { next }];
      return { state: [next], emit };
    });
  }

  dedup(cmp?: (last: T, next: T) => boolean): Lake<T> {
    return this.withLast().mapArray(event => {
      if ("last" in event && (cmp ? cmp(event.last, event.next) : event.last === event.next))
        return [event.next];
      return [];
    });
  }

  rising(): Lake<T> {
    return this.withLast().mapArray(event => "last" in event && event.last ? [] : [event.next]);
  }
  falling(): Lake<T> {
    return this.withLast().mapArray(event => "last" in event && event.last ? [event.next] : []);
  }

  limitTo(n=1): Lake<T> {
    if (n === 0) return Stream.empty.dam();
    let staticBurst: T[] = []; let ib = 0;
    for (const b of this._burstBehavior) {
      staticBurst.push(...b.static);
      if (staticBurst.length >= n)
        return Stream.bursting(staticBurst.slice(0, n));
      if (b.dynamic) break;
      ib++;
    }
    const newDynamic = [
      {
        static: [],
        dynamic: this._burstBehavior[ib].dynamic,
      },
      ...this._burstBehavior.slice(ib+1)
    ];
    const { _subscribe } = this;
    return new Stream({
      flowing: false,
      _burstBehavior: [{ static: staticBurst }],
      _subscribe: cbs => {
        const setUnsub = Bed.mintRolling<void>();
        const receive = Bed.mintBreaker(cbs.receive);
        const commit = Bed.mintBreaker(cbs.commit);
        let destroyed = () => {
          setUnsub(Bed.nocb);
          receive.trip();
          commit.trip();
          cbs.runDry();
        };
        let incr = mintThreshold(n - staticBurst.length, destroyed);
        let sub = _subscribe({
          receive: receive.run,
          commit: id => { incr(); commit.run(id) },
          runDry: destroyed,
        });
        setUnsub(sub.unsubscribe);
        for (const _ of sub.burst) incr();
        return { ...sub, burst: sub.burst.slice(0, n - staticBurst.length) };
      },
    });
  }

  delay(delay: number | ((cb: Bed.Cb)=>Bed.Destr)): Lake<T> {
    if (typeof delay === 'number') {
      const ms = delay;
      delay = cb => {
        const timer = setTimeout(cb, ms);
        return () => clearTimeout(timer);
      };
    }
    return Stream.makeLake<T>((cb, runDry) => {
      let isDestroyed = false;

      let ids = Bed.mintCounter();

      // We need to be able to cancel everything that is in flight
      let inflight: Map<number, Bed.Destr> = new Map();
      Resource.addDestructor(() => {
        for (const nvm of inflight.values()) nvm();
        inflight.clear();
        isDestroyed = true;
      });

      return this.subscribe(
        (value: T) => {
          if (isDestroyed) return;
          const id = ids();
          inflight.set(id, delay(() => {
            inflight.delete(id);
            cb(value);
            // queue drained? finish up
            if (isDestroyed && !inflight.size) runDry();
          }));
        }, () => {
          isDestroyed = true;
          if (!inflight.size) runDry();
          // if not, the last in flight callback will trigger it
        }
      );
    });
  }

  mailbox<K, V>(
    this: Stream<{ key: K, value: V }, flow>,
    // Defaults to using a `Map<K, _>`
    storage?: {
      get(key: K): MailboxSlot<V> | undefined,
      set(key: K, value: MailboxSlot<V> | undefined): void,
      reset(): void,
    },
    burstBehavior?: (key: K) => StreamBurstBehavior<V> | undefined,
    // Can keep the source, but at the cost of broadcasting commits to all
    // downstream events, canceling most of the benefit of the mailboxing
    keepSource: boolean = false,
  ): (key: K) => River<V> {
    const scope = Resource.subScope();
    if (!storage) {
      const streams = new Map<K, MailboxSlot<V>>();
      storage = {
        get: key => streams.get(key),
        set: (key, value) => value ? streams.set(key, value) : streams.delete(key),
        reset: () => streams.clear(),
      };
    }
    const committers: Array<(id: Id)=>void> = [];
    scope.addDestructor(() => storage.reset());
    const { sources } = this._subscribe({
      receive: ({ key, value }) => storage.get(key)?.send(value),
      commit: keepSource ? id => committers.forEach(c => c(id)) : Bed.nocb,
      runDry: scope.destroy,
    });
    return key => {
      if (scope.destroyed()) return Stream.empty;
      return storage.get(key)?.stream ??
        Resource.withScope(scope, () => {
          const downstream = Stream._createProxy<V>(burstBehavior?.(key));
          if (keepSource) {
            const stream = downstream.stream(sources);
            storage.set(key, {
              send: downstream.send,
              stream,
            });
            committers.push(downstream.commit);
            return stream;
          } else {
            const id = Stream.mkId();
            const stream = downstream.stream(new Set([id]));
            storage.set(key, {
              send: value => { downstream.send(value); downstream.commit(id) },
              stream,
            });
            return stream;
          }
          // TODO: delete when back down to zero subscribers?
        });
    };
  }
};

type MailboxSlot<V> = {
  stream: River<V>,
  send(value: V): void,
};

export type Lake<T> = Stream<T, "NotFlowing">;
export type River<T> = Stream<T, "Flowing">;
