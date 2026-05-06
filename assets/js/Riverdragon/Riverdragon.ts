import { Cb, Destr, Maybe, freshId, cleanup, threshold } from "./bed";
import * as Bed from "./bed";

type Id = number;

type StreamBurstBehavior<T> = {
  static: T[],
  dynamic?: (() => T[]) | undefined
}[];
type StreamCallbacks<T> = {
  receive: (value: T) => void,
  commit: (source: Id) => void,
  runDry: () => void,
};
type StreamSubscription<T> = {
  burst: T[],
  sources: Set<Id>,
  unsubscribe: () => void,
};

type Flowing = "Flowing" | "NotFlowing";
class Stream<flow extends Flowing, T> {
  _flow: (numquam: never) => flow = (numquam) => numquam;
  flowing: boolean;
  _burstBehavior: StreamBurstBehavior<T>;
  _subscribe: (callbacks: StreamCallbacks<T>) => StreamSubscription<T>;

  static mkId = freshId();

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
    let unsubscriber = Bed.rolling<void>();
    let hasLoaded = false;
    let isDestroyed = false;
    let sub = this._subscribe({
      receive: value => {
        if (!isDestroyed) receive(value);
      },
      commit: Bed.nocb,
      runDry: () => {
        unsubscriber(Bed.nocb);
        isDestroyed = true;
        if (hasLoaded) runDry?.();
      },
    });
    unsubscriber(sub.unsubscribe);
    for (const v of sub.burst) receive(v);
    hasLoaded = true;
    if (isDestroyed) runDry?.();
    return cleanup(() => unsubscriber(Bed.nocb));
  }

  static makeLake<T>(subscribe: (emit: (value: T) => void, runDry: () => void) => Destr): Stream<"NotFlowing", T> {
    return new Stream<"NotFlowing", T>({
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
        return Bed.loadingBurst<T, StreamSubscription<T>>(captureBurst => {
          const unsubscribe = cleanup(subscribe((value: T) => {
            if (!captureBurst(value)) {
              cbs.receive(value);
              cbs.commit(id);
            }
          }, cbs.runDry));
          return (burst: T[]) => ({ burst, sources: selfSource, unsubscribe });
        });
      },
    });
  }

  static createRiver<T>(burstBehavior?: StreamBurstBehavior<T>, beforeBroadcast?: Cb<T>): {
    send: Cb<T>, stream: Stream<"Flowing", T>, destroy: Destr
  } {
    const id = Stream.mkId();
    const selfSource = new Set([id]);
    const { push, notify, destroy, running } = Bed.subscriptions<StreamCallbacks<T>>();
    return {
      send: (value: T) => {
        notify(({ receive }) => receive(value));
        notify(({ commit }) => commit(id));
      },
      stream: new Stream({
        flowing: true,
        _burstBehavior: burstBehavior ?? [],
        _subscribe: cbs => {
          const burst = (burstBehavior??[]).flatMap(b => b.static.concat(b?.dynamic?.() ?? []));
          const unsubscribe = running() ? push(cbs) : (cbs.runDry(), Bed.nocb);
          return { burst, sources: selfSource, unsubscribe };
        }
      }),
      destroy: () => destroy(Bed.nocb),
    };
  }

  static createStore<T>(initialValue: T) {
    return Stream.createRiver<T>(
      [{ static: [], dynamic: () => [initialValue] }],
      (pushed: T) => initialValue = pushed,
    );
  }

  static bursting = <T>(values: T[]): River<T> =>
    new Stream({
      flowing: true,
      _burstBehavior: values.length ? [{ static: values }] : [],
      _subscribe: () => ({ burst: values, sources: new Set(), unsubscribe: Bed.nocb })
    });
  static pure = <T>(value: T): River<T> => Stream.bursting([ value ]);
  static empty = Stream.bursting([]);

  dam<flow extends Flowing>(): Stream<flow, T> {
    return new Stream({ ...this, flowing: false });
  }

  static oneStream<flow extends Flowing, T>(streams: Stream<flow, T>[]): Stream<flow, T> {
    if (!streams.length) return Stream.empty.dam();
    if (streams.length === 1) return streams[0];
    return new Stream({
      flowing: streams.every(s => s.flowing),
      _burstBehavior: streams.flatMap(s => s._burstBehavior),
      _subscribe: cbs => {
        const runDry = threshold(streams.length, cbs.runDry);
        const unsubs: Destr[] = [];
        const result: StreamSubscription<T> = {
          burst: [],
          sources: new Set(),
          unsubscribe: cleanup<void>(() => { for (const unsub of unsubs) unsub() }),
        };
        result.burst = streams.flatMap(s => {
          const sub = s._subscribe({
            ...cbs, runDry: cleanup(runDry),
          });
          for (const id of sub.sources) sub.sources.add(id);
          unsubs.push(sub.unsubscribe);
          return sub.burst;
        });
        return result;
      }
    });
  }

  static combineStreams<flow extends Flowing, A, B, C>(
    logic: [false | ((value0: A) => boolean), false | ((value1: B) => boolean)],
    combine: (value0: A, value1: B) => C,
    ...streams: [Stream<flow, A>, Stream<flow, B>]
  ): Stream<flow, C> {
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
        const runDry = threshold(2, cbs.runDry);
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
            const handle = ({ shouldCommit, shouldPush }) => {
              if (shouldCommit) commit(id, shouldPush);
            };
            if (logic[0] === false) {
              return handle({ shouldCommit: false, shouldPush: false });
            } else if (logic[1] === false) {
              return handle({ shouldCommit: true, shouldPush: last?.length && logic[0](last[0]) });
            } else {
              return handle({ shouldCommit: !sourcesR.has(id), shouldPush: last?.length && logic[0](last[0]) })
            }
          },

          cbR: (value1: B) => {
            lastValues[1][0] = value1;
            needsPush = true;
          },
          commitR: (id: Id) => {
            const last = needsPush ? lastValues[1] : undefined;
            const handle = ({ shouldCommit, shouldPush }) => {
              if (shouldCommit) commit(id, shouldPush);
            };
            if (logic[1] === false) {
              return handle({ shouldCommit: false, shouldPush: false });
            } else {
              return handle({ shouldCommit: true, shouldPush: last?.length && logic[1](last[0]) })
            }
          },
        };

        let sub0 = streams[0]._subscribe({
          receive: cbL, commit: commitL,
          runDry: cleanup(runDry),
        });
        let sub1 = streams[1]._subscribe({
          receive: cbR, commit: commitR,
          runDry: cleanup(runDry),
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
          unsubscribe: cleanup<void>(sub0.unsubscribe, sub1.unsubscribe),
        };
      }
    })
  }
};

type Lake<T> = Stream<"Flowing" | "NotFlowing", T>;
type River<T> = Stream<"Flowing", T>;
