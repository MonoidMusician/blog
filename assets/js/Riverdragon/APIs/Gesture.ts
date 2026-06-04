import * as Bed from "../Bed.js";
import * as Resource from "../Resource.js";
import { Flowing, Lake, River, Stream } from "../Riverdragon.js";
import { DOM, MaybePromise, Network, repollable, Timer } from "../APIs.js";

export namespace Gesture {
  // A live view of a touch point, including information about events on it
  // specifically, streams of its events in the future, and information aobut
  // the overall gesture it is a part of.
  export type TouchPointLive<gesture = TouchPointGesture> = {
    touch: Touch, // latest information for this point specifically
    start: TouchPointLive<gesture>, // when this touch point was started
    timestamp: number,
    event: TouchEvent, // most recent event
    first: TouchEvent, // first event in gesture
    gesture: gesture, // information about the whole gesture
    movements: River<TouchPointLive<gesture>>, // movements of this point specifically
    release: River<TouchPointLive<gesture>>, // may be touchend or touchcancel
    // TODO: configurable reengage event?
  };

  export type TouchGestureLive<gesture = TouchPointGesture> = {
    current: () => { points: TouchPointLive<gesture>[], gesture: gesture },
    // Store of current gesture information
    gesture: River<gesture>,
    // Bursts active points only (in order), then listen for new points
    points: River<TouchPointLive<gesture>>,
  };


  export function mintLivePoint<gesture = TouchPointGesture>(
    touch: Touch, event0: TouchEvent, gesture: gesture,
  ) {
    return Resource.inSubScope(() => {
      const store = Stream.createStore<TouchPointLive<gesture>>(undefined as any);
      const movements = store.stream.drop(1).unsafeRiver();
      const release = store.stream
        .filter(live => ["touchend", "touchcancel"].includes(live.event.type))
        .limitTo(1).instantiate().stream;

      // Circular dependency :3
      let seed = {};
      let initial: TouchPointLive<gesture> = Object.assign(seed, {
        touch,
        timestamp: event0.timeStamp,
        event: event0, first: event0,
        gesture,
        movements,
        release,
        start: seed as TouchPointLive<gesture>,
      });
      store.send(initial);
      return {
        ...store,
        remove: (ev: TouchEvent, gesture: gesture) => {
          store.send({
            ...store.current(),
            event: ev, timestamp: ev.timeStamp,
            gesture,
          });
          store.destroy();
        },
        update: (touch: Touch, ev: TouchEvent, gesture: gesture) => {
          store.send({
            ...store.current(),
            event: ev, timestamp: ev.timeStamp,
            touch, gesture,
          });
        },
      };
    });
  };

  // export function mintLiveGesture(event: River<TouchEvent>): TouchGestureLive & {
  //   gestures: River<TouchGestureLive>,
  //   destroy: Bed.Destr,
  // };
  export function mintLiveGesture(events: River<TouchEvent>) {
    return Resource.inSubScope(() => {
      const points = new Map<number, ReturnType<typeof mintLivePoint<TouchPointGesture>>>();
      let gestureStore = Stream.createRiverStore<TouchPointGesture>();
      let pointStream = Stream.createRiver([{ static: [], dynamic: () => {
        return [...points.values()].map(p => p.current());
      } }]);
      const gestureMachine = defaultGesture();
      const nextGesture = Stream.createRiverStore<River<TouchPointGesture>>();

      const current = () => ({
        points: new Map([...points.entries()].map(([k, v]) => ([ k, v.current() ]))),
        gesture: gestureStore.current(),
      });

      events.subscribe(ev => {
        const touches = Array.from(ev.touches);
        const gesture = gestureMachine(touches, ev);

        // Start of gesture?
        if (gesture.timestamps.first === gesture.timestamps.last) {
          nextGesture.send(gestureStore.stream);
        }

        // Update the gesture first
        gestureStore.send(gesture);

        // Remove touchpoints no longer present
        for (const k of [...points.keys()]) {
          if (touches.find(t => t.identifier === k)) continue;
          const store = points.get(k)!;
          store.remove(ev, gesture);
          points.delete(k);
        }
        // Update and add touchpoints
        for (const touch of touches) {
          const k = touch.identifier;
          if (points.has(k)) {
            points.get(k)!.update(touch, ev, gesture);
          } else {
            const store = mintLivePoint(touch, ev, gesture);
            points.set(k, store);
            pointStream.send(store.current());
          }
        }

        if (!touches.length) {
          gestureStore
        }
      });
      return {
        current,
        gesture: gestureStore.stream,
        points: pointStream.stream,
        gestures: nextGesture.stream,
        destroy: Resource.selfDestructor(),
      };
    });
  };

  // The three coordinate systems: client, page, and screen.
  export type TouchCoords<more = {}> = {
    client: { x: number, y: number } & more,
    page:   { x: number, y: number } & more,
    screen: { x: number, y: number } & more,
  };
  export const coordSystems = ["client", "page", "screen"] as (keyof TouchCoords)[];
  export const fillCoords = <R>(fn: (k: keyof TouchCoords) => R): Record<keyof TouchCoords, R> => {
    return Object.fromEntries(
      coordSystems.map(k => [k, fn(k)])
    ) as Record<keyof TouchCoords, R>;
  };
  export const noCoords = <more>(more: more): TouchCoords<more> => ({
    client: { x: 0, y: 0, ...more },
    page:   { x: 0, y: 0, ...more },
    screen: { x: 0, y: 0, ...more },
  });

  // A gesture records certain metrics
  export type GestureMetric<T = number> = {
    // Keep the metric stable while touch points are added and removed. Useful
    // for implementing transforms (translation, scaling, rotation), since
    // you cannot rely on there being a consistent number of touch points
    // for the gesture, which would throw off metrics based on individual frames.
    stable: T,
    // The raw/current measurement
    current: T,
  };

  // Helper for dealing with operations on records of numbers
  export type numeric = number | { [k: string]: numeric };
  export const binary = <T extends numeric>(op: (x: number, y: number) => number) => {
    return (xs: T, ys: T): T => {
      if (typeof xs === 'number' && typeof ys === 'number') {
        return op(xs, ys) as T;
      } else if (typeof xs === 'object' && typeof ys === 'object') {
        const r: numeric = {};
        for (const k in ys)
          r[k] = xs[k] !== undefined ? binary<numeric>(op)(xs[k], ys[k] as numeric) : ys[k];
        return r as T;
      } else if (typeof xs === 'number' && typeof ys === 'object') {
        return binary(y => op(xs, y))(ys, ys) as T;
      } else if (typeof xs === 'object' && typeof ys === 'number') {
        return binary(x => op(x, ys))(xs, xs) as T;
      } else throw new TypeError();
    };
  };

  // Stabilize a metric, by feeding in events 
  export const stabilize = <T extends numeric>(init: T, additive = true) => {
    const add: (xs: T, ys: T) => T = additive
      ? binary((x, y) => x+y)
      : binary((x, y) => x*y);
    const sub: (xs: T, ys: T) => T = additive
      ? binary((x, y) => x-y)
      : binary((x, y) => x/y);
    let state = { stable: init, current: init };
    return {
      current: () => state,
      // Jump immediately to a new state
      jump: (sample: T) => {
        return state = { stable: sample, current: sample };
      },
      // Set the stable value without changing the last sample
      nudge: (stable: T) => {
        return state = { stable, current: state.current };
      },
      // Do a small delta update, which updates the stable reading
      // by the amount that the sample changed from last time.
      update: (sample: T) => {
        return state = {
          stable: add(state.stable, sub(sample, state.current)),
          current: sample,
        };
      },
      // Segue to a new sample basis, by keeping the stable reading
      // the same and only updating the most recent sample.
      segue: (sample: T) => {
        return state = {
          stable: state.stable,
          current: sample,
        };
      },
    };
  };

  // Information about a gesture, over time and currently.
  export type TouchPointGesture = {
    // Timestamps
    timestamps: {
      first: number,
      idle: number, // timestamp before last
      last: number,
    },
    // A record of the number of points during the gesture
    nPoints: number[],
    // How long has the stable center moved during the gesture
    distanceTraveled: number,
    // Basic metrics for the gesture at the current time
    metrics: GestureMetrics,
  };
  export type GestureMetrics = {
    // What is the center point of the gesture / how has it translated?
    center: GestureMetric<TouchCoords>,
    // What is the size of the gesture (X, Y, and Euclidean distance)
    // thus how should it effect scaling?
    radius: GestureMetric<TouchCoords<{ r: number }>>,
    // A stable rotation, and a very unstable metric that drives
    // the rotation (see below).
    rotation: GestureMetric<{ degrees: number, radians: number }>,
  };
  // Helper to gather stats about an array (sum, average,
  // min, max, and range)
  export const statsBy = <I>(is: I[], fn: (i: I) => number) => {
    let value = 0;
    let min = Infinity;
    let max = -Infinity;
    for (const i of is) {
      const v = fn(i);
      value += v;
      if (v < min) min = v;
      if (v > max) max = v;
    }
    return { sum: value, avg: value / is.length, min, max, range: max - min };
  };

  export const touchCoordsOf = (touch: Touch): TouchCoords => {
    return {
      client: { x: touch.clientX, y: touch.clientY },
      page:   { x: touch.pageX  , y: touch.pageY   },
      screen: { x: touch.screenX, y: touch.screenY },
    };
  };
  // Measure (unstable) metrics about a group of touch points, statelessly.
  //
  // This measures the center and radius (X, Y, and radial) accurately. The
  // caller is responsible for stabilizing those metrics into usable values for
  // translation and scaling. This also measures rotation mod the number of
  // touch points (mod 180 degrees for two touch points, 120 degrees for three,
  // 90 for four), which needs to be stabilized as well, to handle the
  // wraparound. The benefit of this metric is that it does work statelessly,
  // it does not have to track individual points. Rotation is measured relative
  // to the current centerpoint.
  const metrics: (points: TouchCoords[]) => {
    [k in "center" | "radius" | "rotation"]:
      GestureMetrics[k] extends GestureMetric<infer T>
        ? T : never
  } = points => {
    const stats = fillCoords(k => ({
      x: statsBy(points, p => p[k].x),
      y: statsBy(points, p => p[k].y),
    }));
    const center = fillCoords(k =>
      ({ x: stats[k].x.avg, y: stats[k].y.avg }));
    const radius = fillCoords(k => ({
      x: stats[k].x.range,
      y: stats[k].y.range,
      r: statsBy(points, ({ [k]: { x, y } }) => Math.hypot(x, y)).max,
    }));
    const rotationOf = (p: TouchCoords) => 2*Math.PI + Math.atan2(p.screen.y - center.screen.y, p.screen.x - center.screen.x);
    const rotationRadians = (statsBy(points, rotationOf).sum % (2*Math.PI)) / points.length;
    const rotation = { degrees: 180/Math.PI*rotationRadians, radians: rotationRadians };
    return { center, radius, rotation };
  };

  // 
  export function defaultGesture() {
    let timestamps: TouchPointGesture["timestamps"] =
      { first: 0, idle: 0, last: 0 };
    let nPoints: number[] = [];
    let distanceTraveled = 0;
    let state = {
      center: stabilize(noCoords({})),
      radius: stabilize(noCoords({ r: 0 }), false),
      rotation: stabilize({ degrees: 0, radians: 0 }),
      angle: stabilize({ degrees: 0, radians: 0 }),
    };
    let started = false;
    // Capture the current values to return,
    // and reset to default values
    const reinit = () => {
      const ret = {
        timestamps,
        nPoints,
        distanceTraveled,
        metrics: {
          center: state.center.current(),
          radius: state.radius.current(),
          rotation: state.rotation.current(),
          angle: state.angle.current(),
        },
      };

      timestamps =
        { first: 0, idle: 0, last: 0 };
      nPoints = [];
      distanceTraveled = 0;
      state = {
        center: stabilize(noCoords({})),
        radius: stabilize(noCoords({ r: 0 }), false),
        rotation: stabilize({ degrees: 0, radians: 0 }),
        angle: stabilize({ degrees: 0, radians: 0 }),
      };
      started = false;
      return ret;
    };

    // The main processing function
    return (touches: Touch[], event: TouchEvent): TouchPointGesture => {
      const measured = metrics(Array.from(touches).map(touchCoordsOf));
      const was = state.center.current();
      const rot = state.rotation.current();

      // Handle timestamps before early exit
      timestamps = {
        first: timestamps.first || event.timeStamp,
        idle: timestamps.last || event.timeStamp,
        last: event.timeStamp,
      };

      // Gesture has ended, go home.
      // Ahem, reset state and return the last values.
      if (!touches.length) return reinit();

      // Check if number of points has changed (including if nPoints is empty)
      if (touches.length !== nPoints[nPoints.length - 1])
        nPoints = [...nPoints, touches.length];

      // Update the stabilized metrics
      const updates =
        event.type === "touchmove"
        // Normal updates for everything
        ? {
          center: state.center.update(measured.center),
          radius: state.radius.update(measured.radius),
          rotation: state.rotation.update(measured.rotation),
        } : {
          // Center is a normal segue, meaning that the
          // stable metric does not change.
          center: state.center.segue(measured.center),
          // Rotation is a normal segue too.
          rotation: state.rotation.segue(measured.rotation),
          // Radius only gets measured when length >= 2
          radius: touches.length === 2 && event.type === "touchstart"
            ? state.radius.jump(measured.radius)
            : state.radius.segue(measured.radius),
        };

      // Check for discontinuities in rotation
      const guesses = [-360 / touches.length, 360 / touches.length];
      for (const guess of guesses) {
        const shouldBeAround = rot.stable.degrees;
        const cameUpAs = updates.rotation.stable.degrees;
        const couldBeNearer = cameUpAs + guess;
        console.log({
          shouldBeAround,
          cameUpAs,
          couldBeNearer,
          d1: Math.abs(shouldBeAround - couldBeNearer),
          d2: Math.abs(shouldBeAround - cameUpAs),
        });
        if (Math.abs(shouldBeAround - couldBeNearer) < Math.abs(shouldBeAround - cameUpAs)) {
          const shouldBe = couldBeNearer;
          updates.rotation = state.rotation.nudge({ degrees: shouldBe, radians: shouldBe * Math.PI/180 });
        }
      }

      // Update the distance
      distanceTraveled += +started && Math.hypot(
        was.stable.screen.x - updates.center.stable.screen.x,
        was.stable.screen.y - updates.center.stable.screen.y,
      );

      started = true;
      return {
        timestamps,
        nPoints,
        distanceTraveled,
        metrics: updates,
      };
    };
  };
} // namespace Input.Gesture
