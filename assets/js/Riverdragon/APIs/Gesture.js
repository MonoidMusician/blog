import * as Resource from "../Resource.js";
import { Stream } from "../Riverdragon.js";
export var Gesture;
(function (Gesture) {
    function mintLivePoint(touch, event0, gesture) {
        return Resource.inSubScope(() => {
            const store = Stream.createStore(undefined);
            const movements = store.stream.drop(1).unsafeRiver();
            const release = store.stream
                .filter(live => ["touchend", "touchcancel"].includes(live.event.type))
                .limitTo(1).instantiate().stream;
            // Circular dependency :3
            let seed = {};
            let initial = Object.assign(seed, {
                touch,
                timestamp: event0.timeStamp,
                event: event0, first: event0,
                gesture,
                movements,
                release,
                start: seed,
            });
            store.send(initial);
            return {
                ...store,
                remove: (ev, gesture) => {
                    store.send({
                        ...store.current(),
                        event: ev, timestamp: ev.timeStamp,
                        gesture,
                    });
                    store.destroy();
                },
                update: (touch, ev, gesture) => {
                    store.send({
                        ...store.current(),
                        event: ev, timestamp: ev.timeStamp,
                        touch, gesture,
                    });
                },
            };
        });
    }
    Gesture.mintLivePoint = mintLivePoint;
    ;
    // export function mintLiveGesture(event: River<TouchEvent>): TouchGestureLive & {
    //   gestures: River<TouchGestureLive>,
    //   destroy: Bed.Destr,
    // };
    function mintLiveGesture(events) {
        return Resource.inSubScope(() => {
            const points = new Map();
            let gestureStore = Stream.createRiverStore();
            let pointStream = Stream.createRiver([{ static: [], dynamic: () => {
                        return [...points.values()].map(p => p.current());
                    } }]);
            const gestureMachine = defaultGesture();
            const nextGesture = Stream.createRiverStore();
            const current = () => ({
                points: new Map([...points.entries()].map(([k, v]) => ([k, v.current()]))),
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
                    if (touches.find(t => t.identifier === k))
                        continue;
                    const store = points.get(k);
                    store.remove(ev, gesture);
                    points.delete(k);
                }
                // Update and add touchpoints
                for (const touch of touches) {
                    const k = touch.identifier;
                    if (points.has(k)) {
                        points.get(k).update(touch, ev, gesture);
                    }
                    else {
                        const store = mintLivePoint(touch, ev, gesture);
                        points.set(k, store);
                        pointStream.send(store.current());
                    }
                }
                if (!touches.length) {
                    gestureStore;
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
    }
    Gesture.mintLiveGesture = mintLiveGesture;
    ;
    Gesture.coordSystems = ["client", "page", "screen"];
    Gesture.fillCoords = (fn) => {
        return Object.fromEntries(Gesture.coordSystems.map(k => [k, fn(k)]));
    };
    Gesture.noCoords = (more) => ({
        client: { x: 0, y: 0, ...more },
        page: { x: 0, y: 0, ...more },
        screen: { x: 0, y: 0, ...more },
    });
    Gesture.binary = (op) => {
        return (xs, ys) => {
            if (typeof xs === 'number' && typeof ys === 'number') {
                return op(xs, ys);
            }
            else if (typeof xs === 'object' && typeof ys === 'object') {
                const r = {};
                for (const k in ys)
                    r[k] = xs[k] !== undefined ? Gesture.binary(op)(xs[k], ys[k]) : ys[k];
                return r;
            }
            else if (typeof xs === 'number' && typeof ys === 'object') {
                return Gesture.binary(y => op(xs, y))(ys, ys);
            }
            else if (typeof xs === 'object' && typeof ys === 'number') {
                return Gesture.binary(x => op(x, ys))(xs, xs);
            }
            else
                throw new TypeError();
        };
    };
    // Stabilize a metric, by feeding in events 
    Gesture.stabilize = (init, additive = true) => {
        const add = additive
            ? Gesture.binary((x, y) => x + y)
            : Gesture.binary((x, y) => x * y);
        const sub = additive
            ? Gesture.binary((x, y) => x - y)
            : Gesture.binary((x, y) => x / y);
        let state = { stable: init, current: init };
        return {
            current: () => state,
            // Jump immediately to a new state
            jump: (sample) => {
                return state = { stable: sample, current: sample };
            },
            // Set the stable value without changing the last sample
            nudge: (stable) => {
                return state = { stable, current: state.current };
            },
            // Do a small delta update, which updates the stable reading
            // by the amount that the sample changed from last time.
            update: (sample) => {
                return state = {
                    stable: add(state.stable, sub(sample, state.current)),
                    current: sample,
                };
            },
            // Segue to a new sample basis, by keeping the stable reading
            // the same and only updating the most recent sample.
            segue: (sample) => {
                return state = {
                    stable: state.stable,
                    current: sample,
                };
            },
        };
    };
    // Helper to gather stats about an array (sum, average,
    // min, max, and range)
    Gesture.statsBy = (is, fn) => {
        let value = 0;
        let min = Infinity;
        let max = -Infinity;
        for (const i of is) {
            const v = fn(i);
            value += v;
            if (v < min)
                min = v;
            if (v > max)
                max = v;
        }
        return { sum: value, avg: value / is.length, min, max, range: max - min };
    };
    Gesture.touchCoordsOf = (touch) => {
        return {
            client: { x: touch.clientX, y: touch.clientY },
            page: { x: touch.pageX, y: touch.pageY },
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
    const metrics = points => {
        const stats = Gesture.fillCoords(k => ({
            x: Gesture.statsBy(points, p => p[k].x),
            y: Gesture.statsBy(points, p => p[k].y),
        }));
        const center = Gesture.fillCoords(k => ({ x: stats[k].x.avg, y: stats[k].y.avg }));
        const radius = Gesture.fillCoords(k => ({
            x: stats[k].x.range,
            y: stats[k].y.range,
            r: Gesture.statsBy(points, ({ [k]: { x, y } }) => Math.hypot(x, y)).max,
        }));
        const rotationOf = (p) => 2 * Math.PI + Math.atan2(p.screen.y - center.screen.y, p.screen.x - center.screen.x);
        const rotationRadians = (Gesture.statsBy(points, rotationOf).sum % (2 * Math.PI)) / points.length;
        const rotation = { degrees: 180 / Math.PI * rotationRadians, radians: rotationRadians };
        return { center, radius, rotation };
    };
    // 
    function defaultGesture() {
        let timestamps = { first: 0, idle: 0, last: 0 };
        let nPoints = [];
        let distanceTraveled = 0;
        let state = {
            center: Gesture.stabilize(Gesture.noCoords({})),
            radius: Gesture.stabilize(Gesture.noCoords({ r: 0 }), false),
            rotation: Gesture.stabilize({ degrees: 0, radians: 0 }),
            angle: Gesture.stabilize({ degrees: 0, radians: 0 }),
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
                center: Gesture.stabilize(Gesture.noCoords({})),
                radius: Gesture.stabilize(Gesture.noCoords({ r: 0 }), false),
                rotation: Gesture.stabilize({ degrees: 0, radians: 0 }),
                angle: Gesture.stabilize({ degrees: 0, radians: 0 }),
            };
            started = false;
            return ret;
        };
        // The main processing function
        return (touches, event) => {
            const measured = metrics(Array.from(touches).map(Gesture.touchCoordsOf));
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
            if (!touches.length)
                return reinit();
            // Check if number of points has changed (including if nPoints is empty)
            if (touches.length !== nPoints[nPoints.length - 1])
                nPoints = [...nPoints, touches.length];
            // Update the stabilized metrics
            const updates = event.type === "touchmove"
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
                    updates.rotation = state.rotation.nudge({ degrees: shouldBe, radians: shouldBe * Math.PI / 180 });
                }
            }
            // Update the distance
            distanceTraveled += +started && Math.hypot(was.stable.screen.x - updates.center.stable.screen.x, was.stable.screen.y - updates.center.stable.screen.y);
            started = true;
            return {
                timestamps,
                nPoints,
                distanceTraveled,
                metrics: updates,
            };
        };
    }
    Gesture.defaultGesture = defaultGesture;
    ;
})(Gesture || (Gesture = {})); // namespace Input.Gesture
