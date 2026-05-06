var _applyVector = func(xs, ys, f) {
  var zs = setsize([], size(xs) * size(ys));
  forindex (var i; xs) forindex (var j; ys)
    zs[size(ys)*i + j] = f(xs[i], ys[j]);
  return zs;
};
var _flatMap = func(xs, f) {
  var zs = [];
  foreach (var x; xs) zs ~= f(x);
  return zs;
};

# Function used on the class directly, no reference to `me`
var static = func(f) f;
# Function used on instances, where `me` should be an instance of the class
var prototypal = func(f) f;

var Lake = var River = var Stream = {
  mkId: Bed.freshId(),

  new: static(func(old) {
    var me = { parents: [me] };
    me.flowing = old.flowing;
    me._burstBehavior = old._burstBehavior;
    me._subscribe = old._subscribe;
    return me;
  }),

  subscribe: prototypal(func(receive, runDry = nil) {
    var unsubscriber = Bed.rolling();
    var hasLoaded = false;
    var isDestroyed = false;
    var sub = me._subscribe({
      receive: Resource.saveScope(func(value) {
        if (!isDestroyed) receive(value);
      }),
      commit: Bed.nocb,
      runDry: Resource.saveScope(func() {
        unsubscriber(Bed.nocb);
        isDestroyed = true;
        if (hasLoaded and runDry != nil) runDry();
      }),
    });
    unsubscriber(sub.unsubscribe);
    foreach (var v; sub.burst) receive(v);
    hasLoaded = true;
    if (isDestroyed and runDry != nil) runDry();
    return Resource.tryAddDestructor(Bed.cleanup(Resource.saveScope(func() { unsubscriber(Bed.nocb);; })));
  }),
  subscribe1: prototypal(func(receive, runDry = nil) {
    return me.subscribe(Resource.saveRevolvingScope(receive), runDry);
  }),

  makeLake: static(func(subscribe) {
    return Stream.new({
      flowing: false,
      _burstBehavior: [{ static: [], dynamic: func() {
        var burst = [];
        var unsubscribeImmediately = subscribe(func(value) { append(burst, value); Bed.nocb });
        unsubscribeImmediately();
        return burst;
      } }],
      _subscribe: func(cbs) {
        var myId = Stream.mkId();
        var mySource = [myId];
        return Bed.loadingBurst(func(captureBurst) {
          var unsubscribe = Resource.start(Resource.unSub, subscribe, func(value) {
            if (!captureBurst(value)) {
              cbs.receive(value);
              cbs.commit(myId);
            };;
          }, cbs.runDry).destroy;
          return func(burst) ({ burst: burst, sources: mySource, unsubscribe: unsubscribe });
        });
      },
    });
  }),

  createRiver: static(func(burstBehavior = nil, beforeBroadcast = nil) {
    var myId = Stream.mkId();
    var mySource = [myId];
    var subs = Bed.subscriptions();
    return {
      send: func(value) {
        subs.notify(func(cbs) cbs.receive(value));
        subs.notify(func(cbs) cbs.commit(myId));
        ;;
      },
      stream: Stream.new({
        flowing: true,
        _burstBehavior: burstBehavior or [],
        _subscribe: func(cbs) {
          var burst = _flatMap(burstBehavior or [], func(b) b.static ~ (b.dynamic != nil ? b.dynamic() : []));
          var unsubscribe = if (subs.running()) subs.push(cbs) else { cbs.runDry(); Bed.nocb };
          return { burst: burst, sources: mySource, unsubscribe: unsubscribe };
        }
      }),
      destroy: Resource.tryAddDestructor(func() { subs.destroy(Bed.nocb);; }),
    };
  }),

  createStore: static(func(initialValue) {
    return Stream.createRiver(
      burstBehavior: [{ static: [], dynamic: func() [initialValue] }],
      beforeBroadcast: func(pushed) { initialValue = pushed;; },
    );
  }),

  bursting: static(func(values) Stream.new({
    flowing: true,
    _burstBehavior: size(values) ? [{ static: values }] : [],
    _subscribe: func() ({ burst: values, sources: [], unsubscribe: Bed.nocb })
  })),
  pure: static(func(value) Stream.bursting([value])),

  dam: prototypal(func() {
    var dammed = Stream.new(me);
    dammed.flowing = false;
    return dammed;
  }),

  oneStream: static(func(streams) {
    if (!size(streams)) return Stream.empty;
    if (size(streams) == 1) return streams[0];
    var flowing = true;
    foreach (var s; streams) flowing = flowing and s.flowing;
    return Stream.new({
      flowing: flowing,
      _burstBehavior: _flatMap(streams, func(s) s._burstBehavior),
      _subscribe: func(cbs) {
        var runDry = Bed.threshold(size(streams), cbs.runDry);
        var unsubs = [];
        var result = {
          burst: [],
          sources: [],
          unsubscribe: Bed.cleanup(func() { foreach (var unsub; unsubs) unsub(); unsubs = [];; }),
        };
        result.burst = _flatMap(streams, func(s) {
          var sub = s._subscribe({
            receive: cbs.receive,
            commit: cbs.commit,
            runDry: Bed.cleanup(runDry),
          });
          result.sources ~= sub.sources;
          append(unsubs, sub.unsubscribe);
          return sub.burst;
        });
        return result;
      }
    });
  }),

  combineStreams: static(func(logic, combine, streams...) {
    var _burstBehavior = _applyVector(
      streams[0]._burstBehavior, streams[1]._burstBehavior,
      func(burst0, burst1)
        return {
          static: _applyVector(burst0.static, burst1.static, combine),
          dynamic: (!burst0.dynamic and !burst1.dynamic) ? nil : func()
            _applyVector(
              burst0.static ~ (burst0.dynamic != nil ? burst0.dynamic() : []),
              burst1.static ~ (burst1.dynamic != nil ? burst1.dynamic() : []),
              combine
            ),
        };
    );
    var flowing = true;
    foreach (var s; streams) flowing = flowing and s.flowing;
    return Stream.new({
      flowing: flowing,
      _burstBehavior: _burstBehavior,
      _subscribe: func(cbs) {
        var runDry = Bed.threshold(2, cbs.runDry);
        var UNFILLED = {};
        var lastValues = [UNFILLED, UNFILLED];
        var needsPush = false;
        var sourcesR = [];

        var commit = func(activeId, shouldPush) {
          if (shouldPush) {
            if (lastValues[0] != UNFILLED and lastValues[1] != UNFILLED) {
              cbs.receive(combine(lastValues[0], lastValues[1]));
            } else {} # have not received a value on both sides
          }
          cbs.commit(activeId); ;;
        };
        var cbL = func(value0) {
          lastValues[0] = value0;
          needsPush = true; ;;
        };
        var commitL = func(activeId) {
          if (logic[0] == false) return;
          var last = needsPush ? lastValues[0] : UNFILLED;
          if (logic[1] == false) {
            return commit(activeId, last != UNFILLED and logic[0](last));
          } else {
            foreach (var rightId; sourcesR) if (rightId == activeId) return;
            return commit(activeId, last != UNFILLED and logic[0](last));
          }
        };
        var cbR = func(value1) {
          lastValues[1] = value1;
          needsPush = true; ;;
        };
        var commitR = func(activeId) {
          if (logic[1] == false) return;
          var last = needsPush ? lastValues[1] : UNFILLED;
          return commit(activeId, last != UNFILLED and logic[1](last));
        };

        var sub0 = streams[0]._subscribe({
          receive: cbL, commit: commitL,
          runDry: Bed.cleanup(runDry),
        });
        var sub1 = streams[1]._subscribe({
          receive: cbR, commit: commitR,
          runDry: Bed.cleanup(runDry),
        });
        sourcesR = sub1.sources;
        if (size(sub0.burst)) lastValues[0] = sub0.burst[size(sub0.burst) - 1];
        if (size(sub1.burst)) lastValues[1] = sub1.burst[size(sub1.burst) - 1];
        return {
          burst: _applyVector(sub0.burst, sub1.burst, combine),
          sources:
            (logic[0] != false ? sub0.sources : []) ~
            (logic[1] != false ? sub1.sources : []),
          unsubscribe: Bed.cleanup(sub0.unsubscribe, sub1.unsubscribe),
        };
      }
    })
  }),

  latestStream: static(func(source, mkStream) Stream.makeLake(func(cb) {
    source.subscribe(Resource.saveRevolvingScope(func(v)
      mkStream(v).subscribe(cb)
    ));
  })),

  allStreams: static(func(source, mkStream) Stream.makeLake(func(cb) {
    source.subscribe(Resource.saveScope(func(v)
      mkStream(v).subscribe(cb)
    ));
  })),
};
Stream.empty = Stream.bursting([]);
