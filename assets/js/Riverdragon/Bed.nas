var capture_nothing = {};
var capture_call = { call: call };
var capture = func(namespace, fn) {
  if (namespace == nil) namespace = capture_nothing;
  return bind(fn, namespace);
};

var nocb = capture(nil, func(){});
var consttrue = capture(nil, func() true);
var constfalse = capture(nil, func() false);

var freshId = capture(nil, func() {
  var prevId = -1;
  return func() prevId += 1;
});

var cleanup = capture(capture_call, func(cbs...) func(arg...) {
  var saved = cbs;
  cbs = [];
  foreach (var cb; saved) call(cb, arg);
  ;;
});

var cleanupRunning = func(cb) {
  var running = true;
  return {
    cleanup: func(arg...) {
      var saved = cb;
      cb = nocb;
      var ret = call(saved, arg);
      running = false;
      return ret;
    },
    running: func() running,
  };
};

var rolling = func() {
  var past = nocb;
  return func(cb, arg...) {
    var saved = past;
    var immediately = nil;
    past = func(newArg...) immediately = newArg;
    var ret = call(saved, arg);
    if (immediately == nil) {
      past = cb;
    } else {
      past = nocb;
      var ret = call(cb, immediately);
    }
    return ret;
  };
};

var breakerResettable = capture(nil, func(cb) {
  var needsToRun = true;
  return var me = {
    run: func(value) { if (needsToRun) cb(value); return me },
    trip: func() { needsToRun = false; return me },
    reset: func() { needsToRun = true; return me },
    running: func() needsToRun,
  };
});

var breaker = func(cb) {
  return var me = {
    run: func(value) { cb(value); return me },
    trip: func() { cb = nocb; me.running = constfalse; return me },
    running: consttrue,
  };
};

var subscriptions = func() {
  var ids = freshId();
  var listeners = [];
  var destructor = cleanupRunning(func(onDestroy) {
    forindex (var i; listeners)
      onDestroy(listeners[i].value);
    listeners = [];
    ;;
  });
  return var me = {
    notify: func(onValue) {
      forindex (var i; listeners)
        onValue(listeners[i].value);
      return me;
    },
    push: func(value) {
      if (!destructor.running()) return nocb;
      var thisId = ids();
      append(listeners, { id: thisId, value: value });
      return cleanup(func() {
        forindex (var i; listeners) {
          if (listeners[i].id == thisId) {
            removeat(listeners, i);
            break;
          }
        }
      });
    },
    destroy: destructor.cleanup,
    size: func() listeners.length,
    running: destructor.running,
  };
};

# accumulator

# Only run it on the nth time.
var threshold = func(n, cb) {
  if (n < 1) { cb(); return nocb }
  var count = 0;
  return func() {
    if (!(count -= 1)) {
      cb(); cb = nocb;
    }
  };
};

var prealloc = capture(nil, func(value) {
  return {
    get: func() value,
    set: func(next) { value = next },
    swap: func(next) { var prev = value; value = next; return prev }
  };
});

var loading = capture(nil, func(loader) {
  var isLoading = true;
  var r = loader(func() isLoading);
  isLoading = false;
  return r;
});

var loadingBurst = func(loader) {
  var bursts = [];
  var r = loading(func(isLoading)
    loader(func(value) {
      if (isLoading()) { append(bursts, value); true }
      else false;
    })
  );
  return [r(bursts), bursts = r = nil][0];
};
