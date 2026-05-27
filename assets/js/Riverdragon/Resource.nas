var _currentScope = nil;

var withScope = func(scope, body, arg...) {
  return _withScope(scope, body, arg, caller(1)[0]);
};
var _withScope = func(scope, body, arg = nil, namespace = nil) {
  var saved = _currentScope;
  _currentScope = scope;
  var ret = call(body, arg, nil, namespace);
  _currentScope = saved;
  return ret;
};

var getScope = func() _currentScope;

var noScope = {
  addDestructor: func(),
  destroy: func(),
  destroyed: func() false,
};

var mkSubscope = func(parent) {
  if (parent.destroyed()) die("Parent scope already destroyed");
  var state = { destructors: [], destroyed: false };
  var destroy = func() {
    state.destroyed = true;
    var destructors = state.destructors;
    state.destructors = [];
    for (var i=size(destructors)-1; i>=0; i-=1)
      destructors[i]();
    destroy = func(){};
    ;;
  };
  parent.addDestructor(destroy);
  return {
    destroy: destroy,
    addDestructor: func(cb) {
      if (state.destroyed)
        die("Scope already destroyed");
      append(state.destructors, cb);
      return cb;
    },
    destroyed: func() state.destroyed,
  };
};
var multiScope = func(scopes) {
  if (size(scopes) == 0) return noScope;
  if (size(scopes) == 1) return scopes[0];
  return {
    addDestructor: func(cb) { foreach (var scope; scopes) scope.addDestructor(cb); ;; },
    destroy: func() { foreach (var scope; scopes) scope.destroy(); ;; },
    destroyed: func() { foreach (var scope; scopes) if (scope.destroyed()) return true; false },
  };
};

var start = func(fn, arg...) {
  var scope = mkSubscope(_currentScope == nil ? noScope : _currentScope);
  var result = _withScope(scope, fn, arg);
  return { result: result, scope: scope, destroy: scope.destroy };
};

var saveScope = func(fn) {
  var scope = getScope();
  return func(arg...) _withScope(scope, fn, arg);
};
var saveRevolvingScope = func(fn) {
  var revolving = oneSubScopeAtATime(getScope());
  return func(arg...) _withScope(revolving(), fn, arg);
};

var oneSubScopeAtATime = func(parent) {
  var destroy = func();
  return func() {
    var scope = mkSubscope(parent);
    destroy();
    destroy = scope.destroy;
    return scope;
  };
};



var addDestructor = func(cb) getScope().addDestructor(cb);
var tryAddDestructor = func(cb) {
  var scope = getScope();
  if (scope != nil) scope.addDestructor(cb);
  else return cb;
};
var getAddDestructor = func() getScope().addDestructor;
var selfDestruct = func() getScope().destroy();
var selfDestructor = func() getScope().destroy;

var subScope = func() mkSubscope(getScope());

var scoped = func(body, arg...) {
  var scope = subScope();
  var ret = _withScope(scope, body, arg, caller(1)[0]);
  scope.destroy();
  return ret;
};

var track = func(fn, arg...) {
  var ret = call(fn, arg);
  addDestructor(ret.destroy);
  return ret;
};
var unSub = func(fn, arg...) addDestructor(call(fn, arg));

var tryTrack = func(fn, arg...) {
  var ret = call(fn, arg);
  tryAddDestructor(ret.destroy);
  return ret;
};
var tryUnSub = func(fn, arg...) tryAddDestructor(call(fn, arg));
