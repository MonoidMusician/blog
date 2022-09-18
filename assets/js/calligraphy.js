// node -i -e 'var c = require("./calligraphy.js")'
(function() {
class Vec extends Array {
  constructor(v) {
    super();
    Object.assign(this, v);
  }
  map(f) {return new Vec(super.map(f));}
  slice(...args) {return new Vec(super.slice(...args));}
  toString() {
    return "⟨" + super.join(", ") + "⟩"
  }
}
class Curve extends Array {
  constructor(v) {
    super();
    Object.assign(this, v);
  }
  map(f) {return new Curve(super.map(f));}
  slice(...args) {return new Curve(super.slice(...args));}
  toString() {
    return '<' + this.join(" ") + '>';
  }
}
class Poly extends Array {
  constructor(v) {
    super();
    Object.assign(this, v);
  }
  map(f) {return new Poly(super.map(f));}
  slice(...args) {return new Poly(super.slice(...args));}
  toString() {
    if (!this.length) return "0";
    Poly.prime += 1;
    let cs = this.map(v => v.toString());
    Poly.prime -= 1;
    let vars = ["X", "Y", "Z"];
    return cs.reduce((r, v, i) => v ? (r[0] !== "0" || r[1] === "." ? r + " + " : "") + (v !== "1" ? (v.includes("+") ? "(" + v + ")" : v) + "*" : "")+(vars[Poly.prime]||"t_"+Poly.prime)+"^"+i : r);
  }
}
Poly.prime = 0;
const curveD = ps => new Curve(ps).map(p => new Vec(p));

const construct_like = (d, cs) => {
  if (!cs.length) return d;
  var p = cs[0].__proto__;
  if (p === Array.prototype || p === Number.prototype) {
    return d;
  }
  for (let c of cs) {
    if (p !== c.__proto__) {
      return d;
    }
  }
  return new p.constructor(d);
};

const zipWith = (f,d1,d2) => (cs1, cs2) => {
  var r = [];
  for (let i = 0; i < cs1.length || i < cs2.length; i += 1) {
    var v1 = i in cs1 ? cs1[i] : d1;
    var v2 = i in cs2 ? cs2[i] : d2;
    r.push(f(v1, v2));
  }
  return construct_like(r, [cs1, cs2]);
};

const map = f => vs => vs.map(f);

const compose = (...fs) => i => {
  var o = i;
  for (let f of fs.reverse()) {
    o = f(o);
  }
  return o;
};

const lift2 = (op, annihilate) => (vs1, vs2) => {
  if (vs2 === 0) return vs1;
  if (vs1 === 0) return vs2;
  if (typeof vs1 === 'number' && typeof vs2 === 'number')
    return op(vs1,vs2);
  return zipWith(lift2(op), 0, 0)(vs1, vs2);
};
const lift1 = op => (vs1) => {
  if (typeof vs1 === 'number')
    return op(vs1);
  return vs1.map(lift1(op));
};
const summate = vs1 => {
  if (typeof vs1 === 'number')
    return vs1;
  if (!vs1.length) return 0;
  return vs1.map(sum).reduce((a, b) => a+b);
};
const sum = vs1 => {
  if (!vs1.length) return 0;
  return vs1.reduce(add);
};
const add = lift2((v1,v2) => v1+v2);
const sub = (vs1, vs2) => add(vs1, mul(-1, vs2));
const mul = (c,cs) => lift1(v => c*v)(cs);
const muldot = (cs, css) => sum(zipWith((z, zs) => mul(z, zs))(cs, css));
const dot = (vs1, vs2) => summate(lift2((v1,v2) => mul(v1,v2), true)(vs1, vs2));
const dmul = (cs1, cs2) => cs1.map(c1 => mul(c1, cs2));
const pmul = (cs1, cs2) => sum(cs1.map((c1, i) => raise(i)(mul(c1, cs2))));

const transpose = vss => {
  var rss = [];
  for (let i in vss) {
    for (let j in vss[i]) {
      rss[j] = rss[j] || construct_like([], [vss]);
      rss[j][i] = vss[i][j];
    }
  }
  return construct_like(rss, [...vss]);
};
const transposed = f => compose(map(f), transpose);
const transposing = f => compose(transpose, map(f), transpose);
const bases = dim => {
  var zeros = [];
  for (let i = 0; i < dim; i += 1) zeros.push(0);
  return zeros.map((_, i) => new Vec(zeros.map((_, j) => +(i == j))));
};

const value = cs => t => cs.reduce((r, c, i) => r + c * Math.pow(t, i), 0);
const values = vcs => t => vcs.map(cs => value(cs)(t));
const EVAL = cvs => t => transposed(cs => value(cs)(t))(cvs);
const deriv = cs => cs.map((v, i) => i * v).slice(1);
const DERIV = transposing(deriv);

const raise = n => n <= 0 ? (cs => cs) : cs => raise(n-1)(N(cs));
const N = cs => construct_like([0].concat(cs), [cs]);
const Y = cs => sub(cs, N(cs));

const b = ps => {
  if (typeof ps === 'number') {
    return new Vec(bases(ps)).map(b);
  }
  // c.b(ps) === c.muldot(ps, c.b(ps.length))
  if (ps.length <= 1) return new Poly(ps);
  return add(Y(b(ps.slice(0, -1))), N(b(ps.slice(1))));
};
const B = transposing(b);
const curvature = (XX,YY,XXX,YYY) => {
  return (XX*YYY - YY*XXX)/Math.pow(XX*XX + YY*YY, 1.5);
};
const Bcurvature0 = ([b0,b1,b2,_]) => {
  let b21 = [b2[0]-b1[0],b2[1]-b1[1]];
  let d0 = [b1[0]-b0[0],b1[1]-b0[1]];
  let d0xb21 = d0[0]*b21[1] - d0[1]*b21[0];
  let delta = Math.sqrt(d0[0]*d0[0] + d0[1]*d0[1]);
  return (2/3)*d0xb21/Math.pow(delta, 3);
};
const Bcurvature1 = ([b0,b1,b2,b3]) => -Bcurvature0([b3,b2,b1,b0]);
const Bcurvature = C => t => {
  let [numer, denom] = Bcurvature_polys(C).map(p => value(p)(t));
  return numer/Math.pow(denom, 1.5);
};
const Bcurvature_polys = C => {
  let CC = DERIV(B(C));
  let XX = transpose(CC)[0];
  let YY = transpose(CC)[1];
  let CCC = DERIV(CC);
  let XXX = transpose(CCC)[0];
  let YYY = transpose(CCC)[1];
  return [sub(pmul(XX,YYY), pmul(YY,XXX)), add(pmul(XX,XX), pmul(YY,YY))];
};
const norm2 = v => v[0]*v[0] + v[1]*v[1];
const norm = v => Math.sqrt(norm2(v));
const normalize = v => {let n = norm(v); return [v[0]/n, v[1]/n]};
const cross2 = (v1,v2) => v1[0]*v2[1] - v1[1]*v2[0];
const solve_quartic = cs => {
  let rs = quartic(cs.slice().reverse());
  var ep = 1e-9;
  let rs2 = rs.filter(r => Math.abs(r.im) < ep).map(r => r.re);
  return rs2;
};
const fit_checks = (f0,f1,d0,d1,k0,k1) => {
  let ep = 1e-8;
  let matches = (ki,ko) =>
    (Math.sign(ki) === Math.sign(ko)) || (Math.abs(ki) < ep && Math.abs(ko) < ep);
  let rs = fit(f0,f1,d0,d1,k0,k1);
  let rrs = rs.filter(r => matches(Bcurvature0(r), k0) && matches(Bcurvature1(r), k1));
  return rrs.length ? rrs : rs;
};
const fit_check = (f0,f1,d0,d1,k0,k1) => {
  let rs = fit_checks(f0,f1,d0,d1,k0,k1);
  if (rs.length <= 1) return rs[0];
  let score = ([b0,b1,b2,b3]) => {
    let d0 = sub(b1, b0);
    let d1 = sub(b3, b2);
    return norm2(d0) + norm2(d1);
  };
  return rs.sort((a, b) => score(a) - score(b))[0];
};
const fit = (f0,f1,d0,d1,k0,k1) => {
  d0 = normalize(d0); d1 = normalize(d1);
  let d0xd1 = cross2(d0, d1);
  let a = sub(f1, f0);
  let d0xa = cross2(d0, a);
  let axd1 = cross2(a, d1);
  let δs;
  if (d0xd1 === 0) {
    δs = [Math.sqrt((2/3)*(d0xa)/k0), Math.sqrt((2/3)*(axd1)/k1)];
  } else {
    let ep = 1e-9;
    if (Math.abs(axd1) < ep || Math.abs(d0xa) < ep) {
      throw new Error("parallel " + [a, axd1, d0xa]);
    }
    // https://herbie.uwplse.org/
    /**/
    let ρs;
    //let R0 = (3/2)*(k0*axd1*axd1)/(d0xa*d0xd1*d0xd1);
    let R0 = 1.5 * (((axd1 / d0xa) / (d0xd1 / k0)) / (d0xd1 / axd1));
    //let R1 = (3/2)*(k1*d0xa*d0xa)/(axd1*d0xd1*d0xd1);
    let R1 = 1.5 * (((d0xa / axd1) / (d0xd1 / k1)) / (d0xd1 / d0xa));
    if (!Number.isFinite(R0) || !Number.isFinite(R1)) {
      throw new Error("NaNaNaNaNa " + [R0,R1,k0,k1] + "; " + [a, axd1, d0xa]);
    }
    let c0 = [1-R1, -1, 2*R1*R0, 0, -R1*R0*R0];
    let c1 = [1-R0, -1, 2*R0*R1, 0, -R0*R1*R1];
    let ρ0s = solve_quartic(c0);
    let ρ1s = solve_quartic(c1);
    ρs = [
      ρ0s.map(ρ0 => [ρ0, 1 - R0*ρ0*ρ0]),
      ρ1s.map(ρ1 => [1 - R1*ρ1*ρ1, ρ1]),
      ρ0s.flatMap(ρ0 => ρ1s.map(ρ1 => [ρ0, ρ1])),
    ][1].filter(p => p.every(Number.isFinite));
    if (!ρs.length) {
      throw new Error("no quartic solutions " + [c0, ρ0s, c1, ρ1s].join("; "));
    }
    δs = ρs.map(([ρ0,ρ1]) => [ρ0 * axd1 / d0xd1, ρ1 * d0xa / d0xd1]);
    let δδs = δs;
    δs = δs.filter(([δ0,δ1]) => δ0 >= 0 && δ1 >= 0);
    if (!δs.length && δδs.length) {
      throw new Error("filtered out " + δδs.join("; "));
      //δs = δδs;
    }
  }
  return δs.map(([δ0,δ1]) => [f0,add(f0, mul(δ0, d0)),sub(f1, mul(δ1, d1)),f1]);
};
const fit_existing = P => {
  return fit_check(P[0], P[3], sub(P[1], P[0]), sub(P[3], P[2]), Bcurvature0(P), Bcurvature1(P));
};
const fit_existing_score = P => {
  let Q = fit_existing(P);
  return {
    P, Q,
    P0: Bcurvature0(P), P1: Bcurvature1(P),
    Q0: Bcurvature0(Q), Q1: Bcurvature1(Q),
  };
}

const degree = cs => {
  var d = 0;
  for (let i in cs) {
    if (cs[i]) d = +i;
  }
  return d;
};

const solve = eq => {
  var d = degree(eq);
  if (d === 0) {
    if (eq[0]) return [];
    return null; // everything is a solution
  } else if (d === 1) {
    return [-eq[0]/eq[1]];
  } else if (d === 2) {
    var disc = eq[1]*eq[1] - 4*eq[2]*eq[0];
    var ep = 1e-10;
    if (Math.abs(disc) < ep) disc = 0;
    if (disc < 0) return [];
    if (disc === 0) return [-eq[1]/eq[2]/2];
    var r = Math.sqrt(disc);
    return [+r,-r].map(sr => (-eq[1] + sr)/eq[2]/2);
  }
  throw new Error("Cannot solve equation of degree " + d);
};
const solve_norm = eq => solve_norm_sgn(eq)[0];
const solve_norm_sgn = eq => {
  var sols = solve(eq);
  if (sols.length === 0) {
    var disc = eq[1]*eq[1] - 4*eq[2]*eq[0];
    throw new Error("No solutions to " + eq + " with disc = " + disc);
  }
  var ep = 1e-5;
  var norm_sols = sols.filter(t => 0-ep <= t && t <= 1+ep).map(t => Math.max(0, Math.min(1, t)));
  if (norm_sols.length === 1) {
    return [norm_sols[0], [+1, -1][sols.findIndex(t => t == norm_sols[0])]];
  }
  throw new Error("Solutions were " + sols + " (to " + eq + ")");
};
const solve_deriv = (eq, dq) => {
  if (degree(eq) > 2) throw Error(eq);
  let q = solve_norm(eq);
  if ((2*eq[2]*q + eq[1]) === 0) {
    console.error("DISASTER", {eq,dq,q});
    let rs = solve_deriv_(eq, dq);
    console.error("DISASTER", {eq,dq,q,rs});
    return rs[4];
  }
  return -(dq[2]*q*q + dq[1]*q + dq[0])/(2*eq[2]*q + eq[1]);
};
const solve_deriv2 = (eq, dq, cq) => {
  if (degree(eq) > 2) throw Error(eq);
  let q = solve_norm(eq);
  let qq = solve_deriv(eq, dq);
  return (-2*eq[2]*qq*qq + cq[2]*q*q + cq[1]*q + cq[0])/(2*eq[2]*q + eq[1]);
};
const solve_deriv_ = (eq, dq) => {
  return [ -1e-3, -1e-4, -1e-5, -1e-6, -1e-7, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3 ].map(dp => {
    return (solve_norm(eq) - solve_norm(add(mul(dp, dq), eq)))/dp;
  });
};

const T_THINGY = (P,Q) => {
  var PP = DERIV(B(P));
  var QQ = transpose(DERIV(B(Q)));
  return PP.map(cd => {
    return sub(mul(cd[0], QQ[1]), mul(cd[1], QQ[0]));
  });
};
// PP : p :: QQ : q
// QQ_x(q)*PP_y(p) - QQ_y(q)*PP_x(p) = 0
const T_IMPLICIT = (P,Q) => {
  var PP = transpose(DERIV(B(P)));
  var QQ = transpose(DERIV(B(Q)));
  return sub(dmul(QQ[1], PP[0]), dmul(QQ[0], PP[1]));
};
const T_SOL = (P,Q) => p => solve_norm(values(T_IMPLICIT(P,Q))(p));
const COMP = (P,Q) => p => add(EVAL(B(P))(p), EVAL(B(Q))(T_SOL(P,Q)(p)));
const T_DERIV = (P,Q) => p => {
  // multivariate polynomial in q,p
  let EQ = T_IMPLICIT(P,Q);
  // quadratic equation for q at p
  let eq = values(EQ)(p);
  // derivative of eq
  let dq = values(EQ.map(deriv))(p);
  return solve_deriv(eq, dq);
};
// c.T_DERIVS(c.ex.P, c.ex.Q)(0.36657)
const T_DERIVS = (P,Q) => p => {
  var q = T_SOL(P,Q)(p);
  return [ -1e-3, -1e-4, -1e-5, -1e-6, -1e-7, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3 ].map(dp => {
    return (T_SOL(P,Q)(dp+p) - q)/dp;
  });
};
const PQ_CURVATURE = (P,Q) => {
  let PP = DERIV(B(P));
  let PPE = EVAL(PP);
  let QQ = DERIV(B(Q));
  let QQE = EVAL(QQ);
  let PPPE = EVAL(DERIV(PP));
  let QQQE = EVAL(DERIV(QQ));
  let TT = T_DERIV(P,Q);
  return (p,q=undefined) => {
    if (q === undefined) q = T_SOL(P,Q)(p);
    let PPp = PPE(p);
    let QQq = QQE(q);
    let PPPp = PPPE(p);
    let QQQq = QQQE(q);
    let R = TT(p);
    let XX = PPp[0] + QQq[0]*R;
    let YY = PPp[1] + QQq[1]*R;
    let XXXYY = PPPp[0]*PPp[1] + PPPp[0]*QQq[1]*R + QQQq[0]*R*R*PPp[1] + QQQq[0]*QQq[1]*R*R*R;
    let YYYXX = PPPp[1]*PPp[0] + PPPp[1]*QQq[0]*R + QQQq[1]*R*R*PPp[0] + QQQq[1]*QQq[0]*R*R*R;
    let speed = Math.pow(XX*XX + YY*YY, 3/2);
    let k = (XXXYY - YYYXX)/speed;
    if (!Number.isFinite(k)) {
      console.error("NaN", {k,R,speed,p,q,P,Q});
      throw new Error("NaN");
    }
    return k;
    /*
    (x''y' - y''x')/((x'2 + y'2)^3/2)
    */
  };
};
const T_VERIFY = (P,Q) => (p, q) => value(values(T_IMPLICIT(P,Q))(p))(q === undefined ? T_SOL(P,Q)(p) : q);
const T_VERIFY2 = (P,Q) => (p, q) => value(values(T_IMPLICIT(Q,P))(q === undefined ? T_SOL(P,Q)(p) : q))(p);
const T_VERIFYT = (P,Q) => (p, q) => {
  if (p === undefined) q = T_SOL(P,Q)(p);
  var PPp = EVAL(DERIV(B(P)))(p);
  var TPp = PPp[1]/PPp[0];
  var QQq = EVAL(DERIV(B(Q)))(q);
  var TQq = QQq[1]/QQq[0];
  return [ TPp, TQq, TPp - TQq ];
};
const T_VERIFY3 = (P,Q) => (p, q) => {
  if (q === undefined) q = T_SOL(P,Q)(p);
  return [ T_VERIFY, T_VERIFY2, T_VERIFYT ].map(f => f(P,Q)(p, q));
};

function composite(P,Q) {
  return compositeI(P,Q)[1];
}

function compositeI(P,Q) {
  let P0 = P[0];
  let P1 = P[3];
  let Qt = EVAL(B(Q));
  let q = T_SOL(P,Q);
  let q0 = q(0);
  let Q0 = Qt(q0);
  let q1 = q(1);
  let Q1 = Qt(q1);
  const f0 = add(P0, Q0);
  const f1 = add(P1, Q1);
  const d0 = sub(P[1], P[0]);
  const d1 = sub(P[3], P[2]);
  const k = PQ_CURVATURE(P,Q);
  const k0 = k(0,q0);
  const k1 = k(1,q1);
  return [q0, fit_check(f0,f1,d0,d1,-k0,-k1), q1];
}

function INFLXNS(P) {
  let PP = transpose(DERIV(B(P)));
  let PPP = transpose(DERIV(DERIV(B(P))));
  let EQ = sub(pmul(PPP[0], PP[1]), pmul(PPP[1], PP[0]));
  let rs = solve(EQ);
  return rs;
}

//const T_DERIV = ;

window.calligraphy = {
  Vec, Poly, Curve,
  zipWith, add, sub, mul, compose, transpose, dot, lift1, lift2, map, value, deriv, N, Y, b, B, bases,
  muldot, summate, sum, DERIV, EVAL, values, dmul,
  transposing, transposed,
  solve_norm, solve, solve_deriv, solve_deriv_,
  T_IMPLICIT, T_SOL, T_VERIFY, T_VERIFY2, T_VERIFYT, T_VERIFY3,
  T_DERIV, T_DERIVS, COMP, INFLXNS,
  curvature, Bcurvature, Bcurvature_polys, Bcurvature0, Bcurvature1,
  composite, compositeI,
  fit, fit_check, fit_existing, fit_existing_score,
  raise, pmul, norm, normalize,
  ex: (function() {
    let r = {};
    try {
      r.P = curveD([[7,6],[4,2],[1,2],[5,1]]);
      r.PP = transpose(DERIV(B(r.P)));
      r.Q = curveD([[1,3],[4,5],[6,7],[1,3]]);
      r.QQ = transpose(DERIV(B(r.Q)));
      r.t = 0.4;
      r.tt = T_SOL(r.P, r.Q)(r.t);
    } catch (e) {
      console.log(e);
    }
    return r;
  })(),
};
})();
