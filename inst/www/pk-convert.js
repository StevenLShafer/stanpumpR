// -----------------------------------------------------------------------------
// The PK parameter converter on the help page models/pk-converter
// -----------------------------------------------------------------------------
// Provenance
// ----------
// Drafted by Claude Code (Claude Opus 5.5), 2026-11-01, at the request of
// Steven L. Shafer: his spreadsheet Convert.xls (last updated 12/02) as a
// page of the Help tab.  The formulas are the spreadsheet's, read from its
// cells; the examples in tests/testthat/test-help-pk-converter.R are its own.
//
// The page is plain HTML (helpPKConverterHTML() in R/help-converter.R) with
// no Shiny inputs: every <input> has no id or name and carries
// data-shiny-no-bind-input, and this file answers its events through
// delegated handlers on the document, so it needs no initialisation when the
// help page is (re)rendered and nothing reaches the server.  The state of a
// converter is kept on its own element, so a page rendered afresh starts
// afresh.
//
// The math (PKConvert below) has no DOM and is exported to Node for the tests.
// The model is held as V1 and the micro rate constants, in minutes; every
// other parameter is derived from that.
// -----------------------------------------------------------------------------

var PKConvert = (function() {
  'use strict';
  var LN2 = Math.LN2;

  function positive(x) { return typeof x === 'number' && isFinite(x) && x > 0; }

  // The exponents of a model, fastest first.  Three compartments: the cubic
  // solved by the trigonometric method as cube() in R/cube.R does, each root
  // then polished by Newton's method, since the smallest root is the
  // difference of two nearly equal numbers.  Two: the quadratic in its
  // cancellation-free form.
  function exponents(m) {
    var k10 = m.k10, k12 = m.k12 || 0, k13 = m.k13 || 0, k21 = m.k21 || 0, k31 = m.k31 || 0;
    if (m.n === 1) return [k10];
    if (m.n === 2) {
      var s = k10 + k12 + k21, p = k10 * k21, d = s * s - 4 * p;
      if (!(d >= 0)) return null;
      var r1 = (s + Math.sqrt(d)) / 2;
      return [r1, p / r1];
    }
    var a0 = k10 * k21 * k31;
    var a1 = k10 * k31 + k21 * k31 + k21 * k13 + k10 * k21 + k31 * k12;
    var a2 = k10 + k12 + k13 + k21 + k31;
    var pp = a1 - a2 * a2 / 3;
    var q = 2 * a2 * a2 * a2 / 27 - a1 * a2 / 3 + a0;
    var r = Math.sqrt(-(pp * pp * pp) / 27);
    if (!(r > 0)) return null;
    var phi = Math.acos(Math.max(-1, Math.min(1, (-q / 2) / r))) / 3;
    var r2 = 2 * Math.exp(Math.log(r) / 3);
    var roots = [0, 2, 4].map(function(j) {
      return -(Math.cos(phi + j * Math.PI / 3) * r2 - a2 / 3);
    });
    roots = roots.map(function(x) {
      for (var i = 0; i < 4; i++) {
        var f = ((x - a2) * x + a1) * x - a0;      // -(the cubic in -x)
        var df = (3 * x - 2 * a2) * x + a1;
        if (df === 0) break;
        var nx = x - f / df;
        if (!isFinite(nx)) break;
        x = nx;
      }
      return x;
    });
    return roots.sort(function(a, b) { return b - a; });
  }

  // The coefficients of the unit disposition function, per litre
  function coefficients(m, l) {
    if (m.n === 1) return [1 / m.v1];
    if (m.n === 2) {
      return [(m.k21 - l[0]) / (l[1] - l[0]) / m.v1,
              (m.k21 - l[1]) / (l[0] - l[1]) / m.v1];
    }
    return [0, 1, 2].map(function(i) {
      var j = (i + 1) % 3, k = (i + 2) % 3;
      return (m.k21 - l[i]) * (m.k31 - l[i]) / (l[j] - l[i]) / (l[k] - l[i]) / m.v1;
    });
  }

  // A model checked and expanded into every parameter the spreadsheet lists.
  // Returns {model} or {error}.
  var RATE_NAMES = {1: ['k10'], 2: ['k10', 'k12', 'k21'], 3: ['k10', 'k12', 'k13', 'k21', 'k31']};
  function expand(m) {
    if (!positive(m.v1)) return {error: 'the central volume V1 would be ' + fmt(m.v1) + '; a volume must be positive'};
    var names = RATE_NAMES[m.n];
    for (var i = 0; i < names.length; i++) {
      var k = m[names[i]];
      if (typeof k !== 'number' || isNaN(k)) {
        return {error: names[i] + ' would be complex: there is no real model with these values'};
      }
      if (!positive(k)) {
        return {error: names[i] + ' would be ' + fmt(k) + ' (a rate constant must be positive and finite)'};
      }
    }
    var l = exponents(m);
    if (!l || !l.every(positive)) return {error: 'the exponents would be complex'};
    for (i = 1; i < l.length; i++) {
      if (l[i - 1] - l[i] <= 1e-9 * l[i - 1]) {
        return {error: 'two exponents are equal, so the model has no sum-of-exponentials form'};
      }
    }
    var a = coefficients(m, l);
    var out = {n: m.n, v1: m.v1, k10: m.k10, k12: m.k12 || 0, k13: m.k13 || 0,
               k21: m.k21 || 0, k31: m.k31 || 0, lambda: l, A: a};
    out.v = [m.v1];
    out.cl = [m.v1 * m.k10];
    if (m.n >= 2) { out.v.push(m.v1 * m.k12 / m.k21); out.cl.push(m.v1 * m.k12); }
    if (m.n === 3) { out.v.push(m.v1 * m.k13 / m.k31); out.cl.push(m.v1 * m.k13); }
    out.vdss = out.v.reduce(function(s, x) { return s + x; }, 0);
    out.halfLife = l.map(function(x) { return LN2 / x; });
    var sumA = a.reduce(function(s, x) { return s + x; }, 0);
    out.fraction = a.map(function(x) { return x / sumA; });
    return {model: out};
  }

  // A route that was given exponents must give them back: a check on the
  // algebra, which also catches inputs no mammillary model can produce.
  function checkExponents(res, l) {
    if (res.error) return res;
    var got = res.model.lambda;
    for (var i = 0; i < l.length; i++) {
      if (Math.abs(got[i] - l[i]) > 1e-6 * l[i]) {
        return {error: 'no ' + l.length + '-compartment mammillary model has these exponents with these values'};
      }
    }
    return res;
  }

  function descending(l) { return l.slice().sort(function(a, b) { return b - a; }); }

  function distinct(l) {
    for (var i = 1; i < l.length; i++) if (l[i - 1] - l[i] <= 1e-9 * l[i - 1]) return false;
    return true;
  }
  var EQUAL_EXPONENTS = {error: 'two exponents (or half-lives) are equal; a sum of exponentials needs distinct ones'};

  // Spreadsheet 1: volumes and clearances
  function fromVolumesClearances(n, v, cl) {
    var m = {n: n, v1: v[0], k10: cl[0] / v[0]};
    if (n >= 2) { m.k12 = cl[1] / v[0]; m.k21 = cl[1] / v[1]; }
    if (n === 3) { m.k13 = cl[2] / v[0]; m.k31 = cl[2] / v[2]; }
    return expand(m);
  }

  // Spreadsheet 2: V1 and the micro rate constants
  function fromMicro(n, v1, k) {
    return expand({n: n, v1: v1, k10: k.k10, k12: k.k12, k13: k.k13, k21: k.k21, k31: k.k31});
  }

  // Spreadsheet 3: V1, Vdss, CL and the half-lives.  As in the spreadsheet,
  // one compartment takes CL and the half-life (V1 = CL / lambda), two take
  // V1, CL and both half-lives, three take all six.
  function fromHalfLives(n, p) {
    var l = descending(p.t.map(function(t) { return LN2 / t; }));
    if (!distinct(l)) return EQUAL_EXPONENTS;
    if (n === 1) return expand({n: 1, v1: p.cl / l[0], k10: l[0]});
    var k10 = p.cl / p.v1;
    if (n === 2) {
      var k21 = l[0] * l[1] / k10;
      return checkExponents(expand({n: 2, v1: p.v1, k10: k10, k12: l[0] + l[1] - k21 - k10, k21: k21}), l);
    }
    var g0 = l[0] * l[1] * l[2] / k10;
    var g1 = (p.vdss - p.v1) / p.v1 * g0;
    var f = (l[0] * l[1] + l[0] * l[2] + l[1] * l[2] - g1 - g0) / k10;
    var r = quadraticRoots(f, g0);
    if (!r) return {error: 'k21 and k31 would be complex (the quadratic they solve has no real roots)'};
    var h = (p.vdss / p.v1 - 1) * g0;
    var i = l[0] + l[1] + l[2] - k10 - r[0] - r[1];
    var k13 = (h - i * r[1]) / (r[0] - r[1]);
    return checkExponents(expand({n: 3, v1: p.v1, k10: k10, k12: i - k13, k13: k13,
                                  k21: r[0], k31: r[1]}), l);
  }

  // The roots of x^2 - s x + p = 0, larger first, without cancellation
  function quadraticRoots(s, p) {
    var d = s * s - 4 * p;
    if (!(d >= 0) || !(s > 0)) return null;
    var r1 = (s + Math.sqrt(d)) / 2;
    return [r1, p / r1];
  }

  // Spreadsheet 4: the coefficients and exponents of the unit disposition
  // function.  Each coefficient stays with its exponent when they are sorted.
  function fromCoefficients(n, A, L) {
    var pairs = L.map(function(x, i) { return [x, A[i]]; })
                 .sort(function(a, b) { return b[0] - a[0]; });
    var l = pairs.map(function(p) { return p[0]; });
    var a = pairs.map(function(p) { return p[1]; });
    if (!distinct(l)) return EQUAL_EXPONENTS;
    var sumA = a.reduce(function(s, x) { return s + x; }, 0);
    var v1 = 1 / sumA;
    var res;
    if (n === 1) return expand({n: 1, v1: v1, k10: l[0]});
    if (n === 2) {
      var k21 = (a[0] * l[1] + a[1] * l[0]) / sumA;
      var k10 = l[0] * l[1] / k21;
      res = expand({n: 2, v1: v1, k10: k10, k12: l[0] + l[1] - k21 - k10, k21: k21});
    } else {
      var b = (l[0] * a[2] + l[0] * a[1] + l[2] * a[0] + l[2] * a[1] + l[1] * a[0] + l[1] * a[2]) / sumA;
      var c = (l[0] * l[1] * a[2] + l[0] * l[2] * a[1] + l[1] * l[2] * a[0]) / sumA;
      var r = quadraticRoots(b, c);
      if (!r) return {error: 'k21 and k31 would be complex (the quadratic they solve has no real roots)'};
      var k10w = l[0] * l[1] * l[2] / r[0] / r[1];
      var k12w = ((l[1] * l[2] + l[0] * l[1] + l[0] * l[2]) - r[0] * (l[0] + l[1] + l[2]) -
                  k10w * r[1] + r[0] * r[0]) / (r[1] - r[0]);
      res = expand({n: 3, v1: v1, k10: k10w, k12: k12w,
                    k13: l[0] + l[1] + l[2] - (k10w + k12w + r[0] + r[1]), k21: r[0], k31: r[1]});
    }
    res = checkExponents(res, l);
    if (res.error) return res;
    for (var i = 0; i < n; i++) {
      if (Math.abs(res.model.A[i] - a[i]) > 1e-6 * Math.abs(a[i])) {
        return {error: 'no mammillary model has these coefficients with these exponents'};
      }
    }
    return res;
  }

  // Spreadsheet 5: V1, the exponents, k21 and k31
  function fromExponentsAndRates(n, v1, L, k21, k31) {
    var l = descending(L);
    if (!distinct(l)) return EQUAL_EXPONENTS;
    if (n === 1) return expand({n: 1, v1: v1, k10: l[0]});
    var res;
    if (n === 2) {
      var k10 = l[0] * l[1] / k21;
      res = expand({n: 2, v1: v1, k10: k10, k12: l[0] + l[1] - k21 - k10, k21: k21});
    } else {
      if (k21 === k31) return {error: 'k21 and k31 are equal, which leaves k12 and k13 undetermined'};
      var k10w = l[0] * l[1] * l[2] / (k21 * k31);
      var k12w = ((l[1] * l[2] + l[0] * l[1] + l[0] * l[2]) - k21 * (l[0] + l[1] + l[2]) -
                  k10w * k31 + k21 * k21) / (k31 - k21);
      res = expand({n: 3, v1: v1, k10: k10w, k12: k12w,
                    k13: l[0] + l[1] + l[2] - (k10w + k12w + k21 + k31), k21: k21, k31: k31});
    }
    return checkExponents(res, l);
  }

  // --- The effect site ------------------------------------------------------
  //
  // After a unit bolus, Ce(t) = ke0 sum A_i (exp(-l_i t) - exp(-ke0 t)) /
  // (ke0 - l_i).  Each term is written as exp(-min t) (1 - exp(-|d| t)) / |d|,
  // d = ke0 - l_i, which has no cancellation and tends to t exp(-l_i t) as
  // ke0 meets l_i, so no rate needs nudging apart.

  function plasma(model, t) {
    var s = 0;
    for (var i = 0; i < model.n; i++) s += model.A[i] * Math.exp(-model.lambda[i] * t);
    return s;
  }

  function effect(model, ke0, t) {
    var s = 0;
    for (var i = 0; i < model.n; i++) {
      var l = model.lambda[i], d = Math.abs(ke0 - l);
      var g = d * t < 1e-300 ? t : -Math.expm1(-d * t) / d;
      s += model.A[i] * Math.exp(-Math.min(l, ke0) * t) * g;
    }
    return ke0 * s;
  }

  // The effect site peaks where it meets the plasma curve (dCe/dt =
  // ke0 (Cp - Ce)).  After a bolus Cp only falls and Ce only rises until
  // then, so Cp - Ce changes sign once, and bisection finds it.
  function peakTime(model, ke0) {
    var f = function(t) { return plasma(model, t) - effect(model, ke0, t); };
    var lo = 0, hi = 1 / (ke0 + model.lambda[0]);
    while (f(hi) > 0 && hi < 1e15) { lo = hi; hi *= 2; }
    for (var i = 0; i < 200 && hi - lo > 1e-13 * hi; i++) {
      var mid = (lo + hi) / 2;
      if (f(mid) > 0) lo = mid; else hi = mid;
    }
    return (lo + hi) / 2;
  }

  // ke0 from the time to peak effect after a bolus, as ke0FromTPeakBolus()
  // in R/getDrugPK.R: the peak comes earlier as ke0 rises, so bracket it and
  // bisect, here on log ke0.
  function ke0FromPeakTime(model, tPeak) {
    var lo = 1e-3, hi = 1;
    while (peakTime(model, lo) < tPeak && lo > 1e-12) lo /= 10;
    while (peakTime(model, hi) > tPeak && hi < 1e6) hi *= 10;
    if (peakTime(model, lo) < tPeak || peakTime(model, hi) > tPeak) return NaN;
    var a = Math.log(lo), b = Math.log(hi);
    for (var i = 0; i < 200 && b - a > 1e-14; i++) {
      var mid = (a + b) / 2;
      if (peakTime(model, Math.exp(mid)) > tPeak) a = mid; else b = mid;
    }
    return Math.exp((a + b) / 2);
  }

  // --- Decrement times ------------------------------------------------------
  //
  // The time for the plasma or effect-site concentration to fall by a given
  // percentage after a target-controlled infusion, of a given duration, that
  // held that concentration constant (Hughes, Glass & Jacobs 1992 for the
  // plasma, whose 50% curve is the context-sensitive half-time; Shafer &
  // Varvel 1991 for the effect site; Youngs & Shafer 1994 for any
  // percentage).  The infusion is the simulator's own controller, Shafer &
  // Gregg 1992 as R/tci.R implements it: an update every TCI_INTERVAL (10 s),
  // the effect site reached without overshoot, and the plasma held once the
  // effect site is within TCI_PLASMA_SWITCH (5%) of the target.  The
  // controller's states are the amplitudes of the exponentials, as there.

  var TCI_INTERVAL = 10 / 60;
  var TCI_PLASMA_SWITCH = 0.05;

  // The constants R/tci.R's tciKinetics() takes from getDrugPK(), here from
  // the model.  ke0 is held off an exponent it equals, as
  // separateEigenvalues() does, since the coefficients divide by the gap.
  function tciKinetics(model, ke0) {
    var l = model.lambda.slice(), A = model.A;
    var k = ke0 > 0 ? ke0 : 0;
    l.forEach(function(x) { if (k > 0 && Math.abs(k - x) <= 1e-8 * k) k *= 1 + 1e-8; });
    var ebolus = A.map(function(a, i) { return k > 0 ? a * k / (k - l[i]) : 0; });
    var eSum = ebolus.reduce(function(s, x) { return s + x; }, 0);
    return {
      lambda3: l, lambda4: l.concat([k]), ke0: k,
      tPeak: k > 0 ? peakTime(model, k) : 0,
      pbolus: A.slice(),
      pinf: A.map(function(a, i) { return a / l[i]; }),
      ebolus: ebolus.concat([-eSum]),
      einf: ebolus.map(function(b, i) { return b / l[i]; }).concat([k > 0 ? -eSum / k : 0])
    };
  }

  // The rate for the coming interval: tciRate() in R/tci.R
  function tciRate(p, e, CT, site, dt, K) {
    var Ce = e.reduce(function(s, x) { return s + x; }, 0);
    var effectMode = site === 'effect' && K.ke0 > 0 && Math.abs(Ce - CT) >= TCI_PLASMA_SWITCH * CT;
    var rate, i, j;
    if (!effectMode) {
      var Pd = 0, Pu = 0;
      for (i = 0; i < p.length; i++) {
        var d = Math.exp(-K.lambda3[i] * dt);
        Pd += p[i] * d;
        Pu += K.pinf[i] * (1 - d);
      }
      rate = (CT - Pd) / Pu;
    } else {
      // From the end of the interval out to dt + tPeak + 10 s, a second apart
      var grow = K.lambda4.map(function(x) { return Math.exp(x * dt) - 1; });
      rate = Infinity;
      var any = false;
      for (var tau = dt; tau <= dt + K.tPeak + 10 / 60 + 1e-12; tau += 1 / 60) {
        var B = 0, E = 0;
        for (j = 0; j < K.lambda4.length; j++) {
          var m = Math.exp(-K.lambda4[j] * tau);
          B += e[j] * m;
          E += K.einf[j] * grow[j] * m;
        }
        if (E > 0) { any = true; rate = Math.min(rate, (CT - B) / E); }
      }
      if (!any) rate = 0;
    }
    return {rate: Math.max(0, rate), effectMode: effectMode};
  }

  // The states while a target of 1 is held at one site, every update to
  // tMax.  The interval lengthens as R/tci.R's does once the plasma is being
  // held, to at most max(10 s, min(10 min, tMax / 2000)).
  function tciTrajectory(model, ke0, site, tMax) {
    var K = tciKinetics(model, ke0);
    var n = model.n, p = new Array(n).fill(0), e = new Array(n + 1).fill(0);
    var dtMax = Math.max(TCI_INTERVAL, Math.min(10, tMax / 2000));
    var t = 0, dt = TCI_INTERVAL, out = [];
    while (t < tMax - 1e-9) {
      var step = Math.min(dt, tMax - t);
      var x = tciRate(p, e, 1, site, step, K);
      for (var i = 0; i < n; i++) {
        var d3 = Math.exp(-K.lambda3[i] * step);
        p[i] = p[i] * d3 + K.pinf[i] * x.rate * (1 - d3);
      }
      for (var j = 0; j <= n; j++) {
        var d4 = Math.exp(-K.lambda4[j] * step);
        e[j] = e[j] * d4 + K.einf[j] * x.rate * (1 - d4);
      }
      t += step;
      out.push({t: t, p: p.slice(), e: e.slice()});
      dt = x.rate > 0 && !x.effectMode && t > K.tPeak ? Math.min(dt * 1.1, dtMax) : TCI_INTERVAL;
    }
    return {K: K, states: out};
  }

  // The first time a sum of exponentials, amplitudes a and rates l, falls to
  // (1 - fraction) of its value now.  The decline after an infusion stops
  // falls through the threshold once (the effect site may first rise), so a
  // geometric march finds the crossing and bisection refines it.
  function decrementTime(a, l, fraction) {
    var c = function(t) {
      var s = 0;
      for (var i = 0; i < a.length; i++) s += a[i] * Math.exp(-l[i] * t);
      return s;
    };
    var c0 = c(0), target = (1 - fraction) * c0;
    if (!(c0 > 0)) return NaN;
    var fastest = Math.max.apply(null, l.filter(function(x) { return x > 0; }));
    var lo = 0, hi = 1e-3 / fastest;
    while (c(hi) > target) {
      lo = hi;
      hi *= 1.1;
      if (hi > 1e8) return Infinity;
    }
    for (var k = 0; k < 100 && hi - lo > 1e-12 * hi; k++) {
      var mid = (lo + hi) / 2;
      if (c(mid) > target) lo = mid; else hi = mid;
    }
    return (lo + hi) / 2;
  }

  // Decrement times against the duration of the infusion, at about `points`
  // durations up to tMax, for the plasma and (with a ke0) the effect site.
  // Each site's curve comes from its own infusion: the plasma held for the
  // plasma, the effect site held for the effect site.  A curve starts once
  // its site has reached the target (within the controller's 5%): an
  // effect-site infusion stopped before then never held the effect site, and
  // its "decrement" is measured from an effect site still on its way up.
  function decrementCurves(model, ke0, percent, tMax, points) {
    var fraction = percent / 100, out = {};
    var sites = ke0 > 0 ? ['plasma', 'effect'] : ['plasma'];
    var sum = function(x) { return x.reduce(function(s, v) { return s + v; }, 0); };
    sites.forEach(function(site) {
      var tr = tciTrajectory(model, ke0, site, tMax), K = tr.K, st = tr.states;
      var first = 0;
      while (first < st.length &&
             Math.abs(sum(site === 'plasma' ? st[first].p : st[first].e) - 1) > TCI_PLASMA_SWITCH) first++;
      // Not reached within the longest infusion: no curve, rather than a
      // decrement from a site still on its way up
      if (first === st.length) { out[site] = {duration: [], time: []}; return; }
      var every = Math.max(1, Math.floor((st.length - first) / (points || 300)));
      var dur = [], time = [];
      for (var i = first; i < st.length; i++) {
        if ((i - first) % every !== 0 && i !== st.length - 1) continue;
        var s = st[i];
        dur.push(s.t);
        time.push(site === 'plasma' ? decrementTime(s.p, K.lambda3, fraction)
                                    : decrementTime(s.e, K.lambda4, fraction));
      }
      out[site] = {duration: dur, time: time};
    });
    return out;
  }

  // The fields each definition of the page takes, by number of compartments
  // (the field names of R/help-converter.R)
  var REQUIRED = {
    vc:    {1: ['v1', 'cl1'], 2: ['v1', 'v2', 'cl1', 'cl2'], 3: ['v1', 'v2', 'v3', 'cl1', 'cl2', 'cl3']},
    micro: {1: ['v1', 'k10'], 2: ['v1', 'k10', 'k12', 'k21'], 3: ['v1', 'k10', 'k12', 'k13', 'k21', 'k31']},
    half:  {1: ['cl', 't1'], 2: ['v1', 'cl', 't1', 't2'], 3: ['v1', 'vdss', 'cl', 't1', 't2', 't3']},
    coef:  {1: ['a1', 'l1'], 2: ['a1', 'a2', 'l1', 'l2'], 3: ['a1', 'a2', 'a3', 'l1', 'l2', 'l3']},
    exp:   {1: ['v1', 'l1'], 2: ['v1', 'l1', 'l2', 'k21'], 3: ['v1', 'l1', 'l2', 'l3', 'k21', 'k31']}
  };

  // The model in one fixed layout, whichever definition gave it (Shafer,
  // 2026-11-01): volumes, clearances, exponents, half-lives, coefficients,
  // fractional coefficients, then the micro rate constants.  minutesPerUnit
  // and unit are the time unit shown; values come back in it.
  function summaryRows(model, minutesPerUnit, unit) {
    var f = minutesPerUnit || 1, u = unit || 'min', n = model.n;
    var idx = [1, 2, 3].slice(0, n);
    var each = function(prefix, values, scale) {
      return idx.map(function(i) { return {name: prefix + i, value: values[i - 1] * scale}; });
    };
    var micro = RATE_NAMES[n].slice();
    if (n === 3) micro = ['k10', 'k12', 'k13', 'k21', 'k31'];
    return [
      {label: 'Volumes', units: 'L', cells: each('V', model.v, 1)},
      {label: 'Clearances', units: 'L/' + u, cells: each('CL', model.cl, f)},
      {label: 'Exponents', units: '1/' + u, cells: each('λ', model.lambda, f)},
      {label: 'Half-lives', units: u, cells: each('t½ λ', model.halfLife, 1 / f)},
      {label: 'Coefficients', units: '1/L', cells: each('A', model.A, 1)},
      {label: 'Fractional coefficients', units: '%', cells: idx.map(function(i) {
        return {name: 'A' + i + '/ΣA', value: 100 * model.fraction[i - 1]};
      })},
      {label: 'Micro rate constants', units: '1/' + u, cells: micro.map(function(k) {
        return {name: k, value: model[k] * f};
      })}
    ];
  }

  // A field's text as a number: null when blank, NaN when unreadable.  A
  // comma is read only as a thousands separator (1,110); any other comma (0,5
  // with a decimal comma) is refused rather than read ten times too large.
  function parseNumber(text) {
    var s = String(text).trim();
    if (s === '') return null;
    if (s.indexOf(',') >= 0) {
      if (!/^[+-]?\d{1,3}(,\d{3})+(\.\d*)?([eE][+-]?\d+)?$/.test(s)) return NaN;
      s = s.replace(/,/g, '');
    }
    var x = Number(s);
    return isFinite(x) ? x : NaN;
  }

  function fmt(x, digits) {
    if (typeof x !== 'number' || !isFinite(x)) return String(x);
    return String(parseFloat(x.toPrecision(digits || 6)));
  }

  return {
    exponents: exponents, coefficients: coefficients,
    fromVolumesClearances: fromVolumesClearances, fromMicro: fromMicro,
    fromHalfLives: fromHalfLives, fromCoefficients: fromCoefficients,
    fromExponentsAndRates: fromExponentsAndRates,
    plasma: plasma, effect: effect, peakTime: peakTime,
    ke0FromPeakTime: ke0FromPeakTime, fmt: fmt, parseNumber: parseNumber, summaryRows: summaryRows,
    tciTrajectory: tciTrajectory, decrementTime: decrementTime, decrementCurves: decrementCurves,
    required: REQUIRED
  };
})();

if (typeof module !== 'undefined' && module.exports) module.exports = PKConvert;

// --- The page ----------------------------------------------------------------

if (typeof document !== 'undefined' && typeof jQuery !== 'undefined') (function($) {
  'use strict';
  var P = PKConvert;

  // Minutes per time unit
  var UNIT_MINUTES = {s: 1 / 60, min: 1, h: 60};

  var REQUIRED = P.required;
  var GROUP_TITLES = {vc: 'volumes and clearances', micro: 'V1 and the rate constants',
                      half: 'V1, Vdss, CL and the half-lives', coef: 'the coefficients and exponents',
                      exp: 'V1, the exponents, k21 and k31'};
  var FIELD_LABELS = {v1: 'V1', v2: 'V2', v3: 'V3', cl1: 'CL1', cl2: 'CL2', cl3: 'CL3',
                      k10: 'k10', k12: 'k12', k13: 'k13', k21: 'k21', k31: 'k31',
                      vdss: 'Vdss', cl: 'CL', t1: 't½ λ1', t2: 't½ λ2',
                      t3: 't½ λ3', a1: 'A1', a2: 'A2', a3: 'A3',
                      l1: 'λ1', l2: 'λ2', l3: 'λ3'};

  // The spreadsheet's examples (its tabs 1 and 2), in minutes
  var EXAMPLES = {
    1: {v1: 8, cl1: 4},
    2: {v1: 5, v2: 50, cl1: 3.5, cl2: 2.5},
    3: {v1: 10, v2: 100, v3: 1000, cl1: 3, cl2: 2, cl3: 1}
  };

  // What a field shows, in minutes, from a model
  function modelValue(model, group, field) {
    var i = Number(field.slice(-1)) - 1;
    switch (group) {
      case 'vc': return field.charAt(0) === 'v' ? model.v[i] : model.cl[i];
      case 'micro': return field === 'v1' ? model.v1 : model[field];
      case 'half':
        if (field === 'v1') return model.v1;
        if (field === 'vdss') return model.vdss;
        if (field === 'cl') return model.cl[0];
        return model.halfLife[i];
      case 'coef':
        if (field.charAt(0) === 'a') return model.A[i];
        return model.lambda[i];
      case 'exp':
        if (field === 'v1') return model.v1;
        if (field.charAt(0) === 'l') return model.lambda[i];
        return model[field];
    }
  }

  // Display units <-> minutes
  function toDisplay(x, dim, unit) {
    var f = UNIT_MINUTES[unit];
    if (dim === 'rate' || dim === 'clear') return x * f;
    if (dim === 'time') return x / f;
    return x;
  }
  function fromDisplay(x, dim, unit) {
    var f = UNIT_MINUTES[unit];
    if (dim === 'rate' || dim === 'clear') return x / f;
    if (dim === 'time') return x * f;
    return x;
  }

  var parseEntry = P.parseNumber;

  function state($root) {
    var s = $root.data('pkconv');
    if (!s) {
      s = {n: 3, unit: 'min', active: null, entered: {}, effectActive: null, effectEntered: null,
           model: null, ke0: null};
      $root.data('pkconv', s);
    }
    return s;
  }

  function field($root, group, name) {
    return $root.find('[data-pk-group="' + group + '"] input[data-pk-field="' + name + '"]');
  }

  // Show the rows the number of compartments has, and the time unit's labels
  function layout($root, s) {
    $root.find('[data-pk-n]').each(function() {
      var shown = $(this).attr('data-pk-n').split(',').indexOf(String(s.n)) >= 0;
      $(this).toggle(shown);
    });
    $root.find('[data-pk-unit]').each(function() {
      var dim = $(this).attr('data-pk-unit');
      var u = s.unit;
      $(this).text({rate: '1/' + u, clear: 'L/' + u, time: u}[dim]);
    });
    // Fields a definition derives but never takes (Vdss with fewer than three
    // compartments, V1 with one) are read-only
    $root.find('.pkconv-group input[data-pk-field]').each(function() {
      var $i = $(this), g = $i.closest('[data-pk-group]').attr('data-pk-group');
      var takes = REQUIRED[g][s.n].indexOf($i.attr('data-pk-field')) >= 0;
      $i.prop('readOnly', !takes).toggleClass('pkconv-readonly', !takes);
    });
  }

  function setStatus($root, kind, html) {
    $root.find('.pkconv-status').attr('data-kind', kind).html(html);
  }

  // Recompute everything from the active definition
  function recompute($root) {
    var s = state($root);
    layout($root, s);
    s.model = null;
    var group = s.active;
    var $inputs = $root.find('.pkconv-group input[data-pk-field]');
    $inputs.removeClass('pkconv-entered pkconv-derived pkconv-bad');

    if (!group) {
      $inputs.val('').removeAttr('data-full');
      setStatus($root, 'info', 'Enter a complete set of parameters in any one panel; the others are filled in.');
      summary($root, s);
      effectSite($root, s);
      return;
    }

    var need = REQUIRED[group][s.n], values = {}, missing = [], bad = [];
    need.forEach(function(name) {
      var $f = field($root, group, name);
      var x = parseEntry(s.entered[name] === undefined ? '' : s.entered[name]);
      $f.addClass('pkconv-entered');
      if (x === null) missing.push(FIELD_LABELS[name]);
      else if (!(x > 0)) { bad.push(FIELD_LABELS[name]); $f.addClass('pkconv-bad'); }
      else values[name] = fromDisplay(x, $f.attr('data-pk-dim'), s.unit);
    });

    var res = null;
    if (!missing.length && !bad.length) res = solve(group, s.n, values);

    // Every field the definition did not take is cleared, then filled if solved
    $inputs.each(function() {
      var $i = $(this), g = $i.closest('[data-pk-group]').attr('data-pk-group');
      var name = $i.attr('data-pk-field');
      if (g === group && need.indexOf(name) >= 0) {
        if (this !== document.activeElement) $i.val(s.entered[name] === undefined ? '' : s.entered[name]);
        return;
      }
      $i.val('').removeAttr('data-full');
      if (res && res.model) {
        var x = toDisplay(modelValue(res.model, g, name), $i.attr('data-pk-dim'), s.unit);
        $i.val(P.fmt(x)).attr('data-full', String(x)).addClass('pkconv-derived');
      }
    });

    var from = 'from ' + GROUP_TITLES[group];
    if (bad.length) {
      setStatus($root, 'error', 'Not admissible: ' + bad.join(', ') + (bad.length > 1 ? ' must be' : ' must be a') +
                ' positive number' + (bad.length > 1 ? 's' : '') + ', with a point as the decimal mark.');
    } else if (missing.length) {
      setStatus($root, 'info', 'Defining the model ' + from + '. Still needed: ' + missing.join(', ') + '.');
    } else if (res.error) {
      setStatus($root, 'error', 'Not admissible: ' + res.error + '.');
    } else {
      s.model = res.model;
      setStatus($root, 'ok', 'The model is defined ' + from +
                ' (the entered values are yellow); every other value is derived from them.');
    }
    summary($root, s);
    effectSite($root, s);
  }

  // The model in one fixed layout, whichever panel defined it
  function summary($root, s) {
    var $body = $root.find('.pkconv-summary-body');
    if (!s.model) {
      $body.html('<p class="pkconv-empty">The model appears here when it is defined.</p>');
      return;
    }
    var rows = P.summaryRows(s.model, UNIT_MINUTES[s.unit], s.unit);
    var width = Math.max.apply(null, rows.map(function(r) { return r.cells.length; }));
    var esc = function(t) { return String(t).replace(/&/g, '&amp;').replace(/</g, '&lt;'); };
    var html = rows.map(function(r) {
      var cells = r.cells.map(function(c) {
        return '<td><span class="pkconv-summary-name">' + esc(c.name) + '</span>' + esc(P.fmt(c.value)) + '</td>';
      });
      for (var i = r.cells.length; i < width; i++) cells.push('<td></td>');
      return '<tr><th scope="row">' + esc(r.label) + '</th>' + cells.join('') +
             '<td class="pkconv-unit">' + esc(r.units) + '</td></tr>';
    });
    $body.html('<div class="pkconv-summary-scroll"><table class="pkconv-summary-table"><tbody>' +
               html.join('') + '</tbody></table></div>');
  }

  function solve(group, n, v) {
    switch (group) {
      case 'vc':
        return P.fromVolumesClearances(n, [v.v1, v.v2, v.v3].slice(0, n), [v.cl1, v.cl2, v.cl3].slice(0, n));
      case 'micro':
        return P.fromMicro(n, v.v1, v);
      case 'half':
        return P.fromHalfLives(n, {v1: v.v1, vdss: v.vdss, cl: v.cl, t: [v.t1, v.t2, v.t3].slice(0, n)});
      case 'coef':
        return P.fromCoefficients(n, [v.a1, v.a2, v.a3].slice(0, n), [v.l1, v.l2, v.l3].slice(0, n));
      case 'exp':
        return P.fromExponentsAndRates(n, v.v1, [v.l1, v.l2, v.l3].slice(0, n), v.k21, v.k31);
    }
  }

  // --- The effect site: ke0, its half-life or the time to peak effect --------

  var EFFECT_DIMS = {ke0: 'rate', tke0: 'time', tpeak: 'time'};

  function effectSite($root, s) {
    var $box = $root.find('.pkconv-effect');
    var $fields = $box.find('input[data-pk-effect]');
    $fields.removeClass('pkconv-entered pkconv-derived pkconv-bad');
    s.ke0 = null;
    var $result = $box.find('.pkconv-effect-result').empty();
    var name = s.effectActive;
    if (name) {
      $box.find('input[data-pk-effect="' + name + '"]').addClass('pkconv-entered');
    }
    var x = name ? parseEntry(s.effectEntered) : null;
    $fields.each(function() {
      if ($(this).attr('data-pk-effect') !== name) $(this).val('');
    });
    if (name && x !== null && !(x > 0)) {
      $box.find('input[data-pk-effect="' + name + '"]').addClass('pkconv-bad');
      $result.text('Not admissible: the value must be a positive number, with a point as the decimal mark.').attr('data-kind', 'error');
    } else if (name && x !== null && s.model) {
      var t = fromDisplay(x, EFFECT_DIMS[name], s.unit), ke0;
      if (name === 'ke0') ke0 = t;
      else if (name === 'tke0') ke0 = Math.LN2 / t;
      else ke0 = P.ke0FromPeakTime(s.model, t);
      if (!(ke0 > 0) || !isFinite(ke0)) {
        $result.text('No ke0 gives a peak at that time.').attr('data-kind', 'error');
      } else {
        s.ke0 = ke0;
        var peak = P.peakTime(s.model, ke0);
        var shown = {ke0: ke0, tke0: Math.LN2 / ke0, tpeak: peak};
        $fields.each(function() {
          var k = $(this).attr('data-pk-effect');
          if (k === name) return;
          var v = toDisplay(shown[k], EFFECT_DIMS[k], s.unit);
          $(this).val(P.fmt(v)).addClass('pkconv-derived');
        });
        var cePeak = P.effect(s.model, ke0, peak), cp0 = P.plasma(s.model, 0);
        $result.attr('data-kind', 'ok').text(
          'After a bolus the effect site peaks at ' + P.fmt(toDisplay(peak, 'time', s.unit), 4) + ' ' + s.unit +
          ', at ' + P.fmt(cePeak, 4) + ' per litre of dose: ' + P.fmt(100 * cePeak / cp0, 3) +
          '% of the initial plasma concentration (1/V1).');
      }
    } else if (name && x !== null) {
      $result.text('Define the model above first.').attr('data-kind', 'info');
    }
    plot($root, s);
    decrement($root, s);
  }

  // --- The graph ----------------------------------------------------------------

  function niceStep(range, count) {
    var raw = range / count, mag = Math.pow(10, Math.floor(Math.log10(raw)));
    var r = raw / mag;
    return mag * (r < 1.5 ? 1 : r < 3 ? 2 : r < 7 ? 5 : 10);
  }

  // --- Decrement times ------------------------------------------------------------

  // Shafer and Varvel's recovery curves ran to ten hours
  var DECREMENT_DEFAULT_MAX = 600;

  function decrement($root, s) {
    var $box = $root.find('.pkconv-decrement');
    var $svg = $box.find('.pkconv-dec-svg'), $note = $box.find('.pkconv-dec-note').empty();
    var f = UNIT_MINUTES[s.unit];
    $box.find('input[data-pk-dec="until"]')
        .attr('placeholder', P.fmt(DECREMENT_DEFAULT_MAX / f, 4));
    $box.find('input').removeClass('pkconv-bad');
    if (!s.model) {
      $svg.html('<p class="pkconv-empty">The decrement times appear when the model is defined.</p>');
      return;
    }
    var $pct = $box.find('input[data-pk-dec="pct"]'), pct = parseEntry($pct.val());
    var $until = $box.find('input[data-pk-dec="until"]'), until = parseEntry($until.val());
    var bad = [];
    if (pct === null) pct = 50;
    if (!(pct > 0 && pct < 100)) { bad.push('the decrease must be between 0 and 100%'); $pct.addClass('pkconv-bad'); }
    if (until !== null && !(until > 0)) { bad.push('the longest infusion must be a positive time'); $until.addClass('pkconv-bad'); }
    if (bad.length) {
      $svg.empty();
      $note.text('Not admissible: ' + bad.join('; ') + '.');
      return;
    }
    var tMax = until ? until * f : DECREMENT_DEFAULT_MAX;
    var c = P.decrementCurves(s.model, s.ke0, pct, tMax, 400);
    var series = [{cls: 'pkconv-cp', label: 'Plasma', x: c.plasma.duration, y: c.plasma.time}];
    if (c.effect) series.push({cls: 'pkconv-ce', label: 'Effect site', x: c.effect.duration, y: c.effect.time});
    $svg.html(lineChart(series, {
      xMax: tMax, f: f, unit: s.unit,
      xLabel: 'Duration of the infusion (' + s.unit + ')',
      yLabel: 'Time to a ' + P.fmt(pct, 4) + '% decrease (' + s.unit + ')',
      aria: P.fmt(pct, 4) + '% decrement times against the duration of the infusion'
    }));
    var say = function(sr) {
      if (!sr.time.length) return 'not reached within the infusion';
      var t = sr.time[sr.time.length - 1];
      return isFinite(t) ? P.fmt(t / f, 3) + ' ' + s.unit : 'beyond the search';
    };
    var text = 'After an infusion of ' + P.fmt(tMax / f, 4) + ' ' + s.unit + ': plasma ' + say(c.plasma);
    if (c.effect) text += ', effect site ' + say(c.effect);
    text += '.';
    if (!c.effect) text += ' Enter ke0 or the time to peak effect above to add the effect site.';
    $note.text(text);
  }

  // A line chart of decrement time against duration, both in minutes; the
  // axes are drawn in the unit shown
  function lineChart(series, o) {
    var W = 640, H = 320, L = 64, R = 16, Tp = 14, B = 46, f = o.f;
    var ys = [];
    series.forEach(function(sr) { sr.y.forEach(function(y) { if (isFinite(y)) ys.push(y / f); }); });
    var yMaxRaw = ys.length ? Math.max.apply(null, ys) : 1;
    var yStep = niceStep(yMaxRaw || 1, 5), yMax = Math.ceil(yMaxRaw * 1.02 / yStep) * yStep || yStep;
    var X = function(t) { return L + (W - L - R) * t / o.xMax; };
    var Y = function(v) { return H - B - (H - B - Tp) * v / yMax; };
    var out = ['<svg viewBox="0 0 ' + W + ' ' + H + '" role="img" class="pkconv-graph" aria-label="' + o.aria + '">'];
    for (var y = 0; y <= yMax + yStep / 2; y += yStep) {
      var Yp = Y(y).toFixed(1);
      out.push('<line class="pkconv-grid" x1="' + L + '" x2="' + (W - R) + '" y1="' + Yp + '" y2="' + Yp + '"/>');
      out.push('<text class="pkconv-tick" x="' + (L - 6) + '" y="' + Yp + '" text-anchor="end" dy="0.32em">' +
               P.fmt(y, 3) + '</text>');
    }
    var xStep = niceStep(o.xMax / f, 6);
    for (var x = 0; x <= o.xMax / f + 1e-9 * xStep; x += xStep) {
      var Xp = X(x * f).toFixed(1);
      out.push('<line class="pkconv-grid" y1="' + Tp + '" y2="' + (H - B) + '" x1="' + Xp + '" x2="' + Xp + '"/>');
      out.push('<text class="pkconv-tick" x="' + Xp + '" y="' + (H - B + 16) + '" text-anchor="middle">' +
               P.fmt(x, 4) + '</text>');
    }
    out.push('<line class="pkconv-axis" x1="' + L + '" x2="' + (W - R) + '" y1="' + (H - B) + '" y2="' + (H - B) + '"/>');
    out.push('<line class="pkconv-axis" x1="' + L + '" x2="' + L + '" y1="' + Tp + '" y2="' + (H - B) + '"/>');
    out.push('<text class="pkconv-label" x="' + ((L + W - R) / 2) + '" y="' + (H - 8) + '" text-anchor="middle">' +
             o.xLabel + '</text>');
    out.push('<text class="pkconv-label" transform="translate(14,' + ((Tp + H - B) / 2) + ') rotate(-90)" ' +
             'text-anchor="middle">' + o.yLabel + '</text>');
    series.forEach(function(sr, k) {
      var d = '';
      for (var i = 0; i < sr.x.length; i++) {
        if (!isFinite(sr.y[i])) { d += ''; continue; }
        d += (d ? 'L' : 'M') + X(sr.x[i]).toFixed(1) + ',' + Y(sr.y[i] / f).toFixed(1);
      }
      out.push('<path class="' + sr.cls + '" d="' + d + '"/>');
      // Bottom right: the curves rise with the duration, so that corner is clear
      var ly = H - B - 14 - 18 * (series.length - 1 - k), lx = W - R - 110;
      out.push('<line class="' + sr.cls + '" x1="' + lx + '" x2="' + (lx + 22) + '" y1="' + ly + '" y2="' + ly + '"/>');
      out.push('<text class="pkconv-tick" x="' + (lx + 28) + '" y="' + ly + '" dy="0.32em">' + sr.label + '</text>');
    });
    out.push('</svg>');
    return out.join('');
  }

  // Long enough to see the fall to 2% of the initial plasma concentration and
  // three times the time to peak effect
  function autoHorizon(model, ke0) {
    var c0 = P.plasma(model, 0), lo = 0, hi = 1 / model.lambda[0];
    while (P.plasma(model, hi) > 0.02 * c0 && hi < 1e12) { lo = hi; hi *= 2; }
    for (var i = 0; i < 60; i++) {
      var mid = (lo + hi) / 2;
      if (P.plasma(model, mid) > 0.02 * c0) lo = mid; else hi = mid;
    }
    var t = hi;
    if (ke0) t = Math.max(t, 3 * P.peakTime(model, ke0));
    return t;
  }

  function plot($root, s) {
    var $box = $root.find('.pkconv-plot');
    var $svg = $box.find('.pkconv-svg');
    var $norm = $box.find('input[name="pkconv-norm"]');
    $norm.filter('[value="effect"]').prop('disabled', !s.ke0);
    if (!s.ke0 && $norm.filter(':checked').val() === 'effect') $norm.filter('[value="plasma"]').prop('checked', true);
    if (!s.model) {
      $svg.html('<p class="pkconv-empty">The graph appears when the model is defined.</p>');
      return;
    }
    var m = s.model, ke0 = s.ke0, f = UNIT_MINUTES[s.unit];
    var norm = $norm.filter(':checked').val() || 'none';
    var logY = $box.find('input[data-pk-plot="log"]').prop('checked');
    var until = parseEntry($box.find('input[data-pk-plot="until"]').val());
    var T = until > 0 ? until * f : autoHorizon(m, ke0);

    var N = 600, ts = [], cp = [], ce = [];
    for (var i = 0; i <= N; i++) {
      var t = T * i / N;
      ts.push(t); cp.push(P.plasma(m, t));
      if (ke0) ce.push(P.effect(m, ke0, t));
    }
    var peak = ke0 ? P.peakTime(m, ke0) : null;
    var cePeak = ke0 ? P.effect(m, ke0, peak) : null;
    var scale = norm === 'plasma' ? cp[0] : norm === 'effect' ? cePeak : 1;
    cp = cp.map(function(y) { return y / scale; });
    ce = ce.map(function(y) { return y / scale; });

    var W = 640, H = 340, L = 64, R = 16, Tp = 14, B = 46;
    var all = cp.concat(ce).filter(function(y) { return y > 0 && isFinite(y); });
    var ymax = Math.max.apply(null, all), ymin, yTicks = [];
    var xs = function(t) { return L + (W - L - R) * t / T; };
    var ys;
    if (logY) {
      var top = Math.ceil(Math.log10(ymax)), bottom = Math.floor(Math.log10(Math.min.apply(null, all)));
      bottom = Math.max(bottom, top - 6);
      if (bottom === top) bottom = top - 1;
      ymin = Math.pow(10, bottom);
      ys = function(y) {
        var v = (Math.log10(Math.max(y, ymin)) - bottom) / (top - bottom);
        return H - B - (H - B - Tp) * v;
      };
      for (var e = bottom; e <= top; e++) yTicks.push(Math.pow(10, e));
      ymax = Math.pow(10, top);
    } else {
      var step = niceStep(ymax, 5);
      ymax = Math.ceil(ymax * 1.02 / step) * step;
      ymin = 0;
      ys = function(y) { return H - B - (H - B - Tp) * y / ymax; };
      for (var y = 0; y <= ymax + step / 2; y += step) yTicks.push(y);
    }
    var xStep = niceStep(T / f, 6), xTicks = [];
    for (var x = 0; x <= T / f + 1e-9 * xStep; x += xStep) xTicks.push(x);

    function path(vals) {
      var d = '';
      for (var i = 0; i < vals.length; i++) {
        if (logY && !(vals[i] > 0)) continue;
        d += (d ? 'L' : 'M') + xs(ts[i]).toFixed(1) + ',' + ys(vals[i]).toFixed(1);
      }
      return d;
    }
    var o = [];
    o.push('<svg viewBox="0 0 ' + W + ' ' + H + '" role="img" class="pkconv-graph" ' +
           'aria-label="Plasma' + (ke0 ? ' and effect-site' : '') + ' concentration after a unit bolus">');
    yTicks.forEach(function(v) {
      var Y = ys(v).toFixed(1);
      o.push('<line class="pkconv-grid" x1="' + L + '" x2="' + (W - R) + '" y1="' + Y + '" y2="' + Y + '"/>');
      o.push('<text class="pkconv-tick" x="' + (L - 6) + '" y="' + Y + '" text-anchor="end" dy="0.32em">' +
             P.fmt(v, 3) + '</text>');
    });
    xTicks.forEach(function(v) {
      var X = xs(v * f).toFixed(1);
      o.push('<line class="pkconv-grid" y1="' + Tp + '" y2="' + (H - B) + '" x1="' + X + '" x2="' + X + '"/>');
      o.push('<text class="pkconv-tick" x="' + X + '" y="' + (H - B + 16) + '" text-anchor="middle">' +
             P.fmt(v, 4) + '</text>');
    });
    o.push('<line class="pkconv-axis" x1="' + L + '" x2="' + (W - R) + '" y1="' + (H - B) + '" y2="' + (H - B) + '"/>');
    o.push('<line class="pkconv-axis" x1="' + L + '" x2="' + L + '" y1="' + Tp + '" y2="' + (H - B) + '"/>');
    o.push('<text class="pkconv-label" x="' + ((L + W - R) / 2) + '" y="' + (H - 8) + '" text-anchor="middle">Time (' +
           s.unit + ')</text>');
    var yLabel = norm === 'plasma' ? 'Fraction of the peak plasma concentration' :
                 norm === 'effect' ? 'Fraction of the peak effect-site concentration' :
                 'Concentration after a unit bolus (1/L)';
    o.push('<text class="pkconv-label" transform="translate(14,' + ((Tp + H - B) / 2) + ') rotate(-90)" ' +
           'text-anchor="middle">' + yLabel + '</text>');
    o.push('<path class="pkconv-cp" d="' + path(cp) + '"/>');
    if (ke0) {
      o.push('<path class="pkconv-ce" d="' + path(ce) + '"/>');
      if (peak <= T) {
        var PX = xs(peak).toFixed(1), PY = ys(cePeak / scale).toFixed(1);
        o.push('<line class="pkconv-peak" x1="' + PX + '" x2="' + PX + '" y1="' + PY + '" y2="' + (H - B) + '"/>');
        o.push('<circle class="pkconv-peak-dot" cx="' + PX + '" cy="' + PY + '" r="3.5"/>');
        o.push('<text class="pkconv-tick" x="' + (Number(PX) + 6) + '" y="' + (Number(PY) - 6) + '">peak effect, ' +
               P.fmt(peak / f, 3) + ' ' + s.unit + '</text>');
      }
    }
    var lx = W - R - 150;
    o.push('<line class="pkconv-cp" x1="' + lx + '" x2="' + (lx + 22) + '" y1="' + (Tp + 10) + '" y2="' + (Tp + 10) + '"/>');
    o.push('<text class="pkconv-tick" x="' + (lx + 28) + '" y="' + (Tp + 10) + '" dy="0.32em">Plasma</text>');
    if (ke0) {
      o.push('<line class="pkconv-ce" x1="' + lx + '" x2="' + (lx + 22) + '" y1="' + (Tp + 28) + '" y2="' + (Tp + 28) + '"/>');
      o.push('<text class="pkconv-tick" x="' + (lx + 28) + '" y="' + (Tp + 28) + '" dy="0.32em">Effect site</text>');
    }
    o.push('</svg>');
    $svg.html(o.join(''));
  }

  // --- Events -----------------------------------------------------------------

  function root(el) { return $(el).closest('.pkconv'); }

  // Typing in a panel makes it the definition.  Moving to another panel takes
  // that panel's values as they stand, derived ones included, so changing one
  // number holds the rest of its panel fixed.
  $(document).on('input', '.pkconv .pkconv-group input[data-pk-field]', function() {
    if (this.readOnly) return;
    var $root = root(this), s = state($root);
    var group = $(this).closest('[data-pk-group]').attr('data-pk-group');
    if (s.active !== group) {
      s.active = group;
      s.entered = {};
      REQUIRED[group][s.n].forEach(function(name) {
        var full = field($root, group, name).attr('data-full');
        if (full !== undefined) s.entered[name] = P.fmt(Number(full), 10);
      });
    }
    s.entered[$(this).attr('data-pk-field')] = this.value;
    recompute($root);
  });

  $(document).on('input', '.pkconv .pkconv-effect input[data-pk-effect]', function() {
    var $root = root(this), s = state($root);
    s.effectActive = $(this).attr('data-pk-effect');
    s.effectEntered = this.value;
    effectSite($root, s);
  });

  $(document).on('input change', '.pkconv .pkconv-plot input', function() {
    var $root = root(this);
    plot($root, state($root));
  });

  $(document).on('input', '.pkconv .pkconv-decrement input', function() {
    var $root = root(this);
    decrement($root, state($root));
  });

  $(document).on('change', '.pkconv input[name="pkconv-n"]', function() {
    var $root = root(this), s = state($root);
    s.n = Number(this.value);
    recompute($root);
  });

  // A new time unit converts what was entered, so the model stays the same
  $(document).on('change', '.pkconv input[name="pkconv-unit"]', function() {
    var $root = root(this), s = state($root), old = s.unit;
    s.unit = this.value;
    if (s.active) {
      Object.keys(s.entered).forEach(function(name) {
        var x = parseEntry(s.entered[name]);
        var dim = field($root, s.active, name).attr('data-pk-dim');
        if (x > 0) s.entered[name] = P.fmt(toDisplay(fromDisplay(x, dim, old), dim, s.unit), 10);
      });
    }
    if (s.effectActive) {
      var x = parseEntry(s.effectEntered), dim = EFFECT_DIMS[s.effectActive];
      if (x > 0) s.effectEntered = P.fmt(toDisplay(fromDisplay(x, dim, old), dim, s.unit), 10);
      $root.find('input[data-pk-effect="' + s.effectActive + '"]').val(s.effectEntered);
    }
    $root.find('input[data-pk-plot="until"], input[data-pk-dec="until"]').each(function() {
      var u = parseEntry($(this).val());
      if (u > 0) $(this).val(P.fmt(toDisplay(fromDisplay(u, 'time', old), 'time', s.unit), 10));
    });
    recompute($root);
  });

  $(document).on('click', '.pkconv [data-pk-action]', function(e) {
    e.preventDefault();
    var $root = root(this), s = state($root);
    if ($(this).attr('data-pk-action') === 'example') {
      s.active = 'vc';
      s.entered = {};
      var ex = EXAMPLES[s.n];
      Object.keys(ex).forEach(function(name) {
        var dim = field($root, 'vc', name).attr('data-pk-dim');
        s.entered[name] = P.fmt(toDisplay(ex[name], dim, s.unit), 10);
      });
    } else {
      s.active = null;
      s.entered = {};
      s.effectActive = null;
      s.effectEntered = null;
      $root.find('.pkconv-effect input').val('');
      $root.find('input[data-pk-dec="pct"]').val('50');
      $root.find('input[data-pk-dec="until"]').val('');
    }
    recompute($root);
  });
})(jQuery);
