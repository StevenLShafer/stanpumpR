// stanpumpR viewer — the injection tests
//
// Author:    Dr Charles Minto
// Assisted by: Claude.ai (Anthropic)
//
// Copyright (c) 2026 Charles Minto and Thomas Schnider
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// A run file cannot put markup in the page. Everything the viewer writes
// into its cards is a checked number or plain text: runProblem() refuses a
// field that carries markup before the run is drawn, and the name of a file
// that is not JSON, and the error from it, are written as text. These tests
// drive the built viewer, inst/www/stanpumpR-viewer.html, in headless Chrome or
// Edge and try to break each of those: a run with markup in each field the
// page shows must be refused with a message naming the field, a dropped
// file's name and the JSON error must be shown as text, and unusual but
// valid runs must still load. Injection is detected two ways: the payload's
// own counter, window.__pwned, and any element it leaves inside the cards.
//
// Run them with:  cd tools/viewer/build; npm install; npm test
//
// The browser is env CHROME_PATH if set, else the first of Chrome, Edge or
// a Linux Chrome/Chromium that exists; with none the suite skips. For
// checking the tests against another build — the suite must fail against a
// build from before the checks — set env VIEWER_HTML to that file's path.

import { test, before, after } from 'node:test';
import assert from 'node:assert/strict';
import { createRequire } from 'node:module';
import { existsSync, readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import path from 'node:path';

const require = createRequire(path.resolve(fileURLToPath(import.meta.url), '../../build/package.json'));
const puppeteer = require('puppeteer-core');

const HERE = path.dirname(fileURLToPath(import.meta.url));
const VIEWER = process.env.VIEWER_HTML
  ? path.resolve(process.env.VIEWER_HTML)
  : path.resolve(HERE, '..', '..', '..', 'inst', 'www', 'stanpumpR-viewer.html');
const RUN = JSON.parse(readFileSync(path.resolve(HERE, '..', 'examples', 'propofol-tci-e.json'), 'utf8'));

const BROWSERS = [
  'C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe',
  'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
  '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  '/usr/bin/google-chrome',
  '/usr/bin/chromium',
];
const executablePath = process.env.CHROME_PATH || BROWSERS.find(existsSync);
const skip = executablePath ? false : 'no Chrome or Edge found (set CHROME_PATH)';

// The markup every test tries to sneak in: if it ever reaches the page as
// HTML, the broken image's handler counts it on window.__pwned.
const P = '<img src=x onerror="window.__pwned=(window.__pwned||0)+1">';
const SINKS = ['#message', '#readout', '#amounts', '.flow-tip', '.lbl'];

let browser;
before(async () => {
  if (skip) return;
  browser = await puppeteer.launch({
    executablePath, headless: true,
    args: ['--enable-unsafe-swiftshader', '--use-angle=swiftshader'],
  });
});
after(async () => { if (browser) await browser.close(); });

// A fresh page on the viewer; page errors and console errors are collected
// on page.errors for assertClean() — a test fails on any of them.
async function viewerPage(t) {
  const page = await browser.newPage();
  page.errors = [];
  page.on('pageerror', e => page.errors.push('pageerror: ' + e.message));
  page.on('console', m => {
    // The payload's broken image itself logs a failed-resource error, and
    // the viewer's own file may log favicon 404s; any other error fails.
    if (m.type() === 'error' && !/Failed to load resource/.test(m.text()))
      page.errors.push('console error: ' + m.text());
  });
  await page.goto('file:///' + VIEWER.replace(/\\/g, '/'), { waitUntil: 'load' });
  await page.waitForFunction('typeof Player !== "undefined" && typeof Player.load === "function"');
  t.after(() => page.close());
  return page;
}

const assertClean = page => assert.deepEqual(page.errors, [], 'page errors');

// What the page shows after the dust settles: the payload's count, any
// element it left in a card, and the message card's text and state.
async function probe(page) {
  await new Promise(r => setTimeout(r, 300));
  return page.evaluate(sinks => ({
    pwned: window.__pwned || 0,
    imgs: sinks.flatMap(s => [...document.querySelectorAll(s + ' img')].map(i => s)),
    message: (document.querySelector('#message .card') || {}).innerText || '',
    shown: document.getElementById('message').classList.contains('show'),
  }), SINKS);
}

const loadRun = (page, run) =>
  page.evaluate(r => Player.load(JSON.parse(JSON.stringify(r))), run);

function assertNoInjection(probeResult) {
  assert.equal(probeResult.pwned, 0, 'payload handler ran');
  assert.deepEqual(probeResult.imgs, [], 'injected element in a card: ' + probeResult.imgs);
}

test('the example run loads', { skip }, async t => {
  const page = await viewerPage(t);
  assert.equal(await loadRun(page, RUN), true);
  const p = await probe(page);
  assert.equal(p.shown, false, 'message card shown for a good run');
  const readout = await page.$eval('#readout', e => e.innerText);
  assert.match(readout, /Propofol/);
  assert.match(readout, /µg\/mL/);
  assertClean(page);
});

// One refused field per test: the run is not drawn and the card names it.
const REFUSALS = [
  ['units.conc', r => { r.units.conc = 'mcg/mL' + P; }, 'units.conc'],
  ['units.amount', r => { r.units.amount = P; }, 'units.amount'],
  ['units.rate', r => { r.units.rate = P; }, 'units.rate'],
  ['source', r => { r.source = P; }, 'source'],
  ['patient.sex', r => { r.patient.sex = 'male' + P; }, 'patient.sex'],
  ['patient.weight', r => { r.patient.weight = P; }, 'patient.weight'],
  ['version', r => { r.version = P; }, 'not a number'],
];
for (const [name, mutate, names] of REFUSALS) {
  test(`markup in ${name} is refused`, { skip }, async t => {
    const page = await viewerPage(t);
    const run = structuredClone(RUN);
    mutate(run);
    assert.equal(await loadRun(page, run), false);
    const p = await probe(page);
    assertNoInjection(p);
    assert.ok(p.message.includes(names), `message should name ${names}: ${p.message}`);
    assertClean(page);
  });
}

// A dropped file whose name is markup, holding text that is not JSON.
test('a dropped file name is shown as text', { skip }, async t => {
  const page = await viewerPage(t);
  await page.evaluate(name => {
    const f = new File(['this is not json'], name, { type: 'application/json' });
    const dt = new DataTransfer();
    dt.items.add(f);
    window.dispatchEvent(new DragEvent('drop', { dataTransfer: dt, bubbles: true, cancelable: true }));
  }, P + '.json');
  const p = await probe(page);
  assertNoInjection(p);
  assert.match(p.message, /is not JSON/);
  assertClean(page);
});

// Invalid JSON whose content the error message echoes: a tag short enough
// to survive the echo must arrive as text, not an element.
test('a JSON error echoes its content as text', { skip }, async t => {
  const page = await viewerPage(t);
  await page.evaluate(() => {
    const f = new File(['<x/oncut=__pwned=1>'], 'run.json', { type: 'application/json' });
    const dt = new DataTransfer();
    dt.items.add(f);
    window.dispatchEvent(new DragEvent('drop', { dataTransfer: dt, bubbles: true, cancelable: true }));
  });
  await new Promise(r => setTimeout(r, 300));
  const injected = await page.evaluate(() => {
    const el = document.querySelector('#message [oncut]');
    if (el) el.dispatchEvent(new Event('cut'));
    return !!el;
  });
  assert.equal(injected, false, 'echoed markup became an element');
  const p = await probe(page);
  assertNoInjection(p);
  assert.match(p.message, /is not JSON/);
  assertClean(page);
});

test('unusual but valid runs still load', { skip }, async t => {
  const page = await viewerPage(t);
  const odd = structuredClone(RUN);
  odd.units = { conc: 'ng/mL', amount: 'mcg', rate: 'mcg/min' };
  odd.source = 'stanpumpR 1.4';
  odd.patient.sex = 'female';
  assert.equal(await loadRun(page, odd), true, 'unusual values refused');
  const bare = structuredClone(RUN);
  delete bare.patient;
  assert.equal(await loadRun(page, bare), true, 'run without patient refused');
  const p = await probe(page);
  assertNoInjection(p);
  assertClean(page);
});

// Gemini's audit of 293e168 (2026-11-06).
test('a run with vanishing concentrations and no typical level loads', { skip }, async t => {
  const page = await viewerPage(t);
  const tiny = structuredClone(RUN);
  tiny.typical = 0;
  tiny.cp = tiny.cp.map(() => 1e-120);
  if (tiny.ce) tiny.ce = tiny.ce.map(() => 1e-120);
  // toFixed() refuses more than 100 digits; the decimals were unbounded.
  assert.equal(await loadRun(page, tiny), true);
  assertClean(page);
});

test('non-numbers for volumes, clearances or typical are refused', { skip }, async t => {
  const page = await viewerPage(t);
  for (const [field, value] of [['v1', null], ['v1', '5'], ['cl1', false], ['cl1', []]]) {
    const bad = structuredClone(RUN);
    bad.pk[field] = value;
    assert.equal(await loadRun(page, bad), false, `pk.${field} = ${JSON.stringify(value)} accepted`);
  }
  const bad = structuredClone(RUN);
  bad.typical = 'a lot';
  assert.equal(await loadRun(page, bad), false, 'typical = "a lot" accepted');
  assertClean(page);
});

// ChatGPT's audit of ff7e2f0 (2026-11-06).
test('a saturable drug with unknown input draws no elimination arrow; a linear one draws CL1 x Cp', { skip }, async t => {
  const page = await viewerPage(t);
  const run = structuredClone(RUN);
  run.version = 2;
  run.inputKnown = false;
  run.eliminationLinear = false;
  assert.equal(await loadRun(page, run), true);
  const outSat = await page.evaluate(() => Player.stateAt(Player.run.t[600]).flow.out);
  assert.equal(outSat, 0, 'saturable drug drew an elimination rate');
  run.eliminationLinear = true;
  assert.equal(await loadRun(page, run), true);
  const outLin = await page.evaluate(() => {
    const st = Player.stateAt(Player.run.t[600]);
    return [st.flow.out, Player.run.pk.cl1 * st.cp];
  });
  assert.ok(Math.abs(outLin[0] - outLin[1]) < 1e-12 && outLin[0] > 0, 'linear drug lost its elimination rate');
  assertClean(page);
});

// The 2D view (flat.js, 2026-11-06).
test('the 2D view is the first, draws the run, and switches to 3D and back', { skip }, async t => {
  const page = await viewerPage(t);
  assert.equal(await loadRun(page, RUN), true);
  const first = await page.evaluate(() => ({
    view: Player.view,
    rects: document.querySelectorAll('#flat svg rect').length,
    flatShown: getComputedStyle(document.getElementById('flat')).display,
  }));
  assert.equal(first.view, '2d');
  assert.equal(first.flatShown, 'block');
  assert.ok(first.rects >= 6, 'too few boxes drawn: ' + first.rects);
  // V1's fill height is its concentration on the shared scale.
  const fill = await page.evaluate(() => {
    Player.seek(1500);
    const st = Player.stateAt(1500);
    return { cp: st.cp, top: Flat.scale.top };
  });
  assert.ok(fill.cp > 0 && fill.cp <= fill.top, 'Cp above the axis top');
  await page.click('#viewToggle');
  assert.equal(await page.evaluate(() => [Player.view, getComputedStyle(document.getElementById('flat')).display].join()), '3d,none');
  await page.click('#viewToggle');
  assert.equal(await page.evaluate(() => Player.view), '2d');
  assertClean(page);
});

// ── The 2D view: ChatGPT's audit of 38e0f3a ──────────────────────────────
const clone = r => JSON.parse(JSON.stringify(r));
// Every drawn element's box, against the window, and the text of the labels.
const flatGeometry = page => page.evaluate(() => {
  const W = innerWidth, svg = document.querySelector('#flat svg');
  const out = { W, svgH: +svg.getAttribute('height'), off: [], texts: [] };
  for (const el of svg.querySelectorAll('rect, polygon, text')) {
    const b = el.getBBox();
    if (b.width === 0 && b.height === 0) continue;
    if (b.x < -0.5 || b.x + b.width > W + 0.5 || b.y < -0.5 || b.y + b.height > out.svgH + 0.5)
      out.off.push(`${el.tagName} ${el.textContent} ${b.x.toFixed(1)},${b.y.toFixed(1)} ${b.width.toFixed(1)}x${b.height.toFixed(1)}`);
    if (el.tagName === 'text') out.texts.push({ cls: el.getAttribute('class'), s: el.textContent });
  }
  return out;
});

test('2D: a one-sample peak sets the concentration scale', { skip }, async t => {
  const page = await viewerPage(t);
  const r = clone(RUN); r.cp[1501] = 100;
  assert.equal(await loadRun(page, r), true);
  const top = await page.evaluate(() => Flat.scale.top);
  assert.ok(top >= 100, 'the scale missed the peak: ' + top);
  assertClean(page);
});

test('2D: extreme finite concentrations load, with a finite positive scale', { skip }, async t => {
  for (const [name, c] of [['MAX_VALUE', Number.MAX_VALUE], ['MIN_VALUE', Number.MIN_VALUE], ['1e-120', 1e-120]]) {
    const page = await viewerPage(t);
    const r = clone(RUN);
    for (const k of ['cp', 'ce', 'c2', 'c3']) r[k] = r[k].map(() => c);
    r.typical = c; r.rate = r.rate.map(() => 0); r.target = r.target.map(() => 0); r.mode = r.mode.map(() => 'manual');
    r.inputKnown = false; delete r.given;
    // An endless tick loop would hang here: the evaluate is raced against a timer.
    const ok = await Promise.race([loadRun(page, r), new Promise(res => setTimeout(() => res('timeout'), 10000))]);
    assert.equal(ok, true, name + ': ' + ok);
    const g = await page.evaluate(() => ({ top: Flat.scale.top, n: document.querySelectorAll('#flat svg .tk').length }));
    assert.ok(Number.isFinite(g.top) && g.top > 0, name + ': scale ' + g.top);
    assert.ok(g.n >= 2 && g.n <= 14, name + ': ' + g.n + ' ticks');
    // A concentration that is not zero never reads as zero.
    const geo = await flatGeometry(page);
    for (const x of geo.texts.filter(x => /\bval\b/.test(x.cls)))
      assert.ok(Number(x.s) !== 0, `${name}: a nonzero concentration labelled "${x.s}"`);
    if (name === '1e-120') assert.ok(geo.texts.some(x => x.cls === 'tk' && x.s !== '0' && Number(x.s) > 0), 'every tick reads 0');
    assertClean(page);
  }
});

test('2D: the boxes fit the window when V1 << V3 and V1 >> V3, and tiny volumes do not read 0 L', { skip }, async t => {
  for (const pk of [{ v1: 0.001, v2: 0.002, v3: 100000 }, { v1: 100000, v2: 0.002, v3: 0.001 }]) {
    const page = await viewerPage(t);
    await page.setViewport({ width: 1200, height: 800 });
    const r = clone(RUN); Object.assign(r.pk, pk);
    assert.equal(await loadRun(page, r), true);
    await page.evaluate(() => Player.seek(1500));
    const geo = await flatGeometry(page);
    assert.deepEqual(geo.off, [], JSON.stringify(pk) + ': drawn outside the window');
    for (const x of geo.texts.filter(x => / L$/.test(x.s)))
      assert.ok(parseFloat(x.s) > 0, `a volume labelled "${x.s}"`);
    assertClean(page);
  }
});

test('2D: rate labels have three significant figures; arrow thickness is in proportion below the cap', { skip }, async t => {
  const page = await viewerPage(t);
  const r = clone(RUN);
  r.rate = r.rate.map((x, i) => i === 100 ? 1 : i === 200 ? 2 : 100);
  r.inputKnown = false; delete r.given;
  assert.equal(await loadRun(page, r), true);
  const shaft = T => page.evaluate(T => {
    Player.seek(T);
    const rect = [...document.querySelectorAll('#flat svg rect')].find(e => e.getAttribute('fill') === '#ff6b5e');
    return +rect.getAttribute('width');
  }, T);
  const w1 = await shaft(100.5), w2 = await shaft(200.5);
  assert.ok(Math.abs(w2 / w1 - 2) < 1e-9, `1 and 2 mg/min drawn ${w1} and ${w2} px`);
  for (const [rate, want] of [[12345.67, '12300'], [99.94, '99.9'], [100, '100'], [999, '999'], [1000, '1000'], [0.012345, '0.0123']]) {
    const s = await page.evaluate(rate => {
      Player.run.rate[300] = rate; Player.seek(300.5);
      return [...document.querySelectorAll('#flat svg text.fl.in')].map(e => e.textContent).find(x => x.startsWith('Pump'));
    }, rate);
    assert.equal(s, `Pump ${want} mg/min`);
  }
  assertClean(page);
});

test('2D: in a 360 x 260 window the controls and the view switch show and the drawing scrolls; in 1200 x 800 nothing is under the cards', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 360, height: 260 });
  assert.equal(await loadRun(page, RUN), true);
  await page.evaluate(() => Player.seek(1500));
  await new Promise(r => setTimeout(r, 200));
  const small = await page.evaluate(() => {
    const inView = id => { const b = document.getElementById(id).getBoundingClientRect();
      return b.left >= 0 && b.right <= innerWidth && b.top >= 0 && b.bottom <= innerHeight; };
    const flat = document.getElementById('flat'), ctl = document.getElementById('controls').getBoundingClientRect();
    return { toggle: inView('viewToggle'), play: inView('play'), t: inView('t'), speed: inView('speedSlider'),
             flatBottom: flat.getBoundingClientRect().bottom, ctlTop: ctl.top,
             scrolls: flat.scrollHeight > flat.clientHeight };
  });
  for (const k of ['toggle', 'play', 't', 'speed']) assert.ok(small[k], k + ' is off-screen at 360 x 260');
  assert.ok(small.flatBottom <= small.ctlTop, 'the drawing runs under the controls');
  assert.ok(small.scrolls, 'the drawing does not fit and cannot be scrolled');
  assert.deepEqual((await flatGeometry(page)).off, [], 'drawn outside the drawing');
  // A large window: the drawing ends above the cards.
  await page.setViewport({ width: 1200, height: 800 });
  await new Promise(r => setTimeout(r, 200));
  const big = await underCards(page);
  assert.notEqual(big.cards, null, 'no card shows in a 1200 x 800 window');
  assert.ok(clearOfCards(big), `the drawing reaches ${big.low}, the cards start at ${big.cards}`);
  assertClean(page);
});

// ── The 2D view: Gemini's audit of 712e43a ───────────────────────────────
// The bottom of the drawing against the top of the cards that show.
const underCards = page => page.evaluate(() => {
  const cards = ['readout', 'amounts'].map(id => document.getElementById(id))
    .filter(e => e.offsetHeight > 0).map(e => e.getBoundingClientRect().top);
  let low = 0;
  for (const el of document.querySelectorAll('#flat svg rect, #flat svg polygon, #flat svg text'))
    { const b = el.getBoundingClientRect(); if (b.height > 0) low = Math.max(low, b.bottom); }
  // null, not Infinity, when no card shows: Infinity reaches Node as null,
  // and 500 <= null is false (ChatGPT, 6b52133).
  return { cards: cards.length ? Math.min(...cards) : null, low, compact: document.body.classList.contains('compact') };
});
// The drawing is clear of the cards: below every card that shows, or no card shows.
const clearOfCards = g => g.cards == null || g.low <= g.cards;

test('2D: the first frame after a load is laid out around the cards', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  await new Promise(r => setTimeout(r, 200));
  assert.equal(await loadRun(page, RUN), true);          // no second seek
  const big = await underCards(page);
  assert.ok(clearOfCards(big), `first frame reaches ${big.low}, the cards start at ${big.cards}`);
  await page.setViewport({ width: 360, height: 260 });
  await new Promise(r => setTimeout(r, 200));
  assert.equal(await loadRun(page, RUN), true);
  assert.equal((await underCards(page)).compact, true, 'the cards cover a 360 x 260 window on the first frame');
  assertClean(page);
});

test('2D: the amounts card (a) and a resize across sizes never leave the cards over the drawing, nor flicker', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 800, height: 600 });
  assert.equal(await loadRun(page, RUN), true);
  await page.evaluate(() => Player.seek(1500));
  await page.keyboard.press('a'); await page.evaluate(() => Player.seek(1500));
  await page.keyboard.press('a');                        // the card shows again, paused
  let g = await underCards(page);
  assert.ok(clearOfCards(g), `after a, the drawing reaches ${g.low}, the cards ${g.cards}`);
  for (const [w0, h0, w1, h1] of [[800, 500, 800, 580], [1200, 400, 1200, 800], [800, 580, 800, 500]]) {
    await page.setViewport({ width: w0, height: h0 }); await new Promise(r => setTimeout(r, 150));
    await page.evaluate(() => Player.seek(1500));
    await page.setViewport({ width: w1, height: h1 }); await new Promise(r => setTimeout(r, 150));
    const a = await underCards(page);
    assert.ok(a.compact || clearOfCards(a), `${w0}x${h0} -> ${w1}x${h1}: drawing ${a.low}, cards ${a.cards}`);
    await page.evaluate(() => Player.seek(1501));
    const b = await underCards(page);
    assert.equal(b.compact, a.compact, `${w0}x${h0} -> ${w1}x${h1}: the cards flicker on the next frame`);
  }
  assertClean(page);
});

test('2D: the scale and its ticks, the formatters, and box widths at the extremes', { skip }, async t => {
  const page = await viewerPage(t);
  const r = await page.evaluate(() => ({
    tops: [1, 1.5, 2, 2.5, 4, 10, 100, 9.9995, 2.0000001, 0.3].map(niceTop),
    over: [9.9995, 2.5, 2, 7, 1.5e-120, Number.MAX_VALUE, 5e-324].map(x => { const n = niceTop(x), k = ticks(n); return [n, k[k.length - 1]]; }),
    tickMax: fmtTick(Number.MAX_VALUE), fixMax: fixedOrSig(Number.MAX_VALUE, 2), fix21: fixedOrSig(1.234e22, 1),
    vast: boxWidths([1e308, 1e308, 1e308], 600), cramped: boxWidths([10, 10, 10], 6), none: boxWidths([0, 0, 0], 300),
  }));
  assert.deepEqual(r.tops.slice(0, 7), [1, 1.5, 2, 2.5, 4, 10, 100], 'a round value is its own top');
  assert.equal(r.tops[7], 10);
  assert.ok(r.tops[8] > 2.0000001 && r.tops[8] <= 2.5);
  assert.ok(Math.abs(r.tops[9] - 0.3) < 1e-12);
  for (const [n, last] of r.over) {
    assert.ok(last <= n * (1 + 1e-9), `a tick ${last} above the top ${n}`);
    assert.ok(last >= n * 0.79, `no tick near the top ${n} (last ${last})`);
  }
  assert.notEqual(r.tickMax, 'Infinity');
  assert.equal(r.fixMax, '1.80e+308');
  assert.equal(r.fix21, '1.23e+22');
  assert.deepEqual(r.vast, [200, 200, 200]);
  assert.ok(r.cramped.reduce((s, x) => s + x, 0) <= 6 + 1e-9, 'boxes wider than their room: ' + r.cramped);
  assert.deepEqual(r.none, [0, 0, 0]);
  assertClean(page);
});

test('2D: no two labels overlap, the pump and absorption arrows stand apart, and without Ce the V1-V3 arrow starts at V1', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  const r = clone(RUN);
  r.depot = r.t.map(x => Math.max(0, 500 - 0.5 * x)); r.absorbed = r.t.map(x => Math.min(500, 0.5 * x));
  r.inputKnown = false; delete r.given;
  assert.equal(await loadRun(page, r), true);
  for (const T of [0, 5, 60, 600, 1500, 3600]) {
    const o = await page.evaluate(T => {
      Player.seek(T);
      const bb = [...document.querySelectorAll('#flat svg text')].map(e => ({ s: e.textContent, b: e.getBBox() }));
      const hits = [];
      // A text's box is its font's full height (and a subscript's drop),
      // taller than the letters painted: boxes on adjacent lines can meet by
      // a few pixels where the letters are well apart, by an amount that
      // varies with the system's fonts (ChatGPT, 6b52133: 2 px for "V1" over
      // "6.28 L" on Linux). A real collision overlaps by more than that.
      const SLACK_Y = 4;
      for (let i = 0; i < bb.length; i++) for (let j = i + 1; j < bb.length; j++) {
        const a = bb[i].b, b = bb[j].b;
        if (a.x < b.x + b.width - 0.5 && b.x < a.x + a.width - 0.5 && a.y < b.y + b.height - SLACK_Y && b.y < a.y + a.height - SLACK_Y)
          hits.push(bb[i].s + ' / ' + bb[j].s);
      }
      // The two red arrowheads into V1's top.
      const heads = [...document.querySelectorAll('#flat svg polygon')].filter(p => p.getAttribute('fill') === '#ff6b5e').map(p => p.getBBox());
      const apart = heads.length < 2 || heads[0].x + heads[0].width <= heads[1].x + 0.5 || heads[1].x + heads[1].width <= heads[0].x + 0.5;
      return { hits, apart, heads: heads.length };
    }, T);
    assert.deepEqual(o.hits, [], `T = ${T}: labels overlap`);
    assert.ok(o.apart, `T = ${T}: the pump and absorption arrows overlap`);
  }
  const page2 = await viewerPage(t);
  await page2.setViewport({ width: 1200, height: 800 });
  const r2 = clone(RUN); r2.ce = null; r2.mode = r2.mode.map(() => 'plasma');
  assert.equal(await loadRun(page2, r2), true);
  const gapV1 = await page2.evaluate(() => {
    Player.seek(1500);
    const rects = [...document.querySelectorAll('#flat svg rect[fill="none"]')].map(e => e.getBBox()).sort((a, b) => a.x - b.x);
    const v1 = rects[1];                       // V2, V1, V3 outlines, left to right
    const arrow = [...document.querySelectorAll('#flat svg rect')].filter(e => e.getAttribute('fill') !== 'none')
      .map(e => e.getBBox()).filter(b => b.x > v1.x + v1.width - 1 && b.x < rects[2].x).sort((a, b) => a.x - b.x)[0];
    return arrow.x - (v1.x + v1.width);
  });
  assert.ok(gapV1 >= 0 && gapV1 <= 8, `the V1-V3 arrow starts ${gapV1} px from V1`);
  assertClean(page); assertClean(page2);
});

// ── Sizes: the largest compartment at most SIZE_RATIO_MAX times the smallest
// (Shafer, 2026-11-06), in both views; heights stay concentration.
test('sizes: proportional within 10:1, compressed to exactly 10:1 beyond it, in 2D and 3D', { skip }, async t => {
  for (const [pk, compressed] of [[{ v1: 5, v2: 10, v3: 20 }, false], [{ v1: 6.28, v2: 23.6, v3: 157 }, true],
                                  [{ v1: 0.00001, v2: 1, v3: 100000 }, true], [{ v1: 100000, v2: 1, v3: 0.00001 }, true]]) {
    const page = await viewerPage(t);
    await page.setViewport({ width: 1200, height: 800 });
    const r = clone(RUN); Object.assign(r.pk, pk);
    assert.equal(await loadRun(page, r), true);
    const g = await page.evaluate(() => {
      Player.seek(1500);
      const outl = [...document.querySelectorAll('#flat svg rect[fill="none"]')].filter(e => !e.getAttribute('stroke-dasharray'))
        .map(e => e.getBBox()).sort((a, b) => a.x - b.x);      // V2, V1, V3
      const caption = [...document.querySelectorAll('#flat svg text')].some(e => /not to scale/.test(e.textContent));
      const st = Player.stateAt(1500), top = Flat.scale.top;
      // Each fill's height is its concentration on the one scale, whatever the widths.
      const fills = [...document.querySelectorAll('#flat svg rect[fill-opacity]')].map(e => e.getBBox()).sort((a, b) => a.x - b.x);
      return { w: [outl[1].width, outl[0].width, outl[2].width], ph: outl[0].height, caption,
               h: [fills[1].height, fills[0].height, fills[2].height], c: [st.cp, st.c2, st.c3], top,
               r3d: View.geo.r.slice(1) };
    });
    const vols = [pk.v1, pk.v2, pk.v3], ratio = Math.max(...g.w) / Math.min(...g.w);
    assert.ok(ratio <= 10 * (1 + 1e-6), `${JSON.stringify(pk)}: 2D width ratio ${ratio}`);
    const area = g.r3d.map(x => x * x), aRatio = Math.max(...area) / Math.min(...area);
    assert.ok(aRatio <= 10 * (1 + 1e-6), `${JSON.stringify(pk)}: 3D floor-area ratio ${aRatio}`);
    if (compressed) assert.ok(Math.abs(ratio - 10) < 1e-6 && Math.abs(aRatio - 10) < 1e-6, `${JSON.stringify(pk)}: not exactly 10:1`);
    else for (let i = 1; i < 3; i++) {
      assert.ok(Math.abs(g.w[i] / g.w[0] - vols[i] / vols[0]) < 1e-9, '2D widths not in proportion within 10:1');
      assert.ok(Math.abs(area[i] / area[0] - vols[i] / vols[0]) < 1e-9, '3D areas not in proportion within 10:1');
    }
    // The order of the sizes is the order of the volumes.
    for (let i = 0; i < 3; i++) for (let j = 0; j < 3; j++) if (vols[i] < vols[j]) assert.ok(g.w[i] < g.w[j] && area[i] < area[j]);
    assert.equal(g.caption, compressed, 'the not-to-scale note');
    for (let i = 0; i < 3; i++) assert.ok(Math.abs(g.h[i] - g.c[i] / g.top * g.ph) < 1e-6, `fill ${i} is not its concentration`);
    assertClean(page);
  }
});

// ── Save a video (video.js, 2026-11-07): an H.264 MP4 made in the browser.
// The boxes of an MP4, walked: [{type, start, size, body}] at one level.
function mp4Boxes(buf, start = 0, end = buf.length) {
  const out = [];
  for (let o = start; o + 8 <= end;) {
    const size = buf.readUInt32BE(o), type = buf.toString('latin1', o + 4, o + 8);
    assert.ok(size >= 8 && o + size <= end, `box ${type} at ${o}: size ${size} runs past ${end}`);
    out.push({ type, start: o, size, body: o + 8 });
    o += size;
  }
  return out;
}
const child = (buf, b, type, skip = 0) => mp4Boxes(buf, b.body + skip, b.start + b.size).find(x => x.type === type);

// Whether this browser can encode the viewer's H.264 at the run's size (a
// Chromium built without it cannot): the recording tests skip themselves if not.
const canEncode = page => page.evaluate(async () => !!('VideoEncoder' in window && await videoCodec(Video.plan())));

async function recordVideo(page, view, withPatient) {
  return page.evaluate(async ([view, withPatient]) => {
    // 1 s = 20 min: 91 frames, past the second key frame (sample 61).
    Player.setView(view); Player.setSpeed(1200);
    const plan = Video.plan();
    const blob = await Video.record(plan, withPatient, () => {});
    View.recording = false;
    const buf = new Uint8Array(await blob.arrayBuffer());
    let s = ''; for (let i = 0; i < buf.length; i += 0x8000) s += String.fromCharCode.apply(null, buf.subarray(i, i + 0x8000));
    return { plan, b64: btoa(s) };
  }, [view, withPatient]);
}

test('video: the dialog says what it will make, and refuses a video over 5 minutes', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  assert.equal(await loadRun(page, RUN), true);
  await page.evaluate(() => Player.setSpeed(30));          // 3600 s at 30x: 2 min
  await page.click('#saveVideo');
  let card = await page.evaluate(() => ({ shown: document.getElementById('videoCard').classList.contains('show'),
    what: document.querySelector('#videoCard .what').textContent, go: document.querySelector('#videoCard .go').disabled,
    patient: document.querySelector('#videoCard .patient').checked }));
  assert.ok(card.shown);
  assert.match(card.what, /2D view/); assert.match(card.what, /2 min 0 s of video at 30 frames a second, 1620 × 1080/);
  assert.equal(card.patient, false, 'the patient must be left out unless asked for');
  // A browser without WebCodecs is told so, and Save stays off (CodeRabbit, #688).
  if (!(await page.evaluate(() => 'VideoEncoder' in window))) {
    assert.equal(card.go, true);
    assert.match(await page.evaluate(() => document.querySelector('#videoCard .why').textContent), /cannot make videos/);
    return assertClean(page);
  }
  assert.equal(card.go, false);
  await page.click('#videoCard .cancel');
  await page.evaluate(() => Player.setSpeed(1));            // an hour of video
  await page.click('#saveVideo');
  card = await page.evaluate(() => ({ go: document.querySelector('#videoCard .go').disabled, why: document.querySelector('#videoCard .why').textContent }));
  assert.equal(card.go, true);
  assert.match(card.why, /longest video is 5 min/);
  assertClean(page);
});

test('video: the strip names the patient only when asked; a markup drug name gives a safe file name', { skip }, async t => {
  const page = await viewerPage(t);
  const r = clone(RUN); r.drug = '<img src=x onerror=alert(1)>../propofol';
  assert.equal(await loadRun(page, r), true);
  const s = await page.evaluate(() => ({ off: videoHeading(false), on: videoHeading(true), file: videoFileName(Player.run.drug, '2d') }));
  assert.doesNotMatch(s.off, /kg|cm|male/);
  assert.match(s.on, /70 kg · 170 cm · 40 y · male/);
  assert.match(s.file, /^img-src-x-onerror-alert-1-propofol-2D-\d{4}-\d\d-\d\d\.mp4$/);
  assertClean(page);
});

test('video: each view saves a valid H.264 MP4 of every frame', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  const r = clone(RUN); r.drug = '<img src=x onerror=alert(1)>../propofol';
  assert.equal(await loadRun(page, r), true);
  if (!(await canEncode(page))) return t.skip('this browser cannot encode H.264');
  for (const view of ['2d', '3d']) {
    const { plan, b64 } = await recordVideo(page, view, false);
    const buf = Buffer.from(b64, 'base64');
    const top = mp4Boxes(buf);
    assert.deepEqual(top.map(b => b.type), ['ftyp', 'moov', 'mdat'], view + ': top-level boxes');
    const moov = top[1], trak = child(buf, moov, 'trak'), mdia = child(buf, trak, 'mdia');
    const stbl = child(buf, child(buf, mdia, 'minf'), 'stbl');
    const stsd = child(buf, stbl, 'stsd'), avc1 = mp4Boxes(buf, stsd.body + 8, stsd.start + stsd.size)[0];
    assert.equal(avc1.type, 'avc1');
    assert.equal(buf.readUInt16BE(avc1.body + 24), plan.w); assert.equal(buf.readUInt16BE(avc1.body + 26), plan.h);
    assert.ok(child(buf, avc1, 'avcC', 78), 'no avcC');
    const stsz = child(buf, stbl, 'stsz'), n = buf.readUInt32BE(stsz.body + 8);
    assert.equal(plan.frames, 91);
    assert.equal(n, plan.frames, view + ': every frame a sample');
    let total = 0; for (let i = 0; i < n; i++) total += buf.readUInt32BE(stsz.body + 12 + 4 * i);
    assert.equal(top[2].size - 8, total, 'mdat holds exactly the samples');
    const stco = child(buf, stbl, 'stco');
    assert.equal(buf.readUInt32BE(stco.body + 8), top[2].start + 8, 'the chunk offset is the first sample');
    const stss = child(buf, stbl, 'stss'), nKeys = buf.readUInt32BE(stss.body + 4);
    const keys = Array.from({ length: nKeys }, (_, i) => buf.readUInt32BE(stss.body + 8 + 4 * i));
    assert.equal(keys[0], 1, 'the first sample is a key frame');
    assert.ok(keys.includes(61), view + ': the key frame asked for at 2 s (sample 61) is missing: ' + keys);
    // One timing entry: every sample 1000 ticks of a 30000-per-second clock.
    const stts = child(buf, stbl, 'stts'), mdhd = child(buf, mdia, 'mdhd');
    assert.deepEqual([1, 2, 3].map(i => buf.readUInt32BE(stts.body + 4 * i)), [1, n, 1000]);
    assert.deepEqual([3, 4].map(i => buf.readUInt32BE(mdhd.body + 4 * i)), [30000, n * 1000]);
    // Each sample is length-prefixed NAL units that fill it exactly.
    let o = top[2].start + 8;
    for (let i = 0; i < n; i++) {
      const end = o + buf.readUInt32BE(stsz.body + 12 + 4 * i);
      while (o < end) o += 4 + buf.readUInt32BE(o);
      assert.equal(o, end, `sample ${i} is not whole NAL units`);
    }
  }
  assertClean(page);
});

test('video: Cancel stops it, and the run, the time and the 3D loop are as they were', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  assert.equal(await loadRun(page, RUN), true);
  if (!(await canEncode(page))) return t.skip('this browser cannot encode H.264');
  await page.evaluate(() => { Player.seek(1234); Player.setSpeed(30); });
  await page.click('#saveVideo');
  await page.click('#videoCard .go');
  await new Promise(r => setTimeout(r, 400));
  assert.equal(await page.evaluate(() => Video.busy), true);
  await page.keyboard.press('v');                           // the keys wait while it saves
  await page.click('#videoCard .cancel');
  await page.waitForFunction(() => !Video.busy, { timeout: 30000 });
  const after = await page.evaluate(() => ({ T: Player.T, view: Player.view, recording: View.recording, playing: Player.playing,
    why: document.querySelector('#videoCard .why').textContent }));
  assert.deepEqual(after, { T: 1234, view: '2d', recording: false, playing: false, why: 'Cancelled.' });
  assertClean(page);
});

test('video: while the card is open the keys wait, Esc closes it, and a resize refreshes what it says', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  assert.equal(await loadRun(page, RUN), true);
  await page.evaluate(() => Player.setSpeed(30));
  await page.click('#saveVideo');
  for (const k of ['v', '+', '-', ' ']) await page.keyboard.press(k);
  assert.deepEqual(await page.evaluate(() => [Player.view, Player.speed, Player.playing, Video.shown()]), ['2d', 30, false, true],
                   'a key changed the view, the speed or the play state behind the card');
  await page.setViewport({ width: 800, height: 600 });
  await page.waitForFunction(() => /1440 × 1080/.test(document.querySelector('#videoCard .what').textContent), { timeout: 5000 })
    .catch(() => {});                         // the assertion below says what it read
  assert.match(await page.evaluate(() => document.querySelector('#videoCard .what').textContent), /1440 × 1080/,
               'the card still gives the size before the resize');
  await page.keyboard.press('Escape');
  assert.equal(await page.evaluate(() => Video.shown()), false);
  await page.keyboard.press('v');
  assert.equal(await page.evaluate(() => Player.view), '3d', 'the keys work again once the card is closed');
  assertClean(page);
});

test('video: a run that arrives while a video is made waits, and is loaded when the save ends', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  assert.equal(await loadRun(page, RUN), true);
  if (!(await canEncode(page))) return t.skip('this browser cannot encode H.264');
  await page.evaluate(() => Player.setSpeed(30));
  await page.click('#saveVideo');
  await page.click('#videoCard .go');
  await new Promise(r => setTimeout(r, 300));
  const r2 = clone(RUN); r2.drug = 'second'; r2.t = r2.t.map(x => x / 2);   // half the length: a card that is refreshed says so
  const during = await page.evaluate(r => { const ok = Player.load(r); return { ok, drug: Player.run.drug, busy: Video.busy }; }, r2);
  assert.deepEqual(during, { ok: true, drug: 'propofol', busy: true }, 'the run being filmed was replaced');
  await page.click('#videoCard .cancel');
  await page.waitForFunction(() => !Video.busy, { timeout: 30000 });
  assert.equal(await page.evaluate(() => Player.run.drug), 'second', 'the waiting run was not loaded');
  const card = await page.evaluate(() => ({ what: document.querySelector('#videoCard .what').textContent,
                                            why: document.querySelector('#videoCard .why').textContent }));
  assert.equal(card.why, 'Cancelled.', 'the word on why the save ended was lost');
  assert.match(card.what, /0:00:00 to 0:30:00.*1 min 0 s of video/, 'the card does not describe the run now loaded: ' + card.what);
  assertClean(page);
});

test('video: a run loaded while the card is open (not saving) is described on it', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  assert.equal(await loadRun(page, RUN), true);
  await page.evaluate(() => Player.setSpeed(30));
  await page.click('#saveVideo');
  const half = clone(RUN); half.t = half.t.map(x => x / 2);
  await page.evaluate(r => Player.load(r), half);
  assert.match(await page.evaluate(() => document.querySelector('#videoCard .what').textContent), /0:00:00 to 0:30:00/,
               'the card still describes the run before');
  assertClean(page);
});

test('video: a resize during a save leaves the card describing the window after it; a run that cannot be shown is refused, not queued', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  assert.equal(await loadRun(page, RUN), true);
  if (!(await canEncode(page))) return t.skip('this browser cannot encode H.264');
  await page.evaluate(() => Player.setSpeed(30));
  await page.click('#saveVideo');
  // A resize during a save, then Cancel: the card gives the new size, and still says why it stopped.
  await page.click('#videoCard .go');
  await new Promise(r => setTimeout(r, 300));
  // A broken run (a JSON null) dropped mid-save: refused at once, its message
  // waiting under the card, the run being filmed untouched (CodeRabbit, #688).
  const bad = await page.evaluate(() => ({ ok: Player.load(null), pending: Video.pending, busy: Video.busy, drug: Player.run.drug,
    msg: document.querySelector('#message .card').textContent }));
  assert.deepEqual([bad.ok, bad.pending, bad.busy, bad.drug], [false, null, true, 'propofol']);
  assert.match(bad.msg, /not a JSON object/);
  // A good run queued, then a broken one: the good one loads when the save
  // ends, and the broken one's message is still shown after it.
  const half = clone(RUN); half.drug = 'half'; half.t = half.t.map(x => x / 2);
  assert.equal(await page.evaluate(r => Player.load(r), half), true);
  assert.equal(await page.evaluate(() => Player.load(null)), false);
  await page.setViewport({ width: 800, height: 600 });
  await page.click('#videoCard .cancel');
  await page.waitForFunction(() => !Video.busy, { timeout: 30000 });
  const card = await page.evaluate(() => ({ what: document.querySelector('#videoCard .what').textContent,
                                            why: document.querySelector('#videoCard .why').textContent }));
  assert.match(card.what, /1440 × 1080/, 'the card gives the size before the resize: ' + card.what);
  assert.equal(card.why, 'Cancelled.');
  // Once the card is closed: the queued run is shown, and the refusal is still in view.
  await page.click('#videoCard .cancel');
  const after = await page.evaluate(() => ({ shown: Video.shown(), drug: Player.run.drug,
    msg: document.getElementById('message').classList.contains('show') && document.querySelector('#message .card').textContent }));
  assert.equal(after.shown, false);
  assert.equal(after.drug, 'half');
  assert.match(String(after.msg), /not a JSON object/, 'the refusal was cleared before it could be read');
  assertClean(page);
});

// ── Performance: Gemini's audit of 6b52133 ───────────────────────────────
// Counts the 3D view's drawings on window.__renders.
const countRenders = page => page.evaluate(() => {
  if (!window.__renderWrapped) {
    const draw = View.renderer.render.bind(View.renderer);
    View.renderer.render = (s, c) => { window.__renders++; draw(s, c); };
    window.__renderWrapped = true;
  }
  window.__renders = 0;
});
const renders = page => page.evaluate(() => window.__renders);
const pause = ms => new Promise(r => setTimeout(r, ms));

test('performance card: it measures the 2D view, and the 2D frames\' time never reaches a 3D frame', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  assert.equal(await loadRun(page, RUN), true);
  await page.keyboard.press('p');
  await page.evaluate(() => Player.play());
  // It said "measuring…" for good in 2D, the scene's loop never ending a frame there.
  await page.waitForFunction(() => /Frames\s+\d+ \/s/.test(document.getElementById('perf').innerText), { timeout: 10000 });
  const card = await page.evaluate(() => document.getElementById('perf').innerText);
  assert.match(card, /2D view/);
  // The player's time is cleared at the end of each 2D frame, not kept for the first 3D one.
  await pause(400);
  assert.ok(await page.evaluate(() => Perf.jsFrame) < 50, 'the 2D frames\' time is still accumulating');
  await page.evaluate(() => { Player.pause(); Player.setView('3d'); });
  assert.match(await page.evaluate(() => document.getElementById('perf').innerText), /measuring/, 'the card did not start again on the switch');
  await page.waitForFunction(() => /M pixels/.test(document.getElementById('perf').innerText), { timeout: 20000 });
  assertClean(page);
});

test('3D: a paused, still view is drawn again only when something on it changes', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  assert.equal(await loadRun(page, RUN), true);
  await page.evaluate(() => { Player.setView('3d'); Player.seek(1500); });
  await pause(2000);                                    // drawn, and the ripples settled
  await countRenders(page);
  await pause(1200);
  assert.equal(await renders(page), 0, 'a still, paused view was drawn again');
  // Each change to what is on screen is drawn.
  for (const [what, change] of [
    ['a vessel pointed at', () => View.setHover('c1')],
    ['the camera turned', () => View.orbit(0.2)],
    ['the lens changed', () => View.fov(-5)],
    ['a new time', () => Player.seek(2500)],
    ['another look', () => View.setLook(View.lookName)],
  ]) {
    await countRenders(page);
    await page.evaluate(change);
    await page.waitForFunction(() => window.__renders > 0, { timeout: 10000 }).catch(() => {});
    assert.ok(await renders(page) > 0, what + ' was not drawn');
  }
  await countRenders(page);
  await page.setViewport({ width: 1000, height: 700 });
  await page.waitForFunction(() => window.__renders > 0, { timeout: 10000 }).catch(() => {});
  assert.ok(await renders(page) > 0, 'a resize was not drawn');
  // Playing, it is drawn every frame.
  await countRenders(page);
  await page.evaluate(() => Player.play());
  await page.waitForFunction(() => window.__renders >= 3, { timeout: 20000 }).catch(() => {});
  assert.ok(await renders(page) >= 3, 'not drawn while playing');
  await page.evaluate(() => Player.pause());
  assertClean(page);
});

test('2D frames leave the hidden 3D scene alone; the switch draws the moment shown; a still surface is reshaped only when it changes', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  assert.equal(await loadRun(page, RUN), true);
  const seen = await page.evaluate(() => {
    let n = 0; const draw = View.draw.bind(View);
    View.draw = (st, ui) => { n++; draw(st, ui); };
    for (const T of [100, 900, 1800, 2700]) Player.seek(T);
    const in2d = n;
    Player.setView('3d');
    const st = Player.stateAt(Player.T);
    return { in2d, on3d: n - in2d, h: View.vessels[1].h, want: st.cp * Player.ui.concScale };
  });
  assert.equal(seen.in2d, 0, 'the hidden 3D scene was drawn in 2D');
  assert.equal(seen.on3d, 1);
  assert.ok(Math.abs(seen.h - seen.want) < 1e-9, 'V1 does not show the moment shown');
  await pause(1500);                                    // V1's surface shaped by the scene's frames, ripples settled
  const surf = await page.evaluate(() => [1, 2, 3].map(k => {
    const v = View.vessels[k], g = v.surface.geometry;
    if (!v.group.visible) return null;
    const pos = Array.from(g.attributes.position.array), nor = Array.from(g.attributes.normal.array);
    const again = v.shapeSurface();                     // unchanged: left alone
    v.stillR = null; v.shapeSurface();                  // shaped from scratch
    return { again, same: pos.every((x, i) => x === g.attributes.position.array[i]) &&
                          nor.every((x, i) => x === g.attributes.normal.array[i]) };
  }));
  for (const [k, s] of surf.entries()) if (s) {
    assert.equal(s.again, false, `V${k + 1}'s still surface was reshaped`);
    assert.equal(s.same, true, `V${k + 1}'s kept surface is not the one its state gives`);
  }
  assertClean(page);
});

// ── Small windows: ChatGPT's audit of 6b52133 ────────────────────────────
// The cards that show, their boxes, and the controls' top.
const shownCards = page => page.evaluate(() => {
  const out = {};
  for (const id of ['readout', 'amounts']) {
    const e = document.getElementById(id);
    if (getComputedStyle(e).display === 'none' || e.offsetHeight === 0) continue;
    const b = e.getBoundingClientRect(); out[id] = { left: b.left, right: b.right, top: b.top, bottom: b.bottom, h: b.height };
  }
  const C = typeof Cards === 'undefined' ? {} : Cards;     // none before 6b52133's fix: the boxes still tell
  return { cards: out, ctlTop: document.getElementById('controls').getBoundingClientRect().top, level: C.level, canSwap: C.canSwap };
});

test('small windows: in both views the cards never overlap, start above the window or bury the picture', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  assert.equal(await loadRun(page, RUN), true);
  for (const view of ['2d', '3d']) {
    await page.evaluate(v => Player.setView(v), view);
    for (const [w, h] of [[360, 260], [800, 400], [800, 560], [500, 900], [800, 600], [1200, 800]]) {
      await page.setViewport({ width: w, height: h }); await pause(150);
      await page.evaluate(() => Player.seek(1500));
      const s = await shownCards(page), at = `${view} ${w}x${h}`;
      const cs = Object.values(s.cards);
      for (const c of cs) assert.ok(c.top >= 0 && c.bottom <= s.ctlTop, `${at}: a card is outside the window or over the controls`);
      if (cs.length === 2) assert.ok(cs[0].right <= cs[1].left || cs[1].right <= cs[0].left, `${at}: the cards overlap`);
      if (view === '3d') {
        const hMax = Math.max(0, ...cs.map(c => c.h)), bottom = Math.max(0, ...cs.map(c => c.bottom));
        assert.ok(hMax <= 0.5 * bottom + 0.5, `${at}: the cards cover ${hMax} of the ${bottom} px above them`);
      } else assert.ok(clearOfCards(await underCards(page)), `${at}: the drawing runs under the cards`);
      if (w === 1200) assert.equal(cs.length, 2, `${at}: both cards should show`);
    }
  }
  assertClean(page);
});

test('small windows: one card where both do not fit, a swaps it, and where none fits a says so', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  assert.equal(await loadRun(page, RUN), true);
  // A window where one card fits and either could: ChatGPT's 800 x 600 lost both.
  let found = null;
  for (const [w, h] of [[800, 560], [800, 520], [500, 900], [420, 700], [800, 600]]) {
    await page.setViewport({ width: w, height: h }); await pause(150);
    await page.evaluate(() => Player.seek(1500));
    const s = await shownCards(page);
    if (s.level === 1 && s.canSwap) { found = [w, h, s]; break; }
  }
  assert.ok(found, 'no window size gave one card that a can swap');
  assert.deepEqual(Object.keys(found[2].cards), ['readout'], 'the readout is the card kept');
  await page.keyboard.press('a');
  assert.deepEqual(Object.keys((await shownCards(page)).cards), ['amounts'], 'a did not swap Amounts in');
  await page.evaluate(() => Player.seek(1600));
  assert.deepEqual(Object.keys((await shownCards(page)).cards), ['amounts'], 'the swap did not last to the next frame');
  await page.keyboard.press('a');
  assert.deepEqual(Object.keys((await shownCards(page)).cards), ['readout'], 'a did not swap the readout back');
  // No card fits: a says so, and the note goes.
  await page.setViewport({ width: 360, height: 260 }); await pause(150);
  await page.evaluate(() => Player.seek(1500));
  assert.equal((await shownCards(page)).level, 2);
  await page.keyboard.press('a');
  const note = await page.evaluate(() => ({ shown: document.getElementById('message').classList.contains('show'),
                                            text: document.querySelector('#message .card').textContent }));
  assert.ok(note.shown && /No room for the cards/.test(note.text), 'a did nothing visible where no card fits');
  await page.waitForFunction(() => !document.getElementById('message').classList.contains('show'), { timeout: 5000 });
  assertClean(page);
});

test('video: at 360 x 260 the dialog and both its buttons are inside the window', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 360, height: 260 });
  assert.equal(await loadRun(page, RUN), true);
  await page.click('#saveVideo');
  const g = await page.evaluate(() => {
    const box = s => { const b = document.querySelector('#videoCard ' + s).getBoundingClientRect();
                       return { top: b.top, bottom: b.bottom, left: b.left, right: b.right }; };
    return { card: box('.card'), go: box('.go'), cancel: box('.cancel'), W: innerWidth, H: innerHeight };
  });
  for (const k of ['card', 'go', 'cancel'])
    assert.ok(g[k].top >= 0 && g[k].left >= 0 && g[k].bottom <= g.H && g[k].right <= g.W, `${k} is outside the window: ${JSON.stringify(g[k])}`);
  await page.click('#videoCard .cancel');                   // by the pointer, at its centre
  assert.equal(await page.evaluate(() => Video.shown()), false, 'Cancel could not be pressed');
  assertClean(page);
});

// ── Devin's stress test (2026-11-08) ──────────────────────────────────────
test('the name is Visualizer, in the page and in a video\'s caption', { skip }, async t => {
  const page = await viewerPage(t);
  assert.equal(await loadRun(page, RUN), true);
  const g = await page.evaluate(() => ({ title: document.title, brand: document.getElementById('brand').textContent,
                                         strip: videoHeading(false) }));
  assert.equal(g.title, 'Visualizer');
  assert.match(g.brand, /^Visualizer /);
  assert.match(g.strip, /stanpumpR · Visualizer · Propofol/);
  assert.doesNotMatch(JSON.stringify(g), /Minto Visualizer/);
  assertClean(page);
});

test('Open run is offered on its own, not in a pop-up stanpumpR opened', { skip }, async t => {
  const page = await viewerPage(t);
  assert.notEqual(await page.$eval('#open', e => getComputedStyle(e).display), 'none', 'hidden when opened on its own');
  const popup = await browser.newPage();
  t.after(() => popup.close());
  await popup.goto('file:///' + VIEWER.replace(/\\/g, '/') + '?t=12345', { waitUntil: 'load' });
  await popup.waitForFunction('typeof Player !== "undefined"');
  assert.equal(await popup.$eval('#open', e => getComputedStyle(e).display), 'none', 'offered in stanpumpR\'s pop-up');
});

test('round-off is shown as 0, not as 4.02e-16', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  const r = clone(RUN);
  r.c2[0] = 4.02e-16; r.c3[0] = 3.5e-18;          // what remifentanil's V2 and V3 held at time 0
  assert.equal(await loadRun(page, r), true);
  const g = await page.evaluate(() => {
    Player.seek(Player.run.t[0]);
    const st = Player.stateAt(Player.run.t[0]);
    return { c2: st.c2, A2: st.A2, texts: [...document.querySelectorAll('#flat svg text')].map(e => e.textContent).join(' | ') };
  });
  assert.equal(g.c2, 0);
  assert.equal(g.A2, 0);
  assert.doesNotMatch(g.texts, /e-1[0-9]/, 'a label shows round-off: ' + g.texts);
  // A genuinely small value is still shown (a billionth of the peak is the line).
  const s = clone(RUN); const peak = Math.max(...s.cp); s.c2[0] = peak * 1e-6;
  assert.equal(await loadRun(page, s), true);
  assert.ok(await page.evaluate(() => Player.stateAt(Player.run.t[0]).c2) > 0);
  assertClean(page);
});

// ── The inhaled anesthetics (Shafer, 2026-11-09): a gas run ("kind": "gas",
// tools/viewer/src/gas.js) in the 2D view only.
const GAS = JSON.parse(readFileSync(path.resolve(HERE, '..', 'examples', 'sevoflurane-washin.json'), 'utf8'));
// Desflurane fills muscle far higher than sevoflurane, whose tall liquid once
// hid the head of the arrow into it from every side (Shafer, 2026-11-10).
const DES = JSON.parse(readFileSync(path.resolve(HERE, '..', 'examples', 'desflurane-emergence.json'), 'utf8'));

test('gas: a gas run draws the circuit, lungs, VRG, muscle and fat, and the agent adds up', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  assert.equal(await loadRun(page, GAS), true);
  for (const T of [60, 600, 1200, 1800]) {
    const o = await page.evaluate(T => {
      Player.seek(T);
      const texts = [...document.querySelectorAll('#flat svg text')].map(e => e.textContent);
      const num = s => Number(s.replace(/,/g, ''));
      const cells = [...document.querySelectorAll('#amounts td')].map(e => e.textContent);
      const at = k => num(cells[cells.indexOf(k) + 1]);
      return { texts, readout: document.getElementById('readout').innerText,
               delivered: at('Delivered (fresh gas)'), patient: at('In the patient'), vented: at('Vented'),
               parts: ['Lungs', 'VRG', 'Muscle', 'Fat'].map(at), st: Player.stateAt(T) };
    }, T);
    for (const name of ['Circuit', 'Lungs', 'VRG', 'Muscle', 'Fat', 'end-tidal', 'effect site'])
      assert.ok(o.texts.includes(name), `T = ${T}: no "${name}" in the drawing`);
    assert.match(o.readout, /Sevoflurane/);
    assert.match(o.readout, /End-tidal/);
    assert.match(o.readout, /VRG \(effect site\)/);
    // The card adds up, to its rounding: delivered = in the patient + vented.
    assert.ok(Math.abs(o.delivered - o.patient - o.vented) <= 0.15, `T = ${T}: ${o.delivered} != ${o.patient} + ${o.vented}`);
    assert.ok(Math.abs(o.parts.reduce((a, b) => a + b, 0) - o.patient) <= 0.25, `T = ${T}: the parts do not add to the patient`);
    // And so do the flows: what the blood takes from the lungs is what the tissues take.
    const f = o.st.flow;
    assert.ok(Math.abs(f.blood - (f.vrg + f.mg + f.fg)) < 1e-9);
  }
  assertClean(page);
});

test('gas: the 3D view draws the five glasses, and a drug run afterwards has its own back', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  assert.equal(await loadRun(page, RUN), true);
  await page.evaluate(() => Player.setView('3d'));
  assert.equal(await loadRun(page, GAS), true);
  const shown = () => page.evaluate(() => {
    const vis = o => { for (let q = o; q; q = q.parent) if (!q.visible) return false; return true; };
    return {
      view: Player.view, toggle: getComputedStyle(document.getElementById('viewToggle')).display,
      gas: View.gasVessels ? Object.keys(View.gasVessels).filter(k => vis(View.gasVessels[k].group)) : [],
      drug: [1, 2, 3].filter(k => vis(View.vessels[k].group)),
      labels: [...document.querySelectorAll('#view .label-layer .lbl')].filter(e => e.style.display !== 'none').map(e => e.textContent)
    };
  });
  await page.evaluate(() => Player.seek(600));
  await new Promise(r => setTimeout(r, 200));
  let o = await shown();
  assert.equal(o.view, '3d', 'a gas run loaded in 3D stays in 3D');
  assert.notEqual(o.toggle, 'none');
  assert.deepEqual(o.gas, ['circuit', 'lungs', 'vrg', 'mg', 'fg']);
  assert.deepEqual(o.drug, [], 'no drug vessel with a gas run');
  for (const name of ['Circuit', 'Lungs', 'VRG', 'Muscle', 'Fat'])
    assert.ok(o.labels.some(l => l.startsWith(name)), `no label for ${name}: ${o.labels}`);
  assert.ok(o.labels.some(l => /effect site/.test(l)) && o.labels.some(l => /end-tidal/.test(l)));
  // No two glasses overlap, and each stands clear of the lungs by the arrows' length.
  const geo = await page.evaluate(() => Object.fromEntries(Object.entries(View.gasGeo.c).map(([k, c]) => [k, [c.x, c.z, View.gasGeo.r[k]]])));
  const keys = Object.keys(geo);
  for (let i = 0; i < keys.length; i++) for (let j = i + 1; j < keys.length; j++) {
    const [a, b] = [geo[keys[i]], geo[keys[j]]];
    assert.ok(Math.hypot(a[0] - b[0], a[1] - b[1]) > a[2] + b[2], `${keys[i]} and ${keys[j]} overlap`);
  }
  // Back to a drug: its own vessels, and no glass of the gas run.
  assert.equal(await loadRun(page, RUN), true);
  await page.evaluate(() => Player.seek(600));
  o = await shown();
  assert.equal(o.view, '3d');
  assert.deepEqual(o.gas, []);
  assert.deepEqual(o.drug, [1, 2, 3]);
  assert.ok(!o.labels.some(l => /^Lungs/.test(l)), 'a gas label left over');
  assertClean(page);
});

test('gas: the 3D view starts where no glass, arrow or label is hidden, and labels never overlap as it turns', { skip }, async t => {
  for (const [run, name, T] of [[GAS, 'sevoflurane', 1200], [DES, 'desflurane', 7500]]) for (const [width, height] of [[1200, 800], [800, 600]]) {
    const page = await viewerPage(t);
    await page.setViewport({ width, height });
    assert.equal(await loadRun(page, run), true);
    await page.evaluate(T => { Player.setView('3d'); View.resetView(); Player.seek(T); }, T);
    const best = await page.evaluate(() => View.gasHomeScores.reduce((a, b) => (b.score < a.score ? b : a)));
    assert.equal(best.hidden, 0, `${name}, ${width} x ${height}: a glass is hidden from the starting view`);
    assert.equal(best.lost, 0, `${name}, ${width} x ${height}: an arrow is hidden from the starting view: ${best.why}`);
    // What the page shows: from the start, and from the drug view's start,
    // where the labels of these glasses fall on one another.
    const overlaps = () => page.evaluate(async () => {
      await new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r)));
      const b = [...document.querySelectorAll('#view .label-layer .lbl')].filter(e => e.style.display !== 'none' && e.offsetWidth > 0)
        .map(e => ({ s: e.textContent, r: e.getBoundingClientRect() }));
      const hits = [];
      for (let i = 0; i < b.length; i++) for (let j = i + 1; j < b.length; j++) {
        const p = b[i].r, q = b[j].r;
        if (p.left < q.right - 0.5 && q.left < p.right - 0.5 && p.top < q.bottom - 0.5 && q.top < p.bottom - 0.5) hits.push(b[i].s + ' / ' + b[j].s);
      }
      return { n: b.length, hits };
    });
    let o = await overlaps();
    assert.equal(o.n, 5);
    assert.deepEqual(o.hits, [], `${name}, ${width} x ${height}: labels overlap from the starting view`);
    await page.evaluate(() => { View.persp.position.set(0, 28, 50); View.controls.target.set(0, 3, 0); View.persp.lookAt(View.controls.target); View.invalidate(); });
    o = await overlaps();
    assert.deepEqual(o.hits, [], `${name}, ${width} x ${height}: labels overlap once the scene is turned`);
    assertClean(page);
  }
});

test('gas: in 3D the cards over a glass and an arrow give the numbers', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  assert.equal(await loadRun(page, GAS), true);
  await page.evaluate(() => { Player.setView('3d'); Player.seek(1200); });
  const tip = key => page.evaluate(key => { View.showTip(key, { clientX: 300, clientY: 300 }); return document.querySelector('.flow-tip').innerText; }, key);
  const st = await page.evaluate(() => Player.stateAt(1200));
  assert.match(await tip('gv:lungs'), /Lungs \(end-tidal\)/);
  assert.match(await tip('gv:vrg'), /Vessel-rich group \(effect site\)/);
  assert.match(await tip('gv:circuit'), /holds no agent/);
  const fresh = await tip('ga:fresh');
  assert.match(fresh, /Fresh gas, 1\.0 L\/min/);
  assert.match(fresh, /Delivered/);
  assert.match(await tip('ga:vented'), /pop-off/);
  // After the flow is cut the brain gives agent back: its card says so.
  assert.ok(st.flow.vrg < 0, 'the VRG is giving agent back at 20 minutes');
  assert.match(await tip('ga:vrg'), /VRG → blood → lungs/);
  assertClean(page);
});

test('gas: no two labels overlap, at 1200 x 800 and 800 x 600', { skip }, async t => {
  for (const [width, height] of [[1200, 800], [800, 600]]) {
    const page = await viewerPage(t);
    await page.setViewport({ width, height });
    assert.equal(await loadRun(page, GAS), true);
    for (const T of [0, 30, 120, 600, 660, 1200, 1800]) {
      const hits = await page.evaluate(T => {
        Player.seek(T);
        const bb = [...document.querySelectorAll('#flat svg text')].map(e => ({ s: e.textContent, b: e.getBBox() }));
        const out = [], SLACK_Y = 4;     // as in the drug test above
        for (let i = 0; i < bb.length; i++) for (let j = i + 1; j < bb.length; j++) {
          const a = bb[i].b, b = bb[j].b;
          if (a.x < b.x + b.width - 0.5 && b.x < a.x + a.width - 0.5 && a.y < b.y + b.height - SLACK_Y && b.y < a.y + a.height - SLACK_Y)
            out.push(bb[i].s + ' / ' + bb[j].s);
        }
        return out;
      }, T);
      assert.deepEqual(hits, [], `${width} x ${height}, T = ${T}: labels overlap`);
    }
    assertClean(page);
  }
});

test('gas: markup and malformed fields are refused, the name is shown as text', { skip }, async t => {
  const page = await viewerPage(t);
  for (const [mutate, names] of [[r => { r.units.conc = '%' + P; }, 'units.conc'],
                                 [r => { r.capacity.vrg = 0; }, 'capacity.vrg'],
                                 [r => { r.p.fg = r.p.fg.slice(1); }, 'p.fg'],
                                 [r => { r.fresh[3] = 'x'; }, 'fresh[3]'],
                                 [r => { r.kind = 'liquid'; }, 'kind']]) {
    const r = clone(GAS); mutate(r);
    assert.equal(await loadRun(page, r), false);
    const p = await probe(page);
    assertNoInjection(p);
    assert.ok(p.message.includes(names), `message should name ${names}: ${p.message}`);
  }
  const r = clone(GAS); r.drug = P;
  assert.equal(await loadRun(page, r), true);
  assertNoInjection(await probe(page));
  assertClean(page);
});

test('video: a gas run saves an MP4 in each view', { skip }, async t => {
  const page = await viewerPage(t);
  await page.setViewport({ width: 1200, height: 800 });
  assert.equal(await loadRun(page, GAS), true);
  if (!(await canEncode(page))) return t.skip('this browser cannot encode H.264');
  for (const view of ['2d', '3d']) {
    const { plan, b64 } = await recordVideo(page, view, false);
    assert.equal(plan.view, view);
    const top = mp4Boxes(Buffer.from(b64, 'base64'));
    assert.deepEqual(top.map(b => b.type), ['ftyp', 'moov', 'mdat'], view);
  }
  assertClean(page);
});
