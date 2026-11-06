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
  const big = await page.evaluate(() => {
    const cards = Math.min(...['readout', 'amounts'].map(id => document.getElementById(id))
      .filter(e => e.offsetHeight > 0).map(e => e.getBoundingClientRect().top));
    let low = 0;
    for (const el of document.querySelectorAll('#flat svg rect, #flat svg polygon, #flat svg text'))
      { const b = el.getBoundingClientRect(); if (b.height > 0) low = Math.max(low, b.bottom); }
    return { cards, low };
  });
  assert.ok(big.low <= big.cards, `the drawing reaches ${big.low}, the cards start at ${big.cards}`);
  assertClean(page);
});
