// ═══════════════════════════════════════════════════════════════
//  stanpumpR viewer — the player
//
//  Author:    Dr Charles Minto
//  Assisted by: Claude.ai (Anthropic)
//
//  Copyright (c) 2026 Charles Minto and Thomas Schnider
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.
//
//  Plays one drug's run (a "stanpumpR-viewer" run file, version 1 or 2) in the
//  scene: the vessels, arrows and ribbon at the run's concentrations and
//  rates, at any time, by play or the slider. Nothing here computes
//  pharmacokinetics: between the run's times the concentrations are
//  interpolated linearly, and the rate, target and mode are those in
//  force (the value at the time before).
//
//  A run arrives one of three ways:
//    - from the page that opened this one (window.open), by postMessage:
//      { type: 'stanpumpR-viewer-run', run }. On loading, this page tells
//      its opener it is ready: { type: 'stanpumpR-viewer-ready' }. If
//      stanpumpR cannot make the run it sends the reason instead:
//      { type: 'stanpumpR-viewer-error', message }. A message is taken
//      only from the opener or from this page's origin.
//    - Open run… (a .json file), or a file dropped on the page.
//    - window.STANPUMPR_VIEWER_RUN, if a page that embeds this one sets it.
//
//  Display rules, from the run's typical concentration (the drug's
//  clinical level; when the run gives none, half the run's highest Cp):
//    - heights: the clinical level stands 3 units tall (concScale = 3 /
//      level), so a clinical concentration looks the same for every drug;
//    - decimals: three significant figures at the clinical level;
//    - arrows: drawn as for a syringe of 10/3 x the level per mL (propofol,
//      level 3 mcg/mL: 10 mg/mL), so equipotent rates draw equal arrows;
//    - colour: propofol white, opioids and neuromuscular blockers a light
//      tint of their syringe-label colour, any other drug a pale blue;
//      the run's "color" field, if given, over all of these.
//
//  Flows (version 2, Claude Code for stanpumpR, 2026-11-05): every arrow is
//  read from the amounts as they change, not from clearances, so it holds
//  for any model stanpumpR runs, saturable ones included: the net flow into
//  V2 is V2 x dC2/dt, absorption is d(absorbed)/dt, and elimination is
//  d(eliminated)/dt, where eliminated is the drug given (the run's "given",
//  from time 0) less the drug in V1 + V2 + V3. Over each interval the rate
//  is the interval's own (the series are linear between the times). A run
//  whose input stanpumpR cannot account for ("inputKnown": false) draws
//  elimination as CL1 x Cp and leaves administered and eliminated blank.
// ═══════════════════════════════════════════════════════════════

// Play speeds, seconds of the run per second of play: + and - step through
// them. The first play is at PLAYER_SPEED.
const SPEEDS = [1, 2, 5, 10, 30, 60, 120, 300, 600, 1200, 1800, 3600, 7200, 21600, 43200, 86400];
const PLAYER_SPEED = 1;                  // real time, before a run
const PLAY_SECONDS = 120;                // a run first plays in at most this long
function speedLabel(v) {
    return v === 1 ? 'Real time' : v < 60 ? `1 s = ${v} s` : v < 3600 ? `1 s = ${v / 60} min`
        : v < 86400 ? `1 s = ${v / 3600} hr` : '1 s = 1 day';
}
const BOLUS_SHOW_S = 1;                  // a bolus's label stays this long (seconds of play)
const DRUG_TINT = { opioid: '#bfe0f5', relaxant: '#f5c4c0', propofol: '#f3efe6', other: '#c7d2fe' };
const MODES = ['manual', 'plasma', 'effect'];

// What is wrong with a run, as one line, or null if the viewer can play it.
// Anything from the file that is shown in the page is a checked number or
// plain text (no <>&"'): interpolation into the cards is then safe.
function runProblem(r) {
    if (!r || typeof r !== 'object') return 'The run is not a JSON object.';
    if (r.format !== 'stanpumpR-viewer') return 'This is not a stanpumpR viewer run (format is not "stanpumpR-viewer").';
    if (r.version !== 1 && r.version !== 2) return `This viewer reads version 1 and 2 runs; this one says version ${typeof r.version === 'number' ? r.version : 'not a number'}.`;
    const n = Array.isArray(r.t) ? r.t.length : 0;
    if (n < 2) return 't must be an array of at least two times.';
    for (let i = 0; i < n; i++) {
        if (!Number.isFinite(r.t[i])) return `t[${i}] is not a number.`;
        if (i && r.t[i] <= r.t[i - 1]) return `t must increase: t[${i}] is not after t[${i - 1}].`;
    }
    const series = (k, required) => {
        const v = r[k];
        if (v == null) return required ? `${k} is missing.` : null;
        if (!Array.isArray(v) || v.length !== n) return `${k} must be an array as long as t (${n}).`;
        for (let i = 0; i < n; i++) if (!Number.isFinite(v[i])) return `${k}[${i}] is not a number.`;
        return null;
    };
    for (const [k, req] of [['cp', true], ['rate', true], ['ce', false], ['c2', false], ['c3', false], ['target', false],
                            ['given', false], ['absorbed', false], ['depot', false]]) {
        const p = series(k, req); if (p) return p;
    }
    if (r.c2 == null && r.c3 != null) return 'c3 is given without c2.';
    if (r.inputKnown != null && typeof r.inputKnown !== 'boolean') return 'inputKnown must be true or false.';
    if (r.eliminationLinear != null && typeof r.eliminationLinear !== 'boolean') return 'eliminationLinear must be true or false.';
    if (r.amountScale != null && !(Number.isFinite(r.amountScale) && r.amountScale > 0)) return 'amountScale must be a number above 0.';
    if (r.mode != null) {
        if (!Array.isArray(r.mode) || r.mode.length !== n) return `mode must be an array as long as t (${n}).`;
        const bad = r.mode.findIndex(m => !MODES.includes(m));
        if (bad >= 0) return `mode[${bad}] must be one of ${MODES.join(', ')}.`;
    }
    const pk = r.pk || {};
    const vols = ['v1'].concat(r.c2 != null ? ['v2'] : [], r.c3 != null ? ['v3'] : []);
    for (const k of vols) if (!(Number.isFinite(pk[k]) && pk[k] > 0)) return `pk.${k} must be a volume above 0 (L).`;
    for (const k of ['cl1'].concat(r.c2 != null ? ['cl2'] : [], r.c3 != null ? ['cl3'] : []))
        if (!(Number.isFinite(pk[k]) && pk[k] >= 0)) return `pk.${k} must be a clearance of 0 or more (L/min).`;
    if (r.typical != null && !(Number.isFinite(r.typical) && r.typical >= 0)) return 'typical must be a number of 0 or more.';
    const u = r.units || {};
    for (const k of ['conc', 'amount', 'rate'])
        if (typeof u[k] !== 'string' || !/^[^<>&"']{1,40}$/.test(u[k]))
            return `units.${k} must be short plain text (a unit like "mcg/mL").`;
    if (r.source != null && (typeof r.source !== 'string' || !/^[^<>&"']{0,80}$/.test(r.source)))
        return 'source must be short plain text.';
    const pt = r.patient || {};
    for (const k of ['weight', 'height', 'age'])
        if (pt[k] != null && !Number.isFinite(pt[k])) return `patient.${k} must be a number.`;
    if (pt.sex != null && (typeof pt.sex !== 'string' || !/^[^<>&"']{0,40}$/.test(pt.sex))) return 'patient.sex must be short plain text.';
    if (r.boluses != null) {
        if (!Array.isArray(r.boluses)) return 'boluses must be an array.';
        for (const [i, b] of r.boluses.entries())
            if (!b || !Number.isFinite(b.t) || !Number.isFinite(b.amount)) return `boluses[${i}] needs a time t and an amount.`;
    }
    return null;
}

// The drug's liquid colour, from the run.
function runColour(r) {
    if (typeof r.color === 'string' && /^#[0-9a-f]{6}$/i.test(r.color)) return r.color;
    if (String(r.drug).toLowerCase() === 'propofol') return DRUG_TINT.propofol;
    if (r.category === 'Opioids') return DRUG_TINT.opioid;
    if (r.category === 'Neuromuscular blockade') return DRUG_TINT.relaxant;
    return DRUG_TINT.other;
}

// The model as the readout shows it: the reference up to the first full
// stop after its year (author ..., journal year;vol:pages.), at most 70
// characters; the whole is the readout's tooltip.
function shortModel(s) {
    s = String(s || '').trim();
    const m = s.match(/^.*?\d{4}[^.]*\./);
    let short = m ? m[0] : s;
    if (short.length > 70) short = short.slice(0, 69).replace(/\s+\S*$/, '') + '…';
    return short;
}
// The run's units as the viewer writes them: µg for stanpumpR's mcg.
function microUnits(u) {
    const mu = s => s.replace(/mcg/g, 'µg');
    return { conc: mu(u.conc), amount: mu(u.amount), rate: mu(u.rate) };
}
const escapeHtml = s => String(s).replace(/[&<>"]/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' })[c]);

function hms(sec) {
    const s = Math.round(sec), h = Math.floor(s / 3600), m = Math.floor(s / 60) % 60;
    return `${h}:${String(m).padStart(2, '0')}:${String(s % 60).padStart(2, '0')}`;
}

const Player = {
    run: null, ui: null, T: 0, playing: false, last: null, speed: PLAYER_SPEED,
    // The view shown: '2d' (flat.js, the first, Shafer 2026-11-06) or '3d' (the scene).
    view: '2d',
    $: id => document.getElementById(id),

    init() {
        this.$('play').addEventListener('click', () => this.play());
        this.$('pause').addEventListener('click', () => this.pause());
        this.$('stop').addEventListener('click', () => this.stop());
        this.$('close').addEventListener('click', () => this.close());
        this.$('viewToggle').addEventListener('click', () => this.setView(this.view === '2d' ? '3d' : '2d'));
        this.setView(this.view);
        const sp = this.$('speedSlider');
        sp.max = SPEEDS.length - 1;
        sp.addEventListener('input', () => this.setSpeed(SPEEDS[+sp.value]));
        this.$('t').addEventListener('input', () => { this.pause(); this.seek(+this.$('t').value); View.kick(); });
        this.$('open').addEventListener('click', () => this.$('file').click());
        this.$('file').addEventListener('change', e => { const f = e.target.files[0]; if (f) this.readFile(f); e.target.value = ''; });
        // A run file dropped anywhere on the page.
        window.addEventListener('dragover', e => { e.preventDefault(); document.body.classList.add('over'); });
        window.addEventListener('dragleave', () => document.body.classList.remove('over'));
        window.addEventListener('drop', e => {
            e.preventDefault(); document.body.classList.remove('over');
            const f = e.dataTransfer.files[0]; if (f) this.readFile(f);
        });
        // A run from the page that opened this one.
        window.addEventListener('message', e => {
            // Only stanpumpR's own page, on this origin, may hand this page a
            // run: a page elsewhere that opened it is an opener too.
            if (e.origin !== location.origin) return;
            const d = e.data;
            if (d && d.type === 'stanpumpR-viewer-run') this.load(d.run);
            else if (d && d.type === 'stanpumpR-viewer-error')
                this.message(`stanpumpR could not make this run. ${escapeHtml(String(d.message))}`, true);
        });
        this.setSpeed(PLAYER_SPEED);
        requestAnimationFrame(ts => this.frame(ts));
        this.empty();
        if (window.STANPUMPR_VIEWER_RUN) this.load(window.STANPUMPR_VIEWER_RUN);
        else if (window.opener) this.message('<b>Loading data</b> — this may take 30–45 seconds.');
        else this.message('Waiting for a run from stanpumpR. You can also <b>Open run…</b> or drop a run file (.json) here.');
        // Ready, with the token in this page's address, to this origin only.
        if (window.opener) {
            const token = new URLSearchParams(location.search).get('t');
            try { window.opener.postMessage({ type: 'stanpumpR-viewer-ready', token }, location.origin); } catch (err) { /* opener elsewhere */ }
        }
    },

    // Before any run: the floor and its logo only, from the starting view.
    // The vessels, arrows and ribbon have no sizes until a run gives them.
    empty() {
        for (let k = 1; k <= 3; k++) View.vessels[k].setVisible(false);
        for (const a of Object.values(View.arrows)) a.group.visible = false;
        View.ribbon.setVisible(false);
        View.bolusLabel.show(false);
        View.resetView();
    },

    readFile(f) {
        f.text().then(s => {
            let r; try { r = JSON.parse(s); } catch (err) { this.message(`${escapeHtml(f.name)} is not JSON: ${escapeHtml(err.message)}`, true); return; }
            this.load(r);
        });
    },

    message(html, bad) {
        const m = this.$('message');
        m.querySelector('.card').innerHTML = html;
        m.querySelector('.card').classList.toggle('bad', !!bad);
        m.classList.toggle('show', !!html);
    },

    load(r) {
        // Saving a video: the run being filmed stays until the save ends
        // (CodeRabbit, #688); the new one is loaded then (Video.close()).
        // A run that cannot be shown is refused at once, not queued: its
        // message waits under the video card and shows when the card closes.
        const p = runProblem(r);
        if (typeof Video !== 'undefined' && Video.busy) {
            if (p) { this.message(`This run cannot be shown. ${p}`, true); return false; }
            Video.pending = r; return true;
        }
        if (p) { this.message(`This run cannot be shown. ${p}`, true); return false; }
        this.message('');
        this.pause();
        this.run = r;
        this.units = microUnits(r.units);
        // Concentration x volume (L) x scale is the amount in units.amount.
        this.scale = r.amountScale != null ? r.amountScale : 1;
        this.balance(r);
        // A loop, not Math.max(...r.cp), which overflows the stack on a long run.
        let maxCp = 0;
        for (const x of r.cp) if (x > maxCp) maxCp = x;
        const level = r.typical > 0 ? r.typical : (maxCp > 0 ? maxCp / 2 : 1);
        // toFixed() takes 0 to 100 digits: a vanishing level would ask for more.
        const mag = Math.max(-6, Math.floor(Math.log10(level)));
        this.ui = {
            showCe: r.ce != null, concScale: 3 / level, units: this.units,
            drugRgb: hexRgb(runColour(r)), concDecimals: Math.max(0, 2 - mag), conc: 10 / 3 * level * this.scale
        };
        this.n = 1 + (r.c2 != null) + (r.c3 != null);
        // Sizes compressed to SIZE_RATIO_MAX (flat.js): the readout says so in 3D.
        this.ui.notToScale = displayVolumes([r.pk.v1, r.c2 != null ? r.pk.v2 : 0, r.c3 != null ? r.pk.v3 : 0]).a < 1;
        View.resetGlass();
        View.vessels[1].setVisible(true);       // hidden before the first run (empty())
        View.setShape({ n: this.n, vc: r.pk.v1, v2: r.pk.v2, v3: r.pk.v3 });
        Flat.setRun(r, this.ui, T => this.stateAt(T));
        const slider = this.$('t');
        slider.min = r.t[0]; slider.max = r.t[r.t.length - 1]; slider.step = 'any';
        this.$('end').textContent = hms(r.t[r.t.length - 1]);
        // The first speed plays the whole run in at most PLAY_SECONDS.
        const span = r.t[r.t.length - 1] - r.t[0];
        this.setSpeed(SPEEDS.find(v => span / v <= PLAY_SECONDS) || SPEEDS[SPEEDS.length - 1]);
        this.seek(r.t[0]);
        View.resetView();
        // The video card, if open, describes the run now shown (CodeRabbit, #688).
        if (typeof Video !== 'undefined' && Video.card && Video.shown() && !Video.busy) Video.describe();
        return true;
    },

    // The amount eliminated at each time, as the mass balance gives it:
    // drug administered (the rate in force over each interval, and the
    // boluses up to that time, which the run's Cp already holds) less
    // the drug in V1, V2 and V3.
    //   A version 2 run gives the drug given from time 0 ("given", which
    //   counts what went in before a run that starts at Min time, and the
    //   drug absorbed from a depot); a version 1 run is summed here from its
    //   rate and boluses. With "inputKnown" false there is no balance.
    balance(r) {
        const n = r.t.length, pk = r.pk, b = r.boluses || [];
        if (r.inputKnown === false) { this.elim = null; return; }
        this.elim = new Float64Array(n);
        let inf = 0;
        for (let i = 0; i < n; i++) {
            if (i) inf += r.rate[i - 1] * (r.t[i] - r.t[i - 1]) / 60;
            const given = r.given != null ? r.given[i] : inf + b.reduce((s, x) => s + (x.t <= r.t[i] ? x.amount : 0), 0);
            const body = this.scale * (r.cp[i] * pk.v1 + (r.c2 != null ? r.c2[i] * pk.v2 : 0) + (r.c3 != null ? r.c3[i] * pk.v3 : 0));
            this.elim[i] = Math.max(0, given - body);
        }
    },

    // The index i with t[i] <= T < t[i + 1] (the last interval at the end).
    index(T) {
        const t = this.run.t;
        let lo = 0, hi = t.length - 2;
        while (lo < hi) { const m = (lo + hi + 1) >> 1; if (t[m] <= T) lo = m; else hi = m - 1; }
        return lo;
    },

    // The scene's state at time T (seconds).
    stateAt(T) {
        const r = this.run, i = this.index(T), t = r.t;
        const a = Math.min(1, Math.max(0, (T - t[i]) / (t[i + 1] - t[i])));
        const lerp = v => v == null ? null : v[i] + (v[i + 1] - v[i]) * a;
        // The value in force: the later sample once T reaches it.
        const j = T >= t[i + 1] ? i + 1 : i;
        const cp = Math.max(0, lerp(r.cp)), ce = r.ce != null ? Math.max(0, lerp(r.ce)) : 0;
        const c2 = r.c2 != null ? Math.max(0, lerp(r.c2)) : null, c3 = r.c3 != null ? Math.max(0, lerp(r.c3)) : null;
        const rate = r.rate[j], pk = r.pk;
        const mode = r.mode ? r.mode[j] : 'manual', target = r.target ? r.target[j] : 0;
        // A bolus shows from its time for BOLUS_SHOW_S of play; st.t is the
        // bolus's own time while it shows, so its ring starts once.
        const win = BOLUS_SHOW_S * this.speed;
        const b = (r.boluses || []).filter(b => T >= b.t && T < b.t + win);
        const bolus = b.reduce((s, x) => s + x.amount, 0);
        // Eliminated, interpolated like the concentrations; administered is
        // then what is in the body plus what is eliminated, so the amounts
        // card always adds up.
        const sc = this.scale;
        const A1 = sc * cp * pk.v1, A2 = c2 != null ? sc * c2 * pk.v2 : 0, A3 = c3 != null ? sc * c3 * pk.v3 : 0;
        const elim = this.elim ? lerp(this.elim) : null;
        const depot = r.depot != null ? Math.max(0, lerp(r.depot)) : null;
        const absorbed = r.absorbed != null ? lerp(r.absorbed) : null;
        // A series' rate of change over this interval, per minute.
        const dt = (t[i + 1] - t[i]) / 60;
        const slope = v => v == null ? 0 : (v[i + 1] - v[i]) / dt;
        return {
            t: b.length ? b[0].t : T, n: this.n, cp, ce, c2, c3,
            A1, A2, A3, elim, depot, absorbed,
            total: elim == null ? null : A1 + A2 + A3 + elim + (depot || 0),
            mode, target, bolus, rate,
            flow: {
                in: rate,
                abs: Math.max(0, slope(r.absorbed)),
                to2: c2 != null ? sc * pk.v2 * slope(r.c2) : 0,
                to3: c3 != null ? sc * pk.v3 * slope(r.c3) : 0,
                // Without the drug given, elimination is CL1 x Cp only where it
                // is linear; a saturable drug's is not drawn (ChatGPT, ff7e2f0).
                out: this.elim ? Math.max(0, slope(this.elim))
                    : r.eliminationLinear === false ? 0 : sc * pk.cl1 * cp
            }
        };
    },

    seek(T) {
        const r = this.run; if (!r) return;
        this.T = Math.min(r.t[r.t.length - 1], Math.max(r.t[0], T));
        const st = this.stateAt(this.T);
        View.draw(st, this.ui);
        const slider = this.$('t');
        slider.value = this.T;
        slider.style.setProperty('--pct', `${100 * (this.T - r.t[0]) / (r.t[r.t.length - 1] - r.t[0])}%`);
        this.$('now').textContent = hms(this.T);
        this.readout(st);
        this.amounts(st);
        // After the cards are filled: the 2D layout measures them (Gemini,
        // 712e43a: drawn first, the first frame saw empty cards).
        Flat.draw(st);
    },

    // The amounts card, bottom right (key a): administered, then in the
    // body, with V1, V2 and V3 indented under it, then eliminated.
    // "–" where the run has no such compartment.
    showAmounts: true,
    toggleAmounts() {
        this.showAmounts = !this.showAmounts;
        this.$('amounts').classList.toggle('show', this.showAmounts);
        if (Flat.last) Flat.draw(Flat.last);    // the 2D view's room depends on the cards (Gemini, 712e43a)
    },
    amounts(st) {
        const a = x => x == null ? '–' : x.toFixed(1), n = st.n;
        const row = (name, x, sub) => `<tr><td class="k${sub ? ' sub' : ''}">${name}</td><td>${a(x)}</td></tr>`;
        this.$('amounts').innerHTML = `
          <div class="phead">Amounts <kbd>a</kbd></div>
          <table class="amounts">
            <tr><th></th><th>${this.units.amount}</th></tr>
            ${row(st.depot != null ? 'Administered (bioavailable)' : 'Administered', st.total)}
            ${st.depot != null ? row('In depot', st.depot) : ''}
            ${row('In body', st.A1 + st.A2 + st.A3)}
            ${row('V<sub>1</sub>', st.A1, true)}
            ${row('V<sub>2</sub>', n >= 2 ? st.A2 : null, true)}
            ${row('V<sub>3</sub>', n >= 3 ? st.A3 : null, true)}
            ${row('Eliminated', st.elim)}
          </table>${this.run.inputKnown === false ? '<div class="note">stanpumpR rewrites this drug\'s input, so administered and eliminated are not shown.</div>' : ''}`;
        this.$('amounts').classList.toggle('show', this.showAmounts);
    },

    readout(st) {
        const r = this.run, u = this.units, dp = this.ui.concDecimals;
        const c = x => x == null ? '–' : x.toFixed(dp);
        const name = String(r.drug).charAt(0).toUpperCase() + String(r.drug).slice(1);
        const mode = st.mode === 'effect' ? 'TCI-E target' : st.mode === 'plasma' ? 'TCI-P target' : 'Manual';
        const tgt = st.mode === 'manual' ? '' : ` <span class="tgt">${c(st.target)}</span> ${u.conc}`;
        const p = r.patient || {};
        const pat = [p.weight != null && `${p.weight} kg`, p.height != null && `${p.height} cm`,
                     p.age != null && `${p.age} y`, p.sex].filter(Boolean).join(' · ');
        this.$('readout').innerHTML =
            `<div><span class="drug">${escapeHtml(name)}</span>${r.model ? ` <span class="model" title="${escapeHtml(r.model)}">· ${escapeHtml(shortModel(r.model))}</span>` : ''}</div>` +
            `<div class="time">${hms(this.T)}</div>` +
            `<div><span class="k">${mode}</span>${tgt}</div>` +
            `<div><span class="k">Cp</span> ${c(st.cp)}` + (r.ce != null ? ` · <span class="k">Ce</span> ${c(st.ce)}` : '') +
            (st.c2 != null ? ` · <span class="k">C2</span> ${c(st.c2)}` : '') +
            (st.c3 != null ? ` · <span class="k">C3</span> ${c(st.c3)}` : '') + ` ${u.conc}</div>` +
            `<div><span class="k">Rate</span> ${st.rate.toFixed(2)} ${u.rate}` +
            (st.bolus > 0 ? ` · <span class="k">Bolus</span> ${st.bolus.toFixed(1)} ${u.amount}` : '') + `</div>` +
            (this.ui.notToScale ? `<div class="scale">Vessels not to scale: the largest is drawn ${SIZE_RATIO_MAX}× the smallest. Heights are concentration.</div>` : '') +
            `<div class="foot">${pat ? escapeHtml(pat) + ' · ' : ''}${escapeHtml(r.source || '')} · <kbd>h</kbd> keys</div>`;
    },

    setSpeed(v) {
        this.speed = v;
        this.$('speed').textContent = speedLabel(v);
        this.$('speedSlider').value = Math.max(0, SPEEDS.indexOf(v));
    },
    // dir: +1 faster, -1 slower, along SPEEDS.
    nextSpeed(dir) {
        const i = SPEEDS.indexOf(this.speed);
        this.setSpeed(SPEEDS[Math.min(SPEEDS.length - 1, Math.max(0, i + dir))]);
    },

    togglePlay() { if (this.playing) this.pause(); else this.play(); },
    play() {
        const r = this.run; if (!r || Video.busy) return;
        if (this.T >= r.t[r.t.length - 1]) this.seek(r.t[0]);
        this.playing = true; View.playing = true; this.last = null;
        this.buttons();
    },
    pause() {
        this.playing = false; View.playing = false;
        this.buttons();
    },
    // Show the 2D view or the 3D scene; the run, the time and the controls
    // are the same in both.
    setView(v) {
        this.view = v;
        document.getElementById('flat').style.display = v === '2d' ? 'block' : 'none';
        document.getElementById('view').style.display = v === '3d' ? 'block' : 'none';
        this.$('viewToggle').textContent = v === '2d' ? '3D view' : '2D view';
        this.$('brandView').textContent = v === '2d' ? '2D' : '3D';
        document.body.classList.toggle('flat', v === '2d');
        if (v === '3d') View.resize();
        if (this.run) this.seek(this.T);
    },
    // Stop: pause and go back to the start.
    stop() {
        this.pause();
        if (this.run) { this.seek(this.run.t[0]); View.kick(); }
    },
    buttons() {
        this.$('play').classList.toggle('on', this.playing);
        this.$('pause').classList.toggle('on', !this.playing && !!this.run && this.T > this.run.t[0]);
    },
    // The X: a window the app opened closes itself; one opened any other
    // way (a file opened by hand) cannot, and says so.
    close() {
        window.close();
        setTimeout(() => { if (!window.closed) this.message('Close this window with the browser\'s own controls.'); }, 300);
    },
    frame(ts) {
        const t0 = performance.now();
        if (this.playing && this.run) {
            if (this.last != null) {
                const end = this.run.t[this.run.t.length - 1];
                const T = this.T + Math.min(0.1, (ts - this.last) / 1000) * this.speed;
                if (T >= end) { this.seek(end); this.pause(); } else this.seek(T);
                this.buttons();
            }
            this.last = ts;
        } else this.last = null;
        Perf.addJs(performance.now() - t0);
        requestAnimationFrame(t => this.frame(t));
    }
};

// ── the camera keys: held, the view moving once a frame ──────────
// How far each moves the view per second held.
const VIEW_KEYS = {
    ArrowRight: 'orbitR', ArrowLeft: 'orbitL', ArrowUp: 'eyeUp', ArrowDown: 'eyeDown',
    PageUp: 'panUp', PageDown: 'panDown', i: 'zoomIn', I: 'zoomIn', o: 'zoomOut', O: 'zoomOut',
    '[': 'fovIn', ']': 'fovOut', End: 'flyIn', Home: 'flyOut'
};
const VIEW_MOVES = {
    orbitR:  dt => View.orbit(0.6 * dt),    // the eye flies right
    orbitL:  dt => View.orbit(-0.6 * dt),   // the eye flies left
    eyeUp:   dt => View.moveEye(12 * dt),
    eyeDown: dt => View.moveEye(-12 * dt),
    panUp:   dt => View.pan(12 * dt),
    panDown: dt => View.pan(-12 * dt),
    zoomIn:  dt => View.zoom(Math.pow(0.98, 12 * dt)),
    zoomOut: dt => View.zoom(Math.pow(1.02, 12 * dt)),
    fovIn:   dt => View.fov(-48 * dt),
    fovOut:  dt => View.fov(48 * dt),
    flyIn:   dt => View.fly('in', dt),
    flyOut:  dt => View.fly('out', dt)
};
const Keys = {
    held: new Map(),
    init() {
        const help = document.getElementById('help');
        help.addEventListener('click', e => { if (e.target === help) help.classList.remove('show'); });
        window.addEventListener('keydown', e => {
            if (e.ctrlKey || e.metaKey || e.altKey) return;
            // The video card is open: the keys wait (they would change the view
            // or speed it describes); Esc closes it unless it is saving.
            if (Video.shown()) { if (e.key === 'Escape' && !Video.busy) Video.close(); return; }
            // h (or ?) shows the keys; h, Esc or a click outside the card hides them.
            if (e.key === 'h' || e.key === 'H' || e.key === '?') { help.classList.toggle('show'); e.preventDefault(); return; }
            if (e.key === 'Escape') { help.classList.remove('show'); return; }
            // p: the performance card (perf.js); not listed under h.
            if (e.key === 'p' || e.key === 'P') { Perf.toggle(); return; }
            // a: the amounts card.
            if (e.key === 'a' || e.key === 'A') { Player.toggleAmounts(); return; }
            // v: the 2D or the 3D view.
            if (e.key === 'v' || e.key === 'V') { Player.setView(Player.view === '2d' ? '3d' : '2d'); return; }
            // + and - (= and _ on the same keys, unshifted or shifted): faster or slower play.
            if (e.key === '+' || e.key === '=') { Player.nextSpeed(1); e.preventDefault(); return; }
            if (e.key === '-' || e.key === '_') { Player.nextSpeed(-1); e.preventDefault(); return; }
            // space: play / pause (a focused button takes its own space press).
            if (e.key === ' ' && !(e.target && e.target.tagName === 'BUTTON')) { Player.togglePlay(); e.preventDefault(); return; }
            if (e.shiftKey) return;
            const m = VIEW_KEYS[e.key];
            if (!m) return;
            e.preventDefault();          // not the slider's, nor the page's scroll
            this.held.set(e.code, m);
        });
        window.addEventListener('keyup', e => {
            const was = this.held.get(e.code);
            if (was === 'flyIn' || was === 'flyOut') View.flyEnd();
            this.held.delete(e.code);
        });
        window.addEventListener('blur', () => { this.held.clear(); View.flyEnd(); });
        let last = performance.now();
        const frame = now => {
            const t0 = performance.now();
            const dt = Math.min(0.1, (now - last) / 1000);
            last = now;
            for (const m of new Set(this.held.values())) VIEW_MOVES[m](dt);
            Perf.addJs(performance.now() - t0);
            requestAnimationFrame(frame);
        };
        requestAnimationFrame(frame);
    }
};
