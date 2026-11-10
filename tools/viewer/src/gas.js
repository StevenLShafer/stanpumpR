// ═══════════════════════════════════════════════════════════════
//  stanpumpR viewer — the inhaled anesthetics, in the 2D view
//
//  Written for stanpumpR by Claude Code (Anthropic), 2026-11-09, at the
//  request of Steven L. Shafer.
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
//  A gas run ("kind": "gas", R/viewerRun.R's viewerGasRun()) is one agent's
//  partial pressure in the breathing circuit (what is inspired), the lungs
//  (end-tidal), the vessel-rich group (the effect site), the muscle group and
//  the fat group, in % of an atmosphere, with the fresh gas, its flow and the
//  ventilation in force and the agent delivered since time 0. Drawn after
//  Mapleson's hydraulic analogue of uptake: each compartment a box whose
//  WIDTH is its capacity (volume x tissue:gas partition coefficient) and
//  whose fill HEIGHT is its partial pressure, so the area is the agent it
//  holds, and agent flows from higher to lower partial pressure until the
//  levels are equal. One scale for every box, fixed for the run.
//
//  Every flow is read from the amounts as they change, as for the
//  intravenous drugs: into a tissue, its capacity times the rise in its
//  partial pressure; out of the lungs into the blood, the three together;
//  from the circuit into the lungs, that plus the lungs' own rise; out
//  through the pop-off valve, the agent delivered less the agent held.
//  Red flows into the lungs, green out of them, as into and out of V1.
//
//  The 2D view came first (Shafer, 2026-11-09: "Start with the 2D model");
//  the 3D view draws the same run in gas3d.js.
// ═══════════════════════════════════════════════════════════════

// The compartments, left to right, and what each partial pressure is.
const GAS_KEYS = ['circuit', 'lungs', 'vrg', 'mg', 'fg'];
const GAS_NAMES = { circuit: 'Circuit', lungs: 'Lungs', vrg: 'VRG', mg: 'Muscle', fg: 'Fat' };
const GAS_LONG = { circuit: 'Circuit (inspired)', lungs: 'Lungs (end-tidal)', vrg: 'Vessel-rich group (effect site)',
                   mg: 'Muscle group', fg: 'Fat group' };
const GAS_CIRCUIT_W = 22;        // px: the ideal circuit, which holds no agent
const GAS_TINT = '#d8cdf4';      // the vapour's colour; the effect site is the 2D view's blue
// px above the boxes: the fresh gas and pop-off arrows and their three
// labels, clear of the Visualizer's name in the corner (the circuit is the
// leftmost box, under it).
const GAS_TOP = 128;

// The agent's name as written: nitrousOxide is "Nitrous oxide".
function gasName(drug) {
    const s = String(drug).replace(/([a-z])([A-Z])/g, '$1 $2').toLowerCase();
    return s.charAt(0).toUpperCase() + s.slice(1);
}

// What is wrong with a gas run, as one line, or null. r.t is already checked
// (runProblem()); n is its length.
function gasRunProblem(r, n) {
    const arr = (v, k, min) => {
        if (!Array.isArray(v) || v.length !== n) return `${k} must be an array as long as t (${n}).`;
        for (let i = 0; i < n; i++)
            if (!Number.isFinite(v[i]) || (min != null && v[i] < min)) return `${k}[${i}] must be a number${min != null ? ` of ${min} or more` : ''}.`;
        return null;
    };
    if (!r.p || typeof r.p !== 'object') return 'p must hold the partial pressures (circuit, lungs, vrg, mg, fg).';
    for (const k of GAS_KEYS) { const q = arr(r.p[k], `p.${k}`); if (q) return q; }
    for (const [k, min] of [['fresh', 0], ['freshFlow', 0], ['ventilation', 0], ['delivered', 0]]) {
        const q = arr(r[k], k, min); if (q) return q;
    }
    for (const what of ['capacity', 'volume', 'partition']) {
        const o = r[what];
        if (!o || typeof o !== 'object') return `${what} must give each compartment's.`;
        for (const k of GAS_KEYS) {
            const x = o[k], zeroOk = k === 'circuit' && what !== 'partition';
            if (!(Number.isFinite(x) && (zeroOk ? x >= 0 : x > 0)))
                return `${what}.${k} must be a number ${zeroOk ? 'of 0 or more' : 'above 0'}.`;
        }
    }
    if (r.mac != null && !(Number.isFinite(r.mac) && r.mac > 0)) return 'mac must be a number above 0.';
    if (r.typical != null && !(Number.isFinite(r.typical) && r.typical >= 0)) return 'typical must be a number of 0 or more.';
    if (r.amountScale != null && !(Number.isFinite(r.amountScale) && r.amountScale > 0)) return 'amountScale must be a number above 0.';
    if (r.circuit != null && !['ideal', 'semi-closed', 'open'].includes(r.circuit)) return 'circuit must be "ideal", "semi-closed" or "open".';
    return runTextProblem(r);
}

// ── the player, for a gas run ──────────────────────────────────
Object.assign(Player, {
    // A gas run's setup, from load(): the scales, the decimals, the agent vented.
    gasSetup(r) {
        let peak = 0;
        for (const k of GAS_KEYS) for (const x of r.p[k]) if (x > peak) peak = x;
        this.tiny = peak * TINY_FRACTION;
        const level = r.typical > 0 ? r.typical : (peak > 0 ? peak / 2 : 1);
        const mag = Math.max(-6, Math.floor(Math.log10(level)));
        // The 3D view's heights (gas3d.js): 6 units for one MAC, or for the
        // run's highest partial pressure where that is lower (nitrous oxide,
        // whose MAC is above an atmosphere).
        const level3d = r.mac > 0 && peak > 0 ? Math.min(r.mac, peak) : level;
        this.ui = { gas: true, showCe: false, units: this.units, drugRgb: hexRgb(GAS_TINT),
                    ceRgb: hexRgb(FLAT_COLOURS.ce), concDecimals: Math.max(0, 2 - mag), concScale: 6 / level3d };
        // Vented: the agent delivered less the agent held, at each time.
        const n = r.t.length;
        this.vented = new Float64Array(n);
        for (let i = 0; i < n; i++) {
            let held = 0;
            for (const k of GAS_KEYS) held += r.capacity[k] * r.p[k][i];
            this.vented[i] = r.delivered[i] - this.scale * held;
        }
        // The arrows' scale, for both views: the run's 95th-percentile flow.
        this.ui.gasFlowScale = gasFlowScale(r, T => this.gasStateAt(T));
        const keys = r.capacity.circuit > 0 ? GAS_KEYS : GAS_KEYS.slice(1);
        this.ui.notToScale = displayVolumes(keys.map(k => r.capacity[k])).a < 1;
    },

    gasStateAt(T) {
        const r = this.run, i = this.index(T), t = r.t;
        const a = Math.min(1, Math.max(0, (T - t[i]) / (t[i + 1] - t[i])));
        const lerp = v => v[i] + (v[i + 1] - v[i]) * a;
        const j = T >= t[i + 1] ? i + 1 : i;
        const sc = this.scale, dt = (t[i + 1] - t[i]) / 60;
        const slope = v => (v[i + 1] - v[i]) / dt;
        const p = {}, A = {}, into = {};
        let held = 0;
        for (const k of GAS_KEYS) {
            const x = Math.max(0, lerp(r.p[k]));
            p[k] = x < this.tiny ? 0 : x;
            A[k] = sc * r.capacity[k] * p[k];
            held += A[k];
            // The agent entering the compartment, mL/min, over this interval.
            into[k] = sc * r.capacity[k] * slope(r.p[k]);
        }
        const blood = into.vrg + into.mg + into.fg;
        return {
            t: T, gas: true, p, A, held,
            delivered: lerp(r.delivered), vented: lerp(this.vented),
            fresh: r.fresh[j], freshFlow: r.freshFlow[j], ventilation: r.ventilation[j],
            flow: {
                // Into the circuit with the fresh gas, mL/min of vapour.
                fresh: sc * r.freshFlow[j] * r.fresh[j],
                // From the circuit into the lungs, net of what is breathed out.
                vent: into.lungs + blood,
                // Out of the lungs into the blood, and from it into each tissue.
                blood, vrg: into.vrg, mg: into.mg, fg: into.fg,
                // Out through the pop-off valve.
                vented: slope(this.vented)
            }
        };
    },

    gasReadout(st) {
        const r = this.run, u = this.units, dp = this.ui.concDecimals;
        const c = x => x.toFixed(dp);
        const name = gasName(r.drug);
        const p = r.patient || {};
        const pat = [p.weight != null && `${p.weight} kg`, p.height != null && `${p.height} cm`,
                     p.age != null && `${p.age} y`, p.sex].filter(Boolean).join(' · ');
        const mac = r.mac > 0 ? `<div><span class="k">MAC</span> end-tidal ${(st.p.lungs / r.mac).toFixed(2)} · VRG ${(st.p.vrg / r.mac).toFixed(2)}</div>` : '';
        this.$('readout').innerHTML =
            `<div><span class="drug">${escapeHtml(name)}</span>${r.model ? ` <span class="model" title="${escapeHtml(r.model)}">· ${escapeHtml(shortModel(r.model))}</span>` : ''}</div>` +
            `<div class="time">${hms(this.T)}</div>` +
            `<div><span class="k">Fresh gas</span> ${st.freshFlow.toFixed(1)} L/min at <span class="tgt">${c(st.fresh)}</span> ${u.conc}` +
            ` · <span class="k">Ventilation</span> ${st.ventilation.toFixed(1)} L/min</div>` +
            `<div><span class="k">Inspired</span> ${c(st.p.circuit)} · <span class="k">End-tidal</span> ${c(st.p.lungs)} ${u.conc}</div>` +
            `<div><span class="k">VRG (effect site)</span> <span class="ce">${c(st.p.vrg)}</span> · <span class="k">Muscle</span> ${c(st.p.mg)}` +
            ` · <span class="k">Fat</span> ${c(st.p.fg)} ${u.conc}</div>` + mac +
            (this.ui.notToScale ? `<div class="scale">Glasses not to scale: the largest capacity is drawn ${SIZE_RATIO_MAX}× the smallest. Heights are partial pressure.</div>` : '') +
            `<div class="foot">${pat ? escapeHtml(pat) + ' · ' : ''}${escapeHtml(r.source || '')} · <kbd>h</kbd> keys</div>`;
    },

    // The amounts card: delivered, then in the patient (and the circuit, if it
    // holds any), then vented, in mL of vapour at one atmosphere.
    gasAmounts(st) {
        const a = x => x.toFixed(1), r = this.run;
        const row = (name, x, sub) => `<tr><td class="k${sub ? ' sub' : ''}">${name}</td><td>${a(x)}</td></tr>`;
        const inPatient = st.A.lungs + st.A.vrg + st.A.mg + st.A.fg;
        this.$('amounts').innerHTML = `
          <div class="phead">Amounts <kbd>a</kbd></div>
          <table class="amounts">
            <tr><th></th><th>${escapeHtml(this.units.amount)} vapour</th></tr>
            ${row('Delivered (fresh gas)', st.delivered)}
            ${r.capacity.circuit > 0 ? row('In the circuit', st.A.circuit) : ''}
            ${row('In the patient', inPatient)}
            ${row('Lungs', st.A.lungs, true)}
            ${row('VRG', st.A.vrg, true)}
            ${row('Muscle', st.A.mg, true)}
            ${row('Fat', st.A.fg, true)}
            ${row('Vented', st.vented)}
          </table>`;
        this.$('amounts').classList.toggle('show', this.showAmounts);
    }
});

// The arrows' scale for a gas run: its 95th-percentile flow (each
// interval's largest), so a burst of fresh gas does not make every other
// arrow a hairline; never 0.
function gasFlowScale(r, stateAt) {
    const flows = [];
    for (let i = 0; i < r.t.length - 1; i++) {
        const f = stateAt((r.t[i] + r.t[i + 1]) / 2).flow;
        const m = Math.max(Math.abs(f.fresh), Math.abs(f.vent), Math.abs(f.blood), Math.abs(f.vrg), Math.abs(f.mg), Math.abs(f.fg), Math.abs(f.vented));
        if (Number.isFinite(m)) flows.push(m);
    }
    flows.sort((a, b) => a - b);
    const f95 = flows[Math.floor(0.95 * (flows.length - 1))] || flows[flows.length - 1] || 0;
    return f95 > 0 ? f95 : (flows[flows.length - 1] || 1);
}

// ── the 2D drawing of a gas run ────────────────────────────────
Object.assign(Flat, {
    setGasRun(r, ui, stateAt) {
        const n = r.t.length;
        // One partial-pressure scale: every compartment at every sample, and
        // the fresh gas (the vaporiser's setting, drawn on the circuit).
        let top = 0;
        const up = v => { for (let i = 0; i < n; i++) if (v[i] > top) top = v[i]; };
        for (const k of GAS_KEYS) up(r.p[k]);
        up(r.fresh);
        // The flows' scale, as for a drug: the run's 95th-percentile flow.
        this.scale = { top: niceTop(top > 0 ? top : 1), fMax: ui.gasFlowScale };
    },

    drawGas(st) {
        const r = this.run, u = this.ui.units, dp = this.ui.concDecimals;
        const W = this.el.clientWidth, H = this.el.clientHeight;
        const TOP = GAS_TOP, BOTTOM = FLAT_BOTTOM, MIN_PLOT = FLAT_MIN_PLOT;
        const ctl = this.controlsTop(H);
        Cards.fit('2d');
        this.cardH = Cards.height;
        const floor = this.cardH > 0 ? ctl - this.cardH - 22 : ctl;
        const Hs = Math.max(H, TOP + MIN_PLOT + BOTTOM + (H - ctl));
        const yTop = TOP, yBase = Math.max(TOP + MIN_PLOT, floor - BOTTOM);
        const ph = yBase - yTop;
        // Left to right: the circuit, the lungs, then the tissues. The ideal
        // circuit holds no agent and is a bar; a circuit with a volume is a box.
        const gap = Math.max(48, Math.min(FLAT_GAP, Math.round(W * 0.07)));
        const x0 = 70, x1 = W - 40;
        const keys = r.capacity.circuit > 0 ? GAS_KEYS : GAS_KEYS.slice(1);
        const barW = r.capacity.circuit > 0 ? 0 : GAS_CIRCUIT_W + gap;
        const room = Math.max(keys.length * FLAT_MIN_W, x1 - x0 - 36 - barW - (keys.length - 1) * gap);
        const dv = displayVolumes(keys.map(k => r.capacity[k]));
        const widths = boxWidths(dv.v, room);
        const x = {}, w = {};
        // The circuit's arrows' heads clear the axis's unit.
        let cursor = x0 + 36;
        if (!(r.capacity.circuit > 0)) { x.circuit = cursor; w.circuit = GAS_CIRCUIT_W; cursor += GAS_CIRCUIT_W + gap; }
        keys.forEach((k, i) => { x[k] = cursor; w[k] = widths[i]; cursor += widths[i] + gap; });
        const cx = k => x[k] + w[k] / 2;
        const yOf = c => yBase - Math.min(1, Math.max(0, c / this.scale.top)) * ph;
        const thick = f => Number.isNaN(f) ? 0 : Math.min(FLAT_ARROW_MAX, FLAT_ARROW_MAX * Math.abs(f) / this.scale.fMax);
        // A flow too small to draw: below a millionth of the arrow scale.
        const flows = f => Math.abs(f) > 1e-6 * this.scale.fMax;
        const fmt = v => fixedOrSig(v, dp);
        const amt = v => fixedOrSig(v, v < 10 ? 2 : 1);
        const e = s => escapeHtml(String(s));
        const rateTxt = f => `${e(sig3(Math.abs(f)))} ${e(u.rate)}`;
        const out = [];

        // The partial-pressure axis.
        for (const tk of ticks(this.scale.top)) {
            const y = yOf(tk);
            out.push(`<line x1="${x0 - 6}" y1="${y}" x2="${x0}" y2="${y}" stroke="${FLAT_COLOURS.axis}"/>`,
                     `<text x="${x0 - 10}" y="${y + 4}" text-anchor="end" class="tk">${e(fmtTick(tk))}</text>`);
        }
        out.push(`<line x1="${x0}" y1="${yTop}" x2="${x0}" y2="${yBase}" stroke="${FLAT_COLOURS.axis}"/>`,
                 `<text x="${x0 - 10}" y="${yTop - 14}" text-anchor="end" class="tk">${e(u.conc)}</text>`);

        // One MAC (age-adjusted), across the patient's compartments.
        if (r.mac > 0 && r.mac <= this.scale.top) {
            const y = yOf(r.mac), xa = x.lungs - 4, xb = x.fg + w.fg + 4;
            out.push(`<line x1="${xa}" y1="${y}" x2="${xb}" y2="${y}" stroke="${FLAT_COLOURS.axis}" stroke-dasharray="5 4"/>`,
                     `<text x="${xb + 4}" y="${y + 4}" class="sub">1 MAC</text>`);
        }

        // The value labels sit above the floor's arrow (circuit to lungs) when a box is nearly empty.
        const band = flows(st.flow.vent) ? Math.max(14, thick(st.flow.vent) / 2 + 6) + thick(st.flow.vent) / 2 + 20 : 0;
        const valY = y => Math.min(y, yBase - 4) - 8 > yBase - band - 4 ? yBase - band - 4 : Math.min(y, yBase - 4) - 8;
        const tint = `rgb(${this.ui.drugRgb.map(c => Math.round(c * 255)).join(',')})`;
        const box = k => {
            const y = yOf(st.p[k]), ce = k === 'vrg', bar = k === 'circuit' && !(r.capacity.circuit > 0);
            const vol = r.volume[k], lam = r.partition[k];
            const size = bar ? 'inspired' : k === 'lungs' || k === 'circuit' ? `${fixedOrSig(vol, vol < 10 ? 2 : 1)} L gas`
                : `${fixedOrSig(vol, vol < 10 ? 2 : 1)} L × ${e(sig3(lam))}`;
            out.push(`<rect x="${x[k]}" y="${y}" width="${w[k]}" height="${yBase - y}" fill="${ce ? FLAT_COLOURS.ce : tint}" fill-opacity="0.85"/>`,
                     `<rect x="${x[k]}" y="${yTop}" width="${w[k]}" height="${ph}" fill="none" stroke="${FLAT_COLOURS.box}"${bar ? ' stroke-dasharray="2 3"' : ''}/>`,
                     `<text x="${cx(k)}" y="${yBase + 18}" text-anchor="middle" class="nm">${GAS_NAMES[k]}</text>`,
                     `<text x="${cx(k)}" y="${yBase + 34}" text-anchor="middle" class="sub">${size}</text>`,
                     // The bar's value beside it, clear of the arrows over it.
                     bar ? `<text x="${x[k] + w[k] + 5}" y="${Math.max(yTop + 12, valY(y))}" class="val">${e(fmt(st.p[k]))}</text>`
                         : `<text x="${cx(k)}" y="${valY(y)}" text-anchor="middle" class="val${ce ? ' ce' : ''}">${e(fmt(st.p[k]))}</text>`);
            if (!bar) out.push(`<text x="${cx(k)}" y="${yBase + 50}" text-anchor="middle" class="sub">${e(amt(st.A[k]))} ${e(u.amount)}</text>`);
        };
        for (const k of GAS_KEYS) box(k);
        // What each partial pressure is, over its box.
        out.push(`<text x="${cx('lungs')}" y="${yTop - 6}" text-anchor="middle" class="sub">end-tidal</text>`,
                 `<text x="${cx('vrg')}" y="${yTop - 6}" text-anchor="middle" class="sub ce">effect site</text>`);
        // The fresh gas's own partial pressure (the vaporiser), on the circuit.
        {
            const y = yOf(st.fresh);
            out.push(`<line x1="${x.circuit - 4}" y1="${y}" x2="${x.circuit + w.circuit + 4}" y2="${y}" stroke="${FLAT_COLOURS.target}" stroke-width="2.5"/>`);
        }

        // Fresh gas in and the pop-off out, side by side over the circuit;
        // their labels to the right.
        const label = (lx, ly, anchor, text, cls) =>
            out.push(`<text x="${lx}" y="${ly}" text-anchor="${anchor}" class="fl ${cls}">${text}</text>`);
        const fIn = flows(st.flow.fresh), fOut = flows(st.flow.vented);
        const twi = thick(st.flow.fresh), two = thick(st.flow.vented);
        const hi = headWidth(twi), ho = headWidth(two), cc = cx('circuit');
        const xi = fOut ? cc - hi / 2 - 2 : cc, xo = fIn ? cc + ho / 2 + 2 : cc;
        if (fIn) arrowVert(out, xi, yTop - 70, yTop - 2, twi, FLAT_COLOURS.in);
        if (fOut) arrowVert(out, xo, yTop - 2, yTop - 70, two, FLAT_COLOURS.out);
        const lx = Math.max(xi + hi / 2, xo + ho / 2) + 8;
        label(lx, yTop - 56, 'start', `Fresh gas ${fIn ? rateTxt(st.flow.fresh) : `0 ${e(u.rate)}`}`, 'in');
        label(lx, yTop - 40, 'start', `Vented ${fOut ? rateTxt(st.flow.vented) : `0 ${e(u.rate)}`}`, 'out');
        label(lx, yTop - 24, 'start', `${e(st.freshFlow.toFixed(1))} L/min at ${e(fmt(st.fresh))} ${e(u.conc)}`, 'sub');

        // Breathing: the net flow from the circuit into the lungs, along the floor.
        if (flows(st.flow.vent)) {
            const f = st.flow.vent, tw = thick(f), y = yBase - Math.max(14, tw / 2 + 6);
            const xa = x.circuit + w.circuit, xb = x.lungs;
            arrowH(out, f > 0 ? xa + 6 : xb - 6, f > 0 ? xb - 6 : xa + 6, y, tw, f > 0 ? FLAT_COLOURS.in : FLAT_COLOURS.out);
            label((xa + xb) / 2, y - tw / 2 - 6, 'middle', rateTxt(f), f > 0 ? 'in' : 'out');
        }

        // The blood, under the names: out of the lungs, and into each tissue
        // (or back, on emergence), every arrow from or to one line.
        const yLine = yBase + 98, yHead = yBase + 60;
        out.push(`<line x1="${cx('lungs')}" y1="${yLine}" x2="${cx('fg')}" y2="${yLine}" stroke="${FLAT_COLOURS.axis}" stroke-width="2"/>`,
                 `<text x="${cx('lungs') - 8}" y="${yLine + 4}" text-anchor="end" class="sub">Blood, ${e(u.rate)}</text>`);
        // The blood's flows are numbers only, the unit said once on the line,
        // so they fit between the arrows of a narrow window.
        const num = f => e(sig3(Math.abs(f)));
        if (flows(st.flow.blood)) {
            const f = st.flow.blood, tw = thick(f);
            // Down out of the lungs (green) on induction, up into them (red) on emergence.
            if (f > 0) arrowVert(out, cx('lungs'), yHead, yLine, tw, FLAT_COLOURS.out);
            else arrowVert(out, cx('lungs'), yLine, yHead, tw, FLAT_COLOURS.in);
            label(cx('lungs') + tw / 2 + 6, yBase + 84, 'start', num(f), f > 0 ? 'out' : 'in');
        }
        for (const k of ['vrg', 'mg', 'fg']) {
            const f = st.flow[k];
            if (!flows(f)) continue;
            const tw = thick(f);
            // Up into the tissue, away from the lungs (green); or down out of it, toward them (red).
            if (f > 0) arrowVert(out, cx(k), yLine, yHead, tw, FLAT_COLOURS.out);
            else arrowVert(out, cx(k), yHead, yLine, tw, FLAT_COLOURS.in);
            label(cx(k) + tw / 2 + 6, yBase + 84, 'start', num(f), f > 0 ? 'out' : 'in');
        }

        // What the widths and heights are, in the room right of the circuit's
        // labels (about 6 px a character): in full, briefly, or not at all.
        const free = x1 - (lx + 190);
        const note = [`Widths: capacity (volume × tissue:gas partition)${dv.a < 1 ? `, not to scale (largest ${SIZE_RATIO_MAX}× smallest)` : ''}. Heights: partial pressure.`,
                      `Widths: capacity${dv.a < 1 ? ', not to scale' : ''}`].find(t => t.length * 6 <= free);
        if (note) out.push(`<text x="${x1}" y="${yTop - 40}" text-anchor="end" class="sub">${note}</text>`);
        this.finish(out, W, Hs, st);
    }
});

// A vertical arrow from y0 to y1, either way, its head at y1.
function arrowVert(out, x, y0, y1, w, colour) {
    const dir = y1 > y0 ? 1 : -1, hl = Math.min(14, Math.abs(y1 - y0) / 2), hw = headWidth(w);
    const ya = Math.min(y0, y1 - dir * hl), yb = Math.max(y0, y1 - dir * hl);
    out.push(`<rect x="${x - w / 2}" y="${ya}" width="${w}" height="${yb - ya}" fill="${colour}"/>`,
             `<polygon points="${x - hw / 2},${y1 - dir * hl} ${x + hw / 2},${y1 - dir * hl} ${x},${y1}" fill="${colour}"/>`);
}
