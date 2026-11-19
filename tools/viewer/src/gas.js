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
//  The blood, and what drives the uptake (Shafer, 2026-11-17, from GasMan's
//  display, which shows them): arterial blood is at the end-tidal partial
//  pressure in the engine; mixed venous blood is the tissues' partial
//  pressures weighted by their shares of the cardiac output (the run's
//  `venous`). Uptake is cardiac output x blood:gas coefficient x their
//  difference, so the two are drawn side by side after the lungs, as bars,
//  since blood holds no agent in the engine. The readout gives the cardiac
//  output, the minute and alveolar ventilation and the circuit; the amounts
//  card the uptake (what the tissues hold, which is all the lungs have given
//  the blood) and the greenhouse effect of the agent delivered, all of which
//  reaches the atmosphere, through the pop-off valve now or breathed out after
//  the case (R/gasGreenhouse.R: Sulbaek Andersen 2012's 100-year GWP).
//
//  The 2D view came first (Shafer, 2026-11-09: "Start with the 2D model");
//  the 3D view draws the same run in gas3d.js.
// ═══════════════════════════════════════════════════════════════

// The compartments, left to right, and what each partial pressure is.
const GAS_KEYS = ['circuit', 'lungs', 'vrg', 'mg', 'fg'];
const GAS_NAMES = { circuit: 'Circuit', lungs: 'Lungs', vrg: 'VRG', mg: 'Muscle', fg: 'Fat' };
const GAS_LONG = { circuit: 'Circuit (inspired)', lungs: 'Lungs (end-tidal)', vrg: 'Vessel-rich group (effect site)',
                   mg: 'Muscle group', fg: 'Fat group' };
const GAS_CIRCUIT_W = 22;        // px: the ideal circuit, which holds no agent, and each blood bar
// The blood: arterial (at end-tidal) and mixed venous, neither holding agent.
const GAS_BLOOD_KEYS = ['art', 'ven'];
const GAS_BLOOD_NAMES = { art: 'Art', ven: 'Ven' };
const GAS_BLOOD_COLOURS = { art: '#e0566b', ven: '#8a94f0' };
const GAS_BLOOD_PAIR_GAP = 40;   // px between the two blood bars: room for their values
const GAS_CIRCUIT_NAMES = { ideal: 'Ideal circuit', 'semi-closed': 'Semi-closed circuit', open: 'Open circuit' };
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
    // The blood, the cardiac output, the alveolar ventilation and the
    // greenhouse effect (2026-11-17): optional, so an older run still loads.
    // Arterial blood (2026-11-19): only where an endobronchial tube shunts
    // blood past the ventilated lung; otherwise it is the end-tidal.
    for (const [k, min] of [['venous', 0], ['alveolarVentilation', 0], ['arterial', 0]])
        if (r[k] != null) { const q = arr(r[k], k, min); if (q) return q; }
    if (r.cardiacOutput != null && !(Number.isFinite(r.cardiacOutput) && r.cardiacOutput > 0)) return 'cardiacOutput must be a number above 0.';
    if (r.greenhouse != null) {
        const g = r.greenhouse;
        if (typeof g !== 'object') return 'greenhouse must give gwp100, kgPerLitre, lifetime and carKgPerYear.';
        for (const k of ['gwp100', 'kgPerLitre', 'lifetime', 'carKgPerYear'])
            if (!(Number.isFinite(g[k]) && g[k] > 0)) return `greenhouse.${k} must be a number above 0.`;
        if (g.source != null && (typeof g.source !== 'string' || !/^[^<>&"']{0,80}$/.test(g.source))) return 'greenhouse.source must be short plain text.';
    }
    return runTextProblem(r);
}

// ── the player, for a gas run ──────────────────────────────────
Object.assign(Player, {
    // A gas run's setup, from load(): the scales, the decimals, the agent vented.
    gasSetup(r) {
        let peak = 0;
        for (const k of GAS_KEYS) for (const x of r.p[k]) if (x > peak) peak = x;
        if (r.venous) for (const x of r.venous) if (x > peak) peak = x;
        this.tiny = peak * TINY_FRACTION;
        const level = r.typical > 0 ? r.typical : (peak > 0 ? peak / 2 : 1);
        const mag = Math.max(-6, Math.floor(Math.log10(level)));
        // The 3D view's heights (gas3d.js): 6 units for one MAC, or for the
        // highest the glasses must hold where that is lower (nitrous oxide,
        // whose MAC is above an atmosphere): the partial pressures and the
        // vaporiser's setting, which the glasses are made tall enough for.
        // From the partial pressures alone, a brief wash-in through the
        // semi-closed circuit (0.098% at most in the patient, the vaporiser at
        // 8%) made the glasses 528 units tall, out of every view (Carbon's
        // third UI audit, 2026-11-15, M7).
        let top = peak;
        for (const x of r.fresh) if (x > top) top = x;
        const level3d = r.mac > 0 && top > 0 ? Math.min(r.mac, top) : level;
        this.ui = { gas: true, showCe: false, units: this.units, drugRgb: hexRgb(GAS_TINT),
                    ceRgb: hexRgb(FLAT_COLOURS.ce), concDecimals: Math.max(0, 2 - mag), concScale: 6 / level3d };
        // Vented: the agent delivered less the agent held, at each time. The
        // pop-off valve lets agent out and never in, so what has gone through
        // it never falls and is never below 0. Where the run's amounts say
        // otherwise (agent held that was never delivered: the gas engine with
        // no fresh gas flow), that is the engine's imbalance, not a flow:
        // drawn as one, its size taken without its sign, it was a thick arrow
        // venting 4 mL/min that ran the other way (Carbon's third UI audit,
        // 2026-11-15, H2). The amounts card then does not add up, as it should not.
        const n = r.t.length;
        this.vented = new Float64Array(n);
        for (let i = 0; i < n; i++) {
            let held = 0;
            for (const k of GAS_KEYS) held += r.capacity[k] * r.p[k][i];
            this.vented[i] = Math.max(i ? this.vented[i - 1] : 0, r.delivered[i] - this.scale * held);
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
        // The blood: arterial at end-tidal (or the run's own, with a shunt),
        // mixed venous from the run.
        let venous = null;
        if (r.venous) { const x = Math.max(0, lerp(r.venous)); venous = x < this.tiny ? 0 : x; }
        let arterial = p.lungs;
        if (r.arterial) { const x = Math.max(0, lerp(r.arterial)); arterial = x < this.tiny ? 0 : x; }
        const delivered = lerp(r.delivered);
        // The greenhouse effect: kg of CO2 equivalent in the agent delivered,
        // and at the rate the fresh gas brings it now (mL/min to kg/h).
        const g = r.greenhouse, kgCO2e = mL => mL / 1000 * g.kgPerLitre * g.gwp100;
        const freshNow = sc * r.freshFlow[j] * r.fresh[j];
        return {
            t: T, gas: true, p, A, held,
            arterial, venous,
            // What the tissues hold is all the blood has taken from the lungs
            // since time 0: the engine's blood holds none and nothing is
            // metabolised.
            uptake: A.vrg + A.mg + A.fg,
            delivered, vented: lerp(this.vented),
            co2e: g ? kgCO2e(delivered) : null, co2eRate: g ? kgCO2e(freshNow) * 60 : null,
            cardiacOutput: r.cardiacOutput != null ? r.cardiacOutput : null,
            alveolarVentilation: r.alveolarVentilation ? r.alveolarVentilation[j] : null,
            fresh: r.fresh[j], freshFlow: r.freshFlow[j], ventilation: r.ventilation[j],
            flow: {
                // Into the circuit with the fresh gas, mL/min of vapour.
                fresh: freshNow,
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
        const mac = r.mac > 0 ? `<span class="k">MAC</span> end-tidal ${(st.p.lungs / r.mac).toFixed(2)} · VRG ${(st.p.vrg / r.mac).toFixed(2)}` : '';
        const L = x => escapeHtml(fixedOrSig(x, 1));
        // The circuit, by name; GasMan's semi-closed circuit with its volume.
        const circuit = r.circuit ? GAS_CIRCUIT_NAMES[r.circuit] + (r.capacity.circuit > 0 ? `, ${L(r.volume.circuit)} L` : '') : '';
        // The ventilation row is minute ventilation; the engine takes 30% of
        // it as dead space, and the rest, the alveolar ventilation, exchanges gas.
        const va = st.alveolarVentilation != null ? ` (alveolar ${L(st.alveolarVentilation)})` : '';
        const co = st.cardiacOutput != null ? `<span class="k">Cardiac output</span> ${L(st.cardiacOutput)} L/min` : '';
        // Arterial blood is at end-tidal in the engine; mixed venous after it.
        // In the colour of its bar (2D) and its ring on the lungs' glass (3D).
        const blood = (r.arterial ? ` · <span class="k">Arterial</span> ${c(st.arterial)}` : '') +
            (st.venous != null ? ` · <span class="k">Mixed venous</span> <span class="ven">${c(st.venous)}</span>` : '');
        // As many lines as before the blood and the cardiac output were added
        // (2026-11-17): a taller card leaves the 3D view's starting view less
        // room, and the circuit goes in the foot.
        this.$('readout').innerHTML =
            `<div><span class="drug">${escapeHtml(name)}</span>${r.model ? ` <span class="model" title="${escapeHtml(r.model)}">· ${escapeHtml(shortModel(r.model))}</span>` : ''}</div>` +
            `<div class="time">${hms(this.T)}</div>` +
            `<div><span class="k">Fresh gas</span> ${L(st.freshFlow)} L/min at <span class="tgt">${c(st.fresh)}</span> ${u.conc}` +
            ` · <span class="k">Ventilation</span> ${L(st.ventilation)}${va} L/min</div>` +
            `<div><span class="k">Inspired</span> ${c(st.p.circuit)} · <span class="k">End-tidal</span> ${c(st.p.lungs)}${blood} ${u.conc}</div>` +
            `<div><span class="k">VRG (effect site)</span> <span class="ce">${c(st.p.vrg)}</span> · <span class="k">Muscle</span> ${c(st.p.mg)}` +
            ` · <span class="k">Fat</span> ${c(st.p.fg)} ${u.conc}</div>` +
            (mac || co ? `<div>${mac}${mac && co ? ' · ' : ''}${co}</div>` : '') +
            (this.ui.notToScale ? `<div class="scale">Glasses not to scale: the largest capacity is drawn ${SIZE_RATIO_MAX}× the smallest. Heights are partial pressure.</div>` : '') +
            `<div class="foot">${pat ? escapeHtml(pat) + ' · ' : ''}${circuit ? escapeHtml(circuit) + ' · ' : ''}${escapeHtml(r.source || '')} · <kbd>h</kbd> keys</div>`;
    },

    // The amounts card: delivered, then where it is (the circuit, if it holds
    // any, the lungs, and the uptake, what the tissues have taken from the
    // blood), then vented, in mL of vapour at one atmosphere; and the
    // greenhouse effect of the agent delivered.
    gasAmounts(st) {
        const a = x => x.toFixed(1), r = this.run;
        const row = (name, x, sub) => `<tr><td class="k${sub ? ' sub' : ''}">${name}</td><td>${a(x)}</td></tr>`;
        this.$('amounts').innerHTML = `
          <div class="phead">Amounts, ${escapeHtml(this.units.amount)} of vapour <kbd>a</kbd></div>
          <table class="amounts">
            ${row('Delivered (fresh gas)', st.delivered)}
            ${r.capacity.circuit > 0 ? row('In the circuit', st.A.circuit) : ''}
            ${row('In the lungs', st.A.lungs)}
            ${row('Uptake (tissues)', st.uptake)}
            ${row('VRG', st.A.vrg, true)}
            ${row('Muscle', st.A.mg, true)}
            ${row('Fat', st.A.fg, true)}
            ${row('Vented', st.vented)}
            ${this.gasGreenhouse(st)}
          </table>`;
        this.$('amounts').classList.toggle('show', this.showAmounts);
    },

    // The greenhouse effect (R/gasGreenhouse.R), the amounts table's last row:
    // kg of CO2 equivalent in the agent delivered, all of which reaches the
    // air, through the pop-off valve now or breathed out after the case (its
    // tooltip says so, and how long a typical US car takes to emit as much
    // CO2). One row, with the unit moved from a header row into the heading,
    // so the card is no taller than before (2026-11-17): a taller one left the
    // 3D view no starting view with nothing hidden in an 800 x 600 window,
    // or did not fit beside the readout at all (Cards.fit()). The rate the
    // fresh gas brings it at now is with the fresh gas: the 2D view's label
    // of the setting, the 3D view's card over the fresh gas arrow.
    gasGreenhouse(st) {
        const g = this.run.greenhouse;
        if (!g || st.co2e == null) return '';
        const kg = x => x < 10 ? x.toFixed(2) : x < 100 ? x.toFixed(1) : x.toFixed(0);
        const car = carTime(st.co2e, g.carKgPerYear);
        const why = `Greenhouse effect of the agent delivered, kg of CO2 equivalent: ${g.source || ''} (GWP ${g.gwp100}).` +
                    ` All of it reaches the air, through the pop-off valve now or breathed out after the case.` +
                    (car ? ` As much CO2 as a typical US car emits in ${car}.` : '');
        return `<tr class="ghg" title="${escapeHtml(why)}"><td class="k">CO<sub>2</sub>e delivered, kg</td><td>${kg(st.co2e)}</td></tr>`;
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
        if (r.venous) up(r.venous);
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
        // The blood's two bars stand between the lungs and the tissues.
        const blood = st.venous != null;
        const barW = (r.capacity.circuit > 0 ? 0 : GAS_CIRCUIT_W + gap) +
                     (blood ? 2 * GAS_CIRCUIT_W + GAS_BLOOD_PAIR_GAP + gap : 0);
        const room = Math.max(keys.length * FLAT_MIN_W, x1 - x0 - 36 - barW - (keys.length - 1) * gap);
        const dv = displayVolumes(keys.map(k => r.capacity[k]));
        const widths = boxWidths(dv.v, room);
        const x = {}, w = {};
        // The circuit's arrows' heads clear the axis's unit.
        let cursor = x0 + 36;
        if (!(r.capacity.circuit > 0)) { x.circuit = cursor; w.circuit = GAS_CIRCUIT_W; cursor += GAS_CIRCUIT_W + gap; }
        keys.forEach((k, i) => {
            x[k] = cursor; w[k] = widths[i]; cursor += widths[i] + gap;
            if (k === 'lungs' && blood) {
                x.art = cursor; w.art = GAS_CIRCUIT_W; cursor += GAS_CIRCUIT_W + GAS_BLOOD_PAIR_GAP;
                x.ven = cursor; w.ven = GAS_CIRCUIT_W; cursor += GAS_CIRCUIT_W + gap;
            }
        });
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
        // The blood: arterial (at end-tidal) and mixed venous, side by side,
        // so their difference, which with the cardiac output sets the uptake,
        // is seen. Bars, as the ideal circuit is: blood holds no agent here.
        if (blood) {
            for (const k of GAS_BLOOD_KEYS) {
                const v = k === 'art' ? st.arterial : st.venous, y = yOf(v);
                out.push(`<rect x="${x[k]}" y="${y}" width="${w[k]}" height="${yBase - y}" fill="${GAS_BLOOD_COLOURS[k]}" fill-opacity="0.85"/>`,
                         `<rect x="${x[k]}" y="${yTop}" width="${w[k]}" height="${ph}" fill="none" stroke="${FLAT_COLOURS.box}" stroke-dasharray="2 3"/>`,
                         `<text x="${cx(k)}" y="${yBase + 18}" text-anchor="middle" class="nm">${GAS_BLOOD_NAMES[k]}</text>`,
                         `<text x="${cx(k)}" y="${valY(y)}" text-anchor="middle" class="val">${e(fmt(v))}</text>`);
            }
            out.push(`<text x="${(cx('art') + cx('ven')) / 2}" y="${yTop - 6}" text-anchor="middle" class="sub">blood</text>`);
        }
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
        // The setting, and its greenhouse effect at this rate.
        const ghg = st.co2eRate != null ? ` · ${e(gasCO2eRateText(st))}` : '';
        label(lx, yTop - 24, 'start', `${e(st.freshFlow.toFixed(1))} L/min at ${e(fmt(st.fresh))} ${e(u.conc)}${ghg}`, 'sub');

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
        const free = x1 - (lx + (st.co2eRate != null ? 290 : 190));
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

// How long a typical US car takes to emit kg of CO2 (carKgPerYear a year), as
// minutes, hours or days; null for nothing.
function carTime(kg, carKgPerYear) {
    if (!(kg > 0) || !(carKgPerYear > 0)) return null;
    const min = kg / carKgPerYear * 365.25 * 24 * 60;
    if (min < 1) return '< 1 min';
    if (min < 120) return `${Math.round(min)} min`;
    const h = min / 60;
    if (h < 48) return `${Math.round(h)} h`;
    const d = h / 24;
    return `${d < 10 ? d.toFixed(1) : Math.round(d)} days`;
}

// The greenhouse effect at the rate the fresh gas brings the agent now, kg of
// CO2 equivalent an hour, and as how many typical US cars emit as much (each
// carKgPerYear a year, Sulbaek Andersen 2012).
function gasCO2eRateText(st) {
    const g = Player.run && Player.run.greenhouse;
    if (!g || st.co2eRate == null) return '';
    const kg = fixedOrSig(st.co2eRate, st.co2eRate < 10 ? 2 : 1);
    const cars = st.co2eRate / (g.carKgPerYear / (365.25 * 24));
    const n = cars >= 10 ? String(Math.round(cars)) : cars >= 0.05 ? cars.toFixed(1) : null;
    return `${kg} kg CO₂e an hour` + (n ? `, as ${n} US car${n === '1.0' ? '' : 's'}` : '');
}
