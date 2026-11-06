// ═══════════════════════════════════════════════════════════════
//  stanpumpR viewer — the 2D view
//
//  Written for stanpumpR by Claude Code (Anthropic), 2026-11-06, at the
//  request of Steven L. Shafer, beside Charles Minto's 3D view.
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
//  The same run, the same moment, drawn flat (after Tufte: as little ink as
//  the quantities need, and no ink that misstates them):
//    - each compartment is a box whose WIDTH is its volume and whose fill
//      HEIGHT is its concentration, so the area of the fill is the amount
//      of drug in it; one concentration scale serves every box, fixed for
//      the whole run, so a level can be compared across boxes and times;
//    - V2 to the left of V1, V3 to its right; the effect site a thin bar
//      beside V1, on the same scale (its volume is negligible);
//    - every flow an arrow whose thickness is its rate, on one scale fixed
//      for the run (its 95th-percentile flow is the thickest arrow; a brief
//      loading burst above it is drawn at that thickness, its label exact):
//      red into V1, green out of it, as in the 3D view;
//    - the TCI target a gold line, on V1 (TCI-P) or the effect site (TCI-E).
//  Nothing here computes pharmacokinetics: draw() is handed the player's
//  state for the moment (Player.stateAt()).
// ═══════════════════════════════════════════════════════════════

const FLAT_COLOURS = { in: '#ff6b5e', out: '#3fd47a', ce: '#3b82f6', target: '#ffd166',
                       axis: '#8a9cb0', ink: '#e6edf5', box: 'rgba(230,237,245,0.55)' };
const FLAT_ARROW_MAX = 26;       // px: the thickest arrow, the run's largest flow
const FLAT_GAP = 96;             // px between boxes, holding the arrows between them
const FLAT_CE_W = 12;            // px: the effect-site bar
const FLAT_MIN_W = 3;            // px: the narrowest box, so a tiny volume still shows

const Flat = {
    el: null, run: null, ui: null, scale: null, last: null, cardH: 0,

    init(container) {
        this.el = container;
        window.addEventListener('resize', () => { if (this.last && this.visible()) this.draw(this.last); });
        // The controls wrap on a narrow window: the cards sit above them at
        // whatever height they take (--ctl-h), and the drawing is laid out again.
        const ctl = document.getElementById('controls');
        if (ctl && window.ResizeObserver) new ResizeObserver(() => {
            document.documentElement.style.setProperty('--ctl-h', `${ctl.offsetHeight}px`);
            if (this.last && this.visible()) this.draw(this.last);
        }).observe(ctl);
    },
    visible() { return this.el && this.el.style.display !== 'none'; },

    // A new run: the fixed scales, from every moment of it.
    // stateAt: the player's own function, so the flows are the ones it draws.
    setRun(r, ui, stateAt) {
        this.run = r; this.ui = ui; this.last = null;
        const n = r.t.length;
        // The concentration scale: the highest value of every series at
        // every sample (interpolation never exceeds the samples), and every
        // target in force, so no moment's fill is clipped (ChatGPT, 38e0f3a:
        // a sampled scale missed a one-sample peak).
        let cMax = 0;
        const top = v => { if (v) for (let i = 0; i < n; i++) if (v[i] > cMax) cMax = v[i]; };
        top(r.cp); top(r.ce); top(r.c2); top(r.c3);
        if (r.target) for (let i = 0; i < n; i++)
            if ((!r.mode || r.mode[i] !== 'manual') && r.target[i] > cMax) cMax = r.target[i];
        // The flows: each interval's, at its midpoint (a flow is constant
        // over an interval), and the end.
        const flows = [];
        // (A flow that overflows, at concentrations near Number.MAX_VALUE, is
        // left out of the scale and drawn at the thickest.)
        const consider = st => flows.push(Math.max(Math.abs(st.flow.in), Math.abs(st.flow.abs), Math.abs(st.flow.out),
                                                   Math.abs(st.flow.to2), Math.abs(st.flow.to3)));
        for (let i = 0; i < n - 1; i++) consider(stateAt((r.t[i] + r.t[i + 1]) / 2));
        consider(stateAt(r.t[n - 1]));
        // The arrow scale: the run's 95th-percentile flow, not its largest, so
        // a loading burst of a few seconds does not make every other arrow a
        // hairline; a flow above it is drawn at the thickest, its label exact.
        for (let i = flows.length - 1; i >= 0; i--) if (!Number.isFinite(flows[i])) flows.splice(i, 1);
        flows.sort((a, b) => a - b);
        const f95 = flows[Math.floor(0.95 * (flows.length - 1))] || flows[flows.length - 1] || 0;
        this.scale = { top: niceTop(cMax > 0 ? cMax : 1), fMax: f95 > 0 ? f95 : (flows[flows.length - 1] || 1) };
    },

    draw(st) {
        this.last = st;
        if (!this.visible() || !this.run) return;
        const r = this.run, u = this.ui.units, dp = this.ui.concDecimals;
        const W = this.el.clientWidth, H = this.el.clientHeight;
        // The vertical layout, from the space the controls and cards leave
        // (ChatGPT, 38e0f3a: fixed offsets put elimination off a small
        // window). Above V1: the pump and absorption arrows (TOP); below:
        // the names, volumes, amounts and the elimination arrow (BOTTOM).
        const TOP = 96, BOTTOM = 108, MIN_PLOT = 90;
        const ctl = Math.min(H, (document.getElementById('controls') || { getBoundingClientRect: () => ({ top: H }) })
                                   .getBoundingClientRect().top) - 8;
        // The cards' height, measured while they show (kept while they are
        // left out, below, so the choice does not flip back and forth).
        if (!document.body.classList.contains('compact')) {
            const h = id => { const el = document.getElementById(id); return el ? el.offsetHeight : 0; };
            this.cardH = Math.max(h('readout'), h('amounts'));
        }
        // Above the cards if the plot fits there. If not, the 2D view leaves
        // the cards out: its own labels give every concentration, volume,
        // amount and flow, and the cards return in a larger window or the 3D
        // view. In a window too small even then, the drawing keeps its
        // minimum size and the 2D view scrolls.
        const compact = this.cardH > 0 && ctl - this.cardH - 22 - BOTTOM - TOP < MIN_PLOT;
        document.body.classList.toggle('compact', compact);
        const floor = compact ? ctl : ctl - this.cardH - 22;
        const Hs = Math.max(H, TOP + MIN_PLOT + BOTTOM + (H - ctl));
        const yTop = TOP, yBase = Math.max(TOP + MIN_PLOT, floor - BOTTOM);
        const ph = yBase - yTop;
        // The horizontal layout: the gaps between boxes narrow on a narrow
        // window; every box at least MIN_W px, the rest of the width shared
        // in proportion to volume, so the boxes always fit (ChatGPT, 38e0f3a:
        // a floor on px per litre pushed a vast V3 off-screen).
        const gap = Math.max(48, Math.min(FLAT_GAP, Math.round(W * 0.09)));
        const x0 = 70, x1 = W - 40;
        const k = 1 + (st.c2 != null) + (st.c3 != null);
        const v = [r.pk.v1, st.c2 != null ? r.pk.v2 : 0, st.c3 != null ? r.pk.v3 : 0];
        const room = Math.max(k * FLAT_MIN_W, x1 - x0 - (k - 1) * gap - FLAT_CE_W - 10 - 20);
        const [w1, w2, w3] = boxWidths(v, room);
        // Left to right: V2, V1 with the effect site beside it, V3.
        const bx2 = x0 + 20, bx1 = (st.c2 != null ? bx2 + w2 + gap : x0 + 20), bxe = bx1 + w1 + 6;
        const bx3 = bxe + FLAT_CE_W + gap - 6;
        const yOf = c => yBase - Math.min(1, Math.max(0, c / this.scale.top)) * ph;
        // Thickness is the rate, in proportion up to the arrow scale (a flow
        // above it is drawn at the thickest); a small flow is a hairline, its
        // head and label still showing it (ChatGPT, 38e0f3a: a 1.5 px floor
        // drew 1 and 2 mg/min alike).
        const thick = f => Number.isNaN(f) ? 0 : Math.min(FLAT_ARROW_MAX, FLAT_ARROW_MAX * Math.abs(f) / this.scale.fMax);
        const fmt = x => x == null ? '–' : fixedOrSig(x, dp);
        const amt = x => fixedOrSig(x, x < 10 ? 2 : 1);
        const e = s => escapeHtml(String(s));
        const out = [];

        // The concentration axis: a few round ticks, the unit once.
        for (const tk of ticks(this.scale.top)) {
            const y = yOf(tk);
            out.push(`<line x1="${x0 - 6}" y1="${y}" x2="${x0}" y2="${y}" stroke="${FLAT_COLOURS.axis}"/>`,
                     `<text x="${x0 - 10}" y="${y + 4}" text-anchor="end" class="tk">${e(fmtTick(tk))}</text>`);
        }
        out.push(`<line x1="${x0}" y1="${yTop}" x2="${x0}" y2="${yBase}" stroke="${FLAT_COLOURS.axis}"/>`,
                 `<text x="${x0 - 10}" y="${yTop - 14}" text-anchor="end" class="tk">${e(u.conc)}</text>`);

        const fill = `rgb(${this.ui.drugRgb.map(c => Math.round(c * 255)).join(',')})`;
        const box = (x, w, c, A, name, vol) => {
            const y = yOf(c);
            out.push(`<rect x="${x}" y="${y}" width="${w}" height="${yBase - y}" fill="${fill}" fill-opacity="0.85"/>`,
                     `<rect x="${x}" y="${yTop}" width="${w}" height="${ph}" fill="none" stroke="${FLAT_COLOURS.box}"/>`,
                     `<text x="${x + w / 2}" y="${yBase + 18}" text-anchor="middle" class="nm">${name}</text>`,
                     `<text x="${x + w / 2}" y="${yBase + 34}" text-anchor="middle" class="sub">${e(fixedOrSig(vol, vol < 10 ? 2 : 1))} L</text>`,
                     `<text x="${x + w / 2}" y="${Math.min(y, yBase - 4) - 8}" text-anchor="middle" class="val">${e(fmt(c))}</text>`,
                     `<text x="${x + w / 2}" y="${yBase + 50}" text-anchor="middle" class="sub">${e(amt(A))} ${e(u.amount)}</text>`);
        };
        box(bx1, w1, st.cp, st.A1, 'V<tspan class="s" dy="3">1</tspan>', v[0]);
        if (st.c2 != null) box(bx2, w2, st.c2, st.A2, 'V<tspan class="s" dy="3">2</tspan>', v[1]);
        if (st.c3 != null) box(bx3, w3, st.c3, st.A3, 'V<tspan class="s" dy="3">3</tspan>', v[2]);

        // The effect site, on the same scale.
        if (this.ui.showCe) {
            const y = yOf(st.ce);
            out.push(`<rect x="${bxe}" y="${y}" width="${FLAT_CE_W}" height="${yBase - y}" fill="${FLAT_COLOURS.ce}"/>`,
                     `<rect x="${bxe}" y="${yTop}" width="${FLAT_CE_W}" height="${ph}" fill="none" stroke="${FLAT_COLOURS.box}" stroke-dasharray="2 3"/>`,
                     `<text x="${bxe + FLAT_CE_W / 2}" y="${yBase + 18}" text-anchor="middle" class="nm">Ce</text>`,
                     `<text x="${bxe + FLAT_CE_W + 4}" y="${Math.min(y, yBase - 4) - 8}" class="val ce">${e(fmt(st.ce))}</text>`);
        }

        // The target.
        if (st.mode === 'plasma' || st.mode === 'effect') {
            const y = yOf(st.target), [xa, xb] = st.mode === 'plasma' ? [bx1 - 4, bx1 + w1 + 4] : [bxe - 3, bxe + FLAT_CE_W + 3];
            out.push(`<line x1="${xa}" y1="${y}" x2="${xb}" y2="${y}" stroke="${FLAT_COLOURS.target}" stroke-width="2.5"/>`);
        }

        // Arrows: thickness is the rate. Vertical into V1's top (the pump, and
        // absorption beside it), down out of its floor (elimination);
        // horizontal between V1 and V2 or V3 (the net flow).
        const cx1 = bx1 + w1 / 2;
        const label = (x, y, anchor, text, cls) =>
            out.push(`<text x="${x}" y="${y}" text-anchor="${anchor}" class="fl ${cls}">${text}</text>`);
        const rateTxt = f => `${e(sig3(Math.abs(f)))} ${e(u.rate)}`;
        if (st.flow.in > 0 || st.bolus > 0) {
            const tw = st.bolus > 0 ? FLAT_ARROW_MAX : thick(st.flow.in), xi = st.flow.abs > 0 ? cx1 - 9 : cx1;
            arrowV(out, xi, yTop - 58, yTop - 2, tw, FLAT_COLOURS.in);
            label(xi - tw / 2 - 6, yTop - 40, 'end', st.bolus > 0 ? `Bolus ${e(amt(st.bolus))} ${e(u.amount)}` : `Pump ${rateTxt(st.flow.in)}`, 'in');
        }
        if (st.flow.abs > 0) {
            const tw = thick(st.flow.abs), xa = st.flow.in > 0 || st.bolus > 0 ? cx1 + 9 : cx1;
            arrowV(out, xa, yTop - 58, yTop - 2, tw, FLAT_COLOURS.in);
            label(xa + tw / 2 + 6, yTop - 40, 'start', `Absorbed ${rateTxt(st.flow.abs)}`, 'in');
        }
        if (st.depot != null)
            label(cx1 + 16, yTop - 22, 'start', `Depot ${e(amt(st.depot))} ${e(u.amount)}`, 'sub');
        if (st.flow.out > 0) {
            const tw = thick(st.flow.out);
            arrowV(out, cx1, yBase + 60, yBase + 100, tw, FLAT_COLOURS.out);
            label(cx1 + tw / 2 + 8, yBase + 88, 'start', `Eliminated ${rateTxt(st.flow.out)}`, 'out');
        }
        const between = (f, xL, xR, toLeft) => {
            if (!(Math.abs(f) > 0)) return;
            // f > 0 is out of V1; toLeft: the peripheral box is on the left.
            const leaving = f > 0, tw = thick(f), y = yBase - Math.max(14, tw / 2 + 6);
            const pointsLeft = leaving === toLeft;
            arrowH(out, pointsLeft ? xR - 6 : xL + 6, pointsLeft ? xL + 6 : xR - 6, y, tw, leaving ? FLAT_COLOURS.out : FLAT_COLOURS.in);
            label((xL + xR) / 2, y - tw / 2 - 6, 'middle', rateTxt(f), leaving ? 'out' : 'in');
        };
        if (st.c2 != null) between(st.flow.to2, bx2 + w2, bx1, true);
        if (st.c3 != null) between(st.flow.to3, bxe + FLAT_CE_W, bx3, false);

        this.el.innerHTML = `<svg width="${W}" height="${Hs}" viewBox="0 0 ${W} ${Hs}" xmlns="http://www.w3.org/2000/svg">${out.join('')}</svg>`;
        // A scroll bar that came or went changes the width: lay out once more.
        if (this.el.clientWidth !== W && !this.again) { this.again = true; this.draw(st); this.again = false; return; }
        // Keep every label inside the window: a label beside a box near an
        // edge is moved in, never cut off.
        for (const t of this.el.querySelectorAll('text')) {
            const b = t.getBBox(), dx = b.x < 2 ? 2 - b.x : b.x + b.width > W - 2 ? W - 2 - b.x - b.width : 0;
            if (dx) t.setAttribute('x', Number(t.getAttribute('x')) + dx);
        }
    }
};

// A vertical arrow from y0 down to y1 (y1 > y0), shaft thickness w.
function arrowV(out, x, y0, y1, w, colour) {
    const hl = Math.min(14, (y1 - y0) / 2), hw = Math.max(w * 1.8, 8);
    out.push(`<rect x="${x - w / 2}" y="${y0}" width="${w}" height="${y1 - y0 - hl}" fill="${colour}"/>`,
             `<polygon points="${x - hw / 2},${y1 - hl} ${x + hw / 2},${y1 - hl} ${x},${y1}" fill="${colour}"/>`);
}
// A horizontal arrow from xa to xb, shaft thickness w, at height y.
function arrowH(out, xa, xb, y, w, colour) {
    const dir = xb > xa ? 1 : -1, hl = Math.min(14, Math.abs(xb - xa) / 2), hw = Math.max(w * 1.8, 8);
    const xs = Math.min(xa, xb - dir * hl), xe = Math.max(xa, xb - dir * hl);
    out.push(`<rect x="${xs}" y="${y - w / 2}" width="${xe - xs}" height="${w}" fill="${colour}"/>`,
             `<polygon points="${xb - dir * hl},${y - hw / 2} ${xb - dir * hl},${y + hw / 2} ${xb},${y}" fill="${colour}"/>`);
}

// The boxes' widths: in proportion to volume, sharing room px, none
// narrower than FLAT_MIN_W (a box held at that width is left out of the
// proportion, and the rest share what remains). An absent box (volume 0)
// is 0 wide.
function boxWidths(v, room) {
    const fixed = v.map(() => false);
    let px = 0;
    for (let pass = 0; pass < v.length; pass++) {
        const free = v.reduce((s, x, i) => s + (x > 0 && !fixed[i] ? x : 0), 0);
        const left = room - FLAT_MIN_W * fixed.filter(Boolean).length;
        px = free > 0 ? Math.max(0, left) / free : 0;
        let changed = false;
        v.forEach((x, i) => { if (x > 0 && !fixed[i] && x * px < FLAT_MIN_W) { fixed[i] = true; changed = true; } });
        if (!changed) break;
    }
    return v.map((x, i) => !(x > 0) ? 0 : fixed[i] ? FLAT_MIN_W : x * px);
}

// Fixed decimals, as the readout shows them, unless a value that is not
// zero would read as zero: then three significant figures (ChatGPT, 38e0f3a).
function fixedOrSig(x, dp) {
    if (!Number.isFinite(x)) return Number.isNaN(x) ? '–' : x > 0 ? '∞' : '−∞';
    const t = x.toFixed(dp);
    return x !== 0 && Number(t) === 0 ? sig3(x) : t;
}
// Three significant figures: 0.0123, 1.50, 123, 12300 (not 12346), 1.23e-7.
function sig3(x) {
    if (x === 0) return '0';
    if (!Number.isFinite(x)) return Number.isNaN(x) ? '–' : x > 0 ? '∞' : '−∞';
    const a = Math.abs(x);
    return a >= 1000 && a < 1e21 ? String(Number(x.toPrecision(3))) : x.toPrecision(3);
}

// The axis top: the next 1, 1.5, 2, 2.5, 3, 4, 5, 6 or 8 x 10^n at or above x.
// Always finite and positive: x itself where the round number would not be
// (x near Number.MAX_VALUE or below the smallest normal number).
function niceTop(x) {
    if (!(x > 0) || !Number.isFinite(x)) return 1;
    const p = Math.pow(10, Math.floor(Math.log10(x)));
    if (!(p > 0) || !Number.isFinite(p)) return x;
    for (const m of [1, 1.5, 2, 2.5, 3, 4, 5, 6, 8, 10])
        if (m >= (x / p) * 1.0001) { const t = m * p; return Number.isFinite(t) && t > 0 ? t : x; }
    return x;
}
// The ticks: 0 and round steps to top, built by count (at most a dozen), so
// no value of top can make the loop run on.
function ticks(top) {
    const p = Math.pow(10, Math.floor(Math.log10(top))), m = top / p;
    const step = (m <= 2 ? 0.5 : m <= 5 ? 1 : 2) * p;
    if (!(step > 0) || !Number.isFinite(step) || !(top / step <= 12)) return [0, top];
    const out = [];
    const last = Math.floor(top / step * 1.0001);       // not top * 1.0001, which overflows near MAX_VALUE
    for (let i = 0; i <= last; i++) out.push(i * step);
    return out;
}
// A tick label: at most three significant figures, never 0 for a tick that
// is not (1e-120 reads 1e-120, not 0).
function fmtTick(x) {
    if (x === 0) return '0';
    return String(Number(x.toPrecision(3)));
}
