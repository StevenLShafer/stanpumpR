// ═══════════════════════════════════════════════════════════════
//  stanpumpR viewer — the inhaled anesthetics, in the 3D view
//
//  Written for stanpumpR by Claude Code (Anthropic), 2026-11-10, at the
//  request of Steven L. Shafer, in Charles Minto's scene (scene.js,
//  vessels.js, arrows.js), whose glasses, arrows and labels it uses.
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
//  A gas run (gas.js) as glasses, as a drug's compartments are drawn:
//    - the lungs at the centre, where V1 stands for a drug; the vessel-rich
//      group (the effect site, in the effect site's blue), muscle and fat
//      behind them on an arc, as V2 and V3 are; the breathing circuit to the
//      left, a narrow tube when it holds no agent (stanpumpR's ideal circuit);
//    - each glass's floor area its capacity (volume x tissue:gas partition),
//      compressed as a drug's volumes are (displayVolumes()), and its liquid
//      as tall as the partial pressure, one scale for every glass, every
//      glass as tall as the run's highest partial pressure needs;
//    - the vaporiser's setting a gold ring on the circuit, as a TCI target is
//      on V1, and one MAC a pale ring round the patient's glasses;
//    - the arrows on Charles Minto's scale of cross-section for rate: the
//      fresh gas down into the circuit, the vented gas down out of its base
//      (as elimination leaves V1), the breathing between the circuit and the
//      lungs and the blood between the lungs and each tissue along the
//      floor, red into the lungs and green out of them;
//    - each glass named over its rim with its partial pressure; pointing at a
//      glass or an arrow shows its card.
//  Nothing here computes anything: drawGas() is handed the player's state
//  (Player.gasStateAt()), whose flows are read from the amounts.
// ═══════════════════════════════════════════════════════════════

const GAS_TUBE_R = 1.5;          // the ideal circuit's tube, which holds no agent, scene units
const GAS_CIRCUIT_ANGLE = -95;   // degrees round from straight back: the circuit, to the left
const GAS_FIRST_TISSUE = -25;    // the vessel-rich group's angle; muscle and fat follow round to the right
const GAS_CLEAR = 1.5;           // the least gap between two glasses, scene units
const GAS_ARROW_MAX = 1;         // the thickest arrow, in ARROW_R: a fresh gas flow far above the rest
const GAS_MAC_INK = '#c7d2de';
const GAS_CAPTION = { circuit: 'inspired', lungs: 'end-tidal', vrg: 'effect site' };

Object.assign(View, {
    gasVessels: null, gasArrows: null, gasLabels: null, gasMacRings: null, gasGeo: null, gasShown: false,

    // The glasses, arrows, labels and rings, made once, on the first gas run.
    gasBuild() {
        if (this.gasVessels) return;
        const s = this.scene;
        this.gasVessels = {}; this.gasLabels = {}; this.gasMacRings = {};
        GAS_KEYS.forEach((k, i) => {
            this.gasVessels[k] = new Vessel(s, 10 + i);       // not 1: no painted Cp, no ripples
            this.gasVessels[k].setGlass(this.look);
            this.gasLabels[k] = new SceneLabel(s, 'gas', false);
            if (k !== 'circuit') {
                const pts = [], n = 128;
                for (let j = 0; j < n; j++) {
                    const a = 2 * Math.PI * j / n, b = 2 * Math.PI * (j + 1) / n;
                    pts.push(Math.cos(a), 0, Math.sin(a), Math.cos(b), 0, Math.sin(b));
                }
                const ring = targetLine(pts);
                ring.material.color.setStyle(GAS_MAC_INK, THREE.SRGBColorSpace);
                ring.material.linewidth = 1.5;
                this.gasVessels[k].group.add(ring);
                this.gasMacRings[k] = ring;
            }
        });
        this.gasArrows = {};
        for (const a of ['fresh', 'vented', 'vent', 'vrg', 'mg', 'fg']) this.gasArrows[a] = new FlowArrow(s);
        this.gasVessels.circuit.setTargetRing(0);     // its line made now, so sized below
        for (const m of TARGET_LINE_MATS) m.resolution.set(this.container.clientWidth, this.container.clientHeight);
    },

    // A gas run: the glasses' sizes, places and height, fixed for the run.
    // r: the run; ui: the player's (concScale, the arrows' scale).
    gasSetup(r, ui) {
        this.gasBuild();
        // Floor area from the capacity, compressed as a drug's volumes are,
        // the largest between VESSEL_AREA_MIN and _MAX.
        const keys = r.capacity.circuit > 0 ? GAS_KEYS : GAS_KEYS.slice(1);
        let dv = displayVolumes(keys.map(k => r.capacity[k])).v;
        const big = Math.max(...dv), fit = big > 0 ? Math.min(VESSEL_AREA_MAX, Math.max(VESSEL_AREA_MIN, big)) / big : 1;
        const rad = { circuit: GAS_TUBE_R };
        keys.forEach((k, i) => { rad[k] = Math.sqrt(dv[i] * fit); });
        // Every glass as tall as the run's highest partial pressure, or the
        // fresh gas's, needs: one scale, so levels compare across glasses.
        let top = 0;
        for (const k of GAS_KEYS) for (const x of r.p[k]) if (x > top) top = x;
        for (const x of r.fresh) if (x > top) top = x;
        const glassH = Math.max(GLASS_MIN, Math.ceil((top * ui.concScale * 1.08 + 0.4) / 2) * 2);
        this.gasGeo = { r: rad, c: gasLayout(rad), glassH };
        // The case the close-up and i look at (centreGlass()).
        this.geo = { n: 0, r: [0, rad.lungs, 0, 0], c: [null, new THREE.Vector3(), null, null] };
        this.gasShow(true);
    },

    // Show the gas run's things and hide the drug's, or the reverse.
    gasShow(on) {
        this.gasShown = on;
        this.tipKey = null;
        if (this.gasVessels) {
            for (const v of Object.values(this.gasVessels)) v.setVisible(on);
            for (const l of Object.values(this.gasLabels)) l.show(on);
            for (const a of Object.values(this.gasArrows)) if (!on) a.group.visible = false;
        }
        if (on) {
            for (let k = 1; k <= 3; k++) this.vessels[k].setVisible(false);
            for (const a of Object.values(this.arrows)) a.group.visible = false;
            this.ribbon.setVisible(false);
            this.bolusLabel.show(false);
            this.v1Mlh = 0;
        }
        this.invalidate();
    },

    // One moment of a gas run (Player.gasStateAt()).
    drawGas(st, ui) {
        this.invalidate();
        const G = this.gasGeo, L = this.look, hs = ui.concScale, H = G.glassH;
        for (const k of GAS_KEYS) {
            const v = this.gasVessels[k];
            v.setShape(G.c[k], G.r[k], H);
            v.setGlass(L);
            v.setLiquid(st.p[k], st.p[k] * hs, L, k === 'vrg' ? ui.ceRgb : ui.drugRgb, ui.concDecimals);
            const ring = this.gasMacRings[k];
            if (ring) {
                const y = Player.run.mac > 0 ? Player.run.mac * hs : 0;
                ring.visible = y > 0 && y <= H;
                ring.scale.set(glassRadius(G.r[k]), 1, glassRadius(G.r[k]));
                ring.position.y = y;
            }
            const cap = GAS_CAPTION[k] ? `<br><i>${GAS_CAPTION[k]}</i>` : '';
            this.gasLabels[k].set(G.c[k].clone().setY(H + 0.3),
                `<b>${GAS_NAMES[k]}</b> ${escapeHtml(st.p[k].toFixed(ui.concDecimals))} <i>${escapeHtml(ui.units.conc)}</i>${cap}`);
        }
        // The vaporiser's setting, gold on the circuit.
        this.gasVessels.circuit.setTargetRing(st.fresh * hs);

        // Arrows: cross-section for rate, the run's 95th-percentile flow
        // drawn at ARROW_R, none thicker than GAS_ARROW_MAX of it.
        const rad = f => Math.min(GAS_ARROW_MAX * ARROW_R, ARROW_R * Math.sqrt(Math.abs(f) / ui.gasFlowScale));
        const A = this.gasArrows, len = CONNECT_LENGTH, f = st.flow;
        const up = new THREE.Vector3(0, 1, 0), down = new THREE.Vector3(0, -1, 0);
        const cc = G.c.circuit, rl = G.r.lungs;
        // The fresh gas, down onto the circuit's liquid; the vented gas, down
        // out of its base, as a drug's elimination leaves V1.
        A.fresh.set(cc.clone().setY(st.p.circuit * hs + len + 0.5), down, len, rad(f.fresh), L.arrows.in, L);
        A.vented.set(cc.clone(), down, len, rad(f.vented), L.arrows.out, L);
        // Breathing and the blood: across the gap from the lungs, along the
        // floor, as V1's arrows to V2 and V3.
        const between = (a, centre, flow) => {
            const u = centre.clone().setY(0).normalize(), ar = rad(flow);
            // flow > 0 is out of the lungs (to a tissue), green; < 0 into them, red.
            if (flow >= 0) a.set(u.clone().multiplyScalar(rl).setY(ar), u, len, ar, L.arrows.out, L);
            else a.set(u.clone().multiplyScalar(rl + len).setY(ar), u.clone().negate(), len, ar, L.arrows.in, L);
        };
        // Breathing in (into the lungs) is red: its flow out of the lungs is -vent.
        between(A.vent, cc, -f.vent);
        for (const k of ['vrg', 'mg', 'fg']) between(A[k], G.c[k], f[k]);

        this.tipState = { ...st, units: ui.units, dp: ui.concDecimals };
        const tk = this.tipKey;
        if (tk && !tk.includes(':')) this.tipKey = null;
        this.fillTip();
    },

    // The card over a gas glass or arrow.
    fillGasTip() {
        const el = this.tip, key = this.tipKey, st = this.tipState, r = Player.run;
        if (!r || r.kind !== 'gas' || !st || !st.gas) { el.style.display = 'none'; return; }
        const u = st.units, [kind, k] = key.split(':'), num = x => sig3(Math.abs(x));
        const vapour = x => `<div class="a">${x.toFixed(1)} <i>${escapeHtml(u.amount)} vapour</i></div>`;
        let html;
        if (kind === 'gv') {
            const vol = r.volume[k], cap = r.capacity[k];
            const size = k === 'circuit' && !(cap > 0) ? 'holds no agent'
                : k === 'lungs' || k === 'circuit' ? `${vol.toFixed(2)} L of gas`
                : `${vol.toFixed(1)} L × ${sig3(r.partition[k])} = ${sig3(cap)} L of gas`;
            html = `<div class="k">${GAS_LONG[k]}, ${size}</div>` +
                `<div class="v">${st.p[k].toFixed(st.dp)} <i>${escapeHtml(u.conc)}</i></div>` + (cap > 0 ? vapour(st.A[k]) : '');
        } else {
            const f = st.flow[k];
            let name, into;
            if (k === 'fresh') { name = `Fresh gas, ${st.freshFlow.toFixed(1)} L/min at ${st.fresh.toFixed(st.dp)} ${escapeHtml(u.conc)}`; into = true; }
            else if (k === 'vented') { name = 'Vented through the pop-off valve'; into = false; }
            else if (k === 'vent') { into = f >= 0; name = into ? 'Breathed in, net' : 'Breathed out, net'; }
            else { into = f < 0; name = into ? `${GAS_NAMES[k]} → blood → lungs` : `Lungs → blood → ${GAS_NAMES[k]}`; }
            const sum = k === 'fresh' ? `<div class="a"><span class="k">Delivered</span> ${st.delivered.toFixed(1)} <i>${escapeHtml(u.amount)}</i></div>`
                : k === 'vented' ? `<div class="a"><span class="k">Vented</span> ${st.vented.toFixed(1)} <i>${escapeHtml(u.amount)}</i></div>` : '';
            html = `<div class="k">${name}</div>` +
                `<div class="v dotted"><span class="dot ${into ? 'in' : 'out'}"></span><span>${num(f)} <i>${escapeHtml(u.rate)} vapour</i></span></div>` + sum;
        }
        if (html !== this.tipHtml) { el.innerHTML = html; this.tipHtml = html; }
        el.style.display = 'block';
        const box = this.container.getBoundingClientRect();
        const x = this.tipAt[0] - box.left + 14, y = this.tipAt[1] - box.top + 14;
        el.style.left = Math.min(x, box.width - el.offsetWidth - 8) + 'px';
        el.style.top = Math.min(y, box.height - el.offsetHeight - 8) + 'px';
    }
});

// Where each glass stands: the lungs at the origin; every other glass its own
// radius plus CONNECT_LENGTH out from the lungs' wall, so an arrow of that
// length spans the gap; the circuit at GAS_CIRCUIT_ANGLE, the tissues from
// GAS_FIRST_TISSUE round to the right, each turned on until it stands clear
// of those already placed. Angles from straight back (-z), to the right (+x)
// positive. rad: each glass's radius.
function gasLayout(rad) {
    const rl = rad.lungs;
    const at = (k, deg) => {
        const d = rl + CONNECT_LENGTH + rad[k], a = deg * Math.PI / 180;
        return new THREE.Vector3(d * Math.sin(a), 0, -d * Math.cos(a));
    };
    const c = { lungs: new THREE.Vector3(0, 0, 0), circuit: at('circuit', GAS_CIRCUIT_ANGLE) };
    const placed = [[c.circuit, glassRadius(rad.circuit)]];
    const clear = (p, R) => placed.every(([q, S]) => p.distanceTo(q) >= R + S + GAS_CLEAR);
    let deg = GAS_FIRST_TISSUE;
    for (const k of ['vrg', 'mg', 'fg']) {
        const R = glassRadius(rad[k]);
        let p = at(k, deg);
        while (!clear(p, R) && deg < 250) { deg += 1; p = at(k, deg); }
        c[k] = p; placed.push([p, R]);
    }
    return c;
}
