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
// The gap from the lungs' glass to each other glass, which the breathing and
// blood arrows span: wider than a drug's (CONNECT_LENGTH), so that an arrow's
// head stands clear of a tall liquid behind it (desflurane fills muscle far
// higher than sevoflurane does, and from every side its column hid the head
// of the arrow into it at the drug view's 8 units).
const GAS_GAP = 11;
const GAS_ARROW_MAX = 1;         // the thickest arrow, in ARROW_R: a fresh gas flow far above the rest
const GAS_MAC_INK = '#c7d2de';
const GAS_CAPTION = { circuit: 'inspired', lungs: 'end-tidal', vrg: 'effect site' };
// Below this window size the glasses' labels are one line, name and number:
// the unit and what each compartment is stay in the readout and the cards.
const GAS_COMPACT_W = 1000, GAS_COMPACT_H = 700;
const gasCompact = () => window.innerWidth < GAS_COMPACT_W || window.innerHeight < GAS_COMPACT_H;

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
        // Arrows in proportion to the scene: nitrous oxide's glasses are a
        // third the size of sevoflurane's, and full-size arrows dwarfed them.
        const arrowScale = Math.min(1, Math.max(0.4, Math.max(rad.lungs, rad.vrg, rad.mg, rad.fg) / 10));
        this.gasGeo = { r: rad, c: gasLayout(rad), glassH, arrowScale };
        // The case the close-up and i look at (centreGlass()).
        this.geo = { n: 0, r: [0, rad.lungs, 0, 0], c: [null, new THREE.Vector3(), null, null] };
        // The starting view is chosen when it is first wanted (homePose()),
        // once the cards hold this run and their size is known.
        this.gasHome = null; this.gasRun = r; this.gasUi = ui;
        this.gasShow(true);
    },

    // The starting view of a gas run: of the views from the front half of the
    // scene, at several heights, the one in which the glasses' labels overlap
    // least and the glasses and the arrows between them are least hidden
    // behind nearer glasses, each view as close as frames the whole scene
    // above the cards. A run's glasses differ in size (sevoflurane's fat
    // holds 493 L of gas, nitrous oxide's 16), so no one view serves every
    // run (Shafer, 2026-11-10: the labels overlapped from the drug view's).
    gasHomePose(r, ui) {
        const G = this.gasGeo, H = G.glassH;
        const W = this.container.clientWidth || window.innerWidth, Hpx = this.container.clientHeight || window.innerHeight;
        // The scene keeps clear of the cards (Cards, player.js), where they
        // are in this window (the readout bottom left, the amounts bottom
        // right), and stands above the controls.
        const ctl = document.getElementById('controls');
        const floorY = (ctl ? ctl.getBoundingClientRect().top : Hpx) - 8;
        const cards = ['readout', 'amounts'].map(id => document.getElementById(id))
            .filter(el => el && el.offsetWidth > 0 && getComputedStyle(el).display !== 'none')
            .map(el => { const b = el.getBoundingClientRect(); return { x0: b.left - 6, x1: b.right + 6, y0: b.top - 6, y1: b.bottom }; });
        // The bottom of the room above the cards: the scene is centred in it.
        const roomBottom = Math.min(floorY, ...cards.map(c => c.y0));
        const cam = new THREE.PerspectiveCamera(60, W / Hpx, 0.1, 3000);
        const glasses = GAS_KEYS.map(k => ({ k, c: G.c[k], R: glassRadius(G.r[k]) }));
        // The labels' size, px: their text is set later, so estimated from
        // the longest it can be (name, partial pressure and unit; caption).
        const small = gasCompact();
        const label = k => small ? { w: GAS_NAMES[k].length * 8.5 + 50, h: 26 }
            : { w: Math.max(GAS_NAMES[k].length * 8.5 + 64, GAS_CAPTION[k] ? GAS_CAPTION[k].length * 6.5 : 0) + 18,
                h: GAS_CAPTION[k] ? 40 : 26 };
        // The floor arrows, as three points each across their gap.
        const arrows = ['circuit', 'vrg', 'mg', 'fg'].map(k => {
            const u = G.c[k].clone().setY(0).normalize();
            return [0.25, 0.5, 0.75].map(f => u.clone().multiplyScalar(G.r.lungs + f * GAS_GAP).setY(0.6));
        });
        const aim = glasses.reduce((a, g) => a.add(g.c), new THREE.Vector3()).multiplyScalar(1 / glasses.length).setY(H * 0.35);
        let shiftY = 0;
        const px = v => { const q = v.clone().project(cam); return [(q.x + 1) / 2 * W, (1 - q.y) / 2 * Hpx - shiftY, q.z]; };
        const box = g => {
            let x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity;
            for (const dx of [-g.R, g.R]) for (const dz of [-g.R, g.R]) for (const y of [0, H]) {
                const [x, yy] = px(g.c.clone().add(new THREE.Vector3(dx, y, dz)));
                x0 = Math.min(x0, x); x1 = Math.max(x1, x); y0 = Math.min(y0, yy); y1 = Math.max(y1, yy);
            }
            return { x0, x1, y0, y1 };
        };
        const area = b => Math.max(0, b.x1 - b.x0) * Math.max(0, b.y1 - b.y0);
        const meet = (a, b) => ({ x0: Math.max(a.x0, b.x0), x1: Math.min(a.x1, b.x1), y0: Math.max(a.y0, b.y0), y1: Math.min(a.y1, b.y1) });
        const inside = (b, x, y) => x > b.x0 && x < b.x1 && y > b.y0 && y < b.y1;
        // The camera at (theta, phi), D from the scene, looking at it; the
        // picture then moved up (shiftY px, the camera's view offset, which
        // moves the image and not the camera, so nothing it hides changes)
        // until the scene is centred in the room above the cards.
        const place = (theta, phi, D) => {
            const t = theta * Math.PI / 180, f = phi * Math.PI / 180;
            cam.position.set(aim.x + D * Math.cos(f) * Math.sin(t), aim.y + D * Math.sin(f), aim.z + D * Math.cos(f) * Math.cos(t));
            cam.lookAt(aim); cam.updateMatrixWorld(); cam.updateProjectionMatrix();
            shiftY = 0;
            let y0 = Infinity, y1 = -Infinity;
            for (const g of glasses) {
                const b = box(g), [, ay] = px(g.c.clone().setY(H + 0.3));
                y0 = Math.min(y0, b.y0, ay - label(g.k).h); y1 = Math.max(y1, b.y1);
            }
            shiftY = (y0 + y1) / 2 - (0.08 * Hpx + roomBottom) / 2;
        };
        // The circuit's arrows too: the fresh gas above it, the vented gas below.
        const ends = [H + CONNECT_LENGTH + 0.5, -CONNECT_LENGTH].map(y => G.c.circuit.clone().setY(y));
        // Everything in view: the glasses, their labels and the circuit's
        // arrows inside the window, above the controls and off the cards.
        const clear = b => !cards.some(c => area(meet(b, c)) > 0);
        const fits = () => glasses.every(g => {
            const b = box(g), [ax, ay] = px(g.c.clone().setY(H + 0.3)), l = label(g.k);
            const lb = { x0: ax - l.w / 2, x1: ax + l.w / 2, y0: ay - l.h, y1: ay };
            return b.x0 > 0.04 * W && b.x1 < 0.96 * W && b.y1 < floorY && lb.y0 > 0.08 * Hpx &&
                lb.x0 > 0.02 * W && lb.x1 < 0.98 * W && clear(b) && clear(lb);
        }) && ends.every(p => { const [x, y] = px(p); return x > 0.04 * W && x < 0.96 * W && y > 0.06 * Hpx && y < floorY &&
                                                         clear({ x0: x - 1, x1: x + 1, y0: y - 1, y1: y + 1 }); });
        // What hides things: the liquid, not the glass, which is see-through;
        // each at its highest in the run (at least a sliver).
        const columns = glasses.map(g => {
            let top = 0;
            for (const x of r.p[g.k]) if (x > top) top = x;
            return { k: g.k, c: g.c, R: G.r[g.k], h: Math.max(0.3, top * ui.concScale) };
        });
        // Whether the line from the camera to p passes through the column col
        // before reaching p; blocked(): through any column but skip's.
        const blockedBy = (p, col) => {
            const o = cam.position, d = p.clone().sub(o);
            const ox = o.x - col.c.x, oz = o.z - col.c.z, a = d.x * d.x + d.z * d.z;
            if (!(a > 0)) return false;
            const b = 2 * (ox * d.x + oz * d.z), c = ox * ox + oz * oz - col.R * col.R, disc = b * b - 4 * a * c;
            if (disc < 0) return false;
            let t0 = (-b - Math.sqrt(disc)) / (2 * a), t1 = (-b + Math.sqrt(disc)) / (2 * a);
            if (Math.abs(d.y) > 1e-9) {
                const ya = (0 - o.y) / d.y, yb = (col.h - o.y) / d.y;
                t0 = Math.max(t0, Math.min(ya, yb)); t1 = Math.min(t1, Math.max(ya, yb));
            } else if (o.y < 0 || o.y > col.h) return false;
            return Math.max(t0, 0) < Math.min(t1, 0.999);
        };
        const blocked = (p, skip) => columns.some(col => col.k !== skip && blockedBy(p, col));
        // Each glass's liquid seen at five points of its surface; each arrow at three.
        const surface = col => [[0, 0], [0.7, 0], [-0.7, 0], [0, 0.7], [0, -0.7]].map(([a, b]) =>
            new THREE.Vector3(col.c.x + a * col.R, col.h, col.c.z + b * col.R));
        let best = null;
        const scores = [];
        for (const phi of [22, 27, 32, 37, 42, 48, 54]) for (let theta = -175; theta <= 180; theta += 5) {
            let lo = 10, hi = 500;
            place(theta, phi, hi);
            if (!fits()) continue;
            for (let i = 0; i < 14; i++) { const D = (lo + hi) / 2; place(theta, phi, D); if (fits()) hi = D; else lo = D; }
            place(theta, phi, hi * 1.04);
            const lb = glasses.map(g => { const [x, y] = px(g.c.clone().setY(H + 0.3)), l = label(g.k);
                                          return { k: g.k, x0: x - l.w / 2, x1: x + l.w / 2, y0: y - l.h, y1: y }; });
            const underLabel = (p, own) => { const [x, y] = px(p); return lb.some(b => b.k !== own && inside(b, x, y)); };
            const underLabelKey = p => { const [x, y] = px(p); const b = lb.find(b => inside(b, x, y)); return b ? b.k : null; };
            let labels = 0, hidden = 0, lost = 0;
            for (let i = 0; i < lb.length; i++) for (let j = i + 1; j < lb.length; j++)
                labels += area(meet(lb[i], lb[j])) / Math.min(area(lb[i]), area(lb[j]));
            for (const col of columns) {
                const pts = surface(col);
                hidden += pts.filter(p => blocked(p, col.k) || underLabel(p, col.k)).length / pts.length;
            }
            const why = [];
            arrows.forEach((pts, ai) => pts.forEach((p, pi) => {
                const col = columns.find(c => blockedBy(p, c)), lab = underLabelKey(p);
                if (col || lab) why.push(`${['circuit', 'vrg', 'mg', 'fg'][ai]}#${pi}:${col ? 'col ' + col.k : 'label ' + lab}`);
            }));
            for (const pts of arrows) lost += pts.filter(p => blocked(p, null) || underLabel(p, null)).length / pts.length;
            // Nothing hidden comes first (Shafer, 2026-11-10: "hide nothing");
            // then labels apart, which gasSpreadLabels() can otherwise mend;
            // then, the heights being the partial pressures, a moderate
            // height, which does not flatten them, and the front.
            const score = 10 * hidden + 10 * lost + 3 * labels + 0.25 * Math.abs(theta) / 180 + 0.8 * Math.abs(phi - 30) / 20;
            scores.push({ theta, phi, score, labels, hidden, lost, covered: 0, why });
            if (!best || score < best.score) best = { score, eye: cam.position.clone(), aim: aim.clone(), offset: shiftY / Hpx, theta, phi, labels, hidden, lost };
        }
        this.gasHomeScores = scores;         // every view weighed, for the tests
        return best || { eye: new THREE.Vector3(...HOME_EYE), aim: new THREE.Vector3(...HOME_AIM) };
    },

    // The glasses' labels moved apart on screen where they still overlap
    // (the scene turned by hand): the lowest on the screen, usually the
    // nearest, keeps its place and each one above it is lifted just clear of
    // those already placed. Run after every drawing of the labels, which
    // places them afresh.
    gasSpreadLabels() {
        const items = [];
        for (const l of Object.values(this.gasLabels)) {
            const el = l.el;
            if (!l.obj.visible || el.style.display === 'none') continue;
            const lift = parseFloat(el.style.top) || 0, b = el.getBoundingClientRect();
            if (!(b.width > 0)) continue;
            items.push({ el, x0: b.left, x1: b.right, y0: b.top - lift, y1: b.bottom - lift });
        }
        items.sort((a, b) => b.y1 - a.y1);
        const placed = [];
        for (const it of items) {
            let y1 = it.y1, h = it.y1 - it.y0, moved = true;
            while (moved) {
                moved = false;
                for (const p of placed)
                    if (it.x0 < p.x1 && p.x0 < it.x1 && y1 - h < p.y1 && p.y0 < y1) { y1 = p.y0 - 2; moved = true; }
            }
            const lift = y1 - it.y1;
            const top = lift ? `${lift}px` : '';
            if (it.el.style.top !== top) it.el.style.top = top;
            placed.push({ x0: it.x0, x1: it.x1, y0: y1 - h, y1 });
        }
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
        this.applyOffset();          // a gas run's own picture offset, or the drug view's
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
            const value = escapeHtml(st.p[k].toFixed(ui.concDecimals));
            this.gasLabels[k].set(G.c[k].clone().setY(H + 0.3), gasCompact() ? `<b>${GAS_NAMES[k]}</b> ${value}`
                : `<b>${GAS_NAMES[k]}</b> ${value} <i>${escapeHtml(ui.units.conc)}</i>${GAS_CAPTION[k] ? `<br><i>${GAS_CAPTION[k]}</i>` : ''}`);
        }
        // The vaporiser's setting, gold on the circuit.
        this.gasVessels.circuit.setTargetRing(st.fresh * hs);

        // Arrows: cross-section for rate, the run's 95th-percentile flow
        // drawn at ARROW_R, none thicker than GAS_ARROW_MAX of it.
        const rad = f => G.arrowScale * Math.min(GAS_ARROW_MAX * ARROW_R, ARROW_R * Math.sqrt(Math.abs(f) / ui.gasFlowScale));
        const A = this.gasArrows, len = CONNECT_LENGTH, gap = GAS_GAP, f = st.flow;
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
            if (flow >= 0) a.set(u.clone().multiplyScalar(rl).setY(ar), u, gap, ar, L.arrows.out, L);
            else a.set(u.clone().multiplyScalar(rl + gap).setY(ar), u.clone().negate(), gap, ar, L.arrows.in, L);
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
// radius plus GAS_GAP out from the lungs' wall, so an arrow of that
// length spans the gap; the circuit at GAS_CIRCUIT_ANGLE, the tissues from
// GAS_FIRST_TISSUE round to the right, each turned on until it stands clear
// of those already placed. Angles from straight back (-z), to the right (+x)
// positive. rad: each glass's radius.
function gasLayout(rad) {
    const rl = rad.lungs;
    const at = (k, deg) => {
        const d = rl + GAS_GAP + rad[k], a = deg * Math.PI / 180;
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
