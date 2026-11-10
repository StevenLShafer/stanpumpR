// ═══════════════════════════════════════════════════════════════
//  stanpumpR viewer — the scene: renderer, light, floor, cameras, and
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
//  drawing one state of the simulation
//
//  cylinders.exe (cylinders.cpp myGlutDisplay, init.cpp): gluPerspective
//  60 degrees, the eye at (0, 10, 20) looking at the origin, one distant
//  white light from (1, 1, 1), a teal background. The look (looks.js)
//  decides colours and materials; this file decides where things go.
// ═══════════════════════════════════════════════════════════════

const FLOOR_Y = -(CONNECT_LENGTH + 1.5);   // below the elimination arrow's tip
const VESSEL_AREA_MIN = 20, VESSEL_AREA_MAX = 200;   // the largest vessel's floor area, scene units^2 (setShape())
const GLASS_MIN = 6;                       // the beakers' least height, scene units
const EYE0 = [0, 10, 20];                  // init.cpp 117-119: the starting view's direction
const ZOOM_GAP = 0.3;                      // i stops this far (scene units) outside V1's glass
// The starting view and the close-up held End flies to, from the middle of
// V1's base, the same for every drug. The starting view looks the way
// cylinders.exe's did (EYE0: up 1 for 2 back) and stands far enough back
// to hold the widest scene (fentanyl's V3).
const HOME_EYE = [0, 28, 50], HOME_AIM = [0, 3, 0];
const LAND_EYE = [0, 4, 12], LAND_AIM = [0, 1, 0];   // the close-up, stood back from a wider V1 (landEye())

// The name painted on the floor, fixed (it reads from the starting view;
// orbit round and it is seen from the side or behind, as paint is): in
// the grid row from z = 5 to 10, four squares wide, centred under V1.
// stanpump in the arrows' pale red and R in a pale tint of the R logo's
// blue (#276DC3), both faint so they do not compete with the arrows.
// Shown with the floor.
const LOGO_W = 20, LOGO_H = 5, LOGO_Z = 7.5, LOGO_PX = 96;
const LOGO_PARTS = [['stanpump', '#f8857a', 0.35], ['R', '#7fb2ee', 0.35]];
function floorLogo() {
    const c = document.createElement('canvas');
    c.width = LOGO_W * LOGO_PX; c.height = LOGO_H * LOGO_PX;
    const g = c.getContext('2d');
    const font = px => `800 ${px}px "Segoe UI", system-ui, sans-serif`;
    // The letters as large as fit 94% of the width.
    let px = c.height * 0.8;
    g.font = font(px);
    const text = LOGO_PARTS.map(p => p[0]).join('');
    const w = g.measureText(text).width;
    px = Math.min(px, px * c.width * 0.94 / w);
    g.font = font(px);
    let x = (c.width - g.measureText(text).width) / 2;
    const y = c.height / 2;
    g.textBaseline = 'middle';
    for (const [s, col, alpha] of LOGO_PARTS) {
        g.globalAlpha = alpha; g.fillStyle = col; g.fillText(s, x, y); x += g.measureText(s).width;
    }
    const tex = new THREE.CanvasTexture(c);
    tex.colorSpace = THREE.SRGBColorSpace;
    tex.anisotropy = 8;
    const geo = new THREE.PlaneGeometry(LOGO_W, LOGO_H);
    geo.rotateX(-Math.PI / 2);      // flat, its top away from the starting view
    const m = new THREE.Mesh(geo, new THREE.MeshBasicMaterial({ map: tex, transparent: true, depthWrite: false, toneMapped: false }));
    m.position.set(0, FLOOR_Y + 0.02, LOGO_Z);
    m.renderOrder = 1;              // over the grid
    return m;
}

// The absorption arrow's direction, from the front (+z) toward the left.
const ABSORB_TURN = 60 * Math.PI / 180;

const View = {
    renderer: null, labelRenderer: null, scene: null,
    persp: null, camera: null, controls: null,
    vessels: [], arrows: {}, ribbon: null, bolusLabel: null,
    lit: null,          // c1, c2, c3, ce or bal: pointed at, so in gold
    floor: null, grid: null, hemi: null, sun: null, envTex: null,
    look: null, lookName: DEFAULT_LOOK,
    geo: null, clock: null,
    pk: null,           // the volumes setShape() was given, for the vessels' cards
    playing: false,     // set by the player: the arrows and ripples move while it runs
    recording: false,   // set while a video is saved (video.js): the loop stands aside
    dirty: true,        // the scene has changed since it was last drawn (invalidate())

    init(container) {
        const T = THREE;
        this.container = container;
        const w = container.clientWidth, h = container.clientHeight;

        const r = new T.WebGLRenderer({ antialias: true });
        r.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
        r.setSize(w, h);
        r.outputColorSpace = T.SRGBColorSpace;
        r.toneMapping = T.ACESFilmicToneMapping;
        r.shadowMap.enabled = true;
        r.shadowMap.type = T.PCFSoftShadowMap;
        container.appendChild(r.domElement);
        this.renderer = r;

        const lr = new THREE_ADDONS.CSS2DRenderer();
        lr.setSize(w, h);
        lr.domElement.className = 'label-layer';
        container.appendChild(lr.domElement);
        this.labelRenderer = lr;

        const s = new T.Scene();
        this.scene = s;
        const pm = new T.PMREMGenerator(r);
        this.envTex = pm.fromScene(new THREE_ADDONS.RoomEnvironment(), 0.04).texture;

        this.persp = new T.PerspectiveCamera(60, w / h, 0.1, 3000);
        this.camera = this.persp;
        this.persp.position.set(...EYE0);
        this.controls = new THREE_ADDONS.OrbitControls(this.persp, r.domElement);
        this.controls.enableDamping = true;
        this.controls.dampingFactor = 0.12;
        this.controls.target.set(0, 0, 0);
        // The mouse can never bring the camera onto the point it orbits:
        // at zero distance nothing can move it out again.
        this.controls.minDistance = 0.02;
        this.controls.maxDistance = 1500;
        // The mouse stops a degree short of straight down or up, so upright
        // is always defined and the camera never needs to roll.
        this.controls.minPolarAngle = Math.PI / 180;
        this.controls.maxPolarAngle = Math.PI - Math.PI / 180;

        this.hemi = new T.HemisphereLight(0xffffff, 0x334455, 0.7);
        s.add(this.hemi);
        this.sun = new T.DirectionalLight(0xffffff, 1.8);
        this.sun.position.set(50, 40, 35);
        this.sun.castShadow = true;
        this.sun.shadow.mapSize.set(2048, 2048);
        const sc = this.sun.shadow.camera;
        sc.left = -70; sc.right = 70; sc.top = 70; sc.bottom = -70; sc.near = 1; sc.far = 200;
        this.sun.shadow.bias = -0.0005;
        s.add(this.sun);

        // The floor: plain matte (Lambert), which costs the GPU less over
        // the floor's many pixels than the physically based material; a
        // glow (the look's floor.glow) stands in for the environment's light.
        this.floor = new T.Mesh(new T.CircleGeometry(220, 96),
            new T.MeshLambertMaterial({ color: 0x223344 }));
        this.floor.rotation.x = -Math.PI / 2;
        this.floor.position.y = FLOOR_Y;
        this.floor.receiveShadow = true;
        s.add(this.floor);
        this.grid = new T.GridHelper(300, 60, 0x445566, 0x445566);
        this.grid.position.y = FLOOR_Y + 0.01;
        this.grid.material.transparent = true;
        this.grid.material.opacity = 0.35;
        s.add(this.grid);
        this.logo = floorLogo();
        s.add(this.logo);

        for (let k = 1; k <= 3; k++) this.vessels[k] = new Vessel(s, k);
        for (const a of ['in', 'abs', 'out', 'to2', 'to3']) this.arrows[a] = new FlowArrow(s);
        this.ribbon = new EffectRibbon(s);
        this.vessels[1].setTargetRing(0);       // its line made now, so sized below
        for (const m of TARGET_LINE_MATS) m.resolution.set(w, h);
        this.bolusLabel = new SceneLabel(s, 'given hot', false);  // above the infusion arrow

        // Pointing at a vessel turns its rim gold, at the ribbon brightens
        // it; a vessel or an arrow shows its card (showTip).
        this.ray = new T.Raycaster();
        r.domElement.addEventListener('pointermove', e => this.pointAt(e));
        r.domElement.addEventListener('pointerleave', () => { this.setHover(null); this.showTip(null); });
        this.tip = document.createElement('div');
        this.tip.className = 'flow-tip';
        container.appendChild(this.tip);

        this.applyOffset();
        this.clock = new T.Clock();
        window.addEventListener('resize', () => this.resize());
        this.setLook(this.lookName);
        // A scene that changes outside draw() (a context restored after the
        // GPU dropped it) is drawn again.
        r.domElement.addEventListener('webglcontextrestored', () => this.invalidate());
        // The camera as last drawn, to tell whether it has moved since.
        const lastEye = new T.Matrix4(), lastLens = new T.Matrix4();
        // ts: the browser's time for this frame, on the screen's refresh.
        const loop = ts => {
            // Saving a video (video.js): step() draws each frame instead.
            if (this.recording) { requestAnimationFrame(loop); return; }
            const t0 = performance.now();
            const dt = this.clock.getDelta();
            const now = ts != null ? ts : t0;
            // Hidden behind the 2D view (flat.js): nothing to move or draw,
            // and the performance card measures the 2D view's frames (Gemini,
            // 6b52133: it said "measuring…" there for good, and the 2D frames'
            // time, never cleared, went into the first 3D frame).
            if (this.container.style.display === 'none') {
                Perf.frame(now, performance.now() - t0, null);
                requestAnimationFrame(loop); return;
            }
            // The arrows' stripes and V1's ripples move only while the clock runs.
            const running = this.clockRunning();
            for (const a of this.allArrows()) a.tick(running ? dt : 0);
            const rippled = this.vessels[1].tickRipples(dt, this.v1Mlh || 0, running);
            this.controls.update();
            // Drawn only when something on screen changes: the clock running,
            // the ripples settling, the camera moved (dragged, damped, a key,
            // a flight), or the scene changed (invalidate()). A paused view
            // otherwise kept the GPU drawing the same picture 60 times a
            // second (Gemini, 6b52133). Always while the performance card is
            // shown, so it measures the drawing.
            this.camera.updateMatrixWorld();
            const moved = !lastEye.equals(this.camera.matrixWorld) || !lastLens.equals(this.camera.projectionMatrix);
            if (running || rippled || moved || this.dirty || Perf.shown) {
                this.dirty = false;
                lastEye.copy(this.camera.matrixWorld); lastLens.copy(this.camera.projectionMatrix);
                Perf.render(this.renderer, () => this.renderer.render(this.scene, this.camera));
                this.labelRenderer.render(this.scene, this.camera);
            }
            Perf.frame(now, performance.now() - t0, this.renderer);
            requestAnimationFrame(loop);
        };
        loop();
    },

    // What the pointer is over in the view: an arrow, a vessel's liquid,
    // or the ribbon. Hidden things do not count.
    pointAt(e) {
        const rect = this.renderer.domElement.getBoundingClientRect();
        const ndc = new THREE.Vector2(((e.clientX - rect.left) / rect.width) * 2 - 1,
                                      -((e.clientY - rect.top) / rect.height) * 2 + 1);
        this.ray.setFromCamera(ndc, this.camera);
        const shown = o => { for (let q = o; q; q = q.parent) if (!q.visible) return false; return true; };
        const keyOf = new Map();
        // A vessel: its liquid only (side, bottom and top surface), not the glass.
        for (let k = 1; k <= 3; k++) for (const m of [this.vessels[k].liquid, this.vessels[k].surface]) keyOf.set(m, 'c' + k);
        keyOf.set(this.ribbon.bar, 'ce');
        const arrowOf = new Map();
        for (const [k, a] of Object.entries(this.arrows)) for (const m of a.group.children) arrowOf.set(m, k);
        // A gas run's (gas3d.js): keys with a colon, which a drug's never have.
        if (this.gasVessels) for (const [k, v] of Object.entries(this.gasVessels)) for (const m of [v.liquid, v.surface]) keyOf.set(m, 'gv:' + k);
        if (this.gasArrows) for (const [k, a] of Object.entries(this.gasArrows)) for (const m of a.group.children) arrowOf.set(m, 'ga:' + k);
        // An arrow anywhere along the ray wins: the infusion arrow can stand
        // inside V1's see-through glass, whose near wall the ray meets first.
        const aHit = this.ray.intersectObjects([...arrowOf.keys()], false).find(h => shown(h.object));
        const hit = aHit ? null : this.ray.intersectObjects([...keyOf.keys()], false).find(h => shown(h.object));
        const key = hit ? keyOf.get(hit.object) : null;
        this.setHover(key);
        this.showTip(aHit ? arrowOf.get(aHit.object) : key !== 'ce' ? key : null, e);
    },

    // A card by the pointer while it is over an arrow or a vessel. An
    // arrow: the rate it stands for, the infusion (or a bolus), the
    // elimination, or the net flow between V1 and V2 or V3, in the drug's
    // rate units. A vessel: its concentration and amount.
    // key: in, abs, out, to2, to3, c1, c2, c3 or null (none); e: the pointer event.
    showTip(key, e) {
        this.tipKey = key;
        if (e) this.tipAt = [e.clientX, e.clientY];
        this.fillTip();
    },
    fillTip() {
        const el = this.tip, key = this.tipKey, st = this.tipState;
        if (!key || !st) { el.style.display = 'none'; return; }
        if (key.includes(':')) { this.fillGasTip(); return; }
        const f = st.flow, u = st.units, num = x => Math.abs(x).toFixed(2);
        let html;
        if (key[0] === 'c') {
            const k = +key[1], c = [0, st.cp, st.c2, st.c3][k], a = [0, st.A1, st.A2, st.A3][k];
            const vol = [0, this.pk.vc, this.pk.v2, this.pk.v3][k], V = vol < 10 ? vol.toFixed(2) : vol.toFixed(1);
            html = `<div class="k">V<sub>${k}</sub> = ${V} L</div>` +
                `<div class="v">${k === 1 ? 'Cp' : 'C' + k} ${c == null ? '–' : c.toFixed(st.dp)} <i>${u.conc}</i></div>` +
                `<div class="a">${a.toFixed(1)} <i>${u.amount}</i></div>`;
        } else {
            // The infusion and elimination arrows also carry their running
            // total: administered, or eliminated.
            let name, value, unit = u.rate, into = false, sum = '';
            const total = (k, x) => `<div class="a"><span class="k">${k}</span> ${x.toFixed(1)} <i>${u.amount}</i></div>`;
            if (key === 'in' && st.total != null) sum = total('Administered', st.total);
            if (key === 'in' && st.bolus > 0) { name = 'Bolus'; value = st.bolus.toFixed(1); unit = u.amount; into = true; }
            else if (key === 'in') { name = 'Infusion rate'; value = num(f.in); into = true; }
            else if (key === 'abs') { name = 'Absorption rate'; value = num(f.abs); into = true; if (st.absorbed != null) sum = total('Absorbed', st.absorbed); }
            else if (key === 'out') { name = 'Elimination rate'; value = num(f.out); if (st.elim != null) sum = total('Eliminated', st.elim); }
            else {
                const k = key.slice(2), x = f[key];
                into = x < 0;
                name = into ? `Net V<sub>${k}</sub> → V<sub>1</sub> rate` : `Net V<sub>1</sub> → V<sub>${k}</sub> rate`;
                value = num(x);
            }
            // The dot, in the arrow's colour, in front of the rate.
            html = `<div class="k">${name}</div>` +
                `<div class="v dotted"><span class="dot ${into ? 'in' : 'out'}"></span><span>${value} <i>${unit}</i></span></div>` + sum;
        }
        if (html !== this.tipHtml) { el.innerHTML = html; this.tipHtml = html; }
        el.style.display = 'block';
        const box = this.container.getBoundingClientRect();
        const x = this.tipAt[0] - box.left + 14, y = this.tipAt[1] - box.top + 14;
        el.style.left = Math.min(x, box.width - el.offsetWidth - 8) + 'px';
        el.style.top = Math.min(y, box.height - el.offsetHeight - 8) + 'px';
    },

    // key: c1, c2, c3, ce or null.
    setHover(key) {
        if (key === this.lit) return;
        this.lit = key;
        this.showLit();
    },
    // The vessel pointed at with its rim in gold, or the ribbon brighter.
    showLit() {
        this.invalidate();
        for (let k = 1; k <= 3; k++) this.vessels[k].rimMat.color.set(this.lit === 'c' + k ? '#ffd166' : '#ffffff');
        if (this.gasVessels) for (const [k, v] of Object.entries(this.gasVessels)) v.rimMat.color.set(this.lit === 'gv:' + k ? '#ffd166' : '#ffffff');
        if (this.look) this.ribbon.mat.emissiveIntensity = this.lit === 'ce' ? 0.8 : 0.25;
    },

    // One frame of a video (video.js): the arrows' stripes and V1's ripples
    // moved by dt seconds, as if playing, then the scene and its labels drawn.
    step(dt) {
        for (const a of this.allArrows()) a.tick(dt);
        this.vessels[1].tickRipples(dt, this.v1Mlh || 0, true);
        this.controls.update();
        this.renderer.render(this.scene, this.camera);
        this.labelRenderer.render(this.scene, this.camera);
    },

    // The scene has changed: draw it on the next frame.
    invalidate() { this.dirty = true; },

    // Every arrow in the scene: a drug's, and a gas run's (gas3d.js) once made.
    allArrows() { return Object.values(this.arrows).concat(this.gasArrows ? Object.values(this.gasArrows) : []); },

    // The clock is running while play is on, or for a moment after a step.
    kick() { this.lastStep = performance.now(); },
    clockRunning() {
        return this.playing || (this.lastStep != null && performance.now() - this.lastStep < 450);
    },

    resize() {
        const w = this.container.clientWidth, h = this.container.clientHeight;
        this.renderer.setSize(w, h);
        this.labelRenderer.setSize(w, h);
        for (const m of TARGET_LINE_MATS) m.resolution.set(w, h);   // the targets' lines, in pixels
        this.persp.aspect = w / h;
        this.applyOffset();
        this.invalidate();          // setSize() cleared the canvas
    },

    // Centre the picture a little above the middle of the view, clear of
    // the controls along the bottom: the camera's view is shifted 5% up,
    // so orbiting still turns about the scene's middle.
    applyOffset() {
        const w = this.container.clientWidth, h = this.container.clientHeight;
        const ox = 0, oy = Math.round(h * 0.05);
        this.persp.setViewOffset(w, h, ox, oy, w, h);
        this.persp.updateProjectionMatrix();
    },

    setLook(name) {
        const L = LOOKS[name] || LOOKS[DEFAULT_LOOK];
        this.look = L; this.lookName = name;
        const c = document.createElement('canvas');
        c.width = 2; c.height = 256;
        const g = c.getContext('2d');
        const grad = g.createLinearGradient(0, 0, 0, 256);
        grad.addColorStop(0, L.background[0]); grad.addColorStop(1, L.background[1]);
        g.fillStyle = grad; g.fillRect(0, 0, 2, 256);
        const bg = new THREE.CanvasTexture(c);
        bg.colorSpace = THREE.SRGBColorSpace;
        this.scene.background = bg;
        this.scene.environment = this.envTex;
        this.scene.environmentIntensity = 0.4;
        this.renderer.toneMapping = THREE.ACESFilmicToneMapping;
        this.renderer.toneMappingExposure = L.exposure || 1;
        this.hemi.intensity = 0.35;
        this.sun.intensity = 2.2;
        this.floor.material.color.set(L.floor.color);
        this.floor.material.emissive.setRGB(...L.floor.glow);
        this.grid.material.color.set(L.floor.grid);
        for (let k = 1; k <= 3; k++) this.vessels[k].setGlass(L);
        this.showLit();
    },

    // Glasses back to their least height: a new run or another model.
    resetGlass() { this.glassMax = [0, 0, 0, 0]; },

    // The compartments' radii and places, from the model's volumes:
    // pk { n, vc, v2, v3 }, in L; n compartments, 1 to 3.
    setShape(pk) {
        this.pk = pk;
        // Floor area from the volume, the largest at most SIZE_RATIO_MAX times
        // the smallest (displayVolumes(), flat.js; Shafer, 2026-11-06); the
        // liquid's height is the concentration.
        // The whole scene is also held to a size the camera frames: the
        // largest vessel's floor area between VESSEL_AREA_MIN and _MAX (its
        // volume in L, as Charles Minto drew it, within those bounds); the
        // ratios are kept (a 100000 L V3 was a flat disc filling the view).
        let dv = displayVolumes([pk.vc, pk.n >= 2 ? pk.v2 : 0, pk.n >= 3 ? pk.v3 : 0]).v;
        const big = Math.max(...dv), fit = big > 0 ? Math.min(VESSEL_AREA_MAX, Math.max(VESSEL_AREA_MIN, big)) / big : 1;
        dv = dv.map(x => x * fit);
        const r1 = Math.sqrt(dv[0]), r2 = Math.sqrt(dv[1]), r3 = Math.sqrt(dv[2]);
        this.geo = {
            n: pk.n, r: [0, r1, r2, r3],
            c: [null, new THREE.Vector3(0, 0, 0),
                pk.n >= 2 ? vesselCentre(2, r1, r2) : null,
                pk.n >= 3 ? vesselCentre(3, r1, r3) : null]
        };
        this.vessels[2].setVisible(pk.n >= 2);
        this.vessels[3].setVisible(pk.n >= 3);
        this.invalidate();
    },

    // Draw one state. ui: { showCe, concScale,
    // units: { conc, amount, rate }, drugRgb, concDecimals, conc }.
    // conc is the arrows' scale, in amount per mL: a flow of conc * 2500
    // per hour (ARROW_MLH) draws an arrow of radius ARROW_R.
    draw(st, ui) {
        // An inhaled anesthetic's moment (gas3d.js).
        if (st.gas) { this.drawGas(st, ui); return; }
        this.invalidate();
        const G = this.geo, L = this.look;
        const hs = ui.concScale;
        const h1 = st.cp * hs, h2 = (st.c2 || 0) * hs, h3 = (st.c3 || 0) * hs, he = st.ce * hs;
        // Each glass as tall as its own liquid needs, at least GLASS_MIN, in
        // steps of 2 so it does not creep.
        const tall = top => Math.max(GLASS_MIN, Math.ceil((top * 1.08 + 0.4) / 2) * 2);
        // A glass grows when its liquid needs it and never shrinks during a
        // run (stepping back, or a falling level, leaves it as tall as it
        // has been); resetGlass() returns them to GLASS_MIN.
        this.glassMax = this.glassMax || [0, 0, 0, 0];
        const glassOf = [0, h1, h2, h3].map((h, k) => k ? (this.glassMax[k] = Math.max(this.glassMax[k], tall(h))) : 0);
        const concs = [0, st.cp, st.c2 || 0, st.c3 || 0];
        for (let k = 1; k <= G.n; k++) {
            const v = this.vessels[k];
            v.setShape(G.c[k], G.r[k], glassOf[k]);
            v.setGlass(L);
            v.setLiquid(concs[k], concs[k] * hs, L, ui.drugRgb, ui.concDecimals);
        }
        // The target: a ring round V1 under TCI-P, a frame round the Ce
        // ribbon under TCI-E.
        this.vessels[1].setTargetRing(st.mode === 'plasma' ? st.target * hs : 0);

        // V1's ripples follow the pump, in ml/h (ui.conc: the arrows' scale); a
        // bolus at this instant sends a ring.
        this.v1Mlh = st.flow.in * 60 / ui.conc;
        if (st.bolus > 0 && this.lastBolusT !== st.t) this.vessels[1].bolusRing();
        this.lastBolusT = st.bolus > 0 ? st.t : null;

        // Arrows. The flow in ml/h of the syringe, ARROW_MLH drawing
        // ARROW_R; colours by what the flow does to V1 (green out of it,
        // red into it).
        const rad = f => ARROW_R * Math.sqrt(Math.abs(f) * 60 / ui.conc / ARROW_MLH);
        const up = new THREE.Vector3(0, 1, 0), down = new THREE.Vector3(0, -1, 0);
        const A = this.arrows, r1 = G.r[1], len = CONNECT_LENGTH;
        // Into V1: the pump, or a bolus at this instant (drawn by its dose).
        const inR = st.bolus > 0 ? Math.max(rad(st.bolus) * 1.0, rad(st.flow.in)) : rad(st.flow.in);
        A.in.set(new THREE.Vector3(0, h1 + len, 0), down, len, inR, L.arrows.in, L);
        A.out.set(new THREE.Vector3(0, 0, 0), down, len, rad(st.flow.out), L.arrows.out, L);
        // Absorption from a depot: into V1 from the front left, the side the
        // other vessels and the ribbon leave free, on the floor as V2's and
        // V3's arrows are.
        const ua = new THREE.Vector3(-Math.sin(ABSORB_TURN), 0, Math.cos(ABSORB_TURN)), ra = rad(st.flow.abs);
        A.abs.set(ua.clone().multiplyScalar(r1 + len).setY(ra), ua.clone().negate(), len, ra, L.arrows.in, L);
        for (const k of [2, 3]) {
            const a = A['to' + k];
            if (G.n < k) { a.group.visible = false; continue; }
            const f = st.flow['to' + k], ar = rad(f);
            const u = G.c[k].clone().setY(0).normalize();
            if (f >= 0) a.set(u.clone().multiplyScalar(r1).setY(ar), u, len, ar, L.arrows.out, L);
            else a.set(u.clone().multiplyScalar(r1 + len).setY(ar), u.clone().negate(), len, ar, L.arrows.in, L);
        }
        // What the arrows and vessels stand for, for the card shown over
        // one; the card follows the numbers while the pointer stays on it.
        this.tipState = { ...st, units: ui.units, dp: ui.concDecimals };
        const tk = this.tipKey;
        if (tk && (tk.includes(':') || (tk[0] === 'c' ? +tk[1] > G.n : !this.arrows[tk].group.visible))) this.tipKey = null;
        this.fillTip();

        // Effect site.
        this.ribbon.place(r1);
        this.ribbon.set(he, L, st.ce, ui.concDecimals);
        this.ribbon.setTarget(st.mode === 'effect' ? st.target * hs : 0);
        this.ribbon.setVisible(ui.showCe);

        // A bolus is marked at the top of the infusion arrow at its instant,
        // an event to see where it happens.
        this.bolusLabel.set(new THREE.Vector3(0, h1 + len + 0.4, 0),
            st.bolus > 0 ? `<b>Bolus</b> ${st.bolus.toFixed(1)} <i>${ui.units.amount}</i>` : '');
        this.bolusLabel.show(st.bolus > 0);
    },

    // ── the view ─────────────────────────────────────────────────
    // The starting view (HOME_*): at start, on every change of drug, and
    // where held Home flies to. Fixed, for every drug: the eye and the aim
    // are the same distance from the middle of V1's base and the floor
    // whatever the model, so a bigger vessel looks bigger because it is.
    resetView() {
        this.flight = null;
        this.persp.up.set(0, 1, 0);
        this.controls.target.set(...HOME_AIM);
        this.persp.position.set(...HOME_EYE);
        this.persp.fov = 60; this.persp.updateProjectionMatrix();
        this.persp.lookAt(this.controls.target);
    },

    // The close-up's eye: LAND_EYE, or, for a V1 whose glass reaches it,
    // further back along the same line from LAND_AIM, so it looks at V1
    // from the same angle and stands ZOOM_GAP outside the glass (as i
    // stops there). Never inside V1.
    landEye() {
        const aim = new THREE.Vector3(...LAND_AIM), off = new THREE.Vector3(...LAND_EYE).sub(aim);
        const R = this.geo ? this.centreGlass().R + ZOOM_GAP : 0;
        return aim.add(off.multiplyScalar(Math.max(1, R / Math.hypot(off.x, off.z))));
    },

    // Held End: fly in to the close-up of V1 (LAND_*); held Home: fly out to
    // the starting view (HOME_*). Both fixed, the same for every drug, and
    // each along ONE smooth curve. The camera is taken about the goal's aim
    // point as a distance (on a log scale), an angle round the vertical and
    // an angle down from the vertical, and moved along a cubic Bezier curve
    // through the start, two inner points and the goal, so a swing round
    // the scene is an arc, never a line through it.
    //   In (End): the first inner point is lifted (out beyond the starting
    //   view's distance and higher than it, at the start's angle; less for
    //   a start near the starting view), the second is the approach (half
    //   way in from the starting view's distance, level with the close-up,
    //   at its angle): it comes down steep and flattens out, like a small
    //   plane landing, and arrives level, heading straight in.
    //   Out (Home): the landing in reverse: the first inner point is the
    //   departure (half way out, level with the start, at its angle), the
    //   second is lifted (out beyond the starting view's distance and
    //   higher, at its angle; less for a start near it): it pulls back
    //   level, climbs, and settles down and in to the starting view.
    // The curve is laid out as points and travelled at an even pace on the
    // log scale (the scene grows or shrinks on screen at an even rate),
    // easing in and out; the aim point turns to the goal's early and the
    // lens eases to the goal's. Released, the camera stays where it is;
    // held again with the view untouched, it carries on along the same
    // curve, else a new one. At the goal already, the key does nothing.
    fly(dir, dt) {
        const c = this.controls.target, p = this.persp;
        // A new flight unless this one goes the same way and the camera is
        // where it left it. The orbit controls re-set the camera every
        // frame, which moves it by a rounding error, so "where it left it"
        // is within a millionth of the distance (an exact test restarted
        // the flight every frame).
        const f = this.flight;
        if (!f || f.dir !== dir || !f.at || p.position.distanceTo(f.at) > 1e-6 * f.at.distanceTo(c) + 1e-9) this.flight = this.planFlight(dir);
        const F = this.flight;
        if (!F.len.length) return;
        F.t = Math.min(1, F.t + dt / F.T);
        const s = F.t * F.t * (3 - 2 * F.t);                 // ease in and out
        // The point at s of the way along the curve's length.
        const L = s * F.len[F.len.length - 1];
        let i = 1;
        while (i < F.len.length - 1 && F.len[i] < L) i++;
        const a = F.len[i] > F.len[i - 1] ? (L - F.len[i - 1]) / (F.len[i] - F.len[i - 1]) : 1;
        p.position.lerpVectors(F.pos[i - 1], F.pos[i], a);
        c.lerpVectors(F.tgt[i - 1], F.tgt[i], a);
        p.fov = F.fov[i - 1] + (F.fov[i] - F.fov[i - 1]) * a;
        p.updateProjectionMatrix();
        // Upright all the way: the curves never look near straight down
        // (the nearest is over 20 degrees off), so upright is always defined.
        p.up.set(0, 1, 0);
        p.lookAt(c);
        F.at = p.position.clone();
    },
    // dir: 'in' (End, to the close-up) or 'out' (Home, to the starting view).
    planFlight(dir) {
        const c = this.controls.target, p = this.persp;
        const start = { pos: p.position.clone(), tgt: c.clone(), fov: p.fov };
        const home = { pos: new THREE.Vector3(...HOME_EYE), tgt: new THREE.Vector3(...HOME_AIM), fov: 60 };
        const land = { pos: this.landEye(), tgt: new THREE.Vector3(...LAND_AIM), fov: 60 };
        const goal = dir === 'in' ? land : home;
        const F = { dir, pos: [], tgt: [], fov: [], len: [], t: 0, T: 0, at: null };
        if (start.pos.distanceTo(goal.pos) < 1e-3 * goal.pos.distanceTo(goal.tgt)) return F;

        const O = goal.tgt;
        // [log distance, angle round, angle down from the vertical]
        const toS = v => { const q = new THREE.Spherical().setFromVector3(v.clone().sub(O)); return [Math.log(Math.max(q.radius, 1e-3)), q.theta, q.phi]; };
        const S = toS(start.pos), G = toS(goal.pos), Hm = toS(home.pos);
        let dTh = G[1] - S[1]; dTh = Math.atan2(Math.sin(dTh), Math.cos(dTh));   // the shorter way round
        G[1] = S[1] + dTh;
        // How far the start is from the starting view, 0 (there) to 1 (half
        // way round, ten times the distance, or well off in height): scales
        // the lift, so a start near the starting view does not climb.
        let dH = Hm[1] - S[1]; dH = Math.atan2(Math.sin(dH), Math.cos(dH));
        const far = dir === 'out' ? Math.min(1, Math.abs(dH) / Math.PI) : Math.min(1, Math.max(Math.abs(dH) / Math.PI, Math.abs(S[0] - Hm[0]) / Math.log(10), Math.abs(S[2] - Hm[2]) / 0.8));
        // (Out, only the swing round counts: from in front, the close-up
        // among them, it pulls straight back: the landing in reverse.)
        // Lifted: out to 20% beyond the starting view's distance (a point
        // further off than that only comes in), and 0.35 rad (20 degrees)
        // higher than the higher of it and the starting view.
        const lnOut = Hm[0] + Math.log(1.2);
        const lift = (P, k) => [P[0] + (Math.max(P[0], lnOut) - P[0]) * k, P[1], P[2] + (Math.max(0.15, Math.min(P[2], Hm[2]) - 0.35) - P[2]) * k];
        let L1, L2;
        if (dir === 'in') {
            L1 = lift(S, far); L1[1] += dTh * 0.15;
            L2 = [(Hm[0] + G[0]) / 2, G[1], G[2]];                 // the approach
        } else {
            L1 = [(S[0] + G[0]) / 2, S[1], S[2]];                  // the departure
            L2 = lift(G, far); L2[1] -= dTh * 0.15;
        }
        const pts = [S, L1, L2, G];
        const at = u => {
            const v = 1 - u, w = [v * v * v, 3 * v * v * u, 3 * v * u * u, u * u * u];
            const x = j => w[0] * pts[0][j] + w[1] * pts[1][j] + w[2] * pts[2][j] + w[3] * pts[3][j];
            return {
                pos: new THREE.Vector3().setFromSpherical(new THREE.Spherical(Math.exp(x(0)), x(2), x(1))).add(O),
                // The aim turns to the goal's early (both inner control points at its aim).
                tgt: start.tgt.clone().multiplyScalar(w[0]).addScaledVector(goal.tgt, w[1] + w[2] + w[3]),
                fov: start.fov + (goal.fov - start.fov) * u
            };
        };

        // Lay the curve out as points; the length of each step on the log
        // scale (its length over the distance to the aim).
        const N = 480;
        for (let j = 0; j <= N; j++) {
            const q = at(j / N);
            if (j) {
                const k = F.pos.length - 1, d = 0.5 * (F.pos[k].distanceTo(F.tgt[k]) + q.pos.distanceTo(q.tgt));
                F.len.push(F.len[k] + F.pos[k].distanceTo(q.pos) / Math.max(d, 1e-3));
            } else F.len.push(0);
            F.pos.push(q.pos); F.tgt.push(q.tgt); F.fov.push(q.fov);
        }
        // About 1.4 s between the starting view and the close-up, up to
        // 1.6 s more from far off.
        F.T = 1.4 + 1.6 * far;
        return F;
    },
    flyEnd() { if (this.flight) this.flight.at = this.persp.position.clone(); },

    // The glass at the origin, that the close-up looks at and i stops at:
    // V1's, or a gas run's lungs (gas3d.js).
    centreGlass() {
        if (this.gasShown) { const v = this.gasVessels.lungs; return { R: glassRadius(v.r), H: v.glassH }; }
        return { R: glassRadius(this.geo.r[1]), H: this.vessels[1].glassH };
    },

    // r / R in cylinders.exe: orbit about the vertical by 0.05 rad.
    orbit(da) {
        const c = this.controls.target, p = this.camera.position;
        const v = p.clone().sub(c);
        v.applyAxisAngle(new THREE.Vector3(0, 1, 0), da);
        p.copy(c).add(v);
        this.camera.up.set(0, 1, 0);
        this.camera.lookAt(c);
    },
    // V / v: the eye up or down by 0.5 (scaled to the view's size).
    moveEye(dy) {
        const d = this.camera.position.distanceTo(this.controls.target);
        this.camera.position.y += dy * Math.max(0.5, d / 40);
        this.camera.up.set(0, 1, 0);
        this.camera.lookAt(this.controls.target);
    },
    // Shift with up/down: the whole view up or down.
    pan(dy) {
        const d = this.camera.position.distanceTo(this.controls.target);
        const s = dy * Math.max(0.5, d / 40);
        this.camera.position.y += s; this.controls.target.y += s;
    },
    // i / o: in or out by 2% about V1, as cylinders.exe did (keyboard.cpp:
    // d_EYE_* times 0.98 or 1.02, the view fixed on the origin, V1's base).
    // The camera and the point it looks at are both scaled about the
    // origin, so the view closes on V1 without swinging. It stops at
    // V1's glass: a step that would take the camera within ZOOM_GAP of
    // the glass wall, base or rim is not taken.
    zoom(f) {
        const p = this.camera.position, c = this.controls.target;
        if (f < 1 && this.geo) {
            const q = p.clone().multiplyScalar(f);
            const g = this.centreGlass(), R = g.R + ZOOM_GAP, H = g.H + ZOOM_GAP;
            if (Math.hypot(q.x, q.z) < R && q.y < H && q.y > -ZOOM_GAP) return;
        }
        p.multiplyScalar(f);
        c.multiplyScalar(f);
    },
    // [ and ]: narrower or wider lens, keeping the framing.
    fov(df) {
        const p = this.persp, c = this.controls.target;
        const old = p.fov, nf = Math.min(100, Math.max(8, old + df));
        const k = Math.tan(old * Math.PI / 360) / Math.tan(nf * Math.PI / 360);
        p.fov = nf; p.updateProjectionMatrix();
        const off = p.position.clone().sub(c).multiplyScalar(k);   // before the camera moves
        p.position.copy(c).add(off);
        return nf;
    }
};
