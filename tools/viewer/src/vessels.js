// ═══════════════════════════════════════════════════════════════
//  stanpumpR viewer — the compartments
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
//  As cylinders.exe drew them (DynamicDisplays.cpp drawCylinders,
//  calcRadius): each compartment a cylinder of radius sqrt(V), standing
//  on y = 0, its liquid as tall as the concentration times the height
//  scale, so the liquid's volume stands for the amount. V1 at the
//  origin; V2 turned -45 degrees and V3 +45 degrees about the vertical,
//  each r1 + CONNECT_LENGTH + r away (case 2 and case 3: glRotatef then
//  glTranslatef). With the camera in front (+z) that puts V2 back left
//  and V3 back right, as in the screenshot.
//
//  The glass is new: an open glass beaker round the liquid, so an empty
//  compartment is still there to see.
// ═══════════════════════════════════════════════════════════════

const CONNECT_LENGTH = 8;           // d_CONNECT_LENGTH, init.cpp 125
// The gap between the liquid and its glass, the same width for every
// vessel (scene units), not a share of the radius, so a large V3 has no
// wider gap than V1. 0.07 is the gap V1 had at the default adult.
const GLASS_GAP = 0.07;
function glassRadius(r) { return r + GLASS_GAP; }
const RIM_T = 0.05;                 // rim thickness, the same for every vessel, like a set of glassware

// The liquid's top surface. A meniscus: the surface rises at the glass,
// as water-based liquids do (propofol is an oil-in-water emulsion), by
// MENISCUS_H over the last MENISCUS_W of the radius; the level read is
// the flat middle. Ripples (V1 only, while the clock runs): rings
// travelling out from the middle, where the infusion arrow meets the
// surface, their height the square root of the rate in ml/h, RIPPLE_MAX
// at RIPPLE_MLH and taller above it, the same for every drug; they grow
// to a new height over RIPPLE_GROW and die away over RIPPLE_FADE (time
// constants, screen time). A bolus sends one larger ring out. Scene
// units and seconds.
const MENISCUS_H = 0.08, MENISCUS_W = 0.15;
const RIPPLE_MAX = 0.12, RIPPLE_MLH = 1200, RIPPLE_WAVE = 0.6, RIPPLE_SPEED = 1.2;
const RIPPLE_GROW = 0.15, RIPPLE_FADE = 1;
const BOLUS_RING_H = 0.25, BOLUS_RING_W = 0.25, BOLUS_RING_SPEED = 2.0;
const SURF_RINGS = 48, SURF_SEGS = 96;

// V1's Cp, painted on the front of its liquid (the side facing the
// starting view, +z) and again on the back, just below the surface, curving with the cylinder
// and riding up and down with the level. Fixed, like paint: orbit round
// and it goes out of sight behind the curve. In a darker shade of the
// liquid's own colour (CP_SHADE of it), as if part of the liquid. A band
// CP_BAND_H tall, its top at the surface, the digits' top TOP_GAP below
// it, as Ce's on the ribbon is below the ribbon's top;
// CP_BAND_ASPECT times as long round the cylinder as it is tall (at most
// CP_ARC_MAX round, the number squeezed to fit on a narrow V1); shown
// once the liquid is tall enough to hold it.
// The gap from the top edge to the digits' top, scene units: V1's Cp below
// its surface and Ce below the ribbon's top.
const TOP_GAP = 0.1;
const CP_BAND_H = 0.54, CP_BAND_ASPECT = 4, CP_ARC_MAX = 2 * Math.PI / 3;
const CP_TEX_W = 512, CP_TEX_H = 128;
const CP_SHADE = 0.55;

// A disc of radius 1 at y = 0 as rings of points, so its height can be
// set ring by ring: the middle point, then SURF_RINGS rings of SURF_SEGS
// points, ring i at distance i / SURF_RINGS from the middle.
function surfaceGeometry() {
    const pos = [], idx = [];
    pos.push(0, 0, 0);
    for (let i = 1; i <= SURF_RINGS; i++) {
        const q = i / SURF_RINGS;
        for (let j = 0; j < SURF_SEGS; j++) {
            const a = 2 * Math.PI * j / SURF_SEGS;
            pos.push(q * Math.cos(a), 0, -q * Math.sin(a));
        }
    }
    const at = (i, j) => 1 + (i - 1) * SURF_SEGS + (j % SURF_SEGS);
    for (let j = 0; j < SURF_SEGS; j++) idx.push(0, at(1, j), at(1, j + 1));
    for (let i = 1; i < SURF_RINGS; i++)
        for (let j = 0; j < SURF_SEGS; j++)
            idx.push(at(i, j), at(i + 1, j), at(i + 1, j + 1), at(i, j), at(i + 1, j + 1), at(i, j + 1));
    const g = new THREE.BufferGeometry();
    g.setAttribute('position', new THREE.Float32BufferAttribute(pos, 3));
    g.setIndex(idx);
    g.computeVertexNormals();
    // Each segment's direction out from the middle, for the normals.
    g.userData.cos = Array.from({ length: SURF_SEGS }, (_, j) => Math.cos(2 * Math.PI * j / SURF_SEGS));
    g.userData.sin = Array.from({ length: SURF_SEGS }, (_, j) => Math.sin(2 * Math.PI * j / SURF_SEGS));
    return g;
}

// Where compartment k's centre is, from the radii. cylinders.cpp's
// rotations, written out: glRotatef(a, 0, 1, 0) then glTranslatef(x, 0, 0)
// puts the centre at (x cos a, 0, -x sin a).
function vesselCentre(k, r1, rk) {
    const d = r1 + CONNECT_LENGTH + rk;
    const deg = k === 2 ? -45 : 45;
    const x = k === 2 ? -d : d;
    const a = deg * Math.PI / 180;
    return new THREE.Vector3(x * Math.cos(a), 0, -x * Math.sin(a));
}

class Vessel {
    constructor(scene, k) {
        this.k = k;
        this.group = new THREE.Group();
        scene.add(this.group);

        // Liquid: a unit cylinder standing on y = 0, scaled to (r, h, r).
        // Materials: side, top, bottom.
        const lg = new THREE.CylinderGeometry(1, 1, 1, 96, 1, false);
        lg.translate(0, 0.5, 0);
        this.sideMat = new THREE.MeshPhysicalMaterial({ color: 0xffffff });
        this.topMat = new THREE.MeshPhysicalMaterial({ color: 0xffffff });
        this.botMat = new THREE.MeshPhysicalMaterial({ color: 0xffffff });
        // The cylinder's own flat top is not drawn: the surface below is.
        this.liquid = new THREE.Mesh(lg, [this.sideMat, new THREE.MeshBasicMaterial({ visible: false }), this.botMat]);
        this.liquid.castShadow = true;
        this.liquid.receiveShadow = true;
        this.group.add(this.liquid);
        this.surface = new THREE.Mesh(surfaceGeometry(), this.topMat);
        this.surface.receiveShadow = true;
        this.group.add(this.surface);
        this.men = 0;               // this liquid's meniscus height
        this.phase = 0;             // ripple phase, advanced while the clock runs
        this.amp = 0;               // ripple height now, easing to the rate's
        this.ring = null;           // a bolus ring: its age in seconds
        this.flat = false;          // the surface is meniscus only

        // V1 only: Cp painted on the liquid (CP_BAND_*).
        if (k === 1) {
            this.cpCanvas = document.createElement('canvas');
            this.cpCanvas.width = CP_TEX_W; this.cpCanvas.height = CP_TEX_H;
            this.cpTex = new THREE.CanvasTexture(this.cpCanvas);
            this.cpTex.colorSpace = THREE.SRGBColorSpace;
            this.cpTex.anisotropy = 8;
            this.cpMat = new THREE.MeshBasicMaterial({ map: this.cpTex, transparent: true, depthWrite: false,
                toneMapped: false, polygonOffset: true, polygonOffsetFactor: -2 });
            this.cpBand = null; this.cpBandR = 0; this.cpShown = null;
        }

        // Glass: an open tube, a base, and a rim at the top. Plain see-through
        // (blended over what is behind it, the look's glass.opacity), not
        // three.js's transmission: through a transmission glass only solid
        // things showed, so another glass's wall and rim behind it vanished.
        // It also costs the GPU about half as much a frame. Real glass shows
        // more at a grazing view than face on: the opacity rises from
        // glass.opacity toward glass.edge as (1 - |n.v|)^glass.edgePower,
        // added to three.js's own shader below.
        const gg = new THREE.CylinderGeometry(1, 1, 1, 96, 1, true);
        gg.translate(0, 0.5, 0);
        this.glassMat = new THREE.MeshPhysicalMaterial({
            color: 0xffffff, metalness: 0, roughness: 0.05,
            transparent: true, opacity: 0.18, depthWrite: false,
            side: THREE.DoubleSide, envMapIntensity: 1.2
        });
        this.edge = { edgeOpacity: { value: 0 }, edgePower: { value: 1 } };
        this.glassMat.onBeforeCompile = sh => {
            sh.uniforms.edgeOpacity = this.edge.edgeOpacity;
            sh.uniforms.edgePower = this.edge.edgePower;
            sh.fragmentShader = 'uniform float edgeOpacity;\nuniform float edgePower;\n' + sh.fragmentShader.replace(
                '#include <opaque_fragment>',
                'float graze = 1.0 - abs( dot( normalize( normal ), normalize( vViewPosition ) ) );\n' +
                'diffuseColor.a = mix( diffuseColor.a, edgeOpacity, pow( graze, edgePower ) );\n' +
                '#include <opaque_fragment>');
        };
        this.glassMat.customProgramCacheKey = () => 'glass-edge';
        this.glass = new THREE.Mesh(gg, this.glassMat);
        this.glass.renderOrder = 2;
        this.group.add(this.glass);
        const bg = new THREE.CylinderGeometry(1, 1, 1, 96);
        bg.translate(0, -0.5, 0);
        this.base = new THREE.Mesh(bg, this.glassMat);
        // A hair below the liquid's bottom, not in its plane: two discs in
        // one plane flicker in radial lines (seen from below, through the
        // base), as the graphics card cannot tell which is in front.
        this.base.position.y = -0.01;
        // The glass, its base and its rim are drawn after the floor's grid
        // (renderOrder 2; the grid is see-through too, order 0): drawn
        // after them, the grid's lines showed over the glass.
        this.base.renderOrder = 2;
        this.base.castShadow = true;
        this.group.add(this.base);
        this.rimMat = new THREE.MeshPhysicalMaterial({ color: 0xffffff, roughness: 0.1, transparent: true, opacity: 0.6, depthWrite: false });
        this.rim = null;

        this.r = 0;
        this.h = 0;
        this.glassH = 0;
    }

    // Place and size the vessel: centre, liquid radius, glass height.
    setShape(centre, r, glassH) {
        this.group.position.copy(centre);
        if (r !== this.r) {
            this.r = r;
            if (this.rim) { this.group.remove(this.rim); this.rim.geometry.dispose(); }
            const rg = new THREE.TorusGeometry(glassRadius(r), RIM_T, 12, 96);
            rg.rotateX(Math.PI / 2);
            this.rim = new THREE.Mesh(rg, this.rimMat);
            this.rim.renderOrder = 2;
            this.group.add(this.rim);
        }
        const R = glassRadius(r);
        this.glass.scale.set(R, glassH, R);
        this.base.scale.set(R, 0.05, R);            // flush with the tube, thin
        this.rim.position.y = glassH;
        this.glassH = glassH;
    }

    // The liquid for this concentration, in the look's style.
    // dp: decimals for the concentration painted on V1.
    setLiquid(conc, height, look, drugRgb, dp) {
        const h = Math.max(0, height);
        this.h = h;
        this.liquid.visible = h > 1e-4;
        // The meniscus shrinks with a shallow liquid; the side reaches the
        // meniscus's top so no gap shows at the wall.
        this.men = MENISCUS_H * Math.min(1, h / (4 * MENISCUS_H));
        this.liquid.scale.set(this.r, Math.max(h + this.men, 1e-4), this.r);
        this.surface.visible = h > 1e-4;
        this.surface.position.y = h;
        this.surface.scale.set(this.r, 1, this.r);
        this.flat = false;
        this.shapeSurface();
        const L = look.liquid;
        const style = (LIQUID_STYLES[L.style] || LIQUID_STYLES.solid)({ drug: drugRgb });
        for (const m of [this.sideMat, this.topMat, this.botMat]) {
            m.roughness = L.roughness; m.metalness = 0;
            m.clearcoat = L.clearcoat || 0; m.clearcoatRoughness = 0.2;
            m.transparent = L.opacity < 1; m.opacity = L.opacity;
        }
        this.sideMat.color.setRGB(...style.base, THREE.SRGBColorSpace);
        this.topMat.color.setRGB(...style.top, THREE.SRGBColorSpace);
        this.botMat.color.setRGB(...style.base, THREE.SRGBColorSpace);
        if (this.k === 1) this.paintCp(conc, h, style.base, dp);
    }

    // V1 only: the TCI-P target, a ring round the glass at height (scene
    // units), round the liquid when the look has no glass; a targetLine()
    // (ribbon.js), so TARGET_PX pixels wide in the target's colour. None
    // when height is 0 (no TCI-P target).
    setTargetRing(height) {
        if (!this.tring) {
            const pts = [], n = 128;
            for (let i = 0; i < n; i++) {
                const a = 2 * Math.PI * i / n, b = 2 * Math.PI * (i + 1) / n;
                pts.push(Math.cos(a), 0, Math.sin(a), Math.cos(b), 0, Math.sin(b));
            }
            this.tring = targetLine(pts);
            this.group.add(this.tring);
        }
        this.tring.visible = height > 1e-4;
        if (!this.tring.visible) return;
        const R = glassRadius(this.r);
        this.tring.scale.set(R, 1, R);
        this.tring.position.y = height;
    }

    // V1's painted Cp: the band rebuilt when the radius changes, moved
    // with the level, redrawn when the number or the liquid's colour
    // (rgb, 0 to 1) changes.
    paintCp(conc, h, rgb, dp = 2) {
        const r = this.r * 1.003;               // just outside the liquid's side
        if (r !== this.cpBandR) {
            this.cpBandR = r;
            if (this.cpBand) { this.group.remove(this.cpBand); this.cpBand.children[0].geometry.dispose(); }
            const arc = Math.min(CP_ARC_MAX, CP_BAND_ASPECT * CP_BAND_H / r);
            // Three's cylinder puts angle 0 at +z and x = r sin(angle), so
            // an arc centred on 0 faces +z and reads left to right. A second
            // copy turned half way round is on the back, reading left to
            // right from behind.
            const g = new THREE.CylinderGeometry(r, r, CP_BAND_H, 48, 1, true, -arc / 2, arc);
            this.cpBand = new THREE.Group();
            for (const turn of [0, Math.PI]) {
                const m = new THREE.Mesh(g, this.cpMat);
                m.rotation.y = turn;
                m.renderOrder = 1;
                this.cpBand.add(m);
            }
            this.cpBand.userData.arcLen = arc * r;
            this.group.add(this.cpBand);
        }
        this.cpBand.visible = h >= CP_BAND_H + 0.1;
        if (!this.cpBand.visible) return;
        this.cpBand.position.y = h - CP_BAND_H / 2;
        const txt = conc.toFixed(dp);
        const ink = 'rgb(' + rgb.map(v => Math.round(255 * CP_SHADE * v)).join(',') + ')';
        if (txt + ink === this.cpShown) return;
        this.cpShown = txt + ink;
        const g = this.cpCanvas.getContext('2d');
        g.clearRect(0, 0, CP_TEX_W, CP_TEX_H);
        g.fillStyle = ink;
        g.textAlign = 'center'; g.textBaseline = 'alphabetic';
        // Letters 70% of the band's height; on a band shorter round than
        // CP_BAND_ASPECT (a narrow V1) the canvas is squeezed onto it, so
        // the letters are drawn that much wider to come out true.
        const squeeze = this.cpBand.userData.arcLen / (CP_BAND_ASPECT * CP_BAND_H);
        g.save();
        g.scale(1 / squeeze, 1);
        g.font = `600 ${Math.round(CP_TEX_H * 0.7)}px system-ui, "Segoe UI", sans-serif`;
        const top = TOP_GAP / CP_BAND_H * CP_TEX_H;          // the digits' top, below the surface
        g.fillText(txt, CP_TEX_W * squeeze / 2, top + g.measureText(txt).actualBoundingBoxAscent, CP_TEX_W * squeeze * 0.92);
        g.restore();
        this.cpTex.needsUpdate = true;
    }

    // Heights of the surface's points: meniscus, ripples, bolus ring.
    // Each depends only on the distance from the middle, so the height
    // and its slope are worked out once per ring and copied round it.
    // The normals come from the slope (dy/dd, d the distance in scene
    // units), in the surface's own unscaled space, where a slope s along
    // the radius tilts the normal by s * r: three.js scales them back.
    shapeSurface() {
        const g = this.surface.geometry, p = g.attributes.position.array, n = g.attributes.normal.array;
        const cs = g.userData.cos, sn = g.userData.sin, r = this.r;
        const k = 2 * Math.PI / RIPPLE_WAVE;
        const ripple = this.amp > 1e-4;
        const ringR = this.ring ? this.ring.age * BOLUS_RING_SPEED : 0;
        const ringH = this.ring ? BOLUS_RING_H * Math.exp(-1.5 * this.ring.age) : 0;
        for (let i = 0; i <= SURF_RINGS; i++) {
            const q = i / SURF_RINGS, d = q * r, fade = 1 - q * q, dfade = -2 * q / r;
            const em = this.men * Math.exp(-(1 - q) * r / MENISCUS_W);
            let y = em, s = em / MENISCUS_W;
            if (ripple) {
                const ph = k * d - this.phase;
                y += this.amp * Math.sin(ph) * fade;
                s += this.amp * (k * Math.cos(ph) * fade + Math.sin(ph) * dfade);
            }
            if (ringH > 1e-4) {
                const z = (d - ringR) / BOLUS_RING_W, e = ringH * Math.exp(-z * z);
                y += e * fade;
                s += e * (-2 * z / BOLUS_RING_W * fade + dfade);
            }
            // Local normal: (-s r u, 1) normalised, u the way out from the
            // middle; straight up at the middle point.
            const t = -s * r, len = Math.hypot(t, 1), nr = t / len, ny = 1 / len;
            if (i === 0) {
                p[1] = y; n[0] = 0; n[1] = 1; n[2] = 0;
                continue;
            }
            for (let j = 0, v = 3 * (1 + (i - 1) * SURF_SEGS); j < SURF_SEGS; j++, v += 3) {
                p[v + 1] = y;
                n[v] = nr * cs[j]; n[v + 1] = ny; n[v + 2] = -nr * sn[j];
            }
        }
        g.attributes.position.needsUpdate = true;
        g.attributes.normal.needsUpdate = true;
    }

    // V1's ripples, each frame. mlh: the pump's rate now, ml/h; running: the
    // clock is moving. Stopped, the ripples settle and the surface stills.
    tickRipples(dt, mlh, running) {
        const target = running ? RIPPLE_MAX * Math.sqrt(Math.max(0, mlh) / RIPPLE_MLH) : 0;
        const tau = target > this.amp ? RIPPLE_GROW : RIPPLE_FADE;
        this.amp += (target - this.amp) * Math.min(1, dt / tau);
        if (running) {
            this.phase += dt * 2 * Math.PI * RIPPLE_SPEED / RIPPLE_WAVE;
            if (this.ring) { this.ring.age += dt; if (this.ring.age * BOLUS_RING_SPEED > this.r + 1) this.ring = null; }
        }
        const moving = this.amp > 1e-4 || this.ring;
        if (moving) { this.shapeSurface(); this.flat = false; }
        else if (!this.flat) { this.amp = 0; this.shapeSurface(); this.flat = true; }
    }

    bolusRing() { this.ring = { age: 0 }; }

    setGlass(look) {
        const G = look.glass;
        this.glassMat.color.set(G.tint);
        this.glassMat.roughness = G.roughness;
        this.glassMat.opacity = G.opacity;
        this.glassMat.envMapIntensity = G.reflect;
        this.edge.edgeOpacity.value = G.edge;
        this.edge.edgePower.value = G.edgePower;
        this.rimMat.opacity = G.rim;
    }

    setVisible(v) { this.group.visible = v; }
}
