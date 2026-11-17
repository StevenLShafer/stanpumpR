// ═══════════════════════════════════════════════════════════════
//  stanpumpR viewer — the effect site
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
//  As cylinders.exe drew it (DynamicDisplays.cpp drawEffectSite): a thin
//  blue ribbon 0.5 wide, as tall as Ce times the height scale, standing
//  in front of V1 at z = r1 + 4, the whole turned 10 degrees about the
//  vertical, with a line on the floor from V1's wall to its foot. As
//  there, the ribbon is flat: seen edge-on it all but disappears. Here it
//  is 0.9 wide (RIBBON_W), to hold Ce at the size of V1's painted Cp.
//
//  Ce is written across the top of the ribbon, in a pale tint of the
//  ribbon's colour (CE_TINT of the way to white), on a square as
//  wide as the ribbon, on both faces, once the ribbon is CE_SHOW tall
//  (room for the digits with TOP_GAP above and below). The digits' top
//  TOP_GAP below the ribbon's top.
//
//  Under TCI-E the target is a frame round the ribbon, in the target's
//  colour: a thin flat strip, FRAME_W wide, up each edge from the floor to
//  the target's height, one across there and one across the foot, in the
//  ribbon's plane on the
//  front and the back, whatever Ce is (setTarget()). Flat like the
//  ribbon: edge on, it all but disappears with it.
// ═══════════════════════════════════════════════════════════════

const RIBBON_GAP = 4;          // fLocation = r1 + 4.0
const RIBBON_TURN = 10 * Math.PI / 180;
const RIBBON_W = 0.9;            // cylinders.exe's was 0.5; wider to hold Ce at Cp's letter size
const CE_TEX = 256;            // the number's square texture, pixels
const CE_TINT = 0.85;
const CE_SHOW = 0.5;           // the ribbon's height, scene units, from which Ce is written on it
const CE_FONT = `700 ${Math.round(CE_TEX * 0.42)}px system-ui, "Segoe UI", sans-serif`;
const CE_MAX_W = CE_TEX * 0.9;   // the number squeezed to this width if need be

// The targets' lines: fat lines (three's LineSegments2), TARGET_PX screen
// pixels wide at any distance, in the target's colour, as the target in
// the readout card. points: segment ends, x, y, z each.
// Their materials are kept in TARGET_LINE_MATS for the screen size
// (View.resize()).
const TARGET_INK = '#ffd166';
const TARGET_PX = 2;
const FRAME_W = 0.015;          // the TCI-E frame's strips, scene units
const TARGET_LINE_MATS = [];
function targetLine(points) {
    const A = THREE_ADDONS;
    const g = new A.LineSegmentsGeometry();
    g.setPositions(points);
    const m = new A.LineMaterial({ linewidth: TARGET_PX, worldUnits: false, toneMapped: false });
    m.color.setStyle(TARGET_INK, THREE.SRGBColorSpace);
    TARGET_LINE_MATS.push(m);
    return new A.LineSegments2(g, m);
}

// The Ce number's colour: the ribbon's, CE_TINT of the way to white.
function ceInk(look) {
    const hex = parseInt(look.ribbon.slice(1), 16);   // '#rrggbb', as the looks give it
    return 'rgb(' + [hex >> 16, (hex >> 8) & 255, hex & 255]
        .map(v => Math.round(v + (255 - v) * CE_TINT)).join(',') + ')';
}

class EffectRibbon {
    constructor(scene) {
        this.group = new THREE.Group();
        scene.add(this.group);
        this.mat = new THREE.MeshStandardMaterial({ color: 0x0000ff, roughness: 0.3, emissive: 0x0000ff,
            emissiveIntensity: 0.25, side: THREE.DoubleSide });
        const g = new THREE.PlaneGeometry(RIBBON_W, 1);
        g.translate(0, 0.5, 0);
        this.bar = new THREE.Mesh(g, this.mat);
        this.bar.castShadow = true;
        this.group.add(this.bar);

        // Ce on the top square, front and back (the back turned round so
        // it reads left to right from behind), just off the ribbon.
        this.ceCanvas = document.createElement('canvas');
        this.ceCanvas.width = this.ceCanvas.height = CE_TEX;
        this.ceTex = new THREE.CanvasTexture(this.ceCanvas);
        this.ceTex.colorSpace = THREE.SRGBColorSpace;
        this.ceTex.anisotropy = 8;
        const ceMat = new THREE.MeshBasicMaterial({ map: this.ceTex, transparent: true, depthWrite: false, toneMapped: false });
        this.ceText = new THREE.Group();
        for (const side of [1, -1]) {
            const m = new THREE.Mesh(new THREE.PlaneGeometry(RIBBON_W, RIBBON_W), ceMat);
            m.position.z = 0.004 * side;
            if (side < 0) m.rotation.y = Math.PI;
            this.ceText.add(m);
        }
        this.group.add(this.ceText);
        this.ceShown = null;

        // The TCI-E target's frame: strips up each edge, unit height
        // (scaled to the target), one across the top and one across the
        // foot, on both faces, just off the ribbon (as the Ce number is).
        this.frameMat = new THREE.MeshBasicMaterial({ side: THREE.DoubleSide, toneMapped: false });
        this.frameMat.color.setStyle(TARGET_INK, THREE.SRGBColorSpace);
        this.frame = new THREE.Group();
        this.frameSides = [];
        this.frameTops = [];
        const hw = RIBBON_W / 2;
        for (const z of [0.004, -0.004]) {
            for (const x of [-hw, hw]) {
                const g = new THREE.PlaneGeometry(FRAME_W, 1);
                g.translate(x, 0.5, z);
                const m = new THREE.Mesh(g, this.frameMat);
                this.frame.add(m);
                this.frameSides.push(m);
            }
            const g = new THREE.PlaneGeometry(RIBBON_W + FRAME_W, FRAME_W);
            g.translate(0, 0, z);
            const m = new THREE.Mesh(g, this.frameMat);
            this.frame.add(m);
            this.frameTops.push(m);
            this.frame.add(new THREE.Mesh(g, this.frameMat));   // the foot, at 0
        }
        this.frame.visible = false;
        this.group.add(this.frame);


        this.lineMat = new THREE.MeshStandardMaterial({ color: 0x0000ff, roughness: 0.5 });
        const lg = new THREE.CylinderGeometry(0.04, 0.04, 1, 8);
        lg.rotateX(Math.PI / 2);
        lg.translate(0, 0.04, 0.5);
        this.line = new THREE.Mesh(lg, this.lineMat);
        scene.add(this.line);

        this.r1 = 0;
    }

    // Placed for V1's radius: z = r1 + 4, turned 10 degrees.
    place(r1) {
        this.r1 = r1;
        const z = r1 + RIBBON_GAP;
        this.group.position.set(z * Math.sin(RIBBON_TURN), 0, z * Math.cos(RIBBON_TURN));
        this.group.rotation.y = RIBBON_TURN;
        // The floor line from V1's wall (0, 0, r1) to the ribbon, turned;
        // it stops 0.005 short of the ribbon's plane, so its end does not
        // show through the ribbon's front.
        this.line.rotation.y = RIBBON_TURN;
        this.line.position.set(r1 * Math.sin(RIBBON_TURN), 0, r1 * Math.cos(RIBBON_TURN));
        this.line.scale.set(1, 1, RIBBON_GAP - 0.005);
    }

    // height: the ribbon's, scene units; ce: the number written on it, to
    // dp decimals.
    set(height, look, ce, dp = 2) {
        this.bar.visible = height > 1e-4;
        this.bar.scale.y = Math.max(height, 1e-4);
        this.ceText.visible = height >= CE_SHOW && ce != null;
        if (this.ceText.visible) {
            this.ceText.position.y = height - RIBBON_W / 2;
            const txt = ce.toFixed(dp);
            const ink = ceInk(look);
            if (txt + ink !== this.ceShown) {
                this.ceShown = txt + ink;
                const g = this.ceCanvas.getContext('2d');
                g.clearRect(0, 0, CE_TEX, CE_TEX);
                g.fillStyle = ink;
                g.textAlign = 'center'; g.textBaseline = 'alphabetic';
                g.font = CE_FONT;
                const top = TOP_GAP / RIBBON_W * CE_TEX;     // the digits' top, below the ribbon's top
                g.fillText(txt, CE_TEX / 2, top + g.measureText(txt).actualBoundingBoxAscent, CE_MAX_W);
                this.ceTex.needsUpdate = true;
            }
        }
        this.mat.color.set(look.ribbon);
        this.mat.emissive.set(look.ribbon);
        this.lineMat.color.set(look.ribbon);
    }

    // The TCI-E target's frame at height (scene units); none when height
    // is 0 (no TCI-E target).
    setTarget(height) {
        this.frame.visible = height > 1e-4;
        if (!this.frame.visible) return;
        for (const m of this.frameSides) m.scale.y = height;
        for (const m of this.frameTops) m.position.y = height;
    }

    // The floor line only with the ribbon: none before Ce rises off zero.
    setVisible(v) { this.group.visible = v; this.line.visible = v && this.bar.visible; }

}

