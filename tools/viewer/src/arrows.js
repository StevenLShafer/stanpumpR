// ═══════════════════════════════════════════════════════════════
//  stanpumpR viewer — the flow arrows
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
//  As cylinders.exe drew them (DynamicDisplays.cpp drawArrow): a shaft
//  0.6 of CONNECT_LENGTH long, then a cone 0.4 long with twice the
//  shaft's radius, closed by a disk. The shaft's radius goes as the
//  square root of the flow (calcRadius), so the arrow's cross-section
//  stands for the flow. One scale for every drug, set by the pump:
//  ARROW_MLH of the syringe draws a shaft of radius ARROW_R, and any
//  flow, in or between the glasses, is drawn on that scale. Colour by
//  what the flow does to V1: green, drug
//  leaving V1 (to V2 or V3, or eliminated); red, drug coming into V1
//  (from the pump, or back from V2 or V3). cylinders.cpp line 611:
//  "positive (green) if speeds fall in V1, neg (red) to hinder fall".
//
//  Placement, also as drawArrow:
//   - into V1 (the pump): from CONNECT_LENGTH above the liquid's top,
//     pointing down onto it;
//   - out of V1 (elimination): from V1's base, pointing down;
//   - V1 to Vk: from V1's wall toward Vk across the gap, sitting on the
//     floor of the scene (raised by its own radius); back from Vk: from
//     Vk's wall toward V1.
// ═══════════════════════════════════════════════════════════════

const ARROW_MLH = 2500, ARROW_R = 2.0;   // 2500 ml/h draws radius 2.0
let ARROW_STRIPES = null;   // shared stripe texture for the flow animation

function stripeTexture() {
    if (ARROW_STRIPES) return ARROW_STRIPES;
    const c = document.createElement('canvas');
    c.width = 4; c.height = 64;
    const g = c.getContext('2d');
    g.fillStyle = '#ffffff'; g.fillRect(0, 0, 4, 64);
    g.fillStyle = '#b8b8b8'; g.fillRect(0, 0, 4, 22);
    ARROW_STRIPES = new THREE.CanvasTexture(c);
    ARROW_STRIPES.wrapS = ARROW_STRIPES.wrapT = THREE.RepeatWrapping;
    ARROW_STRIPES.colorSpace = THREE.SRGBColorSpace;
    return ARROW_STRIPES;
}

class FlowArrow {
    constructor(scene) {
        this.group = new THREE.Group();
        scene.add(this.group);
        this.tex = stripeTexture().clone();
        this.tex.needsUpdate = true;
        this.mat = new THREE.MeshStandardMaterial({ color: 0x00ff00, roughness: 0.35, metalness: 0.05 });
        this.headMat = new THREE.MeshStandardMaterial({ color: 0x00ff00, roughness: 0.35, metalness: 0.05 });
        const sg = new THREE.CylinderGeometry(1, 1, 1, 32, 1, true);
        sg.translate(0, 0.5, 0);
        this.shaft = new THREE.Mesh(sg, this.mat);
        const hg = new THREE.ConeGeometry(1, 1, 32, 1, true);
        hg.translate(0, 0.5, 0);
        this.head = new THREE.Mesh(hg, this.headMat);
        const dg = new THREE.CircleGeometry(1, 32);
        dg.rotateX(Math.PI / 2);                      // facing back down the arrow
        this.disk = new THREE.Mesh(dg, this.headMat);
        const eg = new THREE.CircleGeometry(1, 32);
        eg.rotateX(Math.PI / 2);
        this.tail = new THREE.Mesh(eg, this.mat);     // closes the shaft's start
        for (const m of [this.shaft, this.head, this.disk, this.tail]) { m.castShadow = true; this.group.add(m); }
        this.length = 0;
    }

    // start: where the shaft begins; dir: unit vector; len: the whole arrow.
    set(start, dir, len, radius, color, look) {
        if (!(radius > 1e-4) || !(len > 0)) { this.group.visible = false; return; }
        this.group.visible = true;
        this.group.position.copy(start);
        this.group.quaternion.setFromUnitVectors(new THREE.Vector3(0, 1, 0), dir.clone().normalize());
        const ls = 0.6 * len, lh = 0.4 * len;
        this.shaft.scale.set(radius, ls, radius);
        this.tail.scale.set(radius, 1, radius);
        this.head.position.y = ls;
        this.head.scale.set(2 * radius, lh, 2 * radius);
        this.disk.position.y = ls;
        this.disk.scale.set(2 * radius, 1, 2 * radius);
        for (const m of [this.mat, this.headMat]) {
            m.color.set(color);
            m.emissive.set(color);
            m.emissiveIntensity = look.arrows.emissive;
        }
        this.mat.map = this.tex;
        this.tex.repeat.set(1, ls / 1.2);
        this.length = ls;
    }

    // Stripes move along the shaft, in the flow's direction.
    tick(dt) {
        if (this.group.visible) this.tex.offset.y -= dt * 0.9;
    }
}
