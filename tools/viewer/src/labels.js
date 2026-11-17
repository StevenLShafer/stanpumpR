// ═══════════════════════════════════════════════════════════════
//  stanpumpR viewer — text in the scene
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
//  cylinders.exe drew "Cp: 4.00 u/l" with glutBitmapCharacter at
//  (-r1, Cp + 0.5, r1/2) and "Ce: ..." at (0, Ce + 0.5, r1 + 4). Here Cp
//  is painted on V1's liquid and Ce on the ribbon; a vessel's or an
//  arrow's numbers are in the card shown over it. A bolus is an HTML label pinned to a 3D point above the infusion arrow
//  (SceneLabel, CSS2DRenderer), so it stays sharp.
// ═══════════════════════════════════════════════════════════════

class SceneLabel {
    // top: the 3D point is the label's top centre (hangs below it);
    // otherwise its bottom centre (sits above it).
    constructor(scene, cls, top) {
        this.el = document.createElement('div');
        this.el.className = 'lbl ' + (cls || '');
        this.obj = new THREE_ADDONS.CSS2DObject(this.el);
        this.obj.center.set(0.5, top ? 0 : 1);
        scene.add(this.obj);
    }
    set(pos, html) {
        this.obj.position.copy(pos);
        if (this.html !== html) { this.el.innerHTML = html; this.html = html; }
    }
    show(v) { this.obj.visible = v; this.el.style.display = v ? '' : 'none'; }
}
