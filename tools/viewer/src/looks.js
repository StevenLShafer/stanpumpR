// ═══════════════════════════════════════════════════════════════
//  stanpumpR viewer — looks
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
//  Everything about how the scene looks, and nothing about what it
//  shows, is here: background, floor, glass, liquid, arrows, ribbon.
//  A look is a plain object in LOOKS; the viewer uses DEFAULT_LOOK. The
//  liquid's colouring is a named style from LIQUID_STYLES: one colour,
//  the drug's, a touch lighter on the top surface.
// ═══════════════════════════════════════════════════════════════

function hexRgb(hex) {
    const v = parseInt(hex.replace('#', ''), 16);
    return [(v >> 16 & 255) / 255, (v >> 8 & 255) / 255, (v & 255) / 255];
}
function shade(c, k) { return c.map(v => Math.min(1, Math.max(0, v * k))); }

// ── liquid styles ─────────────────────────────────────────────────
// Each returns { top: [r,g,b], base: [r,g,b] } from ctx: { drug: [r,g,b] }.
const LIQUID_STYLES = {
    // One colour: the drug's.
    solid: ctx => ({ top: shade(ctx.drug, 1.08), base: ctx.drug })
};

// ── the looks ─────────────────────────────────────────────────────
const LOOKS = {
    'Glass': {
        background: ['#0e1a24', '#24435a'],        // top, bottom of the sky gradient
        // glow: the floor's own light (linear RGB), standing in for the
        // environment's light, which the floor's plain matte material
        // does not take; matched to the floor's mean colour on screen.
        floor: { color: '#1b2c38', glow: [0.0459, 0.0651, 0.0831], grid: '#2f4b5e' },
        // Plain see-through glass matched to the earlier three.js
        // transmission glass (rendered from three views, compared in
        // 24 px blocks): nearly clear face on (opacity), more opaque toward
        // a grazing view (edge, rising as the grazing term to the power
        // edgePower), a blue tint, little reflection (reflect, the
        // environment's strength), rims at rim.
        glass: { tint: '#2261bc', roughness: 0.034, opacity: 0.039,
                 edge: 0.95, edgePower: 7.9, reflect: 0.034, rim: 0.75 },
        liquid: { style: 'solid', roughness: 0.18, transmission: 0.0, opacity: 1, clearcoat: 0.6 },
        arrows: { out: '#25d366', in: '#ff4d4d', emissive: 0.0 },
        ribbon: '#3b82f6',
        exposure: 1.0
    }
};
const DEFAULT_LOOK = 'Glass';
