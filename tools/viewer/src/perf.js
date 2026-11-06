// ═══════════════════════════════════════════════════════════════
//  stanpumpR viewer — performance (key p)
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
//  A card at the top right, shown and hidden by p (not listed under h: it
//  is for finding out why the viewer is slow on a given machine). Twice
//  a second: the frames drawn per second; our JavaScript per frame (the
//  player's and the view keys' frame, and the scene's frame, three.js's
//  own work for the render included), mean and worst; the GPU's time to
//  draw the 3D picture, from its own timer
//  (EXT_disjoint_timer_query_webgl2), mean and worst, "–" where the
//  browser has no such timer; the time a frame has (the shortest gap
//  between the browser's frame times, the screen's refresh); and the
//  canvas's pixels. Nothing is measured while the card is hidden. It does
//  not see the browser's own work after ours (placing the HTML labels,
//  putting the page on the screen).
// ═══════════════════════════════════════════════════════════════

const Perf = {
    el: null, shown: false,
    gl: null, ext: null, pending: [],
    init(el) { this.el = el; this.reset(performance.now()); },
    toggle() {
        this.shown = !this.shown;
        this.el.classList.toggle('show', this.shown);
        this.reset(performance.now());
        this.el.innerHTML = '<div class="phead">Performance <kbd>p</kbd></div><div class="pbody">measuring…</div>';
    },
    reset(now) {
        this.t0 = now; this.lastFrame = null; this.frames = 0; this.minGap = Infinity;
        this.js = 0; this.jsMax = 0; this.jsFrame = 0;
        this.gpu = 0; this.gpuMax = 0; this.gpuN = 0;
    },
    // The player's and the view keys' frame work (player.js), added to the scene's for the frame.
    addJs(ms) { if (this.shown) this.jsFrame += ms; },
    // The 3D render, inside a GPU timer when the card is shown.
    render(renderer, draw) {
        if (!this.shown) { draw(); return; }
        if (!this.gl) {
            this.gl = renderer.getContext();
            this.ext = this.gl.getExtension('EXT_disjoint_timer_query_webgl2');
        }
        if (!this.ext) { draw(); return; }
        const gl = this.gl, q = gl.createQuery();
        gl.beginQuery(this.ext.TIME_ELAPSED_EXT, q);
        draw();
        gl.endQuery(this.ext.TIME_ELAPSED_EXT);
        this.pending.push(q);
    },
    // End of the scene's frame: ts the browser's time for the frame, ms
    // its JavaScript. The frame times fall on the screen's refreshes, so
    // the shortest gap between them is one refresh, however late our own
    // code starts (its clock gave false short gaps after a late frame).
    frame(ts, ms, renderer) {
        if (!this.shown) return;
        if (this.lastFrame != null && ts > this.lastFrame) this.minGap = Math.min(this.minGap, ts - this.lastFrame);
        this.lastFrame = ts;
        this.frames++;
        const js = ms + this.jsFrame;
        this.jsFrame = 0;
        this.js += js; this.jsMax = Math.max(this.jsMax, js);
        // GPU results arrive a frame or two later; a disjoint one (the GPU
        // was interrupted) is dropped.
        if (this.ext) {
            const gl = this.gl, disjoint = gl.getParameter(this.ext.GPU_DISJOINT_EXT);
            while (this.pending.length && gl.getQueryParameter(this.pending[0], gl.QUERY_RESULT_AVAILABLE)) {
                const q = this.pending.shift();
                if (!disjoint) {
                    const g = gl.getQueryParameter(q, gl.QUERY_RESULT) / 1e6;
                    this.gpu += g; this.gpuN++; this.gpuMax = Math.max(this.gpuMax, g);
                }
                gl.deleteQuery(q);
            }
        }
        const now = performance.now();
        if (now - this.t0 >= 500) { this.show(renderer, now); this.reset(now); }
    },
    show(renderer, now) {
        const s = (now - this.t0) / 1000, n = this.frames;
        const c = renderer.domElement, pr = renderer.getPixelRatio();
        const gap = isFinite(this.minGap) ? this.minGap : null;
        const f1 = x => x.toFixed(1);
        const gpu = !this.ext ? '<td colspan="2">– (no GPU timer in this browser)</td>'
            : this.gpuN ? `<td>${f1(this.gpu / this.gpuN)} ms</td><td>worst ${f1(this.gpuMax)}</td>` : '<td>–</td><td></td>';
        this.el.innerHTML = `
          <div class="phead">Performance <kbd>p</kbd></div>
          <table class="perf">
            <tr><td class="k">Frames</td><td>${Math.round(n / s)} /s</td><td>${gap ? Math.round(1000 / gap) + ' possible' : ''}</td></tr>
            <tr><td class="k">Our code</td><td>${f1(this.js / Math.max(1, n))} ms</td><td>worst ${f1(this.jsMax)}</td></tr>
            <tr><td class="k">GPU</td>${gpu}</tr>
            <tr><td class="k">Frame</td><td>${gap ? f1(gap) + ' ms' : '–'}</td><td>available</td></tr>
            <tr><td class="k">Canvas</td><td colspan="2">${c.width} × ${c.height} (${(c.width * c.height / 1e6).toFixed(1)} M pixels, scale ${pr})</td></tr>
          </table>`;
    }
};
