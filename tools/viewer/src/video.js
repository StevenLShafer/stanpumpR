// ═══════════════════════════════════════════════════════════════
//  stanpumpR viewer — save the run as an MP4 video
//
//  Written for stanpumpR by Claude Code (Anthropic), 2026-11-07, at the
//  request of Steven L. Shafer.
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
//  The whole run, from its first time to its last, at the speed set, in the
//  view shown (2D or 3D, the 3D one from the current viewpoint), as an H.264
//  MP4 that plays in PowerPoint, Keynote and on phones. Everything is done in
//  this window: each frame is drawn at a fixed step (not recorded as it
//  plays, so no frame is dropped and a long run takes less time to save than
//  to watch), encoded by the browser's own WebCodecs encoder, and packed into
//  an MP4 here (mp4()); nothing is sent anywhere and nothing new runs on the
//  server. The readout, the amounts card and the 3D view's labels are page
//  elements over the picture, so they are painted onto each frame
//  (paintElement()), and every video carries a strip naming stanpumpR, the
//  drug, the model and stanpumpR's intended use, since a video travels
//  without the app around it. The patient (weight, height, age, sex) is left
//  out unless the box asking for it is ticked.
// ═══════════════════════════════════════════════════════════════

const VIDEO_FPS = 30;
const VIDEO_MAX_SECONDS = 300;              // the longest video: 5 minutes
const VIDEO_MAX_W = 1920, VIDEO_MAX_H = 1080;
const VIDEO_BITS_PER_PIXEL = 0.1;           // per frame: about 5 Mbit/s at 1620 x 1080
// The codecs tried, in order: H.264 High, Main and Baseline, level 4.0 (to 1920 x 1080).
const VIDEO_CODECS = ['avc1.640028', 'avc1.4d0028', 'avc1.42e028'];
const VIDEO_USE = 'stanpumpR is an advisory simulator intended to facilitate perioperative patient care, ' +
    'teaching and research. It does not control drug delivery and has not been reviewed or cleared by any regulatory agency.';

const Video = {
    busy: false, cancelled: false,
    pending: null,      // a run that arrived while saving, loaded when the save ends
    refused: null,      // the message for a run refused after it, shown again then

    init() {
        this.card = document.getElementById('videoCard');
        document.getElementById('saveVideo').addEventListener('click', () => this.open());
        this.card.querySelector('.cancel').addEventListener('click', () => {
            if (this.busy) this.cancelled = true; else this.close();
        });
        this.card.querySelector('.go').addEventListener('click', () => this.save());
        window.addEventListener('resize', () => { if (this.shown() && !this.busy) this.describe(); });
    },

    // What the video will be, for the run and the speed now.
    plan() {
        const r = Player.run; if (!r) return null;
        const span = r.t[r.t.length - 1] - r.t[0];
        const seconds = span / Player.speed;
        const frames = Math.max(2, Math.round(seconds * VIDEO_FPS) + 1);
        const W = window.innerWidth, H = window.innerHeight;
        const s = Math.min(2, VIDEO_MAX_W / W, VIDEO_MAX_H / H);
        const even = x => Math.max(2, 2 * Math.round(x / 2));
        return { span, seconds, frames, scale: s, w: even(W * s), h: even(H * s), view: Player.view };
    },

    open() {
        if (!Player.run || this.busy) return;
        Player.pause();
        const c = this.card;
        c.querySelector('.bar').style.setProperty('--pct', '0%');
        c.classList.remove('working');
        c.classList.add('show');
        this.describe();
    },
    // What will be made, and whether it can be: again whenever the window is
    // resized while the card is open (CodeRabbit, #688), the view and speed
    // being held (the keys wait, and the card covers the controls).
    describe() {
        const p = this.plan(), c = this.card, q = s => c.querySelector(s);
        if (!p) return;
        q('.what').textContent = `The whole run, ${hms(Player.run.t[0])} to ${hms(Player.run.t[Player.run.t.length - 1])}, ` +
            `in the ${p.view.toUpperCase()} view at the speed set (${speedLabel(Player.speed)}): ` +
            `${videoLength(p.seconds)} of video at ${VIDEO_FPS} frames a second, ${p.w} × ${p.h}.`;
        const tooLong = p.seconds > VIDEO_MAX_SECONDS;
        const unsupported = !('VideoEncoder' in window);
        q('.why').textContent = unsupported ? 'This browser cannot make videos (it has no WebCodecs video encoder). Current Chrome, Edge, Safari and Firefox can.'
            : tooLong ? `The longest video is ${videoLength(VIDEO_MAX_SECONDS)}: raise the speed to make this one shorter.` : '';
        q('.go').disabled = tooLong || unsupported;
        return !(tooLong || unsupported);
    },
    shown() { return this.card.classList.contains('show'); },
    close() { this.card.classList.remove('show', 'working'); },

    async save() {
        if (this.busy || !this.describe()) return;      // and the card says why
        const p = this.plan();
        const c = this.card, q = s => c.querySelector(s);
        const withPatient = q('.patient').checked;
        this.busy = true; this.cancelled = false;
        c.classList.add('working');
        q('.why').textContent = '';
        const T0 = Player.T, r = Player.run;
        try {
            const blob = await this.record(p, withPatient, k => {
                q('.bar').style.setProperty('--pct', `${(100 * k / p.frames).toFixed(1)}%`);
                q('.done').textContent = `${Math.round(100 * k / p.frames)}%`;
            });
            if (blob) {
                const a = document.createElement('a');
                a.href = URL.createObjectURL(blob);
                a.download = videoFileName(r.drug, p.view);
                document.body.appendChild(a); a.click(); a.remove();
                setTimeout(() => URL.revokeObjectURL(a.href), 60000);
                this.close();
            } else {
                c.classList.remove('working');
                q('.why').textContent = 'Cancelled.';
            }
        } catch (err) {
            c.classList.remove('working');
            q('.why').textContent = `The video could not be made: ${err && err.message ? err.message : err}`;
        } finally {
            this.busy = false;
            View.recording = false;
            Player.seek(T0);
            // A run that arrived while the video was being made is loaded now.
            // The card, if still open (cancelled, or failed), then describes
            // the run and window as they are, a resize during the save
            // included, keeping the word on why the save ended (CodeRabbit, #688).
            const why = this.card.querySelector('.why'), said = why.textContent;
            if (this.pending) { const next = this.pending; this.pending = null; Player.load(next); }
            if (this.refused) { Player.message(this.refused, true); this.refused = null; }
            if (this.shown() && this.describe() && said) why.textContent = said;
        }
    },

    // The frames, encoded; the MP4 as a Blob, or null if cancelled.
    async record(p, withPatient, progress) {
        const codec = await videoCodec(p);
        if (!codec) throw new Error('this browser cannot encode H.264 at this size');
        const chunks = [];
        let description = null, failure = null;
        const enc = new VideoEncoder({
            output: (chunk, meta) => {
                const data = new Uint8Array(chunk.byteLength);
                chunk.copyTo(data);
                chunks.push({ data, key: chunk.type === 'key' });
                const d = meta && meta.decoderConfig && meta.decoderConfig.description;
                if (d) description = d instanceof ArrayBuffer ? new Uint8Array(d.slice(0))
                    : new Uint8Array(d.buffer.slice(d.byteOffset, d.byteOffset + d.byteLength));
            },
            error: e => { failure = e; }
        });
        enc.configure(codec);
        const canvas = document.createElement('canvas');
        canvas.width = p.w; canvas.height = p.h;
        const ctx = canvas.getContext('2d');
        const r = Player.run, step = Player.speed / VIDEO_FPS, dt = 1 / VIDEO_FPS;
        View.recording = p.view === '3d';
        try {
            for (let k = 0; k < p.frames; k++) {
                if (this.cancelled) return null;
                if (failure) throw failure;
                Player.seek(Math.min(r.t[r.t.length - 1], r.t[0] + k * step));
                await this.frame(ctx, p, withPatient, dt);
                const frame = new VideoFrame(canvas, { timestamp: Math.round(k * 1e6 / VIDEO_FPS), duration: Math.round(1e6 / VIDEO_FPS) });
                try { enc.encode(frame, { keyFrame: k % (2 * VIDEO_FPS) === 0 }); }
                finally { frame.close(); }
                // Keep the encoder's queue short, and let the page breathe.
                while (enc.encodeQueueSize > 4) await new Promise(res => setTimeout(res, 2));
                if (k % 10 === 0) { progress(k); await new Promise(res => setTimeout(res, 0)); }
            }
            await enc.flush();
        } finally {
            if (enc.state !== 'closed') enc.close();
        }
        if (failure) throw failure;
        if (!description) throw new Error('the encoder gave no H.264 configuration');
        progress(p.frames);
        return mp4(chunks, description, p.w, p.h, VIDEO_FPS);
    },

    // One frame: the picture, the cards and labels over it, the strip.
    async frame(ctx, p, withPatient, dt) {
        const s = p.scale, W = p.w, H = p.h;
        ctx.setTransform(1, 0, 0, 1, 0, 0);
        ctx.fillStyle = '#0e1a24';
        ctx.fillRect(0, 0, W, H);
        if (p.view === '3d') {
            View.step(dt);
            const c = View.renderer.domElement, b = c.getBoundingClientRect();
            ctx.drawImage(c, b.left * s, b.top * s, b.width * s, b.height * s);
        } else {
            const svg = document.querySelector('#flat svg');
            if (svg) {
                const flat = document.getElementById('flat'), b = flat.getBoundingClientRect();
                const img = await svgImage(svg);
                // The drawing as it scrolls in the window, clipped to its box.
                ctx.save();
                ctx.beginPath(); ctx.rect(b.left * s, b.top * s, b.width * s, b.height * s); ctx.clip();
                ctx.drawImage(img, b.left * s, (b.top - flat.scrollTop) * s, svg.width.baseVal.value * s, svg.height.baseVal.value * s);
                ctx.restore();
            }
        }
        ctx.setTransform(s, 0, 0, s, 0, 0);
        const skip = withPatient ? [] : ['foot'];
        if (p.view === '3d') for (const el of document.querySelectorAll('#view .label-layer > *')) paintElement(ctx, el, skip);
        for (const id of ['brand', 'readout', 'amounts']) paintElement(ctx, document.getElementById(id), ['foot']);
        videoStrip(ctx, W / s, H / s, withPatient);
    }
};

// The first codec this browser encodes at this size, as a VideoEncoder config.
async function videoCodec(p) {
    for (const codec of VIDEO_CODECS) {
        const cfg = { codec, width: p.w, height: p.h, framerate: VIDEO_FPS, avc: { format: 'avc' },
                      bitrate: Math.round(VIDEO_BITS_PER_PIXEL * p.w * p.h * VIDEO_FPS) };
        try { if ((await VideoEncoder.isConfigSupported(cfg)).supported) return cfg; } catch (e) { /* try the next */ }
    }
    return null;
}

const videoLength = s => s < 60 ? `${Math.max(1, Math.round(s))} s` : `${Math.floor(s / 60)} min ${Math.round(s % 60)} s`;
// propofol-2D-2026-11-07.mp4: letters, digits and dashes only.
function videoFileName(drug, view) {
    const d = new Date(), z = n => String(n).padStart(2, '0');
    const name = String(drug || 'run').replace(/[^A-Za-z0-9]+/g, '-').replace(/^-|-$/g, '').slice(0, 40) || 'run';
    return `${name}-${view.toUpperCase()}-${d.getFullYear()}-${z(d.getMonth() + 1)}-${z(d.getDate())}.mp4`;
}

// The 2D drawing as an image: the SVG with the styles it draws with (the
// page's stylesheet does not reach an image) written onto its text.
function svgImage(svg) {
    const copy = svg.cloneNode(true), from = svg.querySelectorAll('text, tspan'), to = copy.querySelectorAll('text, tspan');
    from.forEach((el, i) => {
        const cs = getComputedStyle(el);
        to[i].setAttribute('style', `fill:${cs.fill};font-family:${cs.fontFamily};font-size:${cs.fontSize};` +
                                    `font-weight:${cs.fontWeight};font-variant-numeric:${cs.fontVariantNumeric}`);
    });
    const url = URL.createObjectURL(new Blob([new XMLSerializer().serializeToString(copy)], { type: 'image/svg+xml' }));
    const img = new Image();
    img.src = url;
    return img.decode().then(() => { URL.revokeObjectURL(url); return img; },
                             e => { URL.revokeObjectURL(url); throw e; });
}

// Paint one page element as it shows: its box (background, border, rounded
// corners) and its text, at the places the page laid them out, then its
// children. skip: classes left out (the readout's patient line).
function paintElement(ctx, el, skip, alpha = 1) {
    if (!el) return;
    const cs = getComputedStyle(el);
    if (cs.display === 'none' || cs.visibility === 'hidden') return;
    if (skip.some(c => el.classList.contains(c))) return;
    const a = alpha * Number(cs.opacity);
    if (!(a > 0)) return;
    const b = el.getBoundingClientRect();
    if (b.width === 0 && b.height === 0) return;
    ctx.globalAlpha = a;
    const radius = parseFloat(cs.borderTopLeftRadius) || 0;
    if (visibleColour(cs.backgroundColor)) {
        ctx.fillStyle = cs.backgroundColor;
        roundRect(ctx, b.left, b.top, b.width, b.height, radius); ctx.fill();
    }
    // The border: all round, rounded, where the four sides match (the cards,
    // kbd); otherwise side by side (the amounts table's rules, top only).
    const side = k => ({ w: parseFloat(cs[`border${k}Width`]) || 0, c: cs[`border${k}Color`], st: cs[`border${k}Style`] });
    const sides = ['Top', 'Right', 'Bottom', 'Left'].map(side), shown = x => x.w > 0 && x.st !== 'none' && visibleColour(x.c);
    if (sides.every(x => shown(x) && x.w === sides[0].w && x.c === sides[0].c)) {
        const bw = sides[0].w;
        ctx.strokeStyle = sides[0].c; ctx.lineWidth = bw;
        roundRect(ctx, b.left + bw / 2, b.top + bw / 2, b.width - bw, b.height - bw, Math.max(0, radius - bw / 2)); ctx.stroke();
    } else {
        const [t, rt, bt, l] = sides;
        if (shown(t)) { ctx.fillStyle = t.c; ctx.fillRect(b.left, b.top, b.width, t.w); }
        if (shown(bt)) { ctx.fillStyle = bt.c; ctx.fillRect(b.left, b.bottom - bt.w, b.width, bt.w); }
        if (shown(l)) { ctx.fillStyle = l.c; ctx.fillRect(b.left, b.top, l.w, b.height); }
        if (shown(rt)) { ctx.fillStyle = rt.c; ctx.fillRect(b.right - rt.w, b.top, rt.w, b.height); }
    }
    for (const node of el.childNodes) {
        if (node.nodeType === Node.ELEMENT_NODE) paintElement(ctx, node, skip, a);
        else if (node.nodeType === Node.TEXT_NODE && node.textContent.trim()) paintText(ctx, node, cs, a);
    }
    ctx.globalAlpha = 1;
}

// A text node, word by word at the places the page put each word, so a
// line the page wrapped is painted wrapped.
function paintText(ctx, node, cs, alpha) {
    ctx.globalAlpha = alpha;
    ctx.font = `${cs.fontStyle} ${cs.fontWeight} ${cs.fontSize} ${cs.fontFamily}`;
    ctx.fillStyle = cs.color;
    ctx.textBaseline = 'alphabetic';
    const text = node.textContent, range = document.createRange(), re = /\S+/g;
    let line = null, m;
    const flush = () => {
        if (!line) return;
        const mt = ctx.measureText(line.text), asc = mt.fontBoundingBoxAscent, desc = mt.fontBoundingBoxDescent;
        ctx.fillText(line.text, line.x, line.top + (line.h - (asc + desc)) / 2 + asc);
        line = null;
    };
    while ((m = re.exec(text))) {
        range.setStart(node, m.index); range.setEnd(node, m.index + m[0].length);
        const rc = range.getClientRects()[0];
        if (!rc) continue;
        if (line && Math.abs(rc.top - line.top) < 1) {
            line.text = text.slice(line.start, m.index + m[0].length);
        } else {
            flush();
            line = { start: m.index, text: m[0], x: rc.left, top: rc.top, h: rc.height };
        }
    }
    flush();
}

const visibleColour = c => c && c !== 'transparent' && !/rgba\([^)]*,\s*0\)$/.test(c);
function roundRect(ctx, x, y, w, h, r) {
    r = Math.min(r, w / 2, h / 2);
    ctx.beginPath();
    ctx.moveTo(x + r, y); ctx.arcTo(x + w, y, x + w, y + h, r); ctx.arcTo(x + w, y + h, x, y + h, r);
    ctx.arcTo(x, y + h, x, y, r); ctx.arcTo(x, y, x + w, y, r); ctx.closePath();
}

// The strip along the foot of every video: stanpumpR, the drug and model,
// the time, the patient if asked for, and stanpumpR's intended use.
// The strip's first line: stanpumpR, the drug, the model, the patient if asked for.
function videoHeading(withPatient) {
    const r = Player.run, pt = r.patient || {};
    const pat = withPatient ? [pt.weight != null && `${pt.weight} kg`, pt.height != null && `${pt.height} cm`,
                               pt.age != null && `${pt.age} y`, pt.sex].filter(Boolean).join(' · ') : '';
    const name = String(r.drug).charAt(0).toUpperCase() + String(r.drug).slice(1);
    return [`stanpumpR · Minto Visualizer · ${name}`, r.model ? shortModel(r.model) : '', pat].filter(Boolean).join(' · ');
}
function videoStrip(ctx, W, H, withPatient) {
    const h = 40, y = H - h;
    ctx.globalAlpha = 1;
    ctx.fillStyle = 'rgba(8, 14, 20, 0.92)';
    ctx.fillRect(0, y, W, h);
    ctx.textBaseline = 'alphabetic';
    const fit = (t, w) => { if (ctx.measureText(t).width <= w) return t;
        while (t.length > 1 && ctx.measureText(t + '…').width > w) t = t.slice(0, -1); return t + '…'; };
    ctx.font = '600 13px system-ui, "Segoe UI", sans-serif'; ctx.fillStyle = '#e6edf5';
    const timeText = hms(Player.T);
    const tw = ctx.measureText(timeText).width;
    ctx.fillText(timeText, W - 12 - tw, y + 17);
    ctx.fillText(fit(videoHeading(withPatient), W - 36 - tw), 12, y + 17);
    ctx.font = '400 10.5px system-ui, "Segoe UI", sans-serif'; ctx.fillStyle = '#8a9cb0';
    ctx.fillText(fit(VIDEO_USE, W - 24), 12, y + 33);
}

// ── An MP4 of one H.264 track (ISO/IEC 14496-12 and -15), written here so the
// viewer needs no library: ftyp, then moov (its one chunk's offset is known
// once moov's size is), then mdat holding every sample in order.
//   chunks: [{data: Uint8Array, key: boolean}], in decode order (the encoder
//   makes no B-frames, so decode order is display order); description: the
//   avcC record the encoder gave.
function mp4(chunks, description, w, h, fps) {
    const TS = fps * 1000, DELTA = 1000;            // the track's time scale: each frame 1000 ticks
    const n = chunks.length, dur = n * DELTA, movieDur = Math.round(n * 1000 / fps);
    const u8 = a => new Uint8Array(a);
    const u16 = v => u8([(v >>> 8) & 255, v & 255]);
    const u32 = v => u8([(v >>> 24) & 255, (v >>> 16) & 255, (v >>> 8) & 255, v & 255]);
    const str = s => u8([...s].map(c => c.charCodeAt(0)));
    const cat = parts => { const len = parts.reduce((s, p) => s + p.length, 0), out = new Uint8Array(len);
                           let o = 0; for (const p of parts) { out.set(p, o); o += p.length; } return out; };
    const box = (type, ...parts) => { const body = cat(parts); return cat([u32(8 + body.length), str(type), body]); };
    const full = (type, version, flags, ...parts) => box(type, u8([version, (flags >>> 16) & 255, (flags >>> 8) & 255, flags & 255]), ...parts);
    const matrix = cat([u32(0x00010000), u32(0), u32(0), u32(0), u32(0x00010000), u32(0), u32(0), u32(0), u32(0x40000000)]);
    const zeros = k => new Uint8Array(k);

    const ftyp = box('ftyp', str('isom'), u32(512), str('isom'), str('iso2'), str('avc1'), str('mp41'));
    const mvhd = full('mvhd', 0, 0, u32(0), u32(0), u32(1000), u32(movieDur), u32(0x00010000), u16(0x0100), zeros(10),
                      matrix, zeros(24), u32(2));
    const tkhd = full('tkhd', 0, 3, u32(0), u32(0), u32(1), u32(0), u32(movieDur), zeros(8), u16(0), u16(0), u16(0), u16(0),
                      matrix, u32(w << 16), u32(h << 16));
    const mdhd = full('mdhd', 0, 0, u32(0), u32(0), u32(TS), u32(dur), u16(0x55c4), u16(0));   // language "und"
    const hdlr = full('hdlr', 0, 0, u32(0), str('vide'), zeros(12), str('VideoHandler'), u8([0]));
    const vmhd = full('vmhd', 0, 1, zeros(8));
    const dinf = box('dinf', full('dref', 0, 0, u32(1), full('url ', 0, 1)));
    const avc1 = box('avc1', zeros(6), u16(1), zeros(16), u16(w), u16(h), u32(0x00480000), u32(0x00480000), u32(0),
                     u16(1), zeros(32), u16(0x0018), u16(0xffff), box('avcC', description));
    const stsd = full('stsd', 0, 0, u32(1), avc1);
    const stts = full('stts', 0, 0, u32(1), u32(n), u32(DELTA));
    const keys = chunks.map((c, i) => c.key ? i + 1 : 0).filter(Boolean);
    const stss = full('stss', 0, 0, u32(keys.length), ...keys.map(u32));
    const stsc = full('stsc', 0, 0, u32(1), u32(1), u32(n), u32(1));
    const stsz = full('stsz', 0, 0, u32(0), u32(n), ...chunks.map(c => u32(c.data.length)));
    const build = offset => box('moov', mvhd, box('trak', tkhd, box('mdia', mdhd, hdlr,
        box('minf', vmhd, dinf, box('stbl', stsd, stts, stss, stsc, stsz, full('stco', 0, 0, u32(1), u32(offset)))))));
    const moovSize = build(0).length;
    const moov = build(ftyp.length + moovSize + 8);      // the samples start after mdat's own header
    const samples = chunks.reduce((s, c) => s + c.data.length, 0);
    return new Blob([ftyp, moov, u32(8 + samples), str('mdat'), ...chunks.map(c => c.data)], { type: 'video/mp4' });
}
