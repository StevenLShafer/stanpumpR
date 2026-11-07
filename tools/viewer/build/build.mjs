// stanpumpR viewer — the build
//
// Author:    Dr Charles Minto
// Assisted by: Claude.ai (Anthropic)
//
// Copyright (c) 2026 Charles Minto and Thomas Schnider
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// Makes stanpumpR-viewer.html, one file needing nothing else: three.js
// (the parts the viewer uses) bundled by esbuild as one script setting
// window.THREE and window.THREE_ADDONS, then the styles and the viewer's
// scripts from src/, each put in place of its marker in src/viewer.html.
//
//   cd tools/viewer/build; npm install; node build.mjs
//
// It reads tools/viewer/src and writes inst/www/stanpumpR-viewer.html, the
// file the app serves. node build.mjs <root> <out> builds the viewer folder
// at <root> into <out> instead.

import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import * as esbuild from 'esbuild';

const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(process.argv[2] || path.join(here, '..'));
const src = path.join(root, 'src');

const three = await esbuild.build({
    stdin: {
        contents: `
            import * as THREE from 'three';
            import { OrbitControls } from 'three/addons/controls/OrbitControls.js';
            import { CSS2DRenderer, CSS2DObject } from 'three/addons/renderers/CSS2DRenderer.js';
            import { RoomEnvironment } from 'three/addons/environments/RoomEnvironment.js';
            import { LineSegments2 } from 'three/addons/lines/LineSegments2.js';
            import { LineSegmentsGeometry } from 'three/addons/lines/LineSegmentsGeometry.js';
            import { LineMaterial } from 'three/addons/lines/LineMaterial.js';
            window.THREE = THREE;
            window.THREE_ADDONS = { OrbitControls, CSS2DRenderer, CSS2DObject, RoomEnvironment,
                                    LineSegments2, LineSegmentsGeometry, LineMaterial };`,
        resolveDir: here, loader: 'js'
    },
    bundle: true, format: 'iife', minify: true, write: false,
    legalComments: 'inline', target: 'es2020'
});
const threeJs = three.outputFiles[0].text;

// Inline script text must not close its own <script> element.
const safe = s => s.replace(/<\/script/gi, '<\\/script');
let html = fs.readFileSync(path.join(src, 'viewer.html'), 'utf8');
const put = (marker, text) => {
    const m = `/*@@${marker}@@*/`;
    if (!html.includes(m)) throw new Error(`viewer.html has no marker ${m}`);
    html = html.replace(m, () => text);
};
put('three', safe(threeJs));
put('viewer.css', fs.readFileSync(path.join(src, 'viewer.css'), 'utf8'));
for (const f of ['looks.js', 'vessels.js', 'arrows.js', 'ribbon.js', 'labels.js', 'perf.js', 'scene.js', 'flat.js', 'player.js', 'video.js'])
    put(f, safe(fs.readFileSync(path.join(src, f), 'utf8')));
if (/\/\*@@[^@]+@@\*\//.test(html)) throw new Error('a marker was left in viewer.html');

const out = path.resolve(process.argv[3] || path.join(root, '..', '..', 'inst', 'www', 'stanpumpR-viewer.html'));
fs.writeFileSync(out, html);
console.log(`wrote ${out}: ${(Buffer.byteLength(html) / 1024).toFixed(0)} KB (three.js ${(threeJs.length / 1024).toFixed(0)} KB)`);
