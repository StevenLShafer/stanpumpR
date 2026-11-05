// ═══════════════════════════════════════════════════════════════
//  stanpumpR viewer — opening it from stanpumpR
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
//  In the app's page. A click on the button with id "openViewer" (the
//  Minto Visualizer button of the Edit doses dialog; adapted for it by
//  Claude Code, 2026-11-05) opens the viewer in its own window at once (a window opened later, when the
//  server answers, is stopped by popup blockers). The server sends the
//  run as a custom message, "stanpumpR-viewer-run"; the viewer says
//  "stanpumpR-viewer-ready" when it has loaded; whichever comes second,
//  the run is then posted to it. If the server cannot make the run it
//  sends the reason instead ("stanpumpR-viewer-error"), posted the same
//  way, so the window says why rather than waiting. Another click reloads
//  the same window with the run of that moment. A disabled button opens
//  nothing. The dialog shows the button only for a drug the viewer draws. If the browser blocks the
//  window (window.open gives none, or one already closed), the app is
//  told (viewerBlocked) and says so; a blocker that closes the window
//  later is not caught.
//
//  Each click brings the window to the front (focus(): a window that is
//  only reloaded stays wherever it was, behind the app's), and asks for
//  the viewer at an address of its own (?t=, the click's time), so the
//  browser loads the viewer afresh, never a copy it kept from before the
//  file was updated.
// ═══════════════════════════════════════════════════════════════

(function () {
    const URL = 'stanpumpr-assets/stanpumpR-viewer.html';
    // pending: the run, or the server's reason there is none, for the window.
    let win = null, ready = false, pending = null;

    function send() {
        if (win && !win.closed && ready && pending) {
            win.postMessage(pending, location.origin);
            pending = null;
        }
    }

    document.addEventListener('click', function (e) {
        const b = e.target.closest && e.target.closest('#openViewer');
        if (!b || b.disabled) return;
        ready = false; pending = null;
        win = window.open(URL + '?t=' + Date.now(), 'stanpumpRViewer', 'width=1200,height=800');
        if (!win || win.closed) {
            // The browser blocked the window; the app says so.
            win = null;
            if (window.Shiny && Shiny.setInputValue)
                Shiny.setInputValue('viewerBlocked', Date.now(), { priority: 'event' });
            return;
        }
        win.focus();
    });

    window.addEventListener('message', function (e) {
        if (e.source !== win || !e.data || e.data.type !== 'stanpumpR-viewer-ready') return;
        ready = true; send();
    });

    $(document).on('shiny:connected', function () {
        Shiny.addCustomMessageHandler('stanpumpR-viewer-run', function (r) {
            pending = { type: 'stanpumpR-viewer-run', run: r }; send();
        });
        Shiny.addCustomMessageHandler('stanpumpR-viewer-error', function (m) {
            pending = { type: 'stanpumpR-viewer-error', message: m }; send();
        });
    });
})();
