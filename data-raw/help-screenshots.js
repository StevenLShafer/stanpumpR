// Recapture the screenshots of the help's five-minute tour
// (inst/help/quick-start.md -> inst/www/help/quick-start-*.png).
//
// The pictures drift as the interface changes, so they are made by this script
// rather than by hand, from the teaching scenario the tour describes
// (propofol-induction-maintenance), reached by its deep link.
//
// 1. Start the app on a fixed port, with a config.yml whose title is
//    "stanpumpR" and whose debug is 0 (no Debug Section under the plot):
//      Rscript -e 'devtools::load_all("."); shiny::runApp(run_app(), port = 7788)'
// 2. From the repository root, with Playwright installed:
//      node data-raw/help-screenshots.js [http://127.0.0.1:7788] [--visualizer]
//    --visualizer remakes only the Visualizer's four pictures (a drug and an
//    inhaled anesthetic, each in the 2D and the 3D view), from the
//    visualizer-propofol-bolus and visualizer-sevoflurane-emergence scenarios.
//
// Every capture is at twice the CSS pixel size, as the help displays them
// scaled down.

const { chromium } = require('playwright');
const path = require('path');

const BASE = (process.argv.slice(2).find(a => !a.startsWith('--')) || 'http://127.0.0.1:7788').replace(/\/+$/, '');
const ONLY_VISUALIZER = process.argv.includes('--visualizer');
const OUT = path.join(__dirname, '..', 'inst', 'www', 'help');
const SCENARIO = 'propofol-induction-maintenance';
const VIEWPORT = { width: 1400, height: 820 };

const out = name => path.join(OUT, `quick-start-${name}.png`);

// Wait until Shiny is idle and the plot has drawn.
async function settle(page, ms = 1500) {
  await page.waitForFunction(() =>
    window.Shiny && !document.documentElement.classList.contains('shiny-busy'),
    null, { timeout: 60000 });
  await page.waitForTimeout(ms);
}

async function simulatorPage(browser) {
  const page = await browser.newPage({ viewport: VIEWPORT, deviceScaleFactor: 2 });
  await page.goto(`${BASE}/?scenario=${SCENARIO}`);
  await page.waitForSelector('#PlotSimulation img', { timeout: 60000 });
  await settle(page, 3000);
  // the scenario's "Loaded" notification would cover the dose panel
  await page.addStyleTag({ content: '#shiny-notification-panel { display: none !important; }' });
  return page;
}

// Open one accordion panel of the left sidebar (and close the others).
async function openPanel(page, value) {
  const item = page.locator(`.accordion-item[data-value="${value}"]`);
  await item.locator('.accordion-button').click();
  await settle(page, 1200);
  return item;
}

// The Visualizer for one scenario's drug: its pop-up opened by the button
// under the plot, stopped at time T (seconds), in the 2D and then the 3D view.
// The 3D view needs WebGL, which headless Chromium draws in software.
async function visualizer(browser, scenario, T, name) {
  const page = await browser.newPage({ viewport: VIEWPORT, deviceScaleFactor: 2 });
  await page.goto(`${BASE}/?scenario=${scenario}`);
  await page.waitForSelector('#viewerButtonUI [data-viewer-drug]', { timeout: 120000 });
  await settle(page, 2000);
  const [popup] = await Promise.all([page.waitForEvent('popup', { timeout: 30000 }),
                                     page.locator('#viewerButtonUI [data-viewer-drug]').first().click()]);
  await popup.setViewportSize({ width: 1100, height: 700 });
  await popup.waitForFunction(() => typeof Player !== 'undefined' && Player.run, null, { timeout: 120000 });
  await popup.evaluate(T => { Player.setView('2d'); Player.seek(T); }, T);
  await popup.waitForTimeout(800);
  await popup.screenshot({ path: out(`visualizer-${name}-2d`) });
  // A gas run chooses its starting view for the window it is shown in.
  await popup.evaluate(T => { Player.setView('3d'); View.gasHome = null; View.resetView(); Player.seek(T); }, T);
  await popup.waitForTimeout(2000);
  await popup.screenshot({ path: out(`visualizer-${name}-3d`) });
  await page.close();
}

(async () => {
  const browser = await chromium.launch({ args: ['--enable-unsafe-swiftshader', '--use-angle=swiftshader'] });

  // The Visualizer (step 6 of the tour): a propofol bolus at 1.6 minutes, its
  // effect site at its peak; sevoflurane five minutes after the vaporizer is
  // turned off.
  await visualizer(browser, 'visualizer-propofol-bolus', 96, 'propofol');
  await visualizer(browser, 'visualizer-sevoflurane-emergence', 125 * 60, 'sevoflurane');
  if (ONLY_VISUALIZER) { await browser.close(); return; }

  // The opening dialog, as on a first visit (a fresh browser has no record
  // of the weekly welcome), in a window tall enough for the whole dialog:
  // a shorter one scrolls it and cuts off the Start button.
  {
    const page = await browser.newPage({ viewport: { width: VIEWPORT.width, height: 1150 },
                                          deviceScaleFactor: 2 });
    await page.goto(`${BASE}/`);
    const modal = page.locator('.modal-content');
    await modal.waitFor({ timeout: 60000 });
    await page.waitForTimeout(1500);
    await modal.screenshot({ path: out('welcome') });
    await page.close();
  }

  {
    const page = await simulatorPage(browser);
    await page.screenshot({ path: out('simulator') });

    await page.locator('#PlotSimulation').screenshot({ path: out('plot') });

    // The dose panel fills the window's height; a short window keeps the
    // picture to the table and its buttons.
    await page.setViewportSize({ width: VIEWPORT.width, height: 580 });
    await settle(page, 1500);
    await page.locator('aside.dose-sidebar').screenshot({ path: out('doses') });
    // and a tall one shows a whole sidebar panel without its scroll bar
    await page.setViewportSize({ width: VIEWPORT.width, height: 1250 });
    await settle(page, 1500);

    const sidebar = page.locator('aside.sidebar:not(.dose-sidebar):not(.help-sidebar) .accordion').first();
    await openPanel(page, 'Patient Profile');
    await sidebar.screenshot({ path: out('patient') });

    await openPanel(page, 'Patient Profile');   // close it again
    await openPanel(page, 'Graph Options');
    await sidebar.screenshot({ path: out('graph-options') });

    await openPanel(page, 'Graph Options');
    await openPanel(page, 'time');
    await sidebar.screenshot({ path: out('time') });
    await page.close();
  }

  // The scenario's own help page, top of the page only.
  {
    const page = await browser.newPage({ viewport: { width: 1400, height: 620 },
                                          deviceScaleFactor: 2 });
    await page.goto(`${BASE}/?help=scenarios/${SCENARIO}`);
    await page.waitForSelector('h1.help-title', { timeout: 60000 });
    await settle(page, 1500);
    await page.screenshot({ path: out('scenario-page') });
    await page.close();
  }

  await browser.close();
})();
