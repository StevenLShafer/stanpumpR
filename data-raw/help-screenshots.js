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
//      node data-raw/help-screenshots.js [http://127.0.0.1:7788]
//
// Every capture is at twice the CSS pixel size, as the help displays them
// scaled down.

const { chromium } = require('playwright');
const path = require('path');

const BASE = process.argv[2] || 'http://127.0.0.1:7788';
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

(async () => {
  const browser = await chromium.launch();

  // The opening dialog, as on a first visit (a fresh browser has no record
  // of the weekly welcome).
  {
    const page = await browser.newPage({ viewport: VIEWPORT, deviceScaleFactor: 2 });
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
