# The Minto Visualizer (stanpumpR's 3D viewer)

Copyright (c) 2026 Charles Minto and Thomas Schnider. MIT License (see `LICENSE`).
Written by Dr Charles Minto, assisted by Claude.ai; adapted to stanpumpR's
pipeline and the Edit doses dialog by Claude Code, 2026-11-05.

The visualizer shows one drug's run from stanpumpR in 3D:
- **the compartments:** V1, V2 and V3 as cylinders whose floor area is the volume and whose liquid height is the concentration, so the liquid is the amount;
- **the flows:** the pump, absorption from a depot, the net flows between V1 and V2 or V3, and elimination, as arrows whose cross-section is the rate;
- **the effect site:** a ribbon beside V1, with the TCI target marked.

## Where it is in stanpumpR

Double-click a drug's panel on the plot. The **Edit *drug* doses** dialog has a **Minto Visualizer** button after **Delete All Doses**; it is shown only for a drug the visualizer can draw. A click opens the visualizer in a pop-up window at once (a window opened later, when the server answers, would be stopped by popup blockers); if the browser blocks it, the app says so.

The run is a **snapshot of the committed dose table** at the click (press Apply first to see edits), with every covariate the plot uses, and it spans **the plot's x-axis, from Min time to Max time**. The time is shown as a progress bar, as a video player shows it. Drug given before Min time is counted: the vessels start full and the amounts card starts from what was given since time 0.

The controls: **Play**, **Pause**, **Stop** (pause and go back to the start), the progress bar (drag to any time), a **Speed** slider (real time to 1 s = 1 day; a run first plays in at most two minutes), and an **X**, upper left, that closes the window. The mouse turns and zooms the scene; **h** lists the keys.

| File | What it is |
|---|---|
| `R/viewerRun.R` | `viewerSimulate()` runs the app's own pipeline (`recalculatePK()` → `processdoseTable()`) at the visualizer's times; `viewerRun()` turns one drug's result into the run; `viewerProblem()` says why a drug cannot be drawn. |
| `R/app_server.R` | the dialog's button (`viewerAvailable()`), the `openViewer` observer and the `viewerBlocked` notification. |
| `inst/www/viewer-launch.js` | the browser side of the button: opens the pop-up and hands it the run. |
| `inst/www/stanpumpR-viewer.html` | the visualizer, one self-contained file (about 830 KB, three.js 0.170.0 included), **built** from `tools/viewer/src`. |
| `tools/viewer/src/` | its readable source. |
| `tools/viewer/build/` | `build.mjs`, which makes `inst/www/stanpumpR-viewer.html` from `src/`, and the test runner. |
| `tools/viewer/tests/viewer-injection.test.mjs` | a run file cannot put markup in the page. |
| `tools/viewer/examples/propofol-tci-e.json` | a sample (version 1) run, for the injection tests and for a look without stanpumpR. |
| `tests/testthat/test-viewer.R` | stanpumpR's test of `viewerRun()`. |

`tools/` is in `.Rbuildignore`; only `inst/www/stanpumpR-viewer.html` ships in the package.

## Which drugs

Every drug measured in mcg/mL, ng/mL or pg/mL, whatever its model: linear or saturable (Michaelis–Menten), closed form, transit chain or Runge–Kutta. The visualizer computes no pharmacokinetics: it draws stanpumpR's concentrations, and reads every flow from the amounts as they change, so a flow is right for any model.

Not drawn (`viewerProblem()`): inhaled agents; drugs in IU, mmol, mcU, INR or ACT; and drugs that are the sum of separate systems (ketorolac's S and R enantiomers), which one set of compartments cannot show.

**The drug given, and eliminated.** `given` counts intravenous rates and boluses (scheduled repeats and the TCI pump's schedule included), the apparent-oral rates (`mg/day PO`) that run straight into V1, and the drug absorbed from a depot, read off the fall of stanpumpR's `Depot` column (the bioavailable amount still to be absorbed; stanpumpR puts a point just before and at each arrival, so the rise holds no absorption to speak of: about 0.01% of a dose). Eliminated is then given less the drug in V1 + V2 + V3. A few drugs have an input stanpumpR rewrites beyond the dose rows — a zero-order oral or SC release, an enteral infusion, exenatide's saturable SC input — and a metabolite's input is its formation: for those the run says `inputKnown: false`, the amounts card leaves administered and eliminated blank, and elimination is drawn as CL1 × Cp.

**Administered (bioavailable).** For a drug with a depot, the amounts card's administered is the bioavailable dose: 30 mg of oral morphine (F 0.29) is 8.7 mg.

## The run file (format version 2)

`viewerRun()` makes it; Shiny's `sendCustomMessage()` serialises it (`jsonlite::toJSON(run, auto_unbox = TRUE, digits = NA, null = "null")` writes the same file). Version 1 runs (Charles Minto's original) still load.

| Field | Content |
|---|---|
| `format`, `version` | `"stanpumpR-viewer"`, `2` |
| `drug`, `model`, `category` | Labels; `category` (stanpumpR's) sets the colour. |
| `typical` | stanpumpR's typical concentration for the drug: the height scale. |
| `units` | `conc`, `amount`, `rate`: stanpumpR's base units for the drug. |
| `patient` | `weight`, `height`, `age`, `sex`, for the readout. |
| `pk` | `v1, v2, v3` (L) and `cl1, cl2, cl3` (L/min): the vessels' sizes (and, without a known input, CL1 for elimination). |
| `t` | Times, in **seconds**, increasing, from Min time to Max time: 1 s apart, longer on plots over 2 h (at most about 7200 times); the last is Max time. |
| `cp`, `ce`, `c2`, `c3` | Concentrations at each time; `ce`, `c2`, `c3` are `null` where the drug has none. |
| `depot` | The bioavailable drug still in the depot (amount), or `null`. |
| `absorbed` | Cumulative drug that has entered V1 other than intravenously (absorbed from the depot, apparent-oral rates), from time 0, or `null`. |
| `given` | Cumulative drug that has entered the body, from time 0; `null` when `inputKnown` is false. |
| `inputKnown` | Whether the drug given can be accounted for. |
| `rate` | The intravenous rate in force from each time to the next, amount/min. |
| `target`, `mode` | The TCI target in force, and `"effect"`, `"plasma"` or `"manual"`. |
| `boluses` | IV boluses inside the window: `[{t, amount}]`, seconds and amount. |

Between the times, concentrations and amounts are interpolated linearly; each flow is the change over the interval: V2 × ΔC2/Δt into V2, Δabsorbed/Δt for absorption, Δeliminated/Δt for elimination. A file that breaks a rule is refused with a message naming the field.

Display rules, unchanged from Charles Minto's design: the typical concentration stands 3 units tall; every arrow is on one fixed scale (a flow of 2,500 mL/h of a syringe at 10/3 × the typical concentration draws a shaft of radius 2.0, the area proportional to the rate); propofol is white, opioids and neuromuscular blockers a light tint of their syringe-label colour, anything else pale blue; TCI-P shows a gold ring on V1, TCI-E a gold frame on the ribbon.

## Rebuilding after changing `src/`

You need Node.js (22.12 or later for the tests). In `tools/viewer/build/`:

```bash
npm ci
```

```bash
node build.mjs
```

```bash
npm test
```

`npm ci` installs three.js 0.170.0, esbuild 0.25.0 and puppeteer-core 25.14.0 (which drives an installed Chrome or Edge, or `CHROME_PATH`; it downloads no browser). `build.mjs` writes `inst/www/stanpumpR-viewer.html`; commit it with the source change.

## How it was tested

Charles Minto's tests (2026-10-24 and 25), on his version: mass balance to 1e-4 or better in six runs; the TCI-to-manual hand-over at exactly 7.000 mg/min; every drawn height equal to the run's concentration × scale; play at the set speed; the End flight never entering V1's glass; the popup in headless Edge on a patched stanpumpR, the run arriving identical to `viewerRun()` in R to 1e-12; several drugs and drugs it cannot draw; a blocked window.

This version (2026-11-05):
- `tests/testthat/test-viewer.R`: propofol TCI-E (given equals the pump's schedule integrated; eliminated equals ∫CL1·Cp to 1e-3); the hand-over at 7 mg/min; fentanyl boluses with an infusion (given exactly 220 µg); a run from Min time to Max time (starting with the drug given before it, and the same solution as a run from 0); oral morphine (absorbed + depot = F × dose after the lag; eliminated equals ∫CL1·Cp to 5e-3); scheduled IV q4h (every repeat counted); phenytoin (saturable: given exact, eliminated never falling); ketorolac and heparin refused with their reasons.
- `tools/viewer/tests/viewer-injection.test.mjs`: 11 of 11 in headless Chromium.
- In the running app, headless Chromium: `?scenario=tci-propofol` and `?scenario=oral-morphine-depot`; double-click the panel, the dialog's button opens the pop-up, a 7201-time run spanning the plot arrives, Play advances, Pause holds, Stop returns to the start, the speed slider sets the speed, the X closes the window; with `window.open` returning nothing, the app shows the blocked notification.
