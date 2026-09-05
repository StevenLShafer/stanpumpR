# stanpumpR — User's Guide

**Draft.** Written from the code, 2026-09-05. Sections marked *(branch only)*
describe work on `inhaled-gas-engine` that is not yet merged to `master`.

---

## What stanpumpR is

stanpumpR predicts drug concentrations. You describe a patient and a dosing
regimen, and it plots the plasma and effect-site concentrations those doses
would be expected to produce, using published pharmacokinetic models.

**stanpumpR does not control drug delivery.** It is a simulator, for teaching,
research, and thinking about patient care. Nothing it displays is a measurement.
Every curve is a model prediction for a typical patient with the covariates you
entered, and real patients vary substantially around those predictions.

---

## The 60-second tour

1. Open the app. A default patient and an empty dose table are waiting.
2. In the **Doses** table, type a drug name — the cell autocompletes from the
   drug library.
3. Enter a **Time** and a **Dose**, and pick the **Units**.
4. Press **Apply Changes**.
5. The plot redraws with the predicted concentrations.

Everything else is refinement.

---

## Patient Profile

Left sidebar, first panel. These covariates drive the pharmacokinetic models, so
they change the predictions.

| Field | Units | Notes |
|---|---|---|
| Age | years or months | Toggle the unit beside the field |
| Weight | kg or lb | |
| Height | in or cm | |
| Sex | male / female | |

Three further fields — **Pregnant**, **CYP 2D6**, and **Renal Function** — appear
in the interface but are **currently disabled**. The inputs were added ahead of
the models that will use them; no drug in the library yet responds to them.
They are visible so the intent is clear, not because they do anything.

---

## The dose table

This is the main way you talk to the program. It sits to the right of the plot.

### Columns

- **Drug** — autocompletes, and only accepts names in the library. Type a few
  letters and press Enter.
- **Time** — see *Entering times* below.
- **Dose** — a number.
- **Units** — a dropdown whose contents depend on the drug in that row. Propofol
  offers `mg`, `mg/kg`, `mcg/kg/min`, `mg/kg/hr`; remifentanil defaults to
  `mcg/kg/min`. Choosing a per-minute or per-hour unit makes the row an
  **infusion**; a mass unit makes it a **bolus**.

An infusion runs from its time until the next row for that same drug changes it,
or until the end of the simulation. To stop an infusion, add a row for the same
drug at the stop time with a dose of zero.

### Entering times

The time field is forgiving and accepts three forms:

- `12` — twelve minutes
- `1:30` — one hour thirty minutes
- `130` — the same, interpreted as `HH:MM`

Minutes above 59 roll over, so `0:80` becomes `1:20`. Anything that cannot be
read as a time becomes zero rather than raising an error.

### Applying changes

Edits go into a **draft**. Nothing recalculates until you press **Apply
Changes**. This is deliberate: it lets you make several related edits and
simulate them together rather than watching the plot flicker through
half-finished states.

- **Apply Changes** commits the draft and redraws.
- **Undo** / **Redo** step through your edit history.

The one exception is clicking on the plot to add a dose, which applies
immediately — see below.

### Adding and removing rows

Right-click in the table for the row context menu (insert above, insert below,
remove row). Column editing is deliberately disabled.

---

## Reading the plot

### Plasma and effect site

Each drug can be drawn as two lines:

- **Plasma** concentration — what an arterial sample would contain.
- **Effect site** concentration — the concentration at the site of drug action,
  which lags plasma. This is usually what you care about clinically, and it is
  why the effect-site line is drawn solid by default while the plasma line is
  hidden.

Both are configurable under **Graph Options → Plasma line / Effect site line**,
including turning either off.

### The shaded band

**Graph Options → Show typical** controls a shaded band behind each drug:

- **Range** — the usual therapeutic range for that drug, from the `Lower` and
  `Upper` values in the drug library.
- **Mid** — a narrow band at the typical concentration, ±5%.
- **&lt;none&gt;** — no band.

The band is a rough orientation, not a target. It comes from the drug library
and you can edit it (see *Drug Library*).

### Interacting with the plot

- **Hover** over any curve for the precise concentration at that moment.
- **Click** to add a dose of that drug at that time. This bypasses the draft and
  applies immediately.
- **Double-click** on a drug's curve to edit or delete that drug's doses.

---

## Graph Options

| Option | What it does |
|---|---|
| Show typical | The shaded band, above |
| Normalize to | Rescale every curve to its own peak plasma or peak effect-site value, so drugs on wildly different scales can be compared in shape |
| Max time | How far the simulation runs |
| Plasma line / Effect site line | Line style, including none |
| Y axis height | Plot height in pixels |
| Time until threshold | Draws, for each drug, how long until it falls to its recovery threshold (`endCe` in the library). Only available when normalization is off |
| Log Y axis | Log-scale the concentration axis. Unavailable when Time until threshold, Events, or Interaction are showing, because those panels do not have a meaningful log form |

---

## Additional Plots

Checkboxes that add panels beneath the concentration plot.

### MEAC

Plots concentration as a multiple of the drug's **Minimum Effective Analgesic
Concentration**, so several opioids can be compared on one axis of clinical
effect rather than mass.

### Interaction

Models the synergy between propofol and opioid — the probability of no response
to laryngoscopy, using the Bouillon (*Anesthesiology* 2004;100:1360) response
surface. Opioids are converted to remifentanil equivalents first. This is the
panel that shows why a modest opioid dose lets you use much less propofol.

### Events

Draws clinical events on the time axis. The event list includes Induction,
Intubation, Extubation, Emergence, CPB Start/End with temperatures, Clamp
On/Off, Tourniquet On/Off, and Other.

---

## Suggest Dosing

The **Suggest Dosing** button works backwards: you say what effect-site
concentration you want and when, and it finds doses that get you there.

Enter time and target concentration pairs, choose the drug, and confirm.

Two limitations, both stated in the dialog:

- **Decreasing targets are not supported.** Rows that ask for a lower
  concentration than the one before are removed.
- Doses are found by non-linear regression, so it takes a moment. The result is
  good but not provably optimal.

---

## Drug Library and Drug Thresholds

**Drug Library** opens the table behind every drug: colours, units, the
therapeutic range that draws the shaded band, MEAC, and the recovery threshold.
Editing here changes how the current session behaves.

**Drug Thresholds** edits the recovery thresholds alone — the `endCe` values that
*Time until threshold* counts down to.

The library ships with 20 intravenous drugs: propofol, remifentanil, fentanyl,
alfentanil, sufentanil, morphine, pethidine, hydromorphone, methadone, ketamine,
dexmedetomidine, midazolam, etomidate, lidocaine, rocuronium, naloxone,
oxytocin, oxycodone, oliceridine, and remimazolam.

---

## Inhaled anaesthetics *(branch only)*

Not on `master` yet. On `inhaled-gas-engine`, seven further entries appear in the
drug list and behave like any other row in the dose table:

| Entry | Units | Meaning |
|---|---|---|
| air, oxygen, nitrousOxide | L/min | Fresh gas flows |
| sevoflurane, isoflurane, desflurane | % | Vaporiser settings |
| ventilation | L/min | Alveolar ventilation |

Set the flows, the vaporiser, and the ventilation, and the program simulates
alveolar and brain tensions for each agent, plus MAC. Nitrogen is carried
implicitly and washes out; you do not enter it.

The engine reproduces the Gas Man model, including the concentration and second
gas effect — nitrous oxide taken up in bulk concentrates whatever else is in the
alveolus, so sevoflurane rises faster in its presence. It has been validated
against Gas Man itself across five scenarios; see `inst/validation/VALIDATION.md`
for the record.

Cardiac output is fixed at 75 mL/kg and is not currently a user input.

---

## Time display

Above the dose table:

- **Elapsed minutes** — everything counted from zero.
- **Actual time** — enter a **Procedure start** as `HH:MM` and times display as
  clock times.

This changes display and entry only. The simulation is identical.

---

## Email Slide

Enter a recipient and optional comments, press **Send**, and the app emails a
PowerPoint slide of the current simulation, including the dose table and the
pharmacokinetic parameters used.

Requires the mail settings in `config.yml` to be filled in. A local development
copy made from `config.yml.sample` will not send mail until you do.

---

## Debug mode

Add `&debug=1` to the URL. A panel appears beneath the plot with a log and a
performance profiler, and a **Debug level** selector for normal or verbose
output. Off by default in production.

---

## Cautions

- **Predictions, not measurements.** Every curve is what a published model
  expects for a typical patient with the covariates entered. Individual patients
  differ, sometimes by a lot. Nothing here replaces clinical judgement or
  monitoring.
- **The models have domains.** Pharmacokinetic models are fitted over particular
  ranges of age, weight, and clinical condition. Extrapolating far outside those
  ranges — the very young, the very old, the very large, the critically ill —
  produces numbers, but the numbers deserve less confidence than the plot's
  crispness suggests.
- **Disabled covariates.** Pregnancy, CYP2D6, and renal function do not yet
  influence any prediction, even though the fields exist.
- **The interaction panel is one model of one stimulus.** Bouillon's surface
  describes response to laryngoscopy. It is not a general-purpose depth monitor.
- **The shaded band is orientation, not a target.** It is a published typical
  range, editable in the drug library, and it is not tailored to your patient.

---

## Where to go next

- **Examples and Help** — the link in the navigation bar, top right.
- `docs/architecture.md` — how the program is put together.
- `docs/adding-a-drug.md` — adding a drug or a pharmacokinetic model.
- `README.md` — installation and local setup.

---

## Notes for the author of this draft

Written by scanning the source, so it describes what the code does rather than
what was intended. Places worth a second look:

- The **Suggest Dosing** flow is described from the dialog text and `suggest.R`.
  It would be worth walking through a real case to check the description matches
  the experience.
- **Normalization** is described from the input label. The exact behaviour when
  several drugs are shown at once could use confirming.
- The **MEAC panel** description is inferred from the drug library column and the
  plot code; someone who uses it should check the wording.
- No screenshots. The guide would be much better with four or five.
- Nothing here covers the vignettes (`stanpumpR-single-PK`, `stanpumpR-multi-PK`)
  which document the scripting interface for people who want to drive the engine
  from R rather than the app.
