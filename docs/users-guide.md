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

## Pharmacokinetic models and their sources

Every drug in stanpumpR is a published pharmacokinetic model, and which model was
chosen matters as much as the dose you typed. Propofol from Eleveld behaves
differently from propofol from Marsh or Schnider. The table below records the
model actually implemented for each drug.

Each entry is the `reference` field returned by that drug's model function in
`R/drugs_<name>.R`, so this table describes what the program computes rather than
what the literature offers.

| Drug | Model source |
|---|---|
| Propofol | Eleveld DJ et al., *Br J Anaesth* 2018;120(5):942–959. [PMID 29661412](https://pubmed.ncbi.nlm.nih.gov/29661412/) |
| Remifentanil | Minto CF et al., *Anesthesiology* 1997;86:10–23. [PMID 9009935](https://pubmed.ncbi.nlm.nih.gov/9009935/) |
| Fentanyl | Scott JC, Stanski DR. *J Pharmacol Exp Ther* 1987;240(1):159–166. [PMID 3100765](https://pubmed.ncbi.nlm.nih.gov/3100765/) |
| Alfentanil | Scott JC, Stanski DR. *J Pharmacol Exp Ther* 1987;240(1):159–166. [PMID 3100765](https://pubmed.ncbi.nlm.nih.gov/3100765/) |
| Sufentanil | Gepts E et al., *Anesthesiology* 1995;83(6):1194–1204. [PMID 8533912](https://pubmed.ncbi.nlm.nih.gov/8533912/) |
| Morphine | Lötsch J et al., *Clin Pharmacol Ther* 2002;72(2):151–162. [PMID 12189362](https://pubmed.ncbi.nlm.nih.gov/12189362/) |
| Pethidine (meperidine) | Björkman S, *J Pharmacokinet Pharmacodyn* 2003;30(4):285–307. [PMID 14650375](https://pubmed.ncbi.nlm.nih.gov/14650375/) |
| Hydromorphone | Drover DR et al., *Anesthesiology* 2002;97(4):827–836. [PMID 12357147](https://pubmed.ncbi.nlm.nih.gov/12357147/) |
| Methadone | Inturrisi CE et al., *Clin Pharmacol Ther* 1987;41(4):392–401. [PMID 3829576](https://pubmed.ncbi.nlm.nih.gov/3829576/) |
| Ketamine | Domino EF et al., *Clin Pharmacol Ther* 1984;36(5):645–653. [PMID 6488686](https://pubmed.ncbi.nlm.nih.gov/6488686/) |
| Dexmedetomidine | Adult: Dyck JB et al., *Anesthesiology* 1993;78(5):821–828. [PMID 8098191](https://pubmed.ncbi.nlm.nih.gov/8098191/)<br>Age ≤ 1 yr: Zuppa, *Br J Anaesth* 2019 |
| Midazolam | Mould DR et al., *Clin Pharmacol Ther* 1995;58(1):35–43. [PMID 7628181](https://pubmed.ncbi.nlm.nih.gov/7628181/) |
| Etomidate | Arden JR et al., *Anesthesiology* 1986;65(1):19–27. [PMID 3729056](https://pubmed.ncbi.nlm.nih.gov/3729056/) |
| Lidocaine | Schnider TW et al., *Anesthesiology* 1996;84(5):1043–1050. [PMID 8623997](https://pubmed.ncbi.nlm.nih.gov/8623997/) |
| Rocuronium | Plaud B et al., *Clin Pharmacol Ther* 1995;58(2):185–191. [PMID 7648768](https://pubmed.ncbi.nlm.nih.gov/7648768/) |
| Naloxone | Papathanasiou T et al., *Br J Anaesth* 2019;123(2):e204–e214. [PMID 30915992](https://pubmed.ncbi.nlm.nih.gov/30915992/) |
| Oxytocin | Eisenach, unpublished data<br>Second model: Tanaka et al |
| Oxycodone | Lamminsalo M et al., *Expert Opin Drug Deliv* 2019;16(6):649–656. [PMID 31092024](https://pubmed.ncbi.nlm.nih.gov/31092024/) |
| Oliceridine | Dahan A et al., *Anesthesiology* 2020;133(3):559–568. [PMID 32788558](https://pubmed.ncbi.nlm.nih.gov/32788558/) |
| Remimazolam | Eleveld DJ et al., *Br J Anaesth* 2025;135(1):206–217. [PMID 40312166](https://pubmed.ncbi.nlm.nih.gov/40312166/) |

### Reading these honestly

**Two entries are not peer-reviewed literature.** Oxytocin's human model comes
from unpublished Eisenach data, and its second model is cited only as "Tanaka et
al" without a volume or year. Dexmedetomidine's infant model is cited as "Zuppa,
*Br J Anaesth* 2019" without page numbers or a PMID. These three are the weakest
citations in the library and are flagged rather than tidied over.

**A citation is the disposition model, not a guarantee of fit.** Each of these
papers fitted a particular population — often healthy volunteers or elective
surgical patients of a particular age and size. The model is extrapolated
whenever your patient sits outside that population, and the plot gives no visual
hint when that is happening.

**Where a drug has two models**, stanpumpR picks between them on a covariate —
dexmedetomidine switches to the infant model at age ≤ 1 year, for example — so
the reference that applies depends on the patient you entered.

**The effect-site rate constant often comes from a different source than the
disposition model.** Where the two differ, that is noted in
`docs/drug-reference.html`, which also carries each drug's modelled time to peak
effect and its covariate scaling.

### Inhaled agents

The gas model parameters are Gas Man's, taken from `gasman.ini`, and are recorded
with their provenance — including which of them have *no* established provenance
— in `R/gasProperties.R` and `inst/validation/VALIDATION.md`. Nitrogen's MAC in
particular is carried at Gas Man's value while being known to be wrong by a
factor of about 55; it is not used in any calculation.

---

## Inhaled anaesthetics *(branch only)*

Not on `master` yet. On `inhaled-gas-engine`, seven further entries appear in the
drug list and behave like any other row in the dose table:

| Entry | Units | Meaning |
|---|---|---|
| air, oxygen, nitrousOxide | L/min | Fresh gas flows |
| sevoflurane, isoflurane, desflurane | % | Vaporiser settings |
| ventilation | L/min | Minute ventilation |

Set the flows, the vaporiser, and the ventilation, and the program simulates
alveolar and brain tensions for each agent, plus a **MAC equivalents** panel.
Nitrogen is carried implicitly and washes out; you do not enter it.

Two meanings of "MAC" are kept apart. *MAC* is a property of an agent: the
alveolar concentration at which half of patients do not move to incision, 2.1%
for sevoflurane at age 40, lower in the old and higher in the young. It does
not change during an anaesthetic. What changes is the patient's alveolar
concentration *as a multiple of that MAC* — the MAC equivalents panel, summed
over the potent agents present. "1 MAC of sevoflurane" means an alveolar
concentration of one MAC equivalent.

The engine reproduces the Gas Man model, including the concentration and second
gas effect — nitrous oxide taken up in bulk concentrates whatever else is in the
alveolus, so sevoflurane rises faster in its presence. It has been validated
against Gas Man itself across five scenarios; see `inst/validation/VALIDATION.md`
for the record.

Cardiac output is fixed at Gas Man's default, 5 L/min at 70 kg scaled by
(weight / 70)^0.75, and is not currently a user input.

Ventilation is **minute ventilation**. Thirty percent of it is taken to be dead
space, so the alveolar ventilation, which is what exchanges gas, is 70% of what
you enter.

Ventilation must be greater than zero whenever a gas is being given. If you
enter a gas without a ventilation row, one is added for you: 5.7 L/min at 70 kg,
scaled the same way as cardiac output. That is the minute ventilation whose
alveolar part is Gas Man's default alveolar ventilation of 4 L/min. Entering
nitrous oxide also adds an oxygen row, starting at 21% of the fresh gas. Gas
flows and ventilation are rounded to the nearest 0.1 L/min.

### Opioids and MAC

Opioids lower MAC. Under **Graph Options**, ticking *Include opioid - MAC
interaction* reports MAC equivalents relative to the opioid-reduced MAC, so
the same end-tidal concentration reads as more MAC equivalents when an opioid
is on board. The gas concentrations themselves do not change.

The opioids are combined by adding their effect-site concentrations, each as a
multiple of that opioid's MEAC: U = sum of Ce / MEAC. This is the total shown in
the % MEAC plot. The fractional reduction in MAC is then

    R = Emax x U^gamma / (U50^gamma + U^gamma)

and the MAC equivalents are divided by (1 - R). Nitrous oxide is already part
of the MAC equivalents, so it is included.

**This is an approximate model.** The published studies of opioid MAC reduction
do not agree well with one another. The parameters in use (Emax 0.9, U50 1.76,
gamma 1) are a rough fit to nine published points: they give a 50% reduction in
MAC at about 2.2 times MEAC and a ceiling of 90%, and they fall short of the
published reductions at low opioid levels. They are expected to be replaced.
The comparison with the published points is in the header of
`R/opioidMacInteraction.R`.

### Time until threshold, for the inhaled agents

*Time until threshold* (Graph Options) works for the inhaled agents and for MAC
equivalents as it does for the intravenous drugs: at every moment, how long until the
concentration would fall to the threshold if the agent were turned off right
then. It is drawn as a thin black line on each panel, read against the minute
labels at the right-hand edge. The Y axis is linear while it is showing.

What "turned off" means for a gas (S. Shafer, 2026-10-05):

- **Each agent is its own decision.** Turning off the vaporiser and turning off
  the nitrous oxide are separate adjustments, so each panel shows the time for
  that agent alone.
- **The fresh gas flow is turned up so that there is no rebreathing.** That is
  what is done to wake a patient, and it is the clinically important number.
  The time shown therefore does not depend on the flow in use at that moment.
  Any fresh gas flow at or above the minute ventilation achieves it.
- Ventilation stays as it is.

| Panel | What is timed | Default threshold |
|---|---|---|
| sevoflurane, isoflurane, desflurane | Vessel-rich group (brain) tension, the solid line | 0.1 x the age-adjusted MAC of that agent |
| nitrous oxide | Vessel-rich group (brain) tension | 10% |
| MAC equivalents | The summed series itself, which is alveolar, with every agent turned off | 0.1 MAC equivalents |
| oxygen | Not timed | none |

All of these can be changed in the Drug Thresholds dialog. The volatile agents
are shown there at the patient's age, so the number in the dialog is the number
on the plot; they follow MAC if the age is changed. Nitrous oxide comes off fast
enough that its threshold matters little.

Every line is calculated by simulating it: the agent is turned off at each
moment in turn and the washout is run forward, with the gases coupled as they
are in the engine, until the concentration comes down through the threshold.
Checked against making the same change in the dose table and simulating on, the
lines agree to within a few hundredths of a minute.

Remember what each line is asking. The sevoflurane line is the time if the
vaporiser alone is turned off. If nitrous oxide is turned off at the same
moment the sevoflurane goes faster, because nitrous oxide on its way out carries
it along: 15.7 minutes instead of 17.3 in one two-hour example.

One approximation remains, which makes the MAC time a little long: with
*Include opioid - MAC interaction* ticked, the opioid's effect on MAC is held at
its value at the moment the agents are turned off. In truth the opioid would
wear off too.

### Where the engine deliberately differs from Gas Man

The parameters and defaults are Gas Man's, and the intent for now is to give the
same answers Gas Man gives. Nine differences are deliberate (confirmed by
S. Shafer, 2026-10-05) and will remain:

| | Gas Man | stanpumpR | Why |
|---|---|---|---|
| Breathing circuit | Defaults to "Semi-closed": the whole circuit is one well-mixed 8 L volume, so some exhaled gas is rebreathed at any fresh gas flow, however high | The "Ideal" circuit, which Gas Man also offers: no rebreathing once fresh gas flow reaches minute ventilation; below that, the shortfall is made up with exhaled gas. No circuit volume, so no lag | It is how a circle system behaves. The mixing box has no threshold at fresh gas flow = ventilation and understates the inspired concentration at moderate and high flows |
| Ventilation and dead space | The ventilation setting is alveolar ventilation; there is no dead space | The ventilation setting is minute ventilation, 30% of it dead space. Rebreathing stops when fresh gas flow reaches the minute ventilation | Minute ventilation is what is set on a ventilator and read from a monitor |
| Oxygen consumption and gas volume | No oxygen, so no volume is lost to it | Oxygen consumed (3.5 mL/kg/min) shrinks the gas volume, as uptake of an anaesthetic does. Carbon dioxide replaces most of it in the alveoli and is then removed by the absorber from whatever exhaled gas is rebreathed | Without it the gas fractions do not add up at low flows. With 0.3 L/min of oxygen and 1 L/min of nitrous oxide, what leaves the circuit is the 1.3 L/min delivered less the 0.21 L/min consumed: 92% nitrous oxide and 8% oxygen, not the 77% and 23% delivered |
| MAC and age | One MAC per agent, no age term | MAC adjusted for the patient's age: MAC(age) = MAC40 x 10^(-0.00269 x (age - 40)) (Mapleson) | MAC falls about 6% per decade, and the patient's age is already an input |
| MAC across agents | Each agent reported separately | A single MAC-equivalents series, the sum of each potent agent's alveolar concentration as a fraction of its own MAC | Agents given together are additive, and one number is what is titrated to |
| Oxygen | Not modelled | Modelled in the circuit and alveoli, with metabolic consumption of 3.5 mL/kg/min; cannot go below zero | The inspired and alveolar oxygen matter whatever else is given, and a hypoxic mixture should be visible |
| Nitrogen | Carried only if nitrogen is added to the run as an agent | Always carried; its washout from the body is part of the summed uptake that couples the gases | The patient starts full of nitrogen whether or not anyone enters it, and it leaves through the same alveoli |
| Starting nitrogen | 80% (`Ambient=80`) | 78.07%, with oxygen at 20.93% | Room air, so that the gas fractions sum correctly once oxygen is modelled |
| Integration | Each time step is split into sequential sub-updates | Each step is advanced exactly, by matrix exponential | Accuracy does not then depend on the step size |

The breathing circuit follows the rule of thumb that rebreathing stops once
fresh gas flow reaches minute ventilation (Feldman JM, Lampotang S, Hendrickx J. Is rebreathing prevented when FGF equals MV? APSF, 20 October 2022. <https://www.apsf.org/article/is-rebreathing-prevented-when-fgf-equals-mv/>).
The model has no circuit volume, so a change at the vaporiser reaches the
patient at once; the gas already in a real circuit takes a little time to mix
out, which is not clinically important.

Carbon dioxide is not shown as a gas, but it is accounted for. Alveolar gas
holds about 5% of it (100 x carbon dioxide production / alveolar ventilation,
with production at 0.8 of oxygen consumption), so the alveolar concentrations
shown add up to about 95%; inspired gas, which has been through the absorber,
adds up to 100%. Because the patient breathes in slightly more than they breathe
out, the fresh gas flow that stops rebreathing is the minute ventilation plus
what is being taken up, a little above the minute ventilation itself.

Consequences worth knowing when comparing the two side by side:

- Enter in Gas Man the **alveolar** ventilation, 70% of the minute ventilation
  used here, and expect a small difference whenever fresh gas flow is below the
  minute ventilation, where Gas Man's ideal circuit has no dead space to return
  unused gas from.
- At low fresh gas flows expect the concentrations here to run higher than Gas
  Man's, because the oxygen consumed is no longer there to dilute them.
- Set Gas Man's circuit to **Ideal**. This is the largest of the differences.
  With Gas Man left on Semi-closed, 2% sevoflurane at 8 L/min gives an alveolar
  concentration of 0.47% at one minute and 1.59% at thirty; with the ideal
  circuit, here and in Gas Man, it is 1.09% and 1.71%.

- To reproduce a Gas Man MAC value, set the age to 40, where the age adjustment
  is exactly 1, and compare one agent at a time.
- The two integrations do not agree digit for digit at any fixed step size. They
  converge to a common answer as the step shrinks; `tests/testthat/test-gas-convergence.R`
  checks this.
- Add Nitrogen as an agent in Gas Man, delivered at 0% (or at 78% of any air
  flow), before comparing. Without it Gas Man leaves nitrogen washout out of the
  uptake coupling, which by itself moves alveolar sevoflurane by about 0.3-0.5%
  during a wash-in.
- Nitrogen differs slightly throughout because it starts from a different value.
  It is not an anaesthetic here and is not summed into MAC.

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
- The **model source table** was extracted from the `reference` field in each
  `R/drugs_*.R`. Those fields are correct on the `drug-reference-citations`
  branch but NOT on `master`, where several are still placeholders -- propofol's
  reads "Anesthesiology 1998" while the model implemented is Eleveld 2018. That
  branch should be merged before this guide is published, or the table will
  describe something the code does not say.
- Better still, the table should be GENERATED from the drug files rather than
  hand-maintained here, so it cannot drift. It drifted once already.
- Nothing here covers the vignettes (`stanpumpR-single-PK`, `stanpumpR-multi-PK`)
  which document the scripting interface for people who want to drive the engine
  from R rather than the app.
