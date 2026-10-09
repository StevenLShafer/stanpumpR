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

1. Open the app. Tick the drugs you want to display (propofol, fentanyl,
   remifentanil and rocuronium are ticked to begin with) and press **Start**.
   A default patient and a dose table holding a zero dose of each are waiting.
2. In the **Doses** table, edit a row, or type another drug name — the cell
   autocompletes from the drug library.
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
| Adjust weight to fat-free mass | checkbox | On by default. See below. |
| Baseline serum osmolality | mOsm/kg | Before mannitol; 280 by default. Read only by mannitol, which is plotted as the serum osmolality it produces. See [docs/mannitol.md](mannitol.md). |

**Adjust weight to fat-free mass.** Most of the drug models were reported for a
typical 70 kg adult and, if they scaled at all, scaled with total body weight.
Drug clearance tracks lean tissue, not fat, so with this box ticked stanpumpR
computes the patient's fat-free mass from weight, height, age and sex
(Al-Sallami et al. 2015) and scales each model's volumes by the ratio of that to
the fat-free mass of a 70 kg, 170 cm man, and its clearances by the same ratio to
the 0.75 power. The reference man is unchanged; a 70 kg woman or a 120 kg man is
not. Doses you type per kilogram are still converted with total body weight.
Propofol and remifentanil already carry fat-free mass inside their published
models and ignore the box. Untick it to see what total-body-weight scaling
predicts, or to reproduce a simulation made before this option existed. The
full account, with worked examples, is in
[docs/weight-adjustment.md](weight-adjustment.md).

**Serum creatinine** (mg/dL) is optional. The renally cleared drugs (mannitol, vancomycin, gentamicin, cefazolin and sugammadex)
use it to estimate renal function; left blank, they assume a normal creatinine for
the patient's sex. **Pregnant** appears in the interface but is **currently
disabled**: no drug in the library yet responds to it.

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
  **infusion**; a mass unit makes it a **bolus**. For the drugs that support it,
  `Plasma target` and `Effect site target` make the row a **target**: the dose
  is then the concentration you want, and the program works out the infusion
  (see *Target-controlled infusion*).

An infusion runs from its time until the next row for that same drug changes it,
or until the end of the simulation. To stop an infusion, add a row for the same
drug at the stop time with a dose of zero.

### Entering times

Times are in the **Time units** chosen above the dose table (see *Time
display*), and the field is forgiving:

- `12` — twelve of the unit: twelve minutes, or twelve days
- `1.5` — one and a half of the unit
- `1:30` — with *Actual time*, the clock time 01:30 (with *Elapsed time* the
  table takes numbers only; an `H:MM` pasted in is read as hours and minutes)
- `130` — 130 of the unit, not 1:30

Minutes above 59 roll over, so `0:80` becomes `01:20`. A blank time or dose
becomes zero. An entry that is not one plain number (or, for a time, `H:MM`) is
not guessed at: `-5`, `5 mg`, `1.2.3` or `8;30` clears the cell, and the row is
ignored until it is corrected. Scientific notation is read (`1e3` is 1000).

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

- **Hover** over any curve for the precise concentration at that moment: Ce for a
  drug with an effect site, Cp for one without. The time is shown in the chosen
  time units, or as a time of day under Actual time.
- **Click** to add a dose of that drug at that time. This bypasses the draft and
  applies immediately.
- **Double-click** on a drug's curve to edit or delete that drug's doses.

---

## Graph Options

| Option | What it does |
|---|---|
| Show typical | The shaded band, above |
| Normalize to | Rescale every curve to its own peak plasma or peak effect-site value, so drugs on wildly different scales can be compared in shape |
| Max time | How far the simulation runs. The choices follow the Time units: 1–24 hours (minutes, hours), 2–365 days, 4–52 weeks |
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

## Target-controlled infusion

A target-controlled infusion (TCI) pump holds a concentration rather than a
rate: you set the concentration you want in the plasma or at the site of drug
effect, and the pump's pharmacokinetic model computes the infusion needed to
reach it quickly and then hold it. stanpumpR simulates such a pump for
propofol, remifentanil, alfentanil, sufentanil, fentanyl, lidocaine,
hydromorphone, etomidate and ketamine.

Enter a row for the drug with units `Plasma target` or `Effect site target`.
The dose is the target concentration, in the drug's concentration units per
ml (mcg/ml for propofol, ng/ml for the opioids). From that time the controller
takes over:

- **Reaching the target.** With the plasma targeted, the pump gives the bolus
  that fills the central compartment and then the infusion that holds it.
  With the effect site targeted, it gives the larger bolus that makes the
  effect-site concentration *peak* exactly at the target, with no overshoot.
  The plasma concentration overshoots, falls while the effect site rises, and
  the two meet at the target at the time of peak effect. After that, holding
  the plasma at the target holds the effect site there too, so that is what
  the controller does. The method is Shafer and Gregg, *J Pharmacokinet
  Biopharm* 1992;20:147, as implemented in the original STANPUMP.
- **The bolus is a rapid infusion** over the pump's 10-second update interval.
  On the rate panel it is written as a number rather than drawn, since its
  rate would flatten the rest of the panel to zero.
- **Changing the target.** A higher target gives another, smaller loading dose.
  A lower target turns the pump off until the concentration has fallen to the
  new target, then resumes.
- **A target of 0 stops the TCI infusion.**
- **Boluses are allowed** during a TCI infusion. The concentration rises, and
  the controller gives no more drug until it is back at the target.
- **Manual infusions are not.** Setting a target zeroes any infusion that is
  running for that drug, and entering an infusion row stops the TCI infusion:
  the two cannot run together.

A **TCI rate panel** appears below the concentration panels for each drug under
TCI, in mg/kg/min or mcg/kg/min. Hover over it for the rate at any moment. The
rate changes every 10 seconds, so these rows are kept out of the dose table,
where they would make it unusable; they are merged back in when a slide is
emailed, so the Excel dose table lists the pump's complete programme.

Two things the simulated pump does not model. It does not know about oral,
intramuscular or intranasal doses of the same drug, which it treats, like a
real pump would, as an unexpected addition. And it has no upper limit on its
rate, so the loading dose is always delivered within one interval.

---

## Suggest Dosing

The **Suggest Dosing** button is the older way to work backwards from a
concentration: you say what effect-site concentration you want and when, and
it searches for doses that get you there. For the drugs that offer target
units, a TCI target row gives a better answer, faster.

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

The library ships with the anaesthetic drugs (propofol, remifentanil, fentanyl,
alfentanil, sufentanil, morphine, pethidine, hydromorphone, methadone, ketamine,
dexmedetomidine, midazolam, etomidate, lidocaine, rocuronium, oxytocin,
oxycodone, oliceridine, remimazolam, codeine, hydrocodone, oxymorphone,
tramadol), the reversal agents (naloxone, sugammadex, neostigmine,
glycopyrrolate), seven antibiotics (cefazolin, clindamycin, cefalexin,
ceftriaxone, vancomycin, metronidazole, gentamicin), five corticosteroids
(hydrocortisone, methylprednisolone, dexamethasone, prednisolone, prednisone),
and amiodarone in two entries: **amiodarone** for long-term oral therapy (dosed
in mg/day PO, best viewed with *Time units* set to days or weeks), with its
active metabolite **desethylamiodarone**, and **amiodaroneIV** for the first one
to three days of intravenous therapy. The two amiodarone entries are separate
models and their concentrations do not add.

Three things to know about the antibiotics and steroids:

- **Enter the creatinine.** Several of these models (cefazolin, vancomycin,
  gentamicin, sugammadex) and mannitol carry a creatinine-clearance or eGFR
  covariate, computed from the **Serum creatinine** field. Left blank, it is an
  assumed normal creatinine (1.0 mg/dL in men, 0.8 in women): the decline of
  renal function with age is represented, renal impairment is not, and a patient
  with a raised creatinine will clear these drugs more slowly than the plot
  shows.
- **Some rows are not total concentration.** Cefazolin plots **unbound**
  cefazolin (the source model is written on free drug, and free time above MIC
  is the target). Prednisolone plots **free** prednisolone, and prednisone's own
  row is total prednisone with its prednisolone appearing on the prednisolone
  row. Hydrocortisone plots the **increment in total cortisol above baseline**
  from a linearised form of its source model, which is reasonable for stress
  doses and understates the tail at replacement doses. Each drug's reference
  text says what is plotted.
- **No effect site.** The antibiotics and steroids have no meaningful
  equilibration delay for the engine to model (the steroid effect is genomic
  and takes hours), so they are plotted as plasma only, like codeine.

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
| Naloxone | Dowling J et al., *Ther Drug Monit* 2008;30:490–496. [DOI 10.1097/FTD.0b013e3181816214](https://doi.org/10.1097/FTD.0b013e3181816214) (intravenous; clearance on lean body weight). Nasal spray derived from Laffont CM et al., *Front Psychiatry* 2024;15:1399803; k<sub>e0</sub> from Yassen A et al., *Clin Pharmacokinet* 2007;46:965–980 |
| Oxytocin | Eisenach, unpublished data<br>Second model: Tanaka et al |
| Oxycodone | Lamminsalo M et al., *Expert Opin Drug Deliv* 2019;16(6):649–656. [PMID 31092024](https://pubmed.ncbi.nlm.nih.gov/31092024/) |
| Oliceridine | Dahan A et al., *Anesthesiology* 2020;133(3):559–568. [PMID 32788558](https://pubmed.ncbi.nlm.nih.gov/32788558/) |
| Remimazolam | Eleveld DJ et al., *Br J Anaesth* 2025;135(1):206–217. [PMID 40312166](https://pubmed.ncbi.nlm.nih.gov/40312166/) |
| Sugammadex | Kleijn HJ et al., *Br J Clin Pharmacol* 2011;72:415–433. [DOI 10.1111/j.1365-2125.2011.04000.x](https://doi.org/10.1111/j.1365-2125.2011.04000.x) (total sugammadex; rocuronium binding not modelled) |
| Neostigmine | Calvey TN et al., *Br J Clin Pharmacol* 1979;7:149–155. [DOI 10.1111/j.1365-2125.1979.tb00915.x](https://doi.org/10.1111/j.1365-2125.1979.tb00915.x) — **patient 1 of six individual fits; no population model exists**. Time to peak effect from Heier T et al., *Anesthesiology* 2002;97:90–95 |
| Glycopyrrolate | Bartels C et al., *Br J Clin Pharmacol* 2013;76:868–879. [DOI 10.1111/bcp.12118](https://doi.org/10.1111/bcp.12118) (active cation; bromide-labelled dose) |
| Cefazolin | Komatsu T et al., *Antimicrob Agents Chemother* 2024;68:e00267-24. [DOI 10.1128/aac.00267-24](https://doi.org/10.1128/aac.00267-24) (**unbound** cefazolin) |
| Clindamycin | Bouazza N et al., *Br J Clin Pharmacol* 2012;74:971–977. [DOI 10.1111/j.1365-2125.2012.04292.x](https://doi.org/10.1111/j.1365-2125.2012.04292.x) (final table; IV and oral) |
| Cefalexin | Haynes AS et al., *Antimicrob Agents Chemother* 2024;68:e00182-24. [DOI 10.1128/aac.00182-24](https://doi.org/10.1128/aac.00182-24) (oral only, apparent parameters, **fitted in children**) |
| Ceftriaxone | Sanz-Codina M et al., *J Antimicrob Chemother* 2023;78:380–388. [DOI 10.1093/jac/dkac400](https://doi.org/10.1093/jac/dkac400) (total; six healthy men) |
| Vancomycin | Thomson AH et al., *J Antimicrob Chemother* 2009;63:1050–1057. [DOI 10.1093/jac/dkp085](https://doi.org/10.1093/jac/dkp085) |
| Metronidazole | da Silva Neto MJJ et al., *J Antimicrob Chemother* 2021;76:3212–3219. [DOI 10.1093/jac/dkab337](https://doi.org/10.1093/jac/dkab337) (IV; oral F 0.841 from Bergan 1984, absorption from an experimental tablet) |
| Gentamicin | Smit C et al., *J Antimicrob Chemother* 2020;75:3286–3292. [DOI 10.1093/jac/dkaa312](https://doi.org/10.1093/jac/dkaa312) (not in ICU) |
| Hydrocortisone | Bindellini D et al., *J Pharmacokinet Pharmacodyn* 2024;51:809–824. [DOI 10.1007/s10928-024-09934-7](https://doi.org/10.1007/s10928-024-09934-7) — **linearised** in the CBG-saturated regime; oral F 0.88 from Johnson 2018 |
| Methylprednisolone | Hong Y et al., *Pharm Res* 2007;24:1088–1097. [DOI 10.1007/s11095-006-9232-x](https://doi.org/10.1007/s11095-006-9232-x) (IV); oral F 0.82 from Al-Habet and Rogers 1989; oral absorption constant provisional |
| Dexamethasone | Hong Y et al., *Pharm Res* 2007;24:1088–1097 (IV, phosphate-labelled dose); oral F 0.81 from Spoorenberg 2014; oral and IM absorption from Krzyzanski 2021 |
| Prednisolone | Xu J, Winkler J, Derendorf H. *J Pharmacokinet Pharmacodyn* 2007;34:355–372. [DOI 10.1007/s10928-007-9050-8](https://doi.org/10.1007/s10928-007-9050-8) (reversible pair reduced to its exact mammillary equivalent; **free** prednisolone) |
| Prednisone | Xu J, Winkler J, Derendorf H, as above (oral prodrug; total prednisone, with prednisolone on the prednisolone row) |
| Mannitol | Kaneda K et al., *J Clin Pharmacol* 2010;50(5):536–543. [PMID 20051588](https://pubmed.ncbi.nlm.nih.gov/20051588/)<br>Osmolality: Rudehill A et al., *J Neurosurg Anesthesiol* 1993;5(1):4–12. [PMID 8431668](https://pubmed.ncbi.nlm.nih.gov/8431668/). See [docs/mannitol.md](mannitol.md). |
| Amiodarone | Pollak PT, Bouillon T, Shafer SL. *Clin Pharmacol Ther* 2000;67:642–652. [PMID 10872646](https://pubmed.ncbi.nlm.nih.gov/10872646/) (long-term oral; apparent parameters, constant daily input) |
| Desethylamiodarone | Pollak PT, Bouillon T, Shafer SL, as above (formed from all amiodarone cleared, mass basis; not dosed directly) |
| Amiodarone IV | Korth-Bradley JM et al. *J Clin Pharmacol* 1996;36:715–719. [PMID 8877675](https://pubmed.ncbi.nlm.nih.gov/8877675/) (acute intravenous therapy, first one to three days; no metabolite) |

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

**Most models are scaled to fat-free mass, not used exactly as published.** Unless
the *Adjust weight to fat-free mass* box is unticked, every model in the table
except propofol, remifentanil and oxytocin has its volumes and clearances scaled
from the published 70 kg values to the patient's fat-free mass. The published
parameters are what a 70 kg, 170 cm man receives. See
[docs/weight-adjustment.md](weight-adjustment.md).

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

- **Time units** — minutes, hours, days or weeks. A number typed as a time is
  in this unit, the time axis is labelled in it, and it sets the **Max time**
  choices. Changing it rewrites every time in the dose table in the new unit
  (90 minutes becomes 1.5 hours), so the doses stay where they were; it also
  applies any unapplied edits.
- **Time Display**
  - **Elapsed time** — everything counted from zero.
  - **Actual time** (minutes and hours only) — enter a **Procedure start** as
    `HH:MM`; a time with a colon is then a clock time, and a number is counted
    from the procedure start. Switching to elapsed time converts the clock
    times.

This changes display and entry only. The simulation is identical: it always
works in minutes.

Target-controlled infusions and inhaled agents are simulated only on plots of
7 days or less; Suggest Dosing is offered in minutes and hours. A dose beyond
the unit's longest Max time (24 hours, 365 days, 52 weeks) brings a
notification rather than a longer plot.

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
- **Disabled covariates.** Pregnancy does not yet influence any prediction,
  even though the field exists. A blank serum creatinine means an assumed normal
  one, not a measured one.
- **The interaction panel is one model of one stimulus.** Bouillon's surface
  describes response to laryngoscopy. It is not a general-purpose depth monitor.
- **The shaded band is orientation, not a target.** It is a published typical
  range, editable in the drug library, and it is not tailored to your patient.

---

## Where to go next

- **Help** — the Help tab in the navigation bar: this guide's material as pages, a generated
  page for every drug, the models and methods, and loadable teaching scenarios.
- `docs/architecture.md` — how the program is put together.
- `docs/adding-a-drug.md` — adding a drug or a pharmacokinetic model.
- `docs/weight-adjustment.md` — how patient weight, height, age and sex scale the models.
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
