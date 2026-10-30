---
file: NJ_web-page-meeting-fba2e8.pdf
pmid: null
doi: null
citation: "Menon D, Mondick JT, Jayaraman B, Cheung AT, Barrett JS. Pharmacokinetics/pharmacodynamics of intravenous bolus nicardipine in adults undergoing cardiovascular surgery. Poster, PAGE meeting 2007 (Children's Hospital of Philadelphia and University of Pennsylvania). https://www.page-meeting.org/wp-content/uploads/pdf_assets/7146-PAGE_2007_NIC.pdf"
kind: pkpd-model
drugs: [nicardipine]
source_read: "page image (the single poster page rendered; Tables 1-3, Figures 1-3 and the Introduction read on zoomed crops at scale 6). The text layer is a wide multi-column dump and was used only for orientation."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Menon 2007 (PAGE poster): population PK/PD of IV bolus nicardipine in cardiac surgery

## Study
A one-page poster. The PDF metadata give Acrobat PDFMaker for PowerPoint, created 2007-06-07, and the URL names
PAGE 2007.
- **Patients:** 40 anaesthetised adults at the end of cardiovascular surgery, randomised to one of four IV bolus
  doses: 0.25, 0.5, 1.0 or 2.0 mg, n = 10 each (Table 1).
- **Demographics, mean ± SD by group** (Table 1): age 71±3, 65±10, 71±11 and 70±12 years; weight 77±12, 74±12,
  86±19 and 81±17 kg; M/F 8/2, 8/2, 9/1 and 7/3.
- **Anaesthesia:** midazolam, fentanyl, pancuronium and isoflurane.
- **Dosing:** a rapid bolus (<5 s) into the right atrial port of the pulmonary artery catheter, given when SBP
  exceeded 120 mmHg.
- **Sampling:**
  - PK: predose and at 2, 5, 7, 10, 20, 30, 90, 120, 180, 180 and 240 min. The table prints 180 twice.
  - PD: systolic and diastolic pressure predose, every 15 s for up to 240 s after the dose, then every 60 s up to
    30 min.
  - The poster does not state the sampling site or the assay.
- **Modelling:** NONMEM version 6 (Globomax), FO and FOCE with eta-epsilon interaction, exponential IIV, combined
  additive and proportional residual error for PK, additive for PD. The PD end point modelled is MAP (Table 3,
  Figure 5).

## Model and parameters
- **PK:** 2 compartments (Vc, Vp, CLd, CL/V; Figure 4).
- **PD:** an indirect-response model with a tolerance moderator (Figure 4). The drug drives response R (kin,
  kout); a moderator M, formed and lost at ktol, feeds back on R ("PD tolerance", Table 2).
- **PD model building** (Table 2):

  | Model | Result |
  |---|---|
  | Inhibitory Emax | FO OFV 8666, "extremely poor fits", unstable with FOCEI |
  | Indirect response | FO OFV 8318, fails to describe the MAP drop, unstable |
  | Indirect response with moderator | FO OFV 7445; FOCEI OFV 7266; good description of MAP |

Table 3, "current" PK/PD model (rendered). The "Mean value (%CV)" column is a typical value with what is
presumably its precision; the poster does not define it. IIV is given as "% IIV (%CV)".
```yaml
parameters:
  - {name: CL, value: 43.5, unit: L/h, source: "Table 3", note: "(33.1); IIV 136 (28.9)"}
  - {name: V1, value: 1.18, unit: L, source: "Table 3", note: "(51.4); IIV 196 (31.34)"}
  - {name: Q, value: 17.5, unit: L/h, source: "Table 3", note: "(37.2); no IIV"}
  - {name: V2, value: 4.44, unit: L, source: "Table 3", note: "(17.3); no IIV"}
  - {name: Kin, value: 3170, unit: mmHg/h, source: "Table 3", note: "(12.6); IIV 10.3 (44)"}
  - {name: Kout, value: 36, unit: 1/h, source: "Table 3", note: "(12.6); IIV 8.6 (60.7)"}
  - {name: Emax, value: 1, unit: "-", source: "Table 3", note: fixed}
  - {name: EC50, value: 361, unit: "mcg/mL as printed", source: "Table 3", note: "(22.9); IIV 123 (38.4); unit conflicts with the concentration axes in mcg/L, see text"}
  - {name: Ktol, value: 0.00373, unit: 1/h, source: "Table 3", note: "(29.7); IIV 144 (40)"}
  - {name: PK proportional error, value: 30, unit: "%", source: "Table 3", note: "(21.6)"}
  - {name: PK additive error, value: 2.2, unit: ug/L, source: "Table 3", note: "(26.8)"}
  - {name: PD additive error, value: 18.8, unit: mmHg, source: "Table 3", note: "(12.8)"}
```
Derived from Table 3 (my arithmetic):
- Baseline MAP = Kin/Kout = 3170/36 = 88 mmHg.
- Response turnover t1/2 = ln 2/36 h, about 1.2 min.
- Tolerance t1/2 = ln 2/0.00373 h, about 186 h. That is far longer than the 30-min PD record.
- V1 1.18 L is about 0.015 L/kg at 80 kg.

**Clearance rises with dose.** Figure 3, a box plot of clearance by dose group (unit not on the axis), puts mean
CL near 17 and 14 for groups 1-2, near 33 for group 3 and near 60 for group 4 (read from the figure). The
Discussion reports a graded increase in clearance with dose that no covariate explained, and proposes that more
vasodilation raises distributional or elimination clearance.

**Introduction values.** These are background, not this study's estimates, and are uncited: tri-exponential
half-lives of 2.7 min, 44.8 min and 14.4 h; linear PK over 0.5-4 mg/h; onset about 2 min.

**Observed data** (Figure 1, by group): plasma nicardipine on axes labelled **mcg/L**, with peaks about 400,
270, 160 and 265 mcg/L in groups 1-4. MAP is plotted on a second axis in mmHg.

## Pharmacodynamics and end points
- **End point:** mean arterial pressure (mmHg) after an IV bolus in anaesthetised cardiac-surgery patients.
- **Model:** an indirect response with Emax fixed at 1 and EC50 361. The poster prints the unit as **mcg/mL**,
  while its concentration axes (Figure 1) and its PK additive error (ug/L) are in mcg/L. Measured
  concentrations, at most about 400 mcg/L = 0.4 mcg/mL, are about 1000-fold below 361 mcg/mL. They are of the
  same order as 361 only if it is read as mcg/L. That reading is my inference; the poster does not resolve the
  conflict.
- **Other PD values:** no ke0 and no direct-effect EC50. Tolerance is modelled with Ktol 0.00373 /h.
- **Confounding treatment:** several group 3-4 patients received phenylephrine for severe hypotension, which
  the authors say blunts the observed MAP fall.

## Covariates
None retained. No patient characteristic tested explained the dose-related increase in clearance (Discussion).

## Caveats
Limitations the authors give:
- The sampling was not optimal for the clinically effective half-life.
- Single-dose data are not enough to derive dosing guidelines.
- Phenylephrine confounds MAP in groups 3-4.

Limitations I notice:
- The estimates are interim: the table title says "current" model.
- The EC50 unit conflicts with the poster's own figures, as described above.
- The CL IIV is about 136%, and clearance changes with dose, so no single fixed-parameter vector describes all
  four groups.
- The central volume is below plasma volume. The bolus went into the right atrium; this poster does not say where
  samples were drawn.

## Used by stanpumpR
The context file names `inst/extdata/antihypertensiveParameters.csv:116-117`. At 8baa733 the rows citing this
poster are at **lines 133-134** (located by content):
- `CL (poster)` 43.5 L/h, and `EC50 (poster)` 361 "mcg/mL as printed".
- Model `nicardipine__injection__iv__cardiac_surgery_adults__concentration__page2007`.
- Generated by `data-raw/antihypertensiveRegistry.R:1693-1702`.

Related rows and files:
- Lines 132 and 135-137 of the same CSV carry the abstract's CL 23.6, V1 0.22, Q 10.3 and V2 3.08 from a
  different URL, the PAGE abstract page.
- The model's registry row is `inst/extdata/antihypertensiveRegistry.csv:93`, status `requires_source_extraction`,
  not ported.
- `docs/antihypertensives.md:683-685` lists "PAGE nicardipine 2007" as deliberately skipped.
- `inst/extdata/vasoactiveRegistry.csv:32` links this model id to Cheung 1999, the same 40-patient study. The
  library's nicardipine drug (`R/drugs_nicardipine.R`) uses a different source and does not cite this poster.

## Verification against stanpumpR
- `antihypertensiveParameters.csv:133`, CL (poster) 43.5 L/h: Table 3 prints 43.5 L/h. The note's "interim
  values (43.5, 1.18, 17.5, 4.44)" matches Table 3's CL, V1, Q and V2 exactly; the poster calls the model
  "current". Population "40 anaesthetised adults" is Table 1's 4 × 10. **matches**
  - The abstract values in the same note (23.6 and so on) are not on this poster and cannot be checked here.
- `antihypertensiveParameters.csv:134`, EC50 (poster) 361 "mcg/mL as printed", end point `map`: Table 3 prints
  "EC50 (mcg/mL) 361 (22.9)", and the Figure 1 concentration axes read "Plasma nicardipine concentration (mcg/L)".
  The note's thousandfold conflict inside one poster is confirmed, and the end point is MAP. **matches**
- `antihypertensiveRegistry.csv:93`, limitations: "the associated poster prints different values (43.5, 1.18,
  17.5, 4.44) and an EC50 of 361 mcg/mL against plots labelled mcg/L" is correct. "Clearance INCREASED with dose
  and effect" agrees with the poster's Discussion and Figure 3. Rapid bolus 0.25-2 mg and 40 anaesthetised
  cardiovascular-surgery adults are both correct (Table 1). **matches**
- `docs/antihypertensives.md:683-685` ("the abstract and the poster disagree on clearance"): the poster's CL is
  43.5 L/h, against 23.6 in the repo's abstract rows. The disagreement is real as far as this poster goes.
  **matches**
