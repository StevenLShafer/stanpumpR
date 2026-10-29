---
file: NJ_label-corlopam_019922_2020-09-04.pdf
pmid: null
doi: null
citation: "CORLOPAM (fenoldopam mesylate) injection, for intravenous use. US prescribing information, Hospira (Pfizer), NDA 019922 (supplement s019), revised 9/2020; FDA Reference ID 4665497. https://www.accessdata.fda.gov/drugsatfda_docs/label/2020/019922s019lbl.pdf"
kind: label
drugs: [fenoldopam, fenoldopamPediatric]
source_read: "text layer, all 13 PDF pages (PDF page n = printed 'Page n of 12'; PDF p1 is Highlights + contents, p13 the FDA signature page). Read Highlights (p1), 1 Indications and 2 Dosage and Administration (pp2-3), 5 Warnings (p3-4), 8 Use in Specific Populations (pp5-6), 11 Description (pp6-7), 12.1-12.3 Clinical Pharmacology (pp7-11). Rendered pages 2 (section 2.1) and 10 (Table 6, 12.3); confirmed from the images."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# CORLOPAM (fenoldopam) US label, 9/2020

## Study
This is the PLR-format FDA label for Corlopam, initial US approval 1997, revised 9/2020 (Reference ID
4665497). PDF page 1 is the Highlights of the named product, so there is no source file problem. PDF page 13 is
the FDA electronic-signature page (signed 09/02/2020).

**Indications (1, p2):**
- Adults: in-hospital, short-term (up to 48 h) management of severe hypertension.
- Children: in-hospital, short-term (up to 4 h) blood-pressure reduction.

**Dosage (2.1, p2, rendered):**
- Adults: start at **0.01 to 0.3 mcg/kg/min** IV, increasing by 0.05-0.1 mcg/kg/min no more often than every
  15 min. The maximal rate reported in studies was 1.6 mcg/kg/min. Doses under 0.1 mcg/kg/min with slow
  up-titration caused less reflex tachycardia.
- Children: start at 0.2 mcg/kg/min and titrate by 0.3-0.5 mcg/kg/min every 20-30 min to a **maximum of 0.8
  mcg/kg/min**. Higher doses generally gave no further fall in MAP but worsened tachycardia.
- Dilution: adults to 40 mcg/mL (Table 1), children to 60 mcg/mL (Table 2, p3).

**Strength basis (11, p7):** each mL contains fenoldopam mesylate equivalent to 10 mg fenoldopam. The dilution
tables give "mg of drug", so doses are fenoldopam base. The mesylate MW is 401.87.

## Model and parameters
The label gives no compartmental model. Section 12.3 (pp10-11) gives these descriptors:
```yaml
parameters:
  - {name: "t1/2 (adult, mild-moderate hypertension)", value: 5, unit: min, source: "12.3, p10", note: "about 5 min; little R/S difference"}
  - {name: "time to steady state (adult)", value: 20, unit: min, source: "12.3, p10", note: "about 20 min (4 half-lives)"}
  - {name: "Css proportionality range (adult)", value: "0.01-1.6", unit: mcg/kg/min, source: "12.3, p10", note: "steady-state concentrations proportional to rate"}
  - {name: "t1/2 (children 1 month-12 y)", value: "3-5", unit: min, source: "12.3, p11", note: "range as printed"}
  - {name: "CL (children 1 month-12 y)", value: 3, unit: L/h/kg, source: "12.3, p11", note: "as printed; Css proportional to dose 0.05-3.2 mcg/kg/min"}
  - {name: fraction excreted unchanged, value: 0.04, unit: fraction, source: "12.3, p11", note: "about 90% urine, 10% faeces (radiolabel); conjugation, no CYP"}
```
- The adult PK was not influenced by age, gender or race in hypertensive emergency (12.3, p10).
- Clearance was unchanged in ESRD on CAPD and in severe hepatic failure. Haemodialysis has not been studied.
- In children (8.4, p6), PK is independent of age once corrected for body weight.
- Metabolites are inactive in animals (p11). The R-isomer carries the activity, with about 250-fold higher D1
  affinity than the S-isomer (12.1, p7).

## Pharmacodynamics and end points
- **Mild to moderate hypertension (Table 4, p8):** 32 adults on fixed 48-h infusions of 0.04, 0.1, 0.4 and
  0.8 mcg/kg/min.
  - At 1 h, the SBP/DBP change was -22/-9, -22/-18, -26/-19 and -22/-21 mmHg (placebo -15/-5). HR rose up to
    +25 bpm at 0.8.
  - There was no greater response at 0.8 than at 0.4. Partial tolerance appeared at 48 h on the two higher rates.
- **Hypertensive emergencies (Table 5, pp8-9):** 94 adults at 0.01, 0.03, 0.1 and 0.3 mcg/kg/min. At 4 h the
  DBP change was -12, -18, -21 and -29 mmHg.
- **Severe hypertension, open-label (p9):** fenoldopam 0.1-1.5 mcg/kg/min against nitroprusside, response 79%
  against 77%.
- **Paediatric trial (pp9-10; Table 6, p10, rendered):** placebo or 0.05, 0.2, 0.8 or 3.2 mcg/kg/min blinded for
  30 min, then open-label.
  - Group sizes n = 16, 15 (MAP n = 14), 16, 15, 15, total 77, up to 12 y, Tanner 1-2. By age: 2 were under
    1 month, 25 were 1 month-1 y, 7 were 1-2 y and 43 were 2-12 y. 58 were enrolled with surgery and 19 in an ICU.
  - 0.2 mcg/kg/min was the lowest effective dose and 0.8 the dose of maximum effect.
  - MAP change at 30 min: 0, -1, -2, -10 and -10 mmHg (each ± 3). HR change: -6, -4, 5, 7 and 14 bpm.
  - Open-label rates reached 4 mcg/kg/min. Effects grew for 15-25 min.
- **Tachycardia (5.1, p3):** dose-related, especially above 0.1 mcg/kg/min in adults and above 0.8 in children.
- **Intraocular pressure (5.3, p4):** rose by a mean 6.5 mmHg at 0.05-0.5 mcg/kg/min in glaucoma.
- **Not given:** no EC50 and no effect-site delay. The adult concentration-effect relation is absent. Not an
  opioid: no MEAC.

## Covariates
- Body weight: dosing and paediatric CL are per kg.
- Age, gender and race: no effect on adult PK.
- Renal (CAPD) and hepatic failure: CL unchanged.
- No equations are given.

## Caveats
- This label is a revision issued years after the paediatric study. The adult starting range is now 0.01-0.3
  mcg/kg/min; older labels, as quoted by Hammer 2008, gave 0.05-0.3.
- The paediatric 3-5 min half-life and 3 L/h/kg are rounded summaries of the Hammer 2008 trial. They are not a
  separate study.
- Population conflict with Hammer 2008 (PMID_18837982): the label gives 19 of 77 children treated in an ICU.
  The paper describes all as surgical patients having deliberate hypotension. The cause is unresolved.
- Adult half-life "about 5 min" (label) against 9.8 min in Frederickson 1988 as the repo records it. The repo
  keeps these source-specific.

## Used by stanpumpR
- `R/drugs_fenoldopamPediatric.R:18-21` (8baa733): children's effective rate "up to 0.8 mcg/kg/min ... against
  the adult label's 0.05 to 0.3".
- `inst/help/drugs/fenoldopamPediatric.md:3`: the same contrast, "against the adult label's 0.05 to 0.3".
- Also relying on this label's paediatric maximum (not in the context list, found by content):
  - `R/drugs_fenoldopamPediatric.R:128-129`: "the label's paediatric dosing tops out at 0.8".
  - `inst/help/drugs/fenoldopamPediatric.md:21`: the Corlopam label titrates to a maximum of 0.8 mcg/kg/min.
  - `docs/vasoactive-review-2026-10-27.md:9`: the band change B1.
- `inst/extdata/antihypertensiveRegistry.csv:57` is the catalog row `fenoldopam__injection__iv__catalog__none__label`
  (source URL pfizermedical.com/corlopam, not this PDF). It cites "half-life about 5 min and steady state in
  about 20 min".

## Verification against stanpumpR
- **"the adult label's 0.05 to 0.3"** (`R/drugs_fenoldopamPediatric.R:20-21`;
  `inst/help/drugs/fenoldopamPediatric.md:3`): this archived 2020 label (2.1, p2, rendered) says to **initiate at
  0.01 to 0.3 mcg/kg/min**. It increases in 0.05-0.1 steps, and rates up to 1.6 mcg/kg/min were used in studies.
  "0.05 to 0.3" is the adult range Hammer 2008 quotes from an older package insert (PMID_18837982, abstract p1
  and Discussion p6). It is correct as a quotation of Hammer, but it is not what this label says.
  **discrepancy** (attribution/currency; the lower bound is 0.01 in this label).
- **Paediatric maximum 0.8 mcg/kg/min** (drug file 128-129; help 21; vasoactive review B1): this matches 2.1
  (p2) and 12.2 (p10): 0.2 was the lowest effective dose, 0.8 the maximum-effect dose, and higher doses brought no
  further MAP fall but more tachycardia. **matches**
- **Registry row 57's adult t1/2 about 5 min and steady state in about 20 min**: this matches 12.3 (p10), though
  the row links a different (Pfizer Medical) page. **matches**
- **The implemented paediatric model's CL** of 3.06 L/h/kg (Hammer) is consistent with the label's rounded
  3 L/h/kg and 3-5 min half-life for children (p11). The derived 3.15 min falls inside 3-5. **matches**
  (consistency check only; the repo cites Hammer for these numbers).
