---
file: NJ_label-cymbalta_021427_2023-08-21.pdf
pmid: none
doi: none
citation: "CYMBALTA (duloxetine) delayed-release capsules, prescribing information, NDA 021427 (Drugs@FDA 021427s055s057lbl.pdf), Eli Lilly, revised 8/2023; Reference ID 5229383."
kind: label
drugs: [duloxetine]
source_read: "text layer: Highlights, section 2 (Dosage and Administration), 7 (drug interactions, CYP1A2/CYP2D6), 8.5-8.10 (Specific Populations), 12.3 (Pharmacokinetics)"
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: discrepancy
---

# Cymbalta label 8/2023: duloxetine pharmacokinetics

## Study
Product label, section 12.3 and Use in Specific Populations.

## Model and parameters
Section 12.3:
- **Elimination half-life about 12 h (range 8 to 17 h)**; dose-proportional over the therapeutic range; steady state
  after about 3 days.
- Well absorbed. **Median lag before absorption (Tlag) 2 h; Cmax at 6 h.** Evening dosing: 3 h further delay and a
  one-third higher apparent clearance than morning dosing.
- Food: Cmax unchanged, Tmax 6 to 10 h, AUC about 10% lower.
- **Apparent volume of distribution about 1640 L.** Protein binding >90% (albumin, α1-acid glycoprotein).
- Hepatic metabolism by CYP1A2 and CYP2D6; parent about 3% of plasma radioactivity; <1% unchanged in urine; about 70%
  of the dose in urine as metabolites, 20% in feces. Metabolites not pharmacologically significant.
- **No absolute bioavailability value is given** anywhere in the label (it says only "well absorbed").
- No clearance value is given.

```yaml
parameters:
  - {name: t_half, value: 12, unit: h, source: "12.3", note: "about; range 8-17 h"}
  - {name: Tlag, value: 2, unit: h, source: "12.3 Absorption", note: median}
  - {name: Tmax, value: 6, unit: h, source: "12.3 Absorption", note: "fasting; 10 h fed"}
  - {name: V/F, value: 1640, unit: L, source: "12.3 Distribution", note: "apparent, average"}
  - {name: protein_binding, value: ">0.90", unit: fraction, source: "12.3 Distribution"}
```

## Pharmacodynamics and end points
No therapeutic concentration range in the label. Not an opioid.

## Covariates
- Pediatric 7-17 y: steady-state concentrations about 30% lower on average than adults, within adult range (12.3).
- Elderly women (65-77 y, single 40 mg): Cmax unchanged, AUC about 25% higher, t½ about 4 h longer; popPK clearance
  falls about 1% per year from 25 to 75 y (8.5).
- **Sex: half-life similar in men and women; no adjustment** (8.6).
- Smokers: AUC about one-third lower (8.7).
- Hepatic (Child-Pugh B, n = 6, 20 mg): clearance about 15% of normal, AUC 5-fold, t½ about 3-fold (8.9).
- ESRD on haemodialysis: Cmax and AUC about 100% higher; metabolites 7-9 fold; CrCl 30-80 mL/min no effect (8.10).
- Interactions (7): fluvoxamine 100 mg: AUC about 6-fold, Cmax 2.5-fold; paroxetine 20 mg: AUC +60%; fluvoxamine in
  CYP2D6 poor metabolizers: AUC and Cmax 6-fold.

## Caveats
Values are rounded label summaries without study identifiers.

## Used by stanpumpR
R/drugs_duloxetine.R:35 (F "about 50% by the label"), :47 (label mean half-life about 12 h, 8-17);
inst/help/drugs/duloxetine.md:13 (12 h comparison), 17 (bioavailability about 50%).

## Verification against stanpumpR
- Half-life about 12 h (8-17) (R:46-47; help:13): **matches**.
- **"F ... about 50% by the label"** (R:34-35; help:17 "about 50%"): **discrepancy**. This label states no
  bioavailability figure. The 50% value must come from elsewhere (published literature) and should be cited there.
- "there is no lag" (R:39; help:17): the label reports a **median 2 h Tlag** and Cmax at 6 h. The repo's source (Zhong
  2026, trough data, ka fixed) has no lag, so the omission is the source's, but it conflicts with the label's
  absorption profile: the model will peak earlier than the label's 6 h. Not stated in the repo; flagged.
- Sex covariate (R:23, 28, 75-76: women's CL/F 25% lower, t½ 21.9 h): the label says half-life is similar in men and
  women. Not a transcription error (it is Zhong's finding), but the label contradicts it; worth a note in the help.
- V/F 1530 L (Zhong) vs label about 1640 L: consistent (derived: 7% apart).
