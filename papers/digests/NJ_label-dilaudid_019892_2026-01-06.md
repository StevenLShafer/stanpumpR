---
file: NJ_label-dilaudid_019892_2026-01-06.pdf
pmid: none
doi: none
citation: Rhodes Pharmaceuticals. DILAUDID (hydromorphone hydrochloride) oral solution and tablets, CII. US prescribing information, NDA 019891/019892, revised 12/2025 (FDA Reference ID 5715795; accessdata.fda.gov 019891s034lbl019892s043lbl.pdf, posted 2026-01-06).
kind: label
drugs: [hydromorphone]
source_read: text layer; read Highlights, sections 2 (Dosage and Administration), 8.4-8.7, 12.1-12.3 in full; skimmed the rest
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
---

# DILAUDID oral label 2025: hydromorphone tablets and oral solution

## Study
Prescribing information, not a study. Products: DILAUDID tablets 2, 4, 8 mg and oral solution 5 mg/5 mL (1 mg/mL). PK statements summarise unnamed sponsor studies; n, ages and assay not given.

Dosing (section 2.3): oral solution 2.5 to 10 mg (2.5 to 10 mL) every 3 to 6 h as needed; tablets 2 to 4 mg every 4 to 6 h as needed. Supplemental dose 5-15% of total daily usage every 2 h as needed (2.6). Hepatic or renal impairment: start at one-fourth to one-half the usual dose (2.4, 2.5). Conversion from other opioids: start at half the usual starting dose; no conversion table or ratio given. Relative bioavailability of IR versus extended-release hydromorphone stated as unknown.

## Model and parameters
No compartmental model. Section 12.3:
```yaml
parameters:
  - {name: F_oral_8mg_tablet, value: 0.24, unit: fraction, source: "12.3 Absorption", note: "approximately 24%, CV 21%, single dose"}
  - {name: Cmax_8mg_tablet, value: 5.5, unit: ng/mL, source: "12.3 table", note: "mean (CV 33%); column header printed as '(ng)'"}
  - {name: Tmax_8mg_tablet, value: 0.74, unit: h, source: "12.3 table", note: "mean (CV 34%)"}
  - {name: AUC_8mg_tablet, value: 23.7, unit: ng*h/mL, source: "12.3 table", note: "mean (CV 28%)"}
  - {name: t_half_8mg_tablet, value: 2.6, unit: h, source: "12.3 table", note: "mean (CV 18%)"}
  - {name: Cmax_8mg_oral_liquid, value: 5.7, unit: ng/mL, source: "12.3 table", note: "mean (CV 31%)"}
  - {name: Tmax_8mg_oral_liquid, value: 0.73, unit: h, source: "12.3 table", note: "mean (CV 71%)"}
  - {name: AUC_8mg_oral_liquid, value: 24.6, unit: ng*h/mL, source: "12.3 table", note: "mean (CV 29%)"}
  - {name: t_half_8mg_oral_liquid, value: 2.8, unit: h, source: "12.3 table", note: "mean (CV 20%)"}
  - {name: Vss_IV, value: 302.9, unit: L, source: "12.3 Distribution", note: "mean (CV 32%), after IV bolus"}
  - {name: CL_IV, value: 1.96, unit: L/min, source: "12.3 Elimination", note: "mean (CV 20%)"}
  - {name: t_half_IV, value: 2.3, unit: h, source: "12.3 Elimination", note: "terminal, 'about'"}
  - {name: protein_binding, value: "8-19", unit: "%", source: "12.3 Distribution"}
```
Other facts (12.3): peak generally at 0.5 to 1 h; Cmax and AUC0-24 dose-proportional over 2-8 mg; tablet and oral solution bioequivalent at 8 mg (limits not stated). Food (8 mg as 2 mg tablets): Cmax down 25%, Tmax later by 0.8 h, AUC up 35%. >95% of the dose metabolised to hydromorphone-3-glucuronide plus minor 6-hydroxy reduction metabolites; little excreted unchanged.

Derived: oral CL/F = 8000 ug / 23.7 ng.h/mL = 338 L/h = 5.63 L/min (tablet). With the label's own IV CL 1.96 L/min this would imply F ~0.35, not 0.24; the two figures presumably come from different studies (derived; the label does not reconcile them).

## Pharmacodynamics and end points
- MEAC: no value. Section 12.2 states only that the minimum effective analgesic concentration varies widely among patients and may rise over time with tolerance or worsening pain.
- No ke0, t1/2ke0, tPeak or hysteresis reported. Concentration-adverse reaction relationship stated qualitatively (12.2).
- No equianalgesic or potency ratio given (conversion advice is generic).

## Covariates
Hepatic (12.3): single 4 mg oral dose, Cmax and AUCinf about 4-fold higher in moderate (Child-Pugh B) impairment; severe not studied. Renal: 4 mg oral, Cmax and AUC0-48 2-fold higher with CLcr 40-60 mL/min and 3-fold with CLcr < 30 mL/min (versus > 80); terminal t1/2 40 h in severe versus 15 h in normal function (as printed; this "15 h" conflicts with the 2.3-2.8 h t1/2 elsewhere in the label). Age: no effect in geriatric population. Sex: females about 25% higher Cmax with comparable AUC0-24.

## Caveats
Summary statements without n, assay, or study identity. Cmax column unit misprinted "(ng)". IV CL and oral F appear not to come from the same subjects. The renal-impairment half-lives (15 h normal) are inconsistent with the 2.3 h IV half-life, probably from a longer sampling window. The injection label (NDA 019034, separate digest) gives 3-fold versus 4-fold differently for severe renal impairment.

## Used by stanpumpR
- R/drugs_hydromorphone.R:64 (context :64): F = 0.225 said to agree with the DILAUDID label (about 24% for the 8 mg tablet).
- R/drugs_hydromorphone.R:80-90 (context :51-53): quoted bioequivalence sentence; liquid units take the tablet's oral parameters, not a separate formulation.
- inst/help/drugs/hydromorphone.md:15: same two points.
- tests/testthat/test-drugs-hydromorphone.R:120 (context :115): test that liquid equals tablet, citing the label.
- docs/meac-exposure.md:240: F 0.225 agrees with the DILAUDID label (about 24%).

## Verification against stanpumpR
- F about 24% for the 8 mg tablet (drugs_hydromorphone.R:64; hydromorphone.md:15; meac-exposure.md:240): **matches** (12.3, CV 21%). Independent cross-check (derived): the model's oral AUC for 8 mg is 8 mg x 0.225 / 1.2992 L/min = 23.1 ng.h/mL against the label's 23.7 (tablet), so the model's apparent oral clearance (5.77 L/min) reproduces the label's (5.63 L/min) within 3%.
- Quoted bioequivalence sentence (drugs_hydromorphone.R:80-82): **matches** verbatim. The gloss "i.e. Cmax and AUC within the 80-125% limits" is the code's inference; the label does not state the limits (its means are within 4%).
- "No published human study gives the solution's own Cmax or tmax" (drugs_hydromorphone.R:83-84) and "no study reports the solution's own absorption" (hydromorphone.md:15): **discrepancy (wording)**. The label's own 12.3 table gives the 8 mg oral liquid Cmax 5.7 ng/mL, Tmax 0.73 h, AUC 24.6 ng.h/mL and t1/2 2.8 h. Those numbers support the design choice (Tmax 0.73 versus 0.74 h for the tablet), so only the statement is wrong, not the model.
- Test (test-drugs-hydromorphone.R:120): **matches** the label's bioequivalence claim.
- Observation, not a citation discrepancy: the model's peak after 8 mg oral (derived by scaling 1.18 ng/mL at 2.6 mg, about 3.6 ng/mL at 46 min) is below the label's mean Cmax 5.5 ng/mL at 0.74 h; Cmax as a mean of individual peaks always exceeds the peak of the mean curve, but the gap is about 35%. The label's IV disposition (CL 1.96 L/min, Vss 302.9 L, t1/2 2.3 h) differs from the Drover model the code uses (CL 1.30 L/min; derived Vss = 11.2 x (1 + 10 + 84.2) = about 1066 L); the code does not cite the label for disposition.
