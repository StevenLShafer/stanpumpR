---
file: NJ_label-narcan_208411_2015-11.pdf
pmid: none
doi: none
citation: "Adapt Pharma, Inc. NARCAN (naloxone hydrochloride) nasal spray: full prescribing information, Patient Information and Instructions for Use. NDA 208411, original approval labeling (FDA file 208411lbl.pdf), Revised 11/2015, FDA Reference ID 3848912. Supplied by S. Shafer 2026-10-27; source URL not recorded."
kind: label
drugs: [naloxone]
source_read: "text layer, all 18 PDF pages (no printed page numbers): Highlights p1; contents p2; 1-2 Indications and Dosage and Administration pp3-4; 3-5 pp4-6; 6 Adverse Reactions p6; 8 Use in Specific Populations pp6-8; 11 Description pp8-9; 12.1-12.3 pp9-11 (Table 1 and Figure 1 span pp9-10); 13 and 16 pp11-12; 17 pp12-13; Patient Information pp14-15; Instructions for Use pp16-17; Quick Start Guide p18. Table 1 and Figure 1 read on page images of pp9-10 (the text layer scrambles the table)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 408eee2
verification: matches
---

# FDA 2015: NARCAN nasal spray 4 mg prescribing information (original, NDA 208411)

The original prescription label of NARCAN nasal spray, which the archive lacked (the file
`NJ_label-narcan_208411_2026-02-12` is the 2026 OTC carton). Its section 12.3 carries the 4 mg nasal exposure that the
repo now attributes to the FDA clinical pharmacology review of the same NDA.

## Study
Section 12.3 (p9) describes one pharmacokinetic study in **30 healthy adults** (N = 29 per column in Table 1; N = 28
for the relative bioavailability of 4 mg): one spray in one nostril (4 mg, 0.1 mL of 40 mg/mL naloxone HCl) and one
spray in each nostril (8 mg), against a single **0.4 mg intramuscular** injection into the gluteus maximus. Subjects did
not breathe through the nose during spraying and stayed supine for about an hour. Design (crossover or not), assay and
sampling times are not given. The same 30 subjects are the adverse-reaction base (section 6, p6).

Dosing (2.2, p4): one spray (4 mg naloxone HCl) in adults and in pediatric patients of all ages; if there is no
response or a relapse, a new spray every 2 to 3 minutes in alternate nostrils until help arrives. Product: 4 mg
naloxone HCl in 0.1 mL (11, p9), molecular weight 363.84 for the hydrochloride; benzalkonium chloride, edetate
disodium, NaCl, HCl; pH 3.5-5.5.

## Model and parameters
No compartmental model. Table 1 (pp9-10, page image), headed mean pharmacokinetic parameters (CV%); tmax is median
(minimum, maximum):

| Parameter | 4 mg, one spray | 8 mg, two sprays | 0.4 mg IM |
|---|---|---|---|
| tmax (h) | 0.50 (0.17, 1.00) | 0.33 (0.17, 1.00) | 0.38 (0.08, 2.05) |
| Cmax (ng/mL) | 4.83 (43.1) | 9.70 (36.0) | 0.88 (30.5) |
| AUCt (h.ng/mL) | 7.87 (37.4) | 15.3 (23.0) | 1.72 (22.9) |
| AUC0-inf (h.ng/mL) | 7.95 (37.3) | 15.5 (22.7) | 1.76 (22.6) |
| t1/2 (h) | 2.08 (29.5) | 2.10 (32.4) | 1.24 (25.9) |
| Dose-normalized relative BA vs IM (%) | 46.7 (31.4), N = 28 | 43.9 (23.8) | 100 |

```yaml
parameters:
  - {name: Cmax_4mg_IN, value: 4.83, unit: ng/mL, source: "Table 1, p10", note: "mean (CV 43.1%), N = 29"}
  - {name: AUCt_4mg_IN, value: 7.87, unit: h.ng/mL, source: "Table 1, p10", note: "mean (CV 37.4%)"}
  - {name: AUCinf_4mg_IN, value: 7.95, unit: h.ng/mL, source: "Table 1, p10", note: "mean (CV 37.3%)"}
  - {name: tmax_4mg_IN, value: 0.50, unit: h, source: "Table 1, p10", note: "median, range 0.17-1.00"}
  - {name: t_half_4mg_IN, value: 2.08, unit: h, source: "Table 1 p10; Elimination p11", note: "CV 29.5%; IM 1.24 h"}
  - {name: relBA_4mg_IN_vs_IM, value: 46.7, unit: "%", source: "Table 1 p10; text p10", note: "dose-normalized vs 0.4 mg IM = 100, N = 28"}
  - {name: relBA_8mg_IN_vs_IM, value: 43.9, unit: "%", source: "Table 1 p10; text p10", note: "dose-normalized"}
  - {name: Cmax_8mg_IN, value: 9.70, unit: ng/mL, source: "Table 1, p10", note: "mean (CV 36.0%)"}
  - {name: AUCinf_8mg_IN, value: 15.5, unit: h.ng/mL, source: "Table 1, p10"}
  - {name: Cmax_IM_0.4mg, value: 0.88, unit: ng/mL, source: "Table 1, p10"}
  - {name: AUCinf_IM_0.4mg, value: 1.76, unit: h.ng/mL, source: "Table 1, p10"}
  - {name: t_half_neonate_injection, value: 3.1, unit: h, source: "Elimination, p11", note: "mean +/- 0.5 SD, neonatal study of naloxone injection"}
  - {name: CL_F_4mg_IN, value: 503, unit: L/h, source: "derived", note: "(derived: 4 mg HCl / 7.95 h.ng/mL; 8 mg / 15.5 = 516; 0.4 mg IM / 1.76 = 227)"}
```
The median tmax of either nasal dose did not differ significantly from IM (p10). Figure 1 (p10): mean (SD) plasma
naloxone to 6 h; the 4 mg mean curve peaks near 4.5 ng/mL at about 20-30 min (read from the figure). No absolute
bioavailability (no intravenous arm). Distribution: weak protein binding, mainly albumin; crosses the placenta.
Metabolism: hepatic glucuronidation, naloxone-3-glucuronide major. Excretion (after oral or IV doses): 25-40% as urinary
metabolites in 6 h, about 50% in 24 h, 60-70% in 72 h (p11).

## Pharmacodynamics and end points
No MEAC, ke0, tPeak or potency ratio (naloxone is an antagonist). Section 12.2 (p9): after IV naloxone, onset is
usually within 2 minutes, later after SC or IM; duration depends on dose and route. The nasal half-life (2.08 h)
exceeds the IM one (1.24 h), stated in the label without interpretation. No effect end point was measured in the PK
study.

## Covariates
- Pediatric (8.4, pp7-8): may be given at all ages; nasal absorption in children may be erratic or delayed; monitor
  at least 24 h. Supporting parenteral pediatric data used 0.005-0.01 mg/kg.
- Geriatric (8.5, p8): exposure can be higher with reduced hepatic, renal or cardiac function; no numbers.
- Pregnancy (8.1) and lactation (8.2): crosses the placenta; naloxone is minimally orally bioavailable. No weight,
  sex, renal or hepatic dose adjustments.

## Caveats
- One healthy-volunteer study, supine, coached not to sniff; n = 29 analysed. Means are not called geometric here; the
  archived FDA review card (`NJ_fda-review-naloxone_208411_208411Orig1s000ClinPharmR`) records the same digits from its
  Table 2.4.1b as geometric means. The difference in description is not resolved by this document.
- Relative, not absolute, bioavailability: the 0.4 mg IM reference is taken as 100.
- This is the 2015 original label; later Rx and OTC editions exist and were not compared.

## Used by stanpumpR
Locations found by content at 408eee2 (the context's lines were from an older commit):
- `R/drugs_naloxone.R:43-55`: nasal F 0.19 derived from Laffont's CL/F; said to agree with the exposure in the FDA
  clinical pharmacology review of NARCAN (NDA 208411, 2015, Table 2.4.1b: AUC 7.9 ng.h/mL after 4 mg, CL/F about
  500 L/h); contrasted with the review's dose-normalized bioavailability relative to 0.4 mg IM, 0.47 (4 mg) and 0.44
  (8 mg); a closing parenthesis says the note once called this the Narcan label's exposure and that no NARCAN
  prescribing information is in the reference archive.
- `inst/help/drugs/naloxone.md:9`: the same statements (CL/F near 500 L/h; 0.47 and 0.44, dose-normalized, relative
  to 0.4 mg IM).
- `tests/testthat/test-drugs-naloxone.R:115-118`: a comment gives a Narcan 4 mg peak of about 4.8 ng/mL, citing the
  review's Table 2.4.1b; the test checks the model's 4 mg IN peak lies between 3 and 10 ng/mL.

## Verification against stanpumpR
- AUC 7.9 ng.h/mL after 4 mg and CL/F about 500 L/h (`R/drugs_naloxone.R:46-48`, `naloxone.md:9`): **matches**.
  Table 1: AUCt 7.87, AUC0-inf 7.95 h.ng/mL; derived CL/F 4 mg / 7.95 = 503 L/h with the dose as the hydrochloride
  (about 453 L/h if the 4 mg is converted to base, 3.60 mg; derived).
- 0.47 for 4 mg and 0.44 for 8 mg, dose-normalized, relative to 0.4 mg IM (`R/drugs_naloxone.R:48-51`, `naloxone.md:9`):
  **matches** Table 1 (46.7% and 43.9%), with the reference IM route and dose as stated. The older wording the old card
  checked, "0.47-0.52 relative to intramuscular", is no longer on master; this label prints no 0.52 and no range.
- Peak about 4.8 ng/mL after 4 mg (`tests/testthat/test-drugs-naloxone.R:115-116`): **matches** Cmax 4.83 ng/mL.
- So the label now supports every number that PR #513 re-attributed to the review; the label and the review print the
  same values. One statement is now out of date (not a number): the parenthesis at `R/drugs_naloxone.R:52-55` says
  no NARCAN prescribing information is in the reference archive; this file is that prescribing information.
