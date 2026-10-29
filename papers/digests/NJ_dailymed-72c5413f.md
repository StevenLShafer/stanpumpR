---
file: NJ_dailymed-72c5413f.pdf
pmid: null
doi: null
citation: "MAIA Pharmaceuticals, Inc. ANGIOMAX RTU (bivalirudin) injection, 250 mg/50 mL (5 mg/mL), for intravenous use; NDA211215. DailyMed SPL setid 72c5413f-882a-4784-9be6-312c0a0a2a97; revised 11/2024 (marketing end date 05/27/2024 in the SPL); archived 2026-10-21."
kind: label
drugs: [bivalirudin]
source_read: "text layer, all 12 PDF pages: Highlights and contents p1-2; 1 Indications, 2 Dosage and Administration incl. 2.2 renal adjustment p2-3; 3-7 p3-5; 8 Use in Specific Populations incl. 8.6 Renal Impairment p5-6; 10-11 p6-7; 12.1-12.2 p7-8; 12.3 Pharmacokinetics p8; 13-14 p8-10; 16-17, carton and SPL data p10-12. PDF p8 (12.2-12.3) and p9 (Table 2) checked on rendered images."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# ANGIOMAX RTU (bivalirudin) US label, MAIA, revised 11/2024

## Study
Not a study: a PLR-format US label for a ready-to-use bivalirudin solution (bivalirudin trifluoroacetate, average
275 mg salt per 250 mg bivalirudin; free base C98H138N24O33, MW 2180.32; Section 11, PDF p6-7). Indication: anticoagulant
in PCI, including HIT/HITTS (Section 1). The SPL packaging and marketing rows give a marketing start of 08/08/2019 and a
marketing end of 05/27/2024 (PDF p12) although the label is marked "Revised: 11/2024": this SPL describes a product no
longer marketed under this NDA. The PK content is the bivalirudin class text shared with the reference Angiomax label,
not new data for the RTU formulation.

Data sources named in the label: 291 PTCA patients (12.2), PTCA patients with normal to severe renal impairment and on
dialysis (8.6, 12.3), BAT trials 4312 subjects (median age 63, 29-90; median weight 80 kg, 39-120; Section 14, PDF p9),
AT-BAT 51 HIT/HITTS subjects. No n, assay or model is given for the PK values.

## Model and parameters
No model; summary values only (Section 12.3, PDF p8):

```yaml
parameters:
  - {name: CL, value: 3.4, unit: mL/min/kg, source: "12.3 Elimination, PDF p8", note: "PTCA patients, normal renal function; = 0.204 L/h/kg (derived)"}
  - {name: t_half, value: 25, unit: min, source: "12.3 Elimination, PDF p8", note: "PTCA patients, normal renal function"}
  - {name: Css, value: 12.3, unit: mcg/mL, source: "12.3, PDF p8", note: "mean +/- 1.7, after 1 mg/kg bolus + 2.5 mg/kg/h for 4 h, PTCA patients"}
  - {name: t_half_moderate_renal, value: 34, unit: min, source: "12.3 Specific Populations, PDF p8", note: "CL reduced 21% (moderate and severe)"}
  - {name: t_half_severe_renal, value: 57, unit: min, source: "12.3 Specific Populations, PDF p8", note: "CL reduced 21%"}
  - {name: t_half_dialysis, value: 3.5, unit: h, source: "12.3 Specific Populations, PDF p8", note: "CL reduced 70%; about 25% cleared by hemodialysis"}
```
(derived: V = CL x t1/2 / ln2 = 3.4 x 25 / 0.693 = 123 mL/kg, about 0.12 L/kg, for a one-exponential reading of the
25-min half-life.) Plasma protein binding: none except thrombin; not bound to red cells. Metabolism by proteolytic
cleavage; glomerular filtration with tubular secretion and reabsorption of unknown extent (PDF p8). Linear PK after IV
dosing in PTCA patients. No fraction excreted unchanged is given.

## Pharmacodynamics and end points
- Dose- and concentration-dependent prolongation of ACT, aPTT, PT and TT; immediate effect after IV dosing; coagulation
  times return to baseline about 1 h after stopping (12.2, PDF p7-8; also 5.1). Also increases INR.
- 291 PTCA patients: positive dose-ACT correlation for reaching 300 s or 350 s; at 1 mg/kg + 2.5 mg/kg/h for 4 h then
  0.2 mg/kg/h, all patients reached a maximal ACT above 300 s (12.2, PDF p8).
- No concentration-effect equation, EC50 or effect-site constant. Not an opioid: no MEAC or potency ratio.

Dosing (2.1-2.2, PDF p2-3): 0.75 mg/kg IV bolus then 1.75 mg/kg/h for the procedure; ACT 5 min after the bolus to decide
on an extra 0.3 mg/kg bolus; up to 4 h further infusion after PCI in STEMI. No bolus reduction in renal impairment;
infusion 1 mg/kg/h if CrCl < 30 mL/min (Cockcroft-Gault); 0.25 mg/kg/h on hemodialysis.

BAT trial in-hospital end points (Table 2, PDF p9), bivalirudin (n = 2161) vs heparin (n = 2151): primary composite 7.9%
vs 9.3%; death, MI, revascularization 6.2% vs 7.9%; death 0.2% vs 0.2%; MI 3.3% vs 4.2%. Superiority not shown.
Bleeding (6.1): 79/2161 (3.7%) on bivalirudin.

## Covariates
Renal function only: CL similar in mild impairment, -21% in moderate and severe, -70% on dialysis (8.6, 12.3).
Geriatric: more bleeding in the elderly (8.5); no PK by age. Pediatric safety and effectiveness not established (8.4).
No weight-based PK beyond the per-kg CL.

## Caveats
- Single summary numbers with no n, variability, assay or model; the 25-min half-life and the 3.4 mL/min/kg CL come from
  PTCA patients, not healthy volunteers.
- The two renal half-lives (34 and 57 min) are given with one shared clearance reduction (21%), which the label does not
  explain.
- Product listed as no longer marketed (marketing end 05/27/2024) in an SPL revised 11/2024.

## Used by stanpumpR
- `inst/extdata/anticoagulantRegistry.csv:17` (from `data-raw/anticoagulantRegistry.R:120-124`): row `biva_label_pci`,
  IV, PCI, bivalirudin plasma, ACT/ECT, `pending`, no drug; note "Label CL (0.204 L/h/kg) differs from the
  healthy-volunteer fit; no full V/Q/PD vector for PCI patients."
- Related, not listed in the context: `R/drugs_bivalirudin.R:23-36` and `inst/help/drugs/bivalirudin.md:19` cite "the
  label" for the 25-min half-life, the renal and dialysis reductions and 3.4 mL/min/kg (0.204 L/h/kg) in PCI patients.

## Verification against stanpumpR
- **CL 0.204 L/h/kg** (registry row 17; `R/drugs_bivalirudin.R:33`; help line 19): **matches** 3.4 mL/min/kg x 60 / 1000
  (12.3, PDF p8), and it is stated for PTCA patients, as the repo says.
- **"Differs from the healthy-volunteer fit"**: the comparison value 0.323 L/h/kg (Zhang 2012,
  `anticoagulantParameters.csv:13`) is not in this label; the label value itself is correct.
- **"No full V/Q/PD vector for PCI patients"**: **matches**; the label gives only CL, t1/2 and one Css, and no PD equation.
- **25-min terminal half-life in normal renal function** (`R/drugs_bivalirudin.R:24-26`): **matches**.
- **Lower clearance in moderate and severe renal impairment and on dialysis** (`R/drugs_bivalirudin.R:31-33`,
  help line 19): **matches** (-21%, -70%).
- `R/drugs_bivalirudin.R:30-31` says bivalirudin is "about 20% renally cleared" in the same sentence as the label
  citation: **cannot verify** from this label, which gives no fraction (excretion "extent is unknown", PDF p8). The
  figure may come from another source; the sentence reads as if it were the label's.
- The ACT/ECT assay column: the label names ACT, aPTT, PT and TT; it does not mention ECT. Minor; the row describes the
  monitoring context rather than quoting the label.
</content>
</invoke>
