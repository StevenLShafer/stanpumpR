---
file: NJ_label-cardeneiv_019734_2026-03-30.pdf
pmid: null
doi: null
citation: "CARDENE I.V. (nicardipine hydrochloride in dextrose or sodium chloride injection), Baxter Healthcare. Prescribing information, NDA 019734, revised 3/2026 (FDA Reference ID 5771722; accessdata 019734s036lbl.pdf)."
kind: label
drugs: [nicardipine]
source_read: "text layer, all 13 pages: Highlights and contents (PDF p1), 1-2 Indications and Dosage and Administration (p2-p3), 5 Warnings and 6 Adverse Reactions (p4-p5), 8 Use in Specific Populations (p6-p8), 10-11 (p8-p9), 12.1-12.3 Clinical Pharmacology (p9-p12), 14 Clinical Studies (p12-p13), 16 (p13); p5 (adverse-event table) and p12 (12.3 PK values) confirmed on rendered page images"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Cardene I.V. label (NDA 019734, revised 3/2026)

## Study
Prescribing information for premixed nicardipine hydrochloride 0.1 mg/mL (dextrose 4.8% or NaCl 0.86%) and 0.2
mg/mL (NaCl 0.83%), 200 mL GALAXY bags, Baxter. Indication: short-term treatment of hypertension in adults when
oral therapy is not feasible (1.1). Doses are of the hydrochloride (MW 515.99; section 11).

Dosing (2.1, PDF p2): start 5 mg/h; increase by 2.5 mg/h every 5 min (rapid) to 15 min (gradual) to a maximum 15
mg/h; after rapid titration to goal, decrease to 3 mg/h. Oral-to-IV equivalents: 20 mg q8h = 0.5 mg/h, 30 mg q8h =
1.2 mg/h, 40 mg q8h = 2.2 mg/h. Offset: 50% offset of action about 30 min after discontinuation (2.1). With constant
infusion BP begins to fall within minutes and reaches about 50% of its ultimate decrease in about 45 min (2.2).

## Model and parameters
Section 12.3 (PDF p11-p12, rendered p12): concentrations rise rapidly over the first 2 h, approach steady state at
24-48 h; at least 50% fall in the first 2 h after stopping; decline tri-exponential.

```yaml
parameters:
  - {name: t_half_alpha, value: 2.7, unit: min, source: "12.3", note: "rapid early distribution phase"}
  - {name: t_half_beta, value: 44.8, unit: min, source: "12.3", note: "intermediate phase"}
  - {name: t_half_gamma, value: 14.4, unit: h, source: "12.3", note: "terminal, detected only after long infusions"}
  - {name: CL, value: 0.4, unit: L/h/kg, source: "12.3", note: "total plasma clearance"}
  - {name: Vd, value: 8.3, unit: L/kg, source: "12.3", note: "non-compartmental apparent volume"}
  - {name: protein_binding, value: ">95", unit: "%", source: "12.3"}
```

Linear over 0.5-40 mg/h. Metabolised by CYP2C8, 2D6, 3A4; inhibits CYP3A4, 2D6, 2C8, 2C19; radiolabel 49% urine, 43%
faeces in 96 h, none unchanged (12.3, p12). Steady-state PK similar in elderly (> 65 y) and young adults (8.5,
12.3). No V1 or intercompartmental clearances, and no study population or n, are given for the PK values.

## Pharmacodynamics and end points
Normotensive volunteers, 0.25-3 mg/h for 8 h: SBP change < 5, DBP < 3 mmHg (12.2). HR increase 7 +/- 1 bpm
(postoperative) and 8 +/- 1 bpm (severe hypertension) (12.2). Section 14 (p12-p13): 48-h infusion at 4 mg/h in mild
to moderate hypertension, SBP -26 mmHg (17%), DBP -20.7 mmHg (20%); mean time to therapeutic response 77 +/- 5.2 min
(severe hypertension, average maintenance 8 mg/h) and 11.5 +/- 0.8 min (postoperative, 3 mg/h). No ke0, no
therapeutic concentration range. Adverse events (6.1, p5): headache 21/144 (15%) vs 2/100 placebo, hypotension 8
(6%) vs 1, tachycardia 5 (4%) vs 0, nausea/vomiting 7 (5%) vs 1; about 12% discontinued.

## Covariates
Hepatic disease: higher concentrations and longer half-life (12.2, oral; similar at 0.6 mg/h IV for 24 h).
Moderate renal impairment: lower systemic clearance, higher AUC (5.4, 12.2); oral AUC and Cmax about two-fold. Not
quantified as parameters. Pregnancy (8.1): crosses the placenta; maternal and neonatal adverse events listed.
Lactation (8.2): two studies, 18 infants, infant dose < 0.3 mcg/day; the second (7 women, 34 samples, 82%
undetectable, 5.1-18.5 mcg/L) is Bartels 2007.

## Caveats
Label summary values with no study design, n or variability; the 8.3 L/kg Vd is non-compartmental and inflated by
the 14.4-h phase; the terminal phase is much longer than the 1.45 h of the biexponential Modi 1993 model, which
matters for long infusions. Clearance 0.4 L/h/kg is less than half Modi's 0.920.

## Used by stanpumpR
- docs/antihypertensives.md:292 (identifiability table, row "Nicardipine infusion (label)"): three half-lives, CL
  and a non-compartmental Vd; judged underdetermined for a three-compartment model; not implemented.

## Verification against stanpumpR
- **docs/antihypertensives.md:292**: "3C, 6 parameters | 3 half-lives, CL, non-compartmental Vd = 5, and the Vd is
  not a compartment | Underdetermined". Label 12.3 gives exactly three half-lives (2.7 min, 44.8 min, 14.4 h), CL 0.4
  L/h/kg and a non-compartmental Vd 8.3 L/kg; no V1. The count (5 quantities, Vd not a compartment volume) and the
  verdict are supported. **matches**.
