---
file: NJ_label-brevibloc_019386_s039.pdf
pmid: none
doi: none
citation: "Baxter Healthcare Corporation. BREVIBLOC Premixed Injection, BREVIBLOC Double Strength Premixed Injection, BREVIBLOC Injection and BREVIBLOC Double Strength Injection (esmolol hydrochloride): prescribing information (pre-PLR format). NDA 019386, supplement S-039 (FDA file 019386s039lbl.pdf); no revision date printed (document code MLT-01088/4.0). Supplied by S. Shafer 2026-10-27; source URL not recorded."
kind: label
drugs: [esmolol]
source_read: "text layer, all 21 PDF pages (printed page numbers 1-21 equal the PDF pages): product titles pp1-2; Description pp3-4; Clinical Pharmacology pp4-7 (Pharmacokinetics and Metabolism pp4-5, Pharmacodynamics pp6-7, SVT trials p7); Indications pp7-8; Contraindications, Warnings pp8-9; Precautions pp9-11 (renal pp9-10, interactions pp10-11, pregnancy, nursing and pediatric use p11; no geriatric or hepatic subsection); Adverse Reactions pp11-13; Overdosage pp13-14; Dosage and Administration pp14-20 (titration table p15); How Supplied pp20-21. Page image of p15 checked."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 408eee2
verification: matches
---

# FDA (undated, S-039): Brevibloc (esmolol) prescribing information, older edition

An older, pre-PLR edition of the Brevibloc label. The current-format 2023 edition is
`NJ_label-brevibloc_019386_2023-06`; this one is useful for the earlier wording and for a product the 2023 edition no longer
lists (the 100 mg / 5 mL Double Strength vial). The file carries no approval or revision date; S-039 comes from the FDA file
name only.

## Study
Not a study. Products (pp1-2, 20-21): 2500 mg / 250 mL premixed bag (10 mg/mL, NDC 10019-055-61); 2000 mg / 100 mL
Double Strength premixed bag (20 mg/mL, NDC 10019-075-87); 100 mg / 10 mL vial (10 mg/mL, NDC 10019-115-01);
100 mg / 5 mL Double Strength vial (20 mg/mL, NDC 10019-085-01). A 0.5 mg/kg load for a 70 kg patient is 3.5 mL at
10 mg/mL or 1.75 mL at 20 mg/mL (pp18-20).

Indications (pp7-8): rapid control of ventricular rate in atrial fibrillation or flutter in perioperative,
postoperative or other emergent settings, and noncompensatory sinus tachycardia; and treatment of **tachycardia and
hypertension** at induction and intubation, during surgery, on emergence and postoperatively (prevention not
recommended). Contraindications (p8) are narrower than in 2023: sinus bradycardia, heart block above first degree,
cardiogenic shock, overt heart failure.

Trials (p7): two randomized double-blind SVT comparisons with placebo and propranolol (3-6 mg boluses); 50-300
mcg/kg/min effective; 60-70% responded, about 95% of responders at 200 mcg/kg/min or less; average effective dose
**100-115 mcg/kg/min** here (p7), against about 100 in Dosage and Administration (p15) and in the 2023 label.
Hypotension in 20-50%, symptomatic in about 12%, about three times as frequent as with propranolol (53% vs 17%).
Adverse-reaction base (pp11-12): 369 SVT patients and over 600 intra- and postoperative patients.

## Model and parameters
No compartmental model. Kinetic statements (pp4-5) are the same as in the 2023 edition except where noted:

```yaml
parameters:
  - {name: CL, value: 20, unit: L/kg/h, source: "Pharmacokinetics and Metabolism, p5", note: "about; greater than cardiac output"}
  - {name: t_half_distribution, value: 2, unit: min, source: "p5", note: about}
  - {name: t_half_elimination, value: 9, unit: min, source: "p5; also Description p3, Clinical Pharmacology p4, Overdosage p13", note: about}
  - {name: time_to_steady_state_with_load, value: 5, unit: min, source: "p5", note: "50-300 mcg/kg/min"}
  - {name: time_to_steady_state_no_load, value: 30, unit: min, source: "p5; D&A p14", note: about}
  - {name: acid_metabolite_t_half, value: 3.7, unit: h, source: "p5"}
  - {name: acid_metabolite_relative_activity, value: "1/1500", unit: "of esmolol", source: "p5", note: "in animals; the 2023 label says negligible"}
  - {name: acid_metabolite_t_half_renal_disease, value: "about 10-fold normal", unit: "", source: "p5; Precautions pp9-10", note: "2023 label says about 12-fold, to 48 h, in ESRD"}
  - {name: protein_binding, value: 55, unit: "%", source: "p5", note: "acid metabolite 10%"}
  - {name: load_SVT, value: 500, unit: mcg/kg over 1 min, source: "D&A p14; table p15", note: "then 50 mcg/kg/min for 4 min"}
  - {name: max_loading_doses, value: 3, unit: "", source: "D&A p14; table p15", note: "at 0, 5 and 10 min; maintenance 50, 100, 150 then 200 mcg/kg/min"}
  - {name: load_immediate_control, value: 80, unit: mg over 30 s, source: "D&A pp16-17", note: "about 1 mg/kg; then 150 mcg/kg/min, adjustable up to 300"}
  - {name: maintenance_hypertension, value: "250-300", unit: mcg/kg/min, source: "D&A p17", note: "needed by one third of postoperative hypertensive patients"}
  - {name: max_infusion_duration, value: 24, unit: h, source: "D&A pp14, 16", note: "well documented; limited data (N = 48) to 48 h"}
```
Titration table (p15, page image): elapsed 0-1 min load 500 mcg/kg/min (0.5 mg/kg/min); 1-5 maintenance 50; 5-6 load
500; 6-10 maintenance 100; 10-11 load 500; 11-15 maintenance 150; 15-16 no load (bullets); 16-20 maintenance 200
(asterisked: load may be omitted as target approaches, maintenance titrated up to 300 or down); beyond 20 min titrated
to heart rate. Responses usually (over 95%) within 50-200 mcg/kg/min; doses above 200 not recommended; above 300 not
studied (p15). Overdose (p13): fatal boluses 625 mg to 2.5 g (12.5-50 mg/kg).

## Pharmacodynamics and end points
No ke0 or concentration-effect parameters. Blood levels correlate with beta blockade; substantial recovery 10-20 min
after stopping; hemodynamics back to pretreatment 30 min after stopping (p6). 200 mcg/kg/min resembles 4 mg IV
propranolol at rest. Not an opioid: no MEAC or potency ratio.

## Covariates
Renal disease prolongs the acid metabolite's half-life about ten-fold with plasma levels considerably elevated; give
with caution in renal impairment (pp5, 9-10). Pediatric safety not established (p11). No hepatic, geriatric or sex
statements. Doses per kg of body weight, except the 80 mg immediate-control bolus.

## Caveats
- Undated; superseded. Interactions given here and not in 2023 (p10): morphine raised esmolol steady-state blood
  levels by 46%; esmolol was equivocally higher with warfarin; succinylcholine block prolonged from 5 to 8 min.
- The p14 text writes the third load as "0.5 mg/kg/min infused over a 1 minute period", i.e. 0.5 mg/kg.

## Used by stanpumpR
- `docs/antihypertensives.md:1291` (context gave :1279): esmolol is kept in the antihypertensive registry because its
  label and perioperative use concern pressure as well as rate (sotalol and landiolol excluded).

## Verification against stanpumpR
- `docs/antihypertensives.md:1291`, esmolol clause: **matches**. This edition also lists intraoperative and
  postoperative hypertension as an indication (p8) and gives higher maintenance rates (250-300 mcg/kg/min) for blood
  pressure than for rate (p17). The current evidence is the 2023 edition, `NJ_label-brevibloc_019386_2023-06`, which
  says the same. The sotalol and landiolol clause is outside this document.
