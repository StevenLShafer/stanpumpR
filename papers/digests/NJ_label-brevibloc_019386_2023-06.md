---
file: NJ_label-brevibloc_019386_2023-06.pdf
pmid: none
doi: none
citation: "Baxter Healthcare Corporation. BREVIBLOC (esmolol hydrochloride) injection, for intravenous use: full prescribing information (PLR format). NDA 019386, supplement S-053 (FDA file 019386s053lbl.pdf), Revised 06/2023, FDA Reference ID 5191258. Supplied by S. Shafer 2026-10-27; source URL not recorded."
kind: label
drugs: [esmolol]
source_read: "text layer, all 20 PDF pages (the label prints no page numbers): Highlights and contents p1; 1 Indications and 2 Dosage and Administration pp2-5 (Table 1 p2, 2.2 p3); 3-4 p5; 5 Warnings pp6-9; 6 Adverse Reactions pp9-11; 7 Drug Interactions pp11-12; 8 Use in Specific Populations pp12-13; 10 Overdosage pp13-14; 11 Description pp14-15; 12.1-12.2 p16; 12.3 Pharmacokinetics p17; 14 Clinical Studies p18; 16-17 pp18-20. Page images of p17 (12.3) and p20 (last page) checked."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 408eee2
verification: matches
---

# FDA 2023: Brevibloc (esmolol) prescribing information, NDA 019386 S-053

This is the full current-format Brevibloc prescribing information that the archive lacked: it replaces, as evidence,
the carton-art file `NJ_label-brevibloc_019386_2025-11-12`. An older, pre-PLR edition is
`NJ_label-brevibloc_019386_s039`.

## Study
Not a study: the US prescribing information for esmolol hydrochloride injection (Baxter; initial US approval 1986;
the only 2023 change listed is a new Warnings 5.6, Hypoglycemia). Presentations (section 3, Table 2, p5): 2500 mg /
250 mL premixed bag (10 mg/mL), 2000 mg / 100 mL Double Strength premixed bag (20 mg/mL), 100 mg / 10 mL vial
(10 mg/mL); all iso-osmotic in sodium chloride (312 mOsmol/L for the bags, p15).

Indications (section 1, p2): 1.1 rapid, short-term control of ventricular rate in atrial fibrillation or flutter
(perioperative, postoperative or other emergent settings) and noncompensatory sinus tachycardia; 1.2 short-term
treatment of **tachycardia and hypertension** during induction and intubation, surgery, emergence and the
postoperative period. Prophylactic use for these events is not recommended.

Clinical trial base (6.1, p9; 14, p18): 369 patients with supraventricular tachycardia and over 600 intraoperative and
postoperative patients. In two randomized double-blind SVT trials, 50 to 300 mcg/kg/min was better than placebo and
about as effective as bolus propranolol 3-6 mg; 60-70% responded (20% fall in heart rate, rate below 100 bpm, or
rarely conversion), about 95% of responders at 200 mcg/kg/min or less; average effective dose about
100 mcg/kg/min. Hypotension: 20-50% of patients overall, symptomatic in about 12%; 53% with esmolol against 17%
with propranolol.

## Model and parameters
No compartmental model. Section 12.3 (p17) and 10.2/11/12.1 (pp14, 16), all as printed:

```yaml
parameters:
  - {name: CL, value: 20, unit: L/kg/h, source: "12.3, p17", note: "total body clearance, about; stated to exceed cardiac output. (derived: 333 mL/kg/min)"}
  - {name: t_half_distribution, value: 2, unit: min, source: "12.3, p17", note: about}
  - {name: t_half_elimination, value: 9, unit: min, source: "12.3 p17; also 10.2 p14, 11 p14, 12.1 p16", note: about}
  - {name: time_to_steady_state_with_load, value: 5, unit: min, source: "12.3, p17", note: "within five minutes, 50-300 mcg/kg/min"}
  - {name: time_to_steady_state_no_load, value: 30, unit: min, source: "12.3 p17; 2.1 p2", note: about}
  - {name: fraction_unchanged_urine, value: "<2", unit: "%", source: "12.3, p17"}
  - {name: acid_metabolite_urine_24h, value: "73-88", unit: "% of dose", source: "12.3, p17"}
  - {name: acid_metabolite_t_half, value: 3.7, unit: h, source: "12.3, p17", note: "renal clearance about equal to GFR"}
  - {name: acid_metabolite_t_half_ESRD, value: 48, unit: h, source: "12.3, p17", note: "about 12-fold longer; peak concentration doubled"}
  - {name: protein_binding, value: 55, unit: "%", source: "12.3, p17", note: "acid metabolite 10%"}
  - {name: molecular_weight_HCl, value: 331.8, unit: g/mol, source: "11, p14", note: "racemate, one asymmetric centre"}
  - {name: load_SVT, value: 500, unit: mcg/kg over 1 min, source: "2.1 Table 1, p2", note: "optional; then 50 mcg/kg/min for 4 min"}
  - {name: maintenance_range, value: "50-200", unit: mcg/kg/min, source: "2.1, pp2-3", note: "as low as 25 adequate; above 200 little added rate effect"}
  - {name: load_immediate_control, value: 1, unit: mg/kg over 30 s, source: "2.2, p3", note: "then 150 mcg/kg/min if necessary"}
  - {name: max_maintenance_tachycardia, value: 200, unit: mcg/kg/min, source: "2.2, p3"}
  - {name: max_maintenance_hypertension, value: "250-300", unit: mcg/kg/min, source: "2.2, p3", note: "may be required; above 300 not studied"}
  - {name: max_infusion_duration, value: 48, unit: h, source: "2.1, p3"}
```
Linearity: steady-state blood levels rise linearly and elimination is dose-independent over 50-300 mcg/kg/min (12.3).
Methanol from hydrolysis stayed near endogenous levels (<2% of toxic) after 6 h at 300 or 24 h at 150 mcg/kg/min.
Step-wise SVT dosing (Table 1, p2): optional 500 mcg/kg load then 50, 100, 150 mcg/kg/min, each for 4 min, then up to
200. Transition (2.3, p4): halve the infusion 30 min after the first dose of the alternative drug; stop it if control
holds for an hour after the second dose. Overdose (10.3, p14): boluses of 625 mg to 2.5 g (12.5-50 mg/kg) have been
fatal; recovery after 1.75 g over 1 min and 7.5 g over 1 h has been seen.

## Pharmacodynamics and end points
No ke0, EC50 or concentration-effect parameters. Blood levels are said to correlate with the extent of beta blockade;
substantial recovery from beta blockade 10-20 min after stopping, and hemodynamics back to pretreatment by 30 min
(12.2, p16). At 200 mcg/kg/min, resting effects similar to 4 mg IV propranolol, with a larger fall in systolic
pressure during exercise; 300 mcg/kg/min is called the maximum therapeutic dose. Cardioselectivity: no significant rise
in airway resistance at 100-300 mcg/kg/min in 10 mildly asthmatic patients. The acid metabolite has negligible
activity. Heart rate rises moderately above pretreatment 30 min after discontinuation (5.12, p8). Not an opioid: no
MEAC or potency ratio.

## Covariates
- Renal (8.7 p13; 12.3 p17): no dose adjustment for a 4-hour maintenance infusion printed as "150 mcg/kg" (no per-min
  unit on either page); no data above that rate or beyond 4 h. Esmolol concentrations similar in ESRD on dialysis;
  the acid metabolite accumulates (above).
- Hepatic (8.6, p13): no special precautions, because red-cell esterases metabolise it.
- Pediatric (8.4): safety and effectiveness not established. Geriatric (8.5): start at the low end.
- Pregnancy (8.1-8.2, p12): fetal bradycardia reported in the last trimester or labor. No sex, weight or age
  covariates on kinetics are given; all doses are per kg of body weight.

## Caveats
- Kinetics are summary values without population, n or assay; "blood levels" (whole blood presumably, not stated).
- The renal statement omits the time unit of the 150 mcg/kg rate on both pages.
- Interactions (7, pp11-12): digoxin levels up 10-20%; succinylcholine and mivacurium block prolonged; avoid with
  verapamil (fatal arrests) and with vasoconstrictive inotropes.

## Used by stanpumpR
- `docs/antihypertensives.md:1291` (context gave :1279; found by content at 408eee2): the exclusion table says esmolol
  is in the antihypertensive registry because its label and perioperative use concern pressure as well as rate, and
  that sotalol and landiolol are excluded because they do not.

## Verification against stanpumpR
- `docs/antihypertensives.md:1291`, esmolol clause: **matches**. Indication 1.2 (p2) covers intraoperative and
  postoperative tachycardia **and hypertension**, and section 2.2 (p3) gives separate maintenance ceilings for
  tachycardia (200) and hypertension (250-300 mcg/kg/min). The sotalol and landiolol clause concerns other products and
  is outside this document.
- Not a repo location, for reference only: the library's esmolol model (`R/drugs_esmolol.R`, de Bruijn 1987,
  anaesthetised patients) has a 9.9 min elimination half-life and 128 mL/kg/min clearance; the label gives about 9 min
  and about 20 L/kg/h (derived 333 mL/kg/min), closer to the awake-volunteer 285 mL/kg/min that
  `inst/help/drugs/esmolol.md:7` quotes from Sum 1983. The model's help page already explains the difference; nothing
  in the repo attributes a kinetic value to this label.
