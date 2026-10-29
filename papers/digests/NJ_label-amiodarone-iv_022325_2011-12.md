---
file: NJ_label-amiodarone-iv_022325_2011-12.pdf
pmid: none
doi: none
citation: "Baxter Healthcare Corporation. NEXTERONE (amiodarone HCl) injection, for intravenous use, 50 mg/mL vials and prefilled syringe: full prescribing information. NDA 022325, supplement S-002 (FDA file 022325s002lbl.pdf), Revised December 2011, FDA Reference ID 3056994. Supplied by S. Shafer 2026-10-27; source URL not recorded."
kind: label
drugs: [amiodaroneIV]
source_read: "text layer, all 16 PDF pages (printed page numbers 1-16 equal the PDF pages): Highlights and contents p1; 1 Indications and 2 Dosage and Administration pp2-4 (Table 1 p2, Tables 2-3 pp3-4); 3-5 pp4-7; 6 Adverse Reactions pp7-9; 7 Drug Interactions pp9-11; 8 Use in Specific Populations pp11-12; 10-11 p12-13; 12.1 pp13-14; 12.2-12.3 p14; Special Populations and Exposure-Response p15; 13-14 pp15-16; 16-17 p16. Page images of p2 (Table 1) and p15 checked (the text layer scrambles the p15 headings)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 408eee2
verification: matches
---

# FDA 2011: Nexterone (intravenous amiodarone) prescribing information, NDA 022325 S-002

The Nexterone prescribing information the archive lacked: it replaces, as evidence, the 2025 carton-label file
`NJ_label-amiodarone-iv_022325_2025-02-25`. Note the product: this 2011 edition is for the **50 mg/mL concentrate**
(150 mg/3 mL, 450 mg/9 mL and 900 mg/18 mL vials; 150 mg/3 mL prefilled syringe; section 16, p16) diluted before use,
not the premixed 1.5 and 1.8 mg/mL bags of the 2025 cartons. Each mL holds 50 mg amiodarone HCl and 225 mg
sulfobutylether beta-cyclodextrin; no polysorbate 80 or benzyl alcohol (11, p13).

## Study
Not a study. Indication (1, p2): initiation of treatment and prophylaxis of frequently recurring VF and
hemodynamically unstable VT refractory to other therapy, and VT/VF patients who need amiodarone but cannot take it by
mouth; most patients need 48 to 96 hours.

Clinical studies (14, pp15-16): two randomized, parallel dose-response studies of about 300 patients each, about 125 vs
1000 mg over the first 24 hours (one study also about 500 mg), each as a rapid load, a 6-hour slow load and an 18-hour
maintenance infusion continued to hour 48, with 10-minute 150 mg supplements for breakthrough; median VT/VF rate 0.02
vs 0.07 episodes per hour (p = 0.07 in both); mortality unaffected. Safety base: 1836 patients (hypotension 16%,
bradycardia 4.9%; 5.1-5.2, p4).

## Model and parameters
No compartmental model. Dosing, Table 1 (p2, page image):

| Phase | Dose | Rate | Preparation |
|---|---|---|---|
| First rapid load | 150 mg over the first 10 min | 15 mg/min | 3 mL (150 mg) in 100 mL D5W or saline, 1.5 mg/mL |
| Slow load | 360 mg over the next 6 h | 1 mg/min | 18 mL (900 mg) in 500 mL, 1.8 mg/mL |
| Maintenance | 540 mg over the remaining 18 h | 0.5 mg/min | rate of the slow infusion reduced |

```yaml
parameters:
  - {name: first_day_dose, value: 1000, unit: mg, source: "2, p2", note: "about 1000 mg over the first 24 h (derived: Table 1 sums to 1050 mg)"}
  - {name: rapid_load, value: 150, unit: mg over 10 min, source: "Table 1, p2", note: "15 mg/min"}
  - {name: slow_load_rate, value: 1, unit: mg/min, source: "Table 1, p2", note: "360 mg over 6 h"}
  - {name: maintenance_rate, value: 0.5, unit: mg/min, source: "Table 1 and text, p2", note: "540 mg over 18 h; 720 mg per 24 h thereafter; may be increased"}
  - {name: supplemental_dose, value: 150, unit: mg over 10 min, source: "2, p2", note: "in 100 mL, for breakthrough VF or unstable VT"}
  - {name: max_initial_rate, value: 30, unit: mg/min, source: "2, p2"}
  - {name: peak_after_150mg_10min_patients, value: "7-26", unit: mg/L, source: "12.3, p14", note: "range of peaks, patients with VF or unstable VT"}
  - {name: peak_after_5mgkg_15min_healthy, value: "5-41", unit: mg/L, source: "12.3, p14", note: "range of peaks, healthy subjects"}
  - {name: fall_to_10pct_of_peak, value: "30-45", unit: min after end of infusion, source: "12.3, p14"}
  - {name: mean_serum_at_48h, value: "0.7-1.4", unit: mg/L, source: "12.3, p14", note: "after 48 h of 125, 500 or 1000 mg/day plus 150 mg supplements, n = 260"}
  - {name: CL_healthy, value: "63-231", unit: mL/h/kg, source: "12.3, p14", note: "single 5 mg/kg IV; DEA 140-400"}
  - {name: CL_patients, value: "220-440", unit: mL/h/kg, source: "12.3, p14", note: "VT/VF, 2-7 day studies"}
  - {name: t_half_terminal, value: "9-36", unit: days, source: "12.3, p14", note: "sum of four exponentials; DEA 9-30 days"}
  - {name: protein_binding, value: ">96", unit: "%", source: "12.3, p14", note: in vitro}
  - {name: CL_age_over_65, value: 100, unit: mL/h/kg, source: "Special Populations, p15", note: "about; about 150 in younger; t1/2 about 20 to 47 days"}
  - {name: oral_F_assumed, value: 50, unit: "%", source: "2, Table 3, pp3-4", note: "basis for oral doses after IV: <1 week 800-1600, 1-3 weeks 600-800, >3 weeks 400 mg/day"}
```
Other dosing rules (p2-3): mean daily doses above 2100 mg raised the risk of hypotension; up to 0.5 mg/min may continue
for 2 to 3 weeks regardless of age, renal or LV function; above 2 mg/mL for longer than 1 h needs a central line;
in-line filter. Concentrations are of amiodarone in serum; DEA above 0.05 mg/L is not usually seen for several days.

## Pharmacodynamics and end points
No ke0 or concentration-effect parameters. Exposure-response (p15): no established relation between concentration and
therapeutic response for short-term IV use. Acute IV effects are mainly AV-nodal (AH and AVN refractoriness prolonged;
QTc, QRS and HV unchanged; Table 5, p13), with activity before DEA accumulates (12.2, p14). Not an opioid.

## Covariates
Age (lower clearance and longer half-life above 65), cirrhosis (lower DEA Cmax, amiodarone unchanged), severe LV
dysfunction (DEA half-life longer); renal disease and sex without effect (p15). No dose adjustment is defined. Pediatric
use not recommended: in 61 patients aged 30 days to 15 years, hypotension 36%, bradycardia 20%, AV block 15% (8.4,
p12). Pregnancy category D (8.1, p11); breast-feeding to be stopped (8.3, p12). Doses are fixed, not per kg.

## Caveats
- An older edition (2011) for the vial and syringe concentrate; later editions add the premixed bags and were not
  compared. The regimen in Table 1 is what the repo uses, whatever the presentation.
- Kinetic ranges are pooled across small studies without n (except the 48-hour value).

## Used by stanpumpR
Locations found by content at 408eee2 (the context's lines are from an older commit):
- `R/drugs_amiodaroneIV.R:100-103`: the label's first-day regimen (150 mg over 10 min, 1 mg/min for 6 h, then
  0.5 mg/min) gives 0.87 to 1.38 mg/L from hour 1 to 24 in this model.
- `R/help-scenarios.R:240-254` (context :183, :188): scenario `amiodarone-iv-loading`, rows 15 mg/min at 0, 1 mg/min at
  10 and 0.5 mg/min at 370 min.
- `inst/help/drugs/amiodaroneIV.md:30`: the regimen; model peak 5.6 mg/L at the end of the rapid load.
- `inst/help/in-development.md:22`: the label's 24-hour loading regimen offered as a scenario.
- `inst/help/scenarios/amiodarone-iv-loading.md:3` (150 / 360 / 540 mg, 15, 1 and 0.5 mg/min, 1050 mg), `:7` (535 of
  the label's 540 mg by 24 h), `:13` (the supplemental 150 mg over 10 min), `:14` (the label's doses are fixed).
- `tests/testthat/test-drugs-amiodaroneIV.R:78-82` (LABEL_TIMES c(0, 10, 370), LABEL_RATES c(15, 1, 0.5)) and the
  tests that use them (:178-194, :237-251, :269-273, :366-371).

## Verification against stanpumpR
- Regimen (all locations above): **matches** Table 1 (p2): 150 mg over the first 10 min (15 mg/min), 360 mg over the
  next 6 h (1 mg/min), 540 mg over the remaining 18 h (0.5 mg/min). Switch times 10 and 370 min and the 1050 mg total
  are correct (derived: 150 + 360 + 540). The label describes the total as about 1000 mg.
- Supplemental 150 mg over 10 min for breakthrough VF/VT (`amiodarone-iv-loading.md:13`, tests :237-251): **matches**
  (p2).
- Fixed, not weight-based, doses (`amiodarone-iv-loading.md:14`): **matches**.
- Doses as amiodarone HCl, no salt conversion (`R/drugs_amiodaroneIV.R:16`, `amiodaroneIV.md:48`): consistent with the
  label, which doses in mg of amiodarone HCl.
- For reference, not a repo claim about this label: the model's 5.6 mg/L at the end of the 150 mg rapid load
  (`amiodaroneIV.md:30`, scenario page :5) is below the label's range of peaks after 10-minute 150 mg infusions in
  VF/VT patients, 7 to 26 mg/L (12.3, p14). The model's 1.39 mg/L at 48 h (scenario page :15) lies within the label's
  mean 0.7 to 1.4 mg/L at 48 h, which came from different regimens (125 to 1000 mg/day plus supplements).
