---
file: NJ_label-naproxen_017581_2024-11-22.pdf
pmid: none
doi: none
citation: "NAPROSYN (naproxen) tablets, EC-NAPROSYN (naproxen delayed-release tablets), ANAPROX DS (naproxen sodium tablets) prescribing information, NDA 017581 (with 018164, 020067), revised 11/2024; Reference ID 5482791; manufactured for Atnahs Pharma. https://www.accessdata.fda.gov/drugsatfda_docs/label/2024/017581s117,018164s067,020067s026lbl.pdf"
kind: label
drugs: [naproxen]
source_read: "text layer, all 24 pages; read Highlights, 2 Dosage and Administration, 8.4-8.7 Use in Specific Populations, 10 Overdosage, 12.3 Pharmacokinetics in full (incl. Specific Populations), 7 Drug Interactions table skimmed. The EC-NAPROSYN vs NAPROSYN Cmax/Tmax/AUC table is scrambled in the text layer (assignment below by column order)."
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Naprosyn / EC-Naprosyn / Anaprox DS label (NDA 017581, 2024)

## Study
Prescribing information. PK statements are summary values; study populations mostly unspecified (one crossover of EC-NAPROSYN vs NAPROSYN 500 mg twice daily for 1 week in 24 fasted subjects).

## Model and parameters
No compartmental model.
```yaml
parameters:
  - {name: F_oral, value: 0.95, unit: fraction, source: "12.3", note: "in vivo bioavailability, naproxen and naproxen sodium"}
  - {name: t_half, value: "12-17", unit: h, source: "12.3 (intro and Excretion)", note: "elimination half-life, unchanged across products"}
  - {name: time_to_steady_state, value: "4-5", unit: days, source: "12.3"}
  - {name: Vd, value: 0.16, unit: L/kg, source: "12.3 Distribution"}
  - {name: CL, value: 0.13, unit: mL/min/kg, source: "12.3 Excretion", note: "total naproxen"}
  - {name: protein_binding, value: ">99", unit: "% albumin-bound", source: "12.3 Distribution"}
  - {name: Tmax_Naprosyn, value: "2-4", unit: h, source: "12.3 Absorption"}
  - {name: Tmax_AnaproxDS, value: "1-2", unit: h, source: "12.3 Absorption"}
  - {name: Tmax_EC_fasted, value: "4-6 (range 2-12)", unit: h, source: "12.3 Absorption"}
  - {name: Tmax_EC_fed, value: "about 12 (range 4-24)", unit: h, source: "12.3 Food Effects"}
  - {name: Cmax_EC_500bid, value: 94.9, unit: ug/mL, source: "12.3 table", note: "mean, CV 18%; steady state"}
  - {name: Cmax_Naprosyn_500bid, value: 97.4, unit: ug/mL, source: "12.3 table", note: "CV 13%"}
  - {name: Tmax_EC_500bid, value: 4, unit: h, source: "12.3 table", note: "CV 39%"}
  - {name: Tmax_Naprosyn_500bid, value: 1.9, unit: h, source: "12.3 table", note: "CV 61%"}
  - {name: AUC0-12_EC_500bid, value: 845, unit: ug.h/mL, source: "12.3 table", note: "CV 20%; column assignment inferred from scrambled text"}
  - {name: AUC0-12_Naprosyn_500bid, value: 767, unit: ug.h/mL, source: "12.3 table", note: "CV 15%; column assignment inferred"}
  - {name: trough_Css, value: "36.5 / 49.2 / 56.4", unit: mg/L, source: "12.3 Distribution", note: "at 500 / 1000 / 1500 mg/day; less than proportional (saturable binding)"}
```
(derived, 70 kg: CL = 0.13 x 70 = 9.1 mL/min = 0.546 L/h; Vd = 11.2 L; t1/2 = 0.693 x 11.2 / 0.546 = 14.2 h, inside the stated 12-17 h.)

Elimination: hepatic to 6-O-desmethyl naproxen, then acyl glucuronides; about 95% of a dose in urine, mostly as conjugates (66-92%), under 1% unchanged; 3% or less in feces. Metabolite half-lives under 12 h.

Dosing (section 2): RA/OA/AS 250-500 mg Naprosyn twice daily, 275-550 mg Anaprox DS (550 mg naproxen sodium = 500 mg naproxen + 50 mg sodium) twice daily, EC 375-500 mg twice daily; up to 1500 mg/day for up to 6 months. Pain: Anaprox DS 550 mg then 550 mg q12h or 275 mg q6-8h, first-day maximum 1375 mg sodium salt, then 1100 mg/day; Naprosyn 500 mg then 250 mg q6-8h, maximum 1250 mg/day. Gout 750 mg then 250 mg q8h. Pediatric JIA about 10 mg/kg/day in 2 doses; 5 mg/kg in children gives levels like 500 mg in adults.

## Pharmacodynamics and end points
No concentration-effect relationship, EC50, or therapeutic/toxic concentration range. Not an opioid: no MEAC, ke0 or potency ratio.

## Covariates
Qualitative only. Elderly: total concentration unchanged, unbound fraction higher (unbound trough 0.12-0.19% vs 0.05-0.075% of total in young). Hypoalbuminaemia/alcoholic liver disease: total lower, unbound higher. Renal impairment: not studied; not recommended at CrCl < 30 mL/min. Pediatric 5-16 y: terminal half-life similar to adults.

## Caveats
- Half-life and clearance are summary values without population, n or variability.
- The steady-state comparison table is scrambled in the text layer; AUC column assignment inferred.
- Saturable albumin binding above 500 mg/day makes total-drug PK non-linear.

## Used by stanpumpR
- `R/drugs_naproxen.R:56` - model terminal half-life 23 h; "the label gives 12-17 h".
- `R/drugs_naproxen.R:58-59` - model steady state 90% in 3 days of twice-daily dosing; "the label: 4 to 5 days".
- `inst/help/drugs/naproxen.md:21` - model terminal half-life 23 h is longer than the label's 12 to 17 h, close to Vree's 24.7 h.
- Also read: `R/drugs_naproxen.R:7` (550 mg naproxen sodium = 500 mg naproxen), `:61-72` (saturable binding above about 500 mg/day, cited to Davies 1997 and Runkel 1974).

## Verification against stanpumpR
- `R/drugs_naproxen.R:56` and `inst/help/drugs/naproxen.md:21`: label half-life 12 to 17 h (hours, both places). **matches**. The code correctly reports that its 23 h is longer than the label's.
- `R/drugs_naproxen.R:58-59`: label steady state in 4 to 5 days. **matches**.
- `R/drugs_naproxen.R:7`: label Table 1, 550 mg naproxen sodium = naproxen 500 mg with 50 mg sodium. **matches** (the 220 mg OTC tablet is not in this label).
- `:61-72`: the label also documents saturable binding above 500 mg/day (trough Css 36.5, 49.2, 56.4 mg/L at 500, 1000, 1500 mg/day), consistent with the code's description; the code cites other sources for it. **matches** (supporting).
- Not cited by the repo but relevant: label CL 0.13 mL/min/kg is (derived) 0.546 L/h at 70 kg, about 20% above the model's reference-man CL/F of 0.453 L/h; label Vd 0.16 L/kg is 11.2 L at 70 kg against the model's V1 + V2 = 12.5 L. Both within the scatter the code already discusses (Upton 1984 CL/F 0.416 L/h). No discrepancy with what the code claims.
