---
file: NJ_dailymed-820eadcb.pdf
pmid: none
doi: none
citation: "Sodium Nitroprusside Injection 50 mg/2 mL (25 mg/mL) fliptop vial, Meitheal Pharmaceuticals Inc.; ANDA211016; prescribing information dated January 2020 (code TN/Drugs/TN00003457), SPL revised 12/2024; DailyMed setid 820eadcb-1230-4b17-a652-e69b52b3c12f, archived 2026-10-21. https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=820eadcb-1230-4b17-a652-e69b52b3c12f"
kind: label
drugs: [sodium nitroprusside]
source_read: "text layer, all 16 pages (one form-feed chunk per page). Old-format label without numbered sections. Read: boxed warning and Description p.1-2; Clinical Pharmacology incl. Pharmacokinetics and Metabolism, Clinical Trials, Pediatric p.2-5; Indications p.5; Contraindications and Warnings p.6-7; Precautions incl. Pregnancy, Nursing, Pediatric Use p.7-8; Adverse Reactions p.8-9; Overdosage p.10; Dosage and Administration p.11-12; How Supplied, carton panels and SPL product data p.13-16. Page images: p.1, p.5 (paediatric MAP table, scrambled in the text layer), p.12 (infusion-rate table)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Sodium nitroprusside injection (Meitheal) prescribing information, DailyMed

## Study
Not a study: the US label of a generic sodium nitroprusside injection (Meitheal, ANDA211016, marketed from
12/06/2019), 50 mg per 2 mL single-dose vial, for IV infusion only after dilution in D5W. The file is what its name
says (p.1 image).

## Model and parameters
No PK model. Label facts:
- Distribution to a volume about coextensive with extracellular space; cleared by intra-erythrocytic reaction with
  haemoglobin; **circulatory half-life about 2 min** (Clinical Pharmacology, p.2).
- Hypotensive effect **within a minute or two** of an adequate infusion and dissipates almost as rapidly after it
  stops (p.2); excess hypotension self-limited within 1-10 min of stopping (Warnings, p.6).
- One molecule yields one cyanmethaemoglobin and four CN-; red-cell buffering about 175 mcg/kg CN-, i.e. a little
  under 500 mcg/kg nitroprusside (p.3). Endogenous CN- to SCN- conversion about 1 mcg/kg/min, matching a little over
  2 mcg/kg/min nitroprusside (p.3-4).
- Thiosulfate physiological about 0.1 mmol/L; infused thiosulfate half-life about 20 min (p.3).
- **Thiocyanate: normal 50-250 umol/L (3-15 mg/L), renal clearance, half-life about 3 days, doubled or tripled in
  renal failure** (p.4); steady-state half-time of accumulation 3-4 days (p.9).
- Toxic ranges: red-cell CN- binding capacity a little under 200 umol/L (5 mg/L); death reported at 300-3000 umol/L
  (8-80 mg/L) (p.3); thiocyanate mildly neurotoxic at 1 mmol/L (60 mg/L), life-threatening at about 200 mg/L (p.9);
  methaemoglobinaemia >10% only after about 10 mg/kg (p.8-9).
- Paediatric Study 1 (n = 203, randomised 0.3/1/2/3 ug/kg/min for 30 min, stepped 1/3, 2/3, full; no placebo):
  MAP change from baseline -11 +/- 16, -17 +/- 13, -20 +/- 16, -17 +/- 19 mmHg (Table 1, p.5, image-confirmed; n
  50/49/53/51). Study 2 (n = 63): effective for at least 12 h in a randomised withdrawal.
- Dosing (p.1, p.11-12): start 0.3 mcg/kg/min, titrate every few minutes (confirm after 5 min); average effective rate
  about 3 mcg/kg/min; usual 0.5-10, maximum 10 mcg/kg/min for no more than 10 min; prolonged infusions no faster than
  3 mcg/kg/min (1 mcg/kg/min if anuric) to keep SCN- below 1 mmol/L.
- MW 297.95 (dihydrate) (p.2).

```yaml
parameters:
  - {name: t_half_circulatory, value: 2, unit: min, source: "Clinical Pharmacology p.2", note: "about"}
  - {name: onset_hypotensive, value: "1-2", unit: min, source: "Clinical Pharmacology p.2"}
  - {name: t_half_thiocyanate, value: 3, unit: days, source: "p.4", note: "about; normal renal function; x2-3 in renal failure"}
  - {name: t_half_thiosulfate_infused, value: 20, unit: min, source: "p.3"}
  - {name: rate_initial, value: 0.3, unit: mcg/kg/min, source: "Dosage and Administration p.11"}
  - {name: rate_max, value: 10, unit: mcg/kg/min, source: "boxed warning; D&A p.11", note: "no more than 10 min"}
  - {name: rate_cyanide_threshold, value: 2, unit: mcg/kg/min, source: "p.4, p.6"}
  - {name: MW_dihydrate, value: 297.95, unit: g/mol, source: "Description p.2"}
```

## Pharmacodynamics and end points
MAP (paediatric Study 1 above). Effect augmented by ganglionic blockers, negative inotropes and inhaled anaesthetics;
tachyphylaxis reported, mechanism unknown. No concentration-effect relation. Not an opioid; no MEAC.

## Covariates
Renal function (thiocyanate), hepatic insufficiency (caution), elderly (more sensitive), Leber's optic atrophy /
tobacco amblyopia (avoid). Pregnancy category C; fatal fetal cyanide in ewes at 25 mcg/kg/min.

## Caveats
Descriptive label; studies behind the PK statements are not named. No plasma nitroprusside assay or model.

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:114 (8baa733; built by data-raw/antihypertensiveRegistry.R:429):
  catalog record `nitroprusside__injection__iv__catalog__none__label`, descriptive_only; limitations "parent
  circulatory half-life about 2 min with BP onset in 1-2 min and rapid offset"; cyanide production and renal
  thiocyanate elimination (half-life about 3 days with normal renal function) a separate subsystem; the paediatric
  research record is gated.

## Verification against stanpumpR
- **Circulatory half-life about 2 min; onset 1-2 min; rapid offset**: Clinical Pharmacology p.2. **matches**.
- **Thiocyanate renal elimination, half-life about 3 days with normal renal function**: p.4. **matches**.
- **Cyanide and thiocyanate as separate kinetics / subsystem**: a modelling judgement; consistent with the label's
  description of the metabolic pathway. Not a label claim.
- **Paediatric record gated**: concerns Barrett 2015 (see PMID_26283961), not this label. The label's Study 1 had 203
  children; the registry's Barrett record says 202.
