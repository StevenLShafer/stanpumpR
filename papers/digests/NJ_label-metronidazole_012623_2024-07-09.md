---
file: NJ_label-metronidazole_012623_2024-07-09.pdf
pmid: none
doi: none
citation: FLAGYL (metronidazole) tablets 250 mg and 500 mg (Pfizer). Prescribing information, NDA 012623 (012623s072lbl), FDA label dated 2024-07-09 (Reference ID 5407005). https://www.accessdata.fda.gov/drugsatfda_docs/label/2024/012623s072lbl.pdf
kind: label
drugs: [metronidazole]
source_read: text layer (Clinical Pharmacology incl. absorption, distribution, metabolism/excretion and special populations; Microbiology headings; Dosage and Administration skimmed). Old-format label.
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Flagyl label 2024: oral metronidazole

## Study
Not a study: US prescribing information for Flagyl oral tablets. Clinical Pharmacology gives summary
PK; special-population statements cite small studies without n except where shown. Assay not given.

## Model and parameters
No model. Clinical Pharmacology (label pp. 1-3):

```yaml
parameters:
  - {name: tmax_oral, value: "1-2", unit: h, source: "Absorption"}
  - {name: Cmax_250mg, value: 6, unit: mcg/mL, source: "Absorption", note: "oral, peak"}
  - {name: Cmax_500mg, value: 12, unit: mcg/mL, source: "Absorption", note: "oral, peak"}
  - {name: Cmax_2000mg, value: 40, unit: mcg/mL, source: "Absorption", note: "oral, peak"}
  - {name: protein_binding, value: "<20", unit: "%", source: "Distribution", note: "an upper bound, not a measurement"}
  - {name: renal_clearance, value: 10, unit: mL/min/1.73 m2, source: "Metabolism/Excretion", note: approximately}
  - {name: t_half, value: 8, unit: h, source: "Metabolism/Excretion", note: "average, healthy subjects"}
  - {name: urine_recovery, value: "60-80", unit: "% dose", source: "Metabolism/Excretion", note: "parent + metabolites; unchanged ~20% of total; feces 6-15%"}
```

Oral and IV disposition similar; concentrations dose-proportional; no sex difference in bioavailability
(males have lower levels because of weight). CSF, saliva and breast milk concentrations similar to plasma.
Metabolites: side-chain oxidation (hydroxy metabolite, acetic acid metabolite) and glucuronides; parent and
hydroxy metabolite both active in vitro. Renal impairment: single-dose PK unchanged; ESRD (CLcr 8.1 ± 9.1
mL/min) after 500 mg IV: hydroxy-metronidazole Cmax 2-fold and metronidazole acetate 5-fold higher.
Hemodialysis 4-8 h removes 40-65% of the dose; CAPD 7.5 h about 10%. Hepatic impairment (500 mg IV):
AUC24 +54% (Child-Pugh A), +53% (B), +114% (C); halve the dose in Child-Pugh C. Elderly >70 y: hydroxy
metabolite AUC 40-80% higher, parent unchanged. Neonates 28-40 weeks gestation: t½ 109 to 22.5 h,
inversely related to gestational age. Anaerobic dosing example: 7.5 mg/kg every 6 h (about 500 mg at
70 kg), maximum 4 g/24 h.

## Pharmacodynamics and end points
No concentration-effect model in the text read; microbiology section gives in vitro MIC context (not
used here). No ke0.

## Covariates
Hepatic impairment, renal failure/dialysis, age (metabolite), gestational age (neonates); percentages only.

## Caveats
- "Less than 20% bound" is a bound with no method given.
- Peak concentrations are rounded summary values with no population.

## Used by stanpumpR
- `R/drugs_metronidazole.R:64`: the labels' "less than 20% bound" is cited as a bound, contrasted with
  Dorn 2021's measured free fraction 0.96 used for the free-drug MIC threshold.

## Verification against stanpumpR
- `R/drugs_metronidazole.R:63-64`: label says less than 20% of circulating metronidazole is bound to plasma
  proteins; the code quotes it correctly and correctly treats it as a bound (fu > 0.80). Dorn's fu 0.96 is
  consistent with it, and the code's sensitivity range 0.80-1.0 (line 68) starts at the label's bound.
  **matches**.
- Not a cited location, noted for audit: the model's half-time at 70 kg, 8.4 h (line 20), agrees with the
  label's average 8 h. For 500 mg PO at the reference man (V 37.56 L, CL 3.22 L/h, ka 1.336 /h, F 0.841)
  the derived peak is about 9.3 mcg/mL at 2.2 h, against the label's 12 mcg/mL at 1-2 h: about 23% low and
  slightly late. The label gives no population for its figure.
