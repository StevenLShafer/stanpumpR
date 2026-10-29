---
file: NJ_label-valproatesodiuminjection_020593_2020-05-20.pdf
pmid: none
doi: none
citation: "DEPACON (valproate sodium) injection, for intravenous use, US prescribing information, AbbVie; NDA 020593 (s041), FDA label dated 2020-05-20. https://www.accessdata.fda.gov/drugsatfda_docs/label/2020/020593s041lbl.pdf"
kind: label
drugs: [valproate]
source_read: text layer; read Highlights, contents, 2.1-2.3 (Dosage and Administration incl. Replacement Therapy and Administration), 3, 11, 12.2 Pharmacodynamics, 12.3 Pharmacokinetics (bioavailability, infusion-rate study, distribution, elimination, special populations)
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
revised: 2026-10-14 (added total vs unbound valproate and the monitoring warnings)
---

# Depacon label 2020: valproate sodium injection prescribing information

## Study
Regulatory label. Product: valproate sodium injection, each mL equivalent to 100 mg valproic acid, 5 mL vials
(Sections 3, 11). Given as a 60 min infusion, not faster than 20 mg/min, diluted in at least 50 mL; use beyond
14 days not studied (2.1, 2.2). Studies summarised in 12.3: Depakote vs IV 250 mg q6h for 4 days in 18 healthy
men; IV 500 mg vs Depakene syrup 500 mg in 17 healthy men; switch study in patients on 750-4250 mg/day q6h
(n = 24 alone, plus carbamazepine 15, phenytoin 11, phenobarbital 1); infusion-rate crossover in 11 healthy
volunteers (1000 mg over 5, 10, 30, 60 min).

## Model and parameters
No compartmental model. Values (12.3 unless noted):

```yaml
parameters:
  - {name: IV_oral_equivalence, value: "equivalent Cmax, Cmin and total exposure", source: "12.3 Bioavailability", note: "IV as 60 min infusion vs equivalent oral doses"}
  - {name: Depakote_vs_IV_q6h, value: "equivalent AUC, Cmax, Cmin at steady state and after first dose", source: "12.3", note: "250 mg q6h x 4 d, 18 healthy men"}
  - {name: tmax_IV, value: "end of 1 h infusion", source: "12.3"}
  - {name: tmax_Depakote, value: "approximately 4", unit: h, source: "12.3"}
  - {name: Cmax_5min_infusion, value: "145 +/- 32", unit: mcg/mL, source: "12.3", note: "1000 mg over 5 min, mean rate 2.8 mg/kg/min, total valproate"}
  - {name: Cmax_60min_infusion, value: "115 +/- 8", unit: mcg/mL, source: "12.3", note: "1000 mg over 60 min"}
  - {name: CL_total, value: 0.56, unit: "L/h/1.73 m2", source: "12.3 Elimination"}
  - {name: V_total, value: 11, unit: "L/1.73 m2", source: "12.3 Elimination"}
  - {name: t_half_IV, value: "16 +/- 3.0", unit: h, source: "12.3 Elimination", note: "monotherapy, after 1000 mg IV infusion"}
  - {name: free_fraction, value: "approx 10% at 40 to 18.5% at 130 mcg/mL", source: "12.2, 12.3"}
```

Total vs unbound valproate (read on page images 2026-10-14; the label pages print no page numbers):
- 12.2 (PDF p. 32): the concentration-response relation is poorly documented, partly because binding is
  nonlinear and concentration dependent, so total serum valproate is not a reliable index of the bioactive
  (unbound) drug; free fraction about 10% at 40 mcg/mL rising to 18.5% at 130 mcg/mL; free fractions are
  higher than expected in the elderly, in hyperlipidaemia and in hepatic and renal disease.
- 12.3 Bioavailability (PDF p. 33): unbound valproate kinetics are linear (the basis for assuming Depacon /
  Depakote bioequivalence up to 60 mg/kg/day); in the infusion-rate study only total levels were measured, and
  the label says the rise in unbound Cmax at faster rates will be greater than the total Cmax suggests.
- 12.3 (PDF p. 34): CSF concentrations approximate plasma unbound concentrations (about 10% of total); total
  concentration rises less than proportionally with dose because binding saturates, while unbound kinetics are
  linear.
- 12.3 Special populations (PDF p. 35): elderly (68-89 y vs 22-26 y) intrinsic clearance -39%, free fraction
  +44%; liver disease: unbound clearance -50% in 7 cirrhotics and -16% in 4 with acute hepatitis vs 6 healthy,
  unbound fraction 2 to 2.6-fold, so total levels may look normal while free levels are high; renal failure
  (CrCl < 10 mL/min): unbound clearance -27%, binding substantially reduced, so total levels may mislead;
  unbound clearance by sex 4.8 +/- 0.17 (men) and 4.7 +/- 0.07 (women) L/h per 1.73 m2.
- 8.4 Pediatric use (PDF p. 31): variability in free fraction limits the usefulness of total-level monitoring
  in children.

Neonates (<10 days) t½ 10-67 h vs 7-13 h beyond 2 months; children 3 months-10 y have 50% higher clearance per
kg than adults; over 10 y adult-like. Elderly: intrinsic clearance 39% lower, free fraction 44% higher.

Dosing (2.1): replacement therapy — total daily IV dose equivalent to the total daily oral dose, same
frequency; equivalence at steady state was shown only for q6h, so trough monitoring is advised with bid/tid
IV. Daily doses above 250 mg given divided. Max 60 mg/kg/day.

## Pharmacodynamics and end points
Epilepsy range commonly 50-100 mcg/mL total (12.2). Thrombocytopenia more likely at total ≥110 (women) or ≥135
mcg/mL (men) (2.2). Not an opioid: no MEAC, ke0 or potency ratio.

## Covariates
Age (neonate, child, elderly), hepatic and renal disease, inducers; qualitative only.

## Caveats
The label states equivalence of exposure between IV and oral products; it does not give an absolute oral
bioavailability number. Equivalence at steady state was tested only at q6h. Every concentration in the label
(band, Cmax, clearance, volume) is total valproate; the label itself warns that total levels can misrepresent
the active unbound drug, most of all in the elderly, in hepatic or renal disease and at high total levels.

## Used by stanpumpR
- R/drugs_valproate.R:58-62: doses in mg valproic acid equivalents "as the US labels state strengths"; IV and
  oral interchangeable mg for mg, so the apparent parameters are read as absolute (F = 1).
- R/drugs_valproate.R:117 (reference string): "intravenous on the label's 1:1 conversion".

## Verification against stanpumpR
- **Strengths in valproic acid equivalents (valproate.R:58)**: label: each mL "equivalent to 100 mg valproic
  acid"; the Depakote ER label likewise. **Matches.**
- **IV/oral 1:1 (valproate.R:59-62, 117)**: label 2.1 (total daily IV dose equivalent to oral) and 12.3
  (equivalent AUC/Cmax/Cmin; IV 500 mg equivalent to syrup 500 mg). **Matches.** The step from this to
  "absolute F about 1" is an inference (the label gives no number), which the code header correctly records as an
  assumption. Caveat not in the repo: the label's steady-state equivalence was shown only q6h.
- Context: IV t½ 16 ± 3 h vs model 15.0 h; CL 0.56 L/h/1.73 m2 vs model 0.646 L/h at 70 kg — consistent.
  The app's IV units are "mg" and "mg/hr" (CSV row 106); the label's 60 min infusion is a user choice, not
  enforced. **Matches.**
- Total vs unbound (re-checked on master 781013d): R/drugs_valproate.R:42-45 says binding saturates in the
  therapeutic range, that the curve is total valproate on a linear model and that unbound is not plotted;
  inst/help/drugs/valproate.md:11 says the same. Consistent with the label (12.2, 12.3). Not stated in the repo:
  the label's warning that total levels mislead in the elderly and in hepatic or renal disease. No discrepancy.
