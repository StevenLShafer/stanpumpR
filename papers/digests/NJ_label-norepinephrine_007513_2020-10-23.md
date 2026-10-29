---
file: NJ_label-norepinephrine_007513_2020-10-23.pdf
pmid: none
doi: none
citation: "LEVOPHED (norepinephrine bitartrate) injection, for intravenous use, NDA 007513 (Hospira, Inc., a Pfizer company), prescribing information revised 10/2020, posted on Drugs@FDA 2020-10-23 (LAB-1150-2.1). https://www.accessdata.fda.gov/drugsatfda_docs/label/2020/007513s045lbl.pdf"
kind: label
drugs: [norepinephrine]
source_read: "text layer, all 8 pages: highlights and contents (p1), 1-2 Indications, Dosage and Administration (p2), 3-7 (pp3-4), 8 Use in Specific Populations (pp5-6), 10-11 Overdosage, Description (pp6-7), 12.1-12.3 Clinical Pharmacology (pp7-8), 13-17 (p8)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: partly-verified
---

# Levophed label (2020): norepinephrine bitartrate, dosed as the base

## Study
US prescribing information for Levophed, 4 mg/4 mL (1 mg/mL norepinephrine base) in single-dose vials or ampoules;
"Initial U.S. Approval: 1950" (p1). Not a study. This file is the norepinephrine label only; it says nothing about
metoprolol, nicardipine, nitroglycerin, phenylephrine, procainamide, propranolol or verapamil.

## Model and parameters
No model. Label facts:
```yaml
parameters:
  - {name: base_per_bitartrate, value: 1.89, unit: "mg bitartrate (anhydrous) per mg base", source: "11 pp6-7", note: "each mL contains 1 mg base"}
  - {name: Mw_salt, value: 337.3, unit: g/mol, source: "11 p6", note: "tartrate (1:1) monohydrate"}
  - {name: initial_rate, value: "8-12", unit: mcg/min, source: "2.2 p2; highlights p1", note: "of base"}
  - {name: maintenance_rate, value: "2-4", unit: mcg/min, source: "2.2 p2", note: "of base"}
  - {name: dilution, value: 4, unit: mcg/mL, source: "2.3 p2", note: "4 mg in 1000 mL D5W"}
  - {name: Vd, value: 8.8, unit: L, source: "12.3 p7"}
  - {name: t_half, value: 2.4, unit: min, source: "12.3 p7", note: "mean, approximately"}
  - {name: CL_metabolic, value: 3.1, unit: L/min, source: "12.3 p7", note: "average"}
  - {name: protein_binding, value: 25, unit: "%", source: "12.3 p7", note: "approximately"}
  - {name: time_to_steady_state, value: 5, unit: min, source: "12.2-12.3 p7", note: "plasma and pressor response"}
```
Pressor action stops 1-2 min after the infusion is stopped (12.2). Metabolised by COMT and MAO; inactive
metabolites (12.3). Pregnancy: limited data; animal effects at high doses (8.1).

## Pharmacodynamics and end points
Pressor response reaches steady state within 5 min of starting an infusion and ends 1-2 min after stopping (12.2).
No concentration-effect relation. Not an opioid.

## Covariates
None stated (elderly: start low).

## Caveats
Label summary values (Vd, half-life, clearance) are given without a source or population.

## Used by stanpumpR
- R/drugs_norepinephrine.R:60-67 and inst/help/drugs/norepinephrine.md:7: "Dutch and US labels state norepinephrine
  as the base", so a dose is the base; the model's 2.1 L/min clearance is the known metabolic clearance only on that
  reading.
- The context file also maps the generic "as labelled" dose-basis sentences of metoprolol, nicardipine,
  nitroglycerin, phenylephrine, procainamide, propranolol and verapamil (their R files and help pages) to this
  document; none of them is about this product.

## Verification against stanpumpR
- US label states the dose as norepinephrine base (drugs_norepinephrine.R:62-66; help :7): **matches** (p1, p2
  sections 2.2-3, pp6-7 section 11: 1 mg base = 1.89 mg bitartrate).
- Reading the doses as the tartrate would halve the true clearance (:63-65): consistent with the 1.89 factor
  (derived). The label's own average metabolic clearance is 3.1 L/min, higher than the model's 2.1 L/min; the base
  reading (2.1) is nearer to it than the tartrate reading (about 1.1, derived). The repo does not quote the 3.1.
- "Dutch label" (the SmPC): **cannot verify**; not in this file.
- Metoprolol tartrate, nicardipine HCl, nitroglycerin (McNiff 1981 quoted by "the US label"), phenylephrine HCl,
  procainamide HCl and its 25-30 min loading infusion, propranolol HCl, verapamil HCl: **cannot verify** from this
  file, which is a different product's label.
