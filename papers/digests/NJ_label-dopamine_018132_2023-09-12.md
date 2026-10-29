---
file: NJ_label-dopamine_018132_2023-09-12.pdf
pmid: null
doi: null
citation: "Dopamine Hydrochloride Injection, USP (Hospira, Inc.; distributed by Hospira, adverse reactions to Pfizer). FDA prescribing information, NDA 018132 (supplement s071), Revised 09/2023, Reference ID 5242053. https://www.accessdata.fda.gov/drugsatfda_docs/label/2023/018132Orig1s071lbl.pdf"
kind: label
drugs: [dopamine]
source_read: "text layer, whole label (9 pages: Highlights, sections 1-17); page image of PDF page 8 (sections 12.1-12.3) rendered and read"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: matches
---

# Dopamine HCl Injection label (Hospira), rev. 09/2023

## Study
Not a study: the FDA prescribing information for dopamine HCl 40 mg/mL injection (single-dose vials of 200 mg/5 mL
and 400 mg/10 mL; section 3, PDF p. 4). Indication: to improve hemodynamic status in distributive shock or shock from
reduced cardiac output (section 1, p. 2). Initial U.S. approval 1974.

Dosing (section 2.2, PDF p. 3): start at 2 to 5 mcg/kg/min continuous IV infusion in adults and children; titrate in
5 to 10 mcg/kg/min increments by hemodynamic response; do not exceed 50 mcg/kg/min. Dilutions (section 2.1, p. 2):
200 or 400 mg in 250, 500 or 1000 mL give 200 to 1600 mcg/mL; 3200 mcg/mL mentioned for fluid restriction (p. 3).
Infusion-rate formula: mL/h = dose (mcg/kg/min) x weight (kg) x 60 / concentration (mcg/mL). Each mL of the 40 mg/mL
product contains 40 mg dopamine HCl = 32.31 mg dopamine base (section 11, p. 7); MW of the HCl salt 189.64.
MAO inhibitor within 2 to 3 weeks: start at no more than 1/10 of the usual dose (Table 1, section 7, p. 5).

## Model and parameters
No compartmental model. Section 12.3 (PDF p. 8, confirmed on the page image) gives:
```yaml
parameters:
  - {name: t_half_adult, value: "<2", unit: min, source: "section 12.3, PDF p. 8", note: "elimination half-life in adults, upper bound only"}
  - {name: CL_pediatric, value: "46 to 168", unit: mL/kg/min, source: "section 12.3, PDF p. 8", note: "reported range in critically ill infants and children; higher in younger patients"}
  - {name: V_neonate, value: "0.6 to 4", unit: L/kg, source: "section 12.3, PDF p. 8", note: "reported apparent volume of distribution in neonates"}
  - {name: t_half_neonate, value: "5 to 11", unit: min, source: "section 12.3, PDF p. 8", note: "elimination half-life in neonates"}
  - {name: fraction_MAO_COMT, value: 0.75, unit: fraction, source: "section 12.3, PDF p. 8", note: "about 75% metabolized by MAO and COMT to HVA and DOPAC"}
  - {name: fraction_to_norepinephrine, value: 0.25, unit: fraction, source: "section 12.3, PDF p. 8", note: "about 25% converted to norepinephrine in adrenergic nerve terminals"}
  - {name: renal_excretion_24h, value: 0.80, unit: fraction, source: "section 12.3, PDF p. 8", note: "about 80% excreted renally as inactive metabolites within 24 h"}
```
The label states the source of the pediatric numbers only as "reported" (no reference). No adult clearance or
volume. Does not cross the blood-brain barrier to a significant extent.

## Pharmacodynamics and end points
Section 12.2 (PDF p. 8, page image): onset within 5 min of IV administration, duration of action less than about
10 min. Dose-dependent receptor effects: below 5 mcg/kg/min D1/D2 (renal, mesenteric, coronary vasodilation);
5 to 10 mcg/kg/min beta-1 (heart rate and contractility); above 10 mcg/kg/min alpha-1 (vasoconstriction, blood
pressure). These bands are infusion rates, not concentrations. **No plasma concentration, therapeutic range, toxic
level, ke0 or EC50 is given anywhere in the label.** Overdose manifests as excessive blood pressure (section 10).
Not an opioid: no MEAC or potency ratio applies.

## Covariates
None quantified. Pediatric clearance higher in younger patients (range only). Geriatric: start at the low end
(section 8.5). Halogenated anesthetics sensitize the myocardium (section 7).

## Caveats
- A class-style label for an old drug: PK values are ranges or bounds without study detail or citation.
- Dose is expressed as dopamine HCl (the salt); 40 mg HCl = 32.31 mg base.
- The label's endogenous-catecholamine status means measured plasma dopamine includes an endogenous baseline; the
  label says nothing about it.

## Used by stanpumpR
- `inst/extdata/therapeuticRanges.csv:197` (context file): registry source for the dopamine row, band status
  none_established, "no therapeutic concentration range; titrated to effect (label read: dopamine, Hospira,
  rev. 09/2023)".

## Verification against stanpumpR
- **therapeuticRanges.csv, dopamine row** (in this snapshot the row is at line 198; line 197 is norepinephrine,
  so the context file's line number is off by one): lower/upper/typical 0, units ng/mL, band_status
  none_established, source "no therapeutic concentration range; titrated to effect (label read: dopamine,
  Hospira, rev. 09/2023)", verification "label read". The label (Hospira distributor, Revised 09/2023 on the
  Highlights) gives no concentration range of any kind, and its dosing is titration to hemodynamic response
  (section 2.2). **matches.** The ng/mL unit on an empty band is not tested by the label.
- Nothing in the row depends on a number from the label, so no unit conversion applies.
