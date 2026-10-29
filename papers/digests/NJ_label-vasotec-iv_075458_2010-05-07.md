---
file: NJ_label-vasotec-iv_075458_2010-05-07.pdf
pmid: null
doi: null
citation: "Hospira, Inc. Enalaprilat Injection, USP 1.25 mg/mL (1 mL and 2 mL vials, NDC 0409-2122). ANDA 075458, package insert EN-2436, revised March 2010 (FDA action date 2010-05-07). https://www.accessdata.fda.gov/drugsatfda_docs/label/2010/075458s007lbl.pdf"
kind: label
drugs: [enalaprilat]
source_read: "text layer of the single PDF page (a printer's proof of the whole insert, part of it set upside down; the text layer reads in two interleaved columns); read in full: pregnancy box, Description, Clinical Pharmacology incl. Pharmacokinetics and Metabolism and Pharmacodynamics, Indications, Contraindications, Warnings incl. Fetal/Neonatal Morbidity and Mortality, Precautions incl. Pregnancy, Nursing, Pediatric and Geriatric Use, Adverse Reactions, Overdosage, Dosage and Administration, How Supplied. The page was rendered whole and the Description and Pharmacokinetics paragraphs checked on high-resolution crops."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Enalaprilat Injection USP, Hospira (ANDA 075458), insert revised March 2010

Source file note: the stem says "vasotec-iv", but the document is Hospira's generic Enalaprilat Injection USP under
ANDA 075458 (insert EN-2436, "Approved for FDA Submission" stamp dated 03-15-10), not Merck's Vasotec I.V. The
application number in the file name matches, and the product and text are the enalaprilat injection label the repo
refers to, so the card treats it as a valid source.

## Study
Not a study: generic enalaprilat injection label (1.25 mg/mL anhydrous enalaprilat, benzyl alcohol 9 mg; MW 384.43).
Clinical trials: 349 patients (168 hypertension, 153 CHF, 28 coronary disease); hypotension 3.4% (Adverse Reactions).

## Model and parameters
No model. Pharmacokinetics and Metabolism (rendered crop):

```yaml
parameters:
  - {name: t_half_accumulation, value: 11, unit: h, source: "Pharmacokinetics and Metabolism", note: "approximately; determined from oral administration of multiple doses of enalapril maleate"}
  - {name: urinary_recovery_IV, value: ">0.90", unit: fraction of dose, source: "Pharmacokinetics and Metabolism", note: "unchanged within 24 h"}
  - {name: dialysis_clearance, value: 62, unit: mL/min, source: "Pharmacokinetics and Metabolism"}
```
Single IV dose: polyexponential profile with a prolonged terminal phase attributed to a small, saturable ACE-bound
fraction. Disposition unchanged until GFR <= 30 mL/min. No volumes, clearances or exponents.

## Pharmacodynamics and end points
Onset usually within 15 min, maximum within 1-4 h, duration about 6 h at the recommended dose. No concentration-effect
model; not an opioid. Dosing: 1.25 mg IV over 5 min every 6 h; 0.625 mg initial dose on diuretics or CrCl <= 30 mL/min;
IV-to-oral conversion 5 mg (or 2.5 mg) enalapril once daily. Overdose experience: 80 mg IV over 15 min.

## Covariates
Renal function. Pregnancy categories C (first trimester) and D (second and third), with the USE IN PREGNANCY box;
pediatric safety not established.

## Caveats
- Accumulation half-life only, from oral enalapril; no IV elimination half-life. No study sizes for PK statements.
- The PDF is a single oversize proof page, so page references are not meaningful; sections are named instead.

## Used by stanpumpR
- `R/drugs_enalaprilat.R:34-37` (8baa733; the context's line 32 has moved) and `inst/help/drugs/enalaprilat.md:22`:
  the label's "about 11 hour" accumulation half-life comes from repeated ORAL enalapril and must not be read as an
  intravenous elimination half-life; given as part of why IV enalaprilat is not offered.

## Verification against stanpumpR
- "about 11 hour" accumulation half-life, determined from oral multiple doses of enalapril maleate: **matches**
  (rendered Pharmacokinetics paragraph).
- "not an intravenous elimination half-life": **matches**; the label offers no IV elimination half-life at all.
- The help page's gloss that the 11 h "is a property of the formation-limited profile" is the repo's interpretation;
  the label itself attributes the prolonged terminal phase to saturable ACE binding and says only that the 11 h was
  determined from oral dosing. Not a contradiction, but not the label's statement.
- That IV enalaprilat is a real product: **matches** (this label).
