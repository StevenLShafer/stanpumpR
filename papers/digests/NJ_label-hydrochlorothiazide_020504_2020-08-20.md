---
file: NJ_label-hydrochlorothiazide_020504_2020-08-20.pdf
pmid: null
doi: null
citation: "MICROZIDE (hydrochlorothiazide, USP) Capsules 12.5 mg. US prescribing information, Allergan USA (manufactured by Watson Pharma Private Ltd). NDA 020504, revised August 2020 (v2.0USPI6154; FDA Reference ID 4659169); accessdata.fda.gov 020504s026lbl.pdf."
kind: label
drugs: [hydrochlorothiazide]
source_read: "text layer, all 9 PDF pages (form-feed chunks = pages; the document is short, so it was read in full). Old-format label with no numbered sections: Description and Clinical Pharmacology (p1), Pharmacokinetics and Metabolism, Pharmacodynamics, Clinical Studies (p2), Indications, Contraindications, Warnings (p3), Precautions incl. pregnancy, pediatric and elderly use (pp4-6), Adverse Reactions (pp6-7), Overdosage and Dosage and Administration (p8). Rendered page 1 to confirm identity."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# MICROZIDE (hydrochlorothiazide 12.5 mg capsules) label, August 2020

## Study
The archived PDF is the FDA-hosted MICROZIDE label (NDA 020504, Allergan, revised August 2020). Rendered page 1 is
that product, so there is no source file problem. It is the old, unnumbered label format. The product is a single
strength, a 12.5 mg capsule. It is indicated for hypertension, alone or with other antihypertensives, and the label
gives no edema indication.

**Dosage (p8):** the adult initial dose is one 12.5 mg capsule once daily. Total daily doses above 50 mg are not
recommended. For the elderly (p6), start at 12.5 mg and titrate in 12.5 mg steps.

## Model and parameters
Pharmacokinetics and Metabolism (p2) gives descriptive values only. There is no compartmental model and no CL or V.
```yaml
parameters:
  - {name: F (oral absorption), value: "65-75", unit: "%", source: "p2", note: "'well absorbed'; reduced in congestive heart failure"}
  - {name: Tmax, value: "1-5", unit: h, source: "p2", note: "peak within 1 to 5 h of dosing"}
  - {name: Cmax range, value: "70-490", unit: ng/mL, source: "p2", note: "after oral doses of 12.5 to 100 mg; linear with dose"}
  - {name: whole blood/plasma ratio, value: "1.6-1.8", unit: ratio, source: "p2"}
  - {name: protein binding, value: "40-68", unit: "%", source: "p2", note: "approximately"}
  - {name: plasma elimination t1/2, value: "6-15", unit: h, source: "p2", note: "'has been reported'; prolonged in renal disease"}
  - {name: urinary recovery, value: "55-77", unit: "% of dose", source: "p2", note: "after 12.5-100 mg; >95% of the absorbed dose excreted unchanged"}
  - {name: food effect on F, value: -10, unit: "%", source: "p2"}
  - {name: food effect on Cmax, value: -20, unit: "%", source: "p2"}
  - {name: Tmax fasted to fed, value: "1.6 to 2.9", unit: h, source: "p2"}
```
The label gives no dialysis clearance; the degree of removal is "not established" (p8).

**Dose-normalised Cmax (derived):** 70/12.5 is about 5.6 and 490/100 about 4.9 ng/mL per mg. For 25 mg that is
roughly 120-140 ng/mL.

## Pharmacodynamics and end points
- **Time course (p2):** antihypertensive onset within 2 h, peak effect at about 4 h, activity up to 24 h. Thiazides
  do not lower a normal blood pressure.
- **Clinical study (p2):** in an 87-patient, 4-week placebo-controlled trial, the 12.5 mg capsule lowered seated
  systolic and diastolic pressure more than placebo.
- **Concentrations:** the label gives no concentration-effect relation and no therapeutic or toxic concentration.

## Covariates
The label gives no equations, only qualitative effects:
- Renal disease raises concentrations and prolongs the half-life.
- Absorption is reduced in CHF.
- The elderly may show a greater blood-pressure fall.
- Cholestyramine reduces absorption by up to 85% and colestipol by up to 43% (p5).

## Caveats
- Every number is a literature-style range or descriptor with no study, n or assay attached.
- The 6-15 h half-life is shorter than the terminal 14.8 h of the Van Wart model. A label range like this is
  usually reported without separating distribution from terminal phases; that is my inference.
- The banner says this label "may not be the latest approved".

## Used by stanpumpR
Locations by content at 8baa733:
- `inst/help/drugs/hydrochlorothiazide.md:101-112`, "Dosing, and a check against the label". It says a 25 mg dose
  in a 70 kg 50-year-old man peaks at about 147 ng/mL at 2.3 h, against a labelled time to peak of 1 to 5 h. The
  help also says the usual dose is 12.5-25 mg daily, with 50 mg for oedema (lines 103-105).
- `tests/testthat/test-drugs-hydrochlorothiazide.R:245-257` asserts the simulated tmax after 25 mg is between 1 and
  5 h ("labelled time to peak is 1 to 5 h"). Its comment says "the label's descriptors are in the registry's
  catalog row for this product". That row is `inst/extdata/antihypertensiveRegistry.csv:67`, whose URL is a
  different document, the DailyMed authorized-generic capsule label `NJ_dailymed-a3eb7294`, also under NDA020504.

## Verification against stanpumpR
- **Labelled time to peak, 1 to 5 h** (help 109; test 256-257): the label (p2) says peak plasma concentrations
  are observed within 1 to 5 h. **matches**
  - The label's more specific figure, Tmax 1.6 h fasted and 2.9 h fed, brackets the model's 2.3 h. That is a
    tighter independent check, which the repo does not quote.
- **Peak of about 147 ng/mL after 25 mg** (help 107-108): it is a model output, not a label value. It lies a
  little above the label's dose-normalised range, about 120-140 ng/mL by linear scaling of 70-490 ng/mL over
  12.5-100 mg (derived). The label calls concentrations dose-linear, but its range is not tied to a population, so
  this is only a rough check. **consistent; cannot verify precisely**
- **Which label:** the context note asked whether a specific label lies behind the claim. This MICROZIDE label
  and the DailyMed authorized-generic label (`NJ_dailymed-a3eb7294`, NDA020504) print identical Pharmacokinetics
  text, so the 1-5 h figure is the same in both. **matches**
- **The help's usual doses** (12.5-25 mg daily, 50 mg for oedema) are not from this label. This label gives a
  12.5 mg start, a 50 mg/day ceiling and no oedema indication. The sentence does not claim the label as its
  source. **not applicable**
