---
file: NJ_label-sertraline_019839_2023-08-22.pdf
pmid: none
doi: none
citation: "ZOLOFT (sertraline hydrochloride) tablets and oral solution, US prescribing information. Pfizer/Viatris; NDA 019839 (tablets) and NDA 020990 (oral solution); revised 8/2023 (FDA approval letter date 2023-08-22). https://www.accessdata.fda.gov/drugsatfda_docs/label/2023/019839s108,20990s062lbl.pdf"
kind: label
drugs: [sertraline]
source_read: partial - text layer; Highlights, section 2 Dosage and Administration (2.1-2.5), section 12.3 Pharmacokinetics in full (absorption, metabolism, protein binding, specific populations), and 8.5/8.6 headings. Warnings, adverse reactions, clinical studies, medication guide not digested.
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: discrepancy
---

# Zoloft (sertraline) label, revised 8/2023

## Study
FDA label. Products: tablets 25, 50, 100 mg; oral solution 20 mg/mL. Initial US approval 1991.
Dosing (2.1, Table 1): MDD and OCD start 50 mg/day; PD, PTSD, SAD start 25 mg/day; therapeutic range
50-200 mg/day; paediatric OCD 6-12 y start 25 mg, 13-17 y start 50 mg. Increase by 25-50 mg/day no more often
than weekly, max 200 mg/day; 2.1 justifies the weekly interval by a "24-hour elimination half-life".
PMDD (2.2): 50 mg/day, continuous up to 150 mg or luteal-phase up to 100 mg. Mild hepatic impairment (2.4):
half the dose; moderate/severe not recommended.

## Model and parameters
No compartmental model. Summary PK (section 12.3):
```yaml
parameters:
  - {name: Tmax, value: "4.5-8.4", unit: h, source: "12.3 Absorption", note: "range of mean Tmax after 50-200 mg once daily for 14 days"}
  - {name: terminal t1/2 (sertraline), value: 26, unit: h, source: "12.3 Absorption", note: "'average ... about 26 hours'; stated in the paragraph on 14-day once-daily dosing; regimen of the estimate not specified"}
  - {name: terminal t1/2 (label 2.1), value: 24, unit: h, source: "2.1", note: "used to justify weekly titration"}
  - {name: accumulation, value: 2, unit: fold, source: "12.3", note: "approximate, to steady state reached after one week of once-daily dosing"}
  - {name: dose proportionality, value: "50-200", unit: mg, source: "12.3", note: "single-dose study, Cmax and AUC proportional"}
  - {name: t1/2 (N-desmethylsertraline), value: "62-104", unit: h, source: "12.3 Metabolism"}
  - {name: desmethylsertraline accumulation day 1 to 14, value: "5-9", unit: fold, source: "12.3", note: "AUC0-24, Cmax, Cmin"}
  - {name: protein binding, value: 98, unit: "%", source: "12.3", note: "in vitro, 20-500 ng/mL"}
  - {name: geriatric clearance, value: -40, unit: "% vs 25-32 y", source: "12.3", note: "16 elderly, 100 mg/day x14 d; steady state 2-3 weeks"}
  - {name: mild hepatic exposure, value: 3, unit: fold, source: "12.3", note: "Child-Pugh 5-8 (n=10), 50 mg/day x21 d; desmethyl 2-fold"}
  - {name: paediatric AUC/Cmax (weight-adjusted), value: -22, unit: "%", source: "12.3", note: "61 children 6-17 y"}
```
- Absolute bioavailability not stated; extensive first-pass metabolism. Food slightly increases Cmax and AUC;
  tablet and solution single-dose bioavailability approximately equal.
- Mass balance (2 men, radiolabel): sertraline < 5% of plasma radioactivity; 40-45% in urine over 9 days (no
  unchanged drug), 40-45% in faeces including 12-14% unchanged.
- Renal impairment (CLcr 10-60 mL/min, haemodialysis; 200 mg/day x21 d): PK and binding unchanged.
- Cimetidine 800 mg/day: sertraline AUC +50%, Cmax +24%, half-life +26%.

## Pharmacodynamics and end points
No exposure-response, therapeutic range or toxic concentration in the label. QT study: at 400 mg/day
(about 3-fold steady-state exposure) largest mean ddQTc 10 ms. Not an opioid.

## Covariates
Age (clearance -40% in elderly), hepatic impairment; no renal or sex effect; paediatric weight-adjusted exposure
22% lower.

## Caveats
The label does not say whether the 26 h half-life is from single or multiple dosing; it sits in a paragraph
describing a 14-day once-daily study, and section 2.1 quotes 24 h.

## Used by stanpumpR
- R/drugs_sertraline.R:50 - model terminal t1/2 33 h (multiple-dose V2) and 26.6 h (single-dose V2) compared
  with "the label's ~26 h, which is a single-dose value".
- inst/help/drugs/sertraline.md:7 - "27 hours with the single-dose volume, close to the label's 26".

## Verification against stanpumpR
- **Value ~26 h** (R/drugs_sertraline.R:50; help :7): label 12.3 says about 26 h. **matches**.
- **"which is a single-dose value"** (R/drugs_sertraline.R:50): the label does not identify the regimen; the
  sentence follows the description of 50-200 mg once daily for 14 days, which if anything suggests a
  multiple-dose estimate. The label does not support calling it single-dose, and so the argument that the
  single-dose V2's 26.6 h "matches" it while the chronic V2's 33 h does not rests on an unsupported premise.
  **discrepancy** (wording/interpretation; no number in the model depends on it).
- **Help "26.6" rounded to "27 hours"** (help :7) vs code "26.6 h" (:47): consistent rounding. **matches**.
- **Help "4.5 to 8.4 hours in the literature"** (inst/help/drugs/sertraline.md:8, not a listed location): this
  range is the label's 14-day mean Tmax; the label is the likely source. Consistent with the label.
- The label is not named in the repo (only "the label"); its identity is Zoloft NDA 019839/020990, rev. 8/2023.
