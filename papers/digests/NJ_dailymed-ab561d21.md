---
file: NJ_dailymed-ab561d21.pdf
pmid: null
doi: null
citation: "LISINOPRIL tablets USP, for oral use (Solco Healthcare US / Zhejiang Huahai, ANDA076164; Revised 08/2024), repackaged by Preferred Pharmaceuticals Inc. (40 mg, NDC 68788-7012). DailyMed setid ab561d21-7466-4d8b-8d06-291c1d4d82ff; SPL revised 5/2026; downloaded 2026-10-21."
kind: label
drugs: [lisinopril]
source_read: "text layer, 23 PDF pages (23 form-feed chunks = pages). Read Highlights (p1), table of contents (pp2-3), 1 Indications and 2 Dosage and Administration incl. 2.4 renal impairment (pp3-6), 5 Warnings (pp6-8), 8 Use in Specific Populations (pp12-14), 10 Overdosage and 11 Description (p14), 12.1-12.3 Clinical Pharmacology (pp15-17), 14 Clinical Studies headings (pp17-19) and the SPL product data (pp21-23). Rendered page 16 (12.2-12.3) and confirmed its numbers from the image; other pages are clean prose from the text layer."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Lisinopril tablets (Solco, repackaged by Preferred Pharmaceuticals), US label 2024/2026

## Study
This is a generic US lisinopril label in PLR format. It is the Solco Healthcare / Zhejiang Huahai text (ANDA076164,
revised 08/2024), repackaged by Preferred Pharmaceuticals; the SPL listing covers the 40 mg tablet only (pp21-23).
Initial US approval was 1988. Page 1 is a lisinopril label, as named. One display panel on p21 is headed "30 mg" but
shows 40 mg tablets: this is a packaging oddity, not a wrong document.

**Dosage (section 2, pp4-6):**
- Hypertension: start at 10 mg once daily (5 mg if the patient is on a diuretic). The usual range is 20-40 mg/day.
  Doses up to 80 mg have been used but did not appear to give greater effect.
- Children 6 years and older with GFR > 30 mL/min/1.73 m²: start at 0.07 mg/kg (up to 5 mg) once daily. The
  maximum is 0.61 mg/kg (40 mg).
- Heart failure: start at 5 mg (2.5 mg if sodium < 130 mEq/L), up to 40 mg.
- Acute MI: 5 mg, then 5 mg at 24 h, then 10 mg at 48 h, then 10 mg daily.
- Renal impairment (2.4): no change if CrCl > 30 mL/min. Halve the initial dose at CrCl 10-30. Start at 2.5 mg if
  CrCl < 10 or on haemodialysis.

## Model and parameters
Section 12.3 (PDF pp16-17) is descriptive only. It has no compartmental model and no CL or V for adults.
```yaml
parameters:
  - {name: Tmax (adults), value: 7, unit: h, source: "12.3, p16", note: "within about 7 h; small delay in acute MI"}
  - {name: extent of absorption, value: 0.25, unit: fraction, source: "12.3, p16", note: "from urinary recovery; range 6-60% across 5-80 mg"}
  - {name: F in NYHA II-IV heart failure, value: 0.16, unit: fraction, source: "12.3, p16", note: "absolute; V slightly smaller"}
  - {name: effective t1/2 on multiple dosing, value: 12, unit: h, source: "12.3, p16"}
  - {name: Tmax (children 6-16 y), value: 6, unit: h, source: "12.3, p17", note: "steady-state peak within 6 h, n=29, 0.1-0.2 mg/kg"}
  - {name: extent of absorption (children), value: 0.28, unit: fraction, source: "12.3, p17"}
  - {name: CL/F typical (30 kg child), value: 10, unit: L/h, source: "12.3, p17", note: "increases in proportion to renal function"}
```
- Lisinopril is not metabolised and is excreted unchanged entirely in urine. It does not appear to bind other serum
  proteins. Food does not alter its bioavailability.
- Terminal phase: concentrations decline with a prolonged terminal phase that the label attributes, probably, to
  saturable binding to ACE. That phase is not dose-proportional and does not contribute to accumulation.
- Renal impairment slows elimination, but this becomes clinically important only below a GFR of 30 mL/min. Below
  that, peak and trough rise, Tmax lengthens and steady state takes longer to reach. Lisinopril is dialysable.
- In 22 children with a kidney transplant, dose-normalised exposures were in the range seen in other children.

## Pharmacodynamics and end points
- Blood-pressure onset is about 1 h after a dose and the peak reduction is by 6 h (12.2, p16). At every dose the
  mean effect at 24 h was substantially smaller than at 6 h, and doses of 20 mg or more gave a more consistent
  24-h effect.
- Indomethacin reduced the effect in 36 patients, though not significantly.
- Black patients respond less (8.6, 12.1).
- The label gives no concentration-effect model and no therapeutic concentration.

## Covariates
- Renal function (CrCl/GFR thresholds 30 and 10 mL/min).
- Heart failure lowers F to 16%.
- Children: CL/F scales with weight and renal function (30 kg child = 10 L/h).
- No dose adjustment for the elderly (8.5).

## Caveats
- The adult PK is descriptive, with no study sizes.
- The 25% absorption is an urinary-recovery estimate with sixfold to tenfold intersubject spread.
- The label gives no CL/V that could build a model.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:76` (at 8baa733): `lisinopril__ir_tablet__oral__catalog__none__label`.
  - Status: `official_label_descriptive` / `descriptive_only`.
  - Limitation text: label Tmax about 7 h, about 25% absorbed, effective half-life about 12 h. It adds that plasma
    and tissue ACE binding can give a terminal phase the label's descriptors do not identify, and that BP
    coefficients are missing.
- The implemented lisinopril model is Thomson 1989 (registry line 77), not this label.

## Verification against stanpumpR
- Tmax about 7 h, about 25% absorbed and an effective t1/2 of 12 h: all three are printed in 12.3 (p16). **matches**
- ACE-binding terminal phase: the label says the prolonged terminal phase probably represents saturable binding to
  ACE and is not dose-proportional. It does not say "plasma and tissue" ACE; that detail is the row's. The substance
  **matches**.
- BP coefficients missing: the label gives onset and peak times and no concentration-effect parameters. **matches**
- Descriptive-only: the label has no adult CL or V. **matches**
