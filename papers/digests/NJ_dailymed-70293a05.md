---
file: NJ_dailymed-70293a05.pdf
pmid: null
doi: null
citation: "DILTIAZEM HYDROCHLORIDE extended-release capsules, USP, 120 mg, 180 mg and 240 mg (once daily). US prescribing information, Accord Healthcare Inc. (manufactured by Intas Pharmaceuticals), ANDA206997. DailyMed setid 70293a05-d314-43fa-a2b1-398cfb39edf8; text issued October 2023, SPL revised 10/2025; downloaded 2026-10-21."
kind: label
drugs: [diltiazem]
source_read: "text layer, 18 PDF pages (18 form-feed chunks = pages). Old-format label with no numbered sections or table of contents. Read Description (p1), Clinical Pharmacology incl. Pharmacodynamics and Pharmacokinetics and Metabolism (pp2-5), Indications, Contraindications, Warnings, Precautions incl. Drug Interactions, Pregnancy, Nursing Mothers, Pediatric Use (pp5-8), Adverse Reactions (pp8-11), Overdosage (p11), Dosage and Administration (p12), How Supplied and SPL product data (pp12-18). Rendered page 4 (Pharmacokinetics) and confirmed its numbers from the image."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Diltiazem HCl once-daily extended-release capsules (Accord), US label 2023/2025

## Study
This is a generic US label for once-daily diltiazem hydrochloride extended-release capsules (Accord, ANDA206997).
The strengths are 120, 180 and 240 mg. Each capsule holds multiple 60 mg degradable controlled-release tablets that
release over 24 h. The text was issued October 2023 and the SPL was revised 10/2025. Page 1 is this product, as
named, so there is no source file problem. It is an oral product only: it has no intravenous information, and
stanpumpR's diltiazem is intravenous.

**Dosage (D&A, p12):**
- Hypertension: start at 180 or 240 mg once daily. The usual studied range is 180-480 mg once daily. Some patients
  aged 60 or over respond to 120 mg. The maximum is 540 mg once daily, with limited experience at that dose.
- Angina: start at 120 mg once daily and titrate up to 480 mg over 7-14 days.
- Take on an empty stomach in the morning, and swallow whole.

## Model and parameters
The label gives descriptive values only (Clinical Pharmacology, PDF p4, text confirmed on the rendered page). It
names no studies, gives no n for most of them and has no population model.
```yaml
parameters:
  - {name: F (immediate-release oral), value: 0.40, unit: fraction, source: "p4", note: "approximately 40%, absolute vs IV"}
  - {name: F (this ER capsule, single dose), value: 0.41, unit: fraction, source: "p4", note: "41% (± 14), absolute vs IV"}
  - {name: plasma elimination t1/2, value: "3.0-4.5", unit: h, source: "p4", note: "same page also says 2 to 5 h, beside total radioactivity t1/2 about 20 h"}
  - {name: apparent steady-state t1/2 (ER once daily), value: "5-10", unit: h, source: "p4", note: "attributed to continued absorption, not elimination"}
  - {name: Tmax (ER), value: "4-6", unit: h, source: "p4", note: "absorption begins within 1 h"}
  - {name: protein binding, value: "70-80", unit: "%", source: "p4", note: in vitro}
  - {name: unchanged in urine, value: "2-4", unit: "% of dose", source: "p4"}
  - {name: desacetyldiltiazem / parent, value: "10-20", unit: "% of parent concentration", source: "p4", note: "25-50% as potent a coronary vasodilator"}
  - {name: therapeutic blood level, value: "40-200", unit: ng/mL, source: "p4", note: "the label says 'appear to be'"}
  - {name: AUC ratio 240 vs 120 mg, value: 2.3, unit: fold, source: "p4", note: "360 vs 240: 1.6; 480 vs 240: 2.4; more than dose-proportional"}
  - {name: high-fat breakfast, value: "AUC +13% and +19%; Cmax +37% and +51%", unit: "-", source: "p5", note: "two values given, not attributed to doses"}
```
- Nonlinearity: the label states a departure from linearity, with t1/2 rising slightly with dose and AUC rising
  more than in proportion from 120 to 480 mg.
- Cirrhosis: t1/2 increased and bioavailability rose by 69%.
- Severe renal impairment: the PK profile was no different from normal.

## Pharmacodynamics and end points
- Trough supine DBP after 4 weeks was -5.1, -6.9, -6.9 and -10.6 mmHg at 120, 240, 360 and 480 mg/day. SBP was -2.6,
  -6.5, -4.8 and -10.6 (p3-4). Responder rates were 31%, 42%, 48% and 69%.
- The trough effect (24 h) kept more than half of the peak effect, which came 3-6 h after the dose.
- A dose-escalation study gave DBP/SBP of -6.1/-4.7 at 180 mg, -9.3/-7.2 at 360 mg and -10.2/-6.7 at 540 mg/day.
- In 303 hypertensive patients given up to 540 mg there was no block beyond first degree. The maximum PR increase was
  0.08 s (p3).
- IV diltiazem 20 mg prolongs AH conduction and AV-nodal refractory periods by about 20% (p3).
- No ke0 or concentration-effect model is given. No heart-rate or atrial-fibrillation response data are given: the
  label has no atrial-fibrillation indication.

## Covariates
Hepatic cirrhosis (69% higher F and a longer t1/2); no renal effect; food. Patients aged 60 or over may need a lower
dose. The label gives no equations.

## Caveats
- The label gives no study sizes or designs for the PK values, which are descriptive means.
- Internal inconsistency on p4: diltiazem's t1/2 is given as "2 to 5 hours" (against radioactivity) and as "3.0 to
  4.5 hours" a few lines later.
- The high-fat-breakfast effects are given as two numbers each, without saying which dose or study each belongs to.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:41` (at 8baa733): `diltiazem__er_capsule_once_daily__oral__catalog__none__label`.
  - Moiety: "diltiazem (desacetyldiltiazem activity not quantified)", oral once-daily ER capsule.
  - Status: `official_label_descriptive` / `descriptive_only`, no stanpumpR drug.
  - Limitation text: label F about 41% (SD 14), Tmax 4-6 h, and the 5-10 h ER half-life is absorption-limited (the
    intrinsic one is 3-4.5 h). It also warns that PR-interval and atrial-fibrillation rate PD must not be read as BP.

## Verification against stanpumpR
- F 41% (SD 14): the label prints 41% (± 14) for the ER single dose. **matches**
- Tmax 4-6 h: matches the label. **matches**
- The 5-10 h apparent half-life is absorption-limited, against an intrinsic 3-4.5 h: the label gives both numbers
  and attributes the prolongation to continued absorption. **matches** (the label's other figure, 2-5 h, is not
  quoted by the row).
- Desacetyldiltiazem activity "not quantified": the label gives it only as 25-50% coronary-vasodilator potency at
  10-20% of parent concentrations, with no PK. The row's wording is fair. **matches**
- The PR-interval caution has a basis in this label (0.08 s maximum PR increase). The "atrial-fibrillation rate PD"
  part comes from elsewhere (the IV literature), since this label has no such data. That is not a discrepancy.
  **cannot verify from this label**
- Descriptive-only with no stanpumpR drug: the label gives no fitted vector, so this is consistent. The implemented
  `R/drugs_diltiazem.R` is intravenous, from Hoglund 1988, and does not use this label. **matches**
