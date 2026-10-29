---
file: NJ_dailymed-032835ab.pdf
pmid: null
doi: null
citation: "Minoxidil Tablets USP (2.5 mg; label text covers 2.5 and 10 mg). Prescribing information, DailyMed SPL setid 032835ab-1c8b-418a-9009-b6ebe58676dc, version 2. Repackager Carilion Materials Management (NDC 68151-2031); source product Sun Pharmaceutical Industries, ANDA072709; label text Rev 02, November 2014; SPL revised 1/2015."
kind: label
drugs: [minoxidil]
source_read: "text layer, all 14 PDF pages. Boxed Warning and Description p1; Clinical Pharmacology 1-4 (incl. 3 Absorption and Metabolism) pp1-3; Indications, Contraindications, Warnings pp3-5; Precautions incl. Pregnancy, Nursing, Pediatric, Geriatric pp5-7; Adverse Reactions pp7-8; Overdosage and Dosage and Administration p8; How Supplied and patient information pp9-13; SPL product data p14. Rendered page 1 to confirm the document's identity. This old-format label has no numbered section 12."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: partly-verified
---

# Minoxidil tablets label (DailyMed, Carilion repack of Sun ANDA072709)

## Study
- **Document:** a US prescribing information in the old format, for the generic oral minoxidil tablet. It is a
  repackager's SPL (Carilion Materials Management, one 2.5 mg tablet per cup) of Sun Pharmaceutical's ANDA072709
  product, marketed since 12/14/1995.
- **Version:** label text Rev 02, November 2014; SPL revised 1/2015 (pp13-14).
- **Content:** no clinical study is described beyond summary statements. Minoxidil is C9H15N5O, MW 209.25 (p1).
- **Identity:** the document is what its name says. Rendered page 1 shows the minoxidil boxed warning.

## Model and parameters
No PK model and no parameter table. The pharmacokinetic statements are in Clinical Pharmacology 3, "Absorption
and Metabolism" (p2):

```yaml
parameters:
  - {name: oral absorption, value: ">= 90", unit: "%", source: "Clin Pharm 3, p2", note: "in experimental animals and man"}
  - {name: Tmax (parent), value: "<= 1", unit: h, source: "Clin Pharm 3, p2", note: "peak within the first hour, then a rapid fall"}
  - {name: plasma half-life, value: 4.2, unit: h, source: "Clin Pharm 3, p2", note: "average, man"}
  - {name: fraction metabolised, value: 90, unit: "%", source: "Clin Pharm 3, p2", note: "mainly glucuronide at the N-oxide; metabolites much less active"}
  - {name: plasma protein binding, value: 0, unit: "%", source: "Clin Pharm 3, p2", note: "renal clearance equals GFR"}
  - {name: peak blood level at 100 mg/day, value: "1641 and 2441", unit: ng/mL, source: "Overdosage, p8", note: "two patients, radioimmunoassay"}
  - {name: suggested overdose level, value: "> 2000", unit: ng/mL, source: "Overdosage, p8", note: "'a substantial increase above' this unless the maximum dose is known not to be exceeded"}
```

Minoxidil and its metabolites are removable by haemodialysis (p2).

## Pharmacodynamics and end points
Clinical Pharmacology 2 (pp1-2):
- **Time course:** after an effective single oral dose, blood pressure starts to fall within 30 min and reaches a
  minimum at 2-3 h. It recovers at about 30% per day, arithmetically linear, so the total effect lasts about 75 h.
- **Concentration-effect:** the extent and time course of the fall do not correspond closely to plasma
  concentration.
- **Dose-response:** the fall in BP is linear in log dose, with a slope proportional to the severity of
  hypertension that approaches zero at a supine diastolic pressure near 85 mm Hg.
- **Time to maximum effect** on chronic dosing: 7 days on 10 mg/day, 5 days on 20 mg/day, 3 days on 40 mg/day.
- **Response:** supine diastolic pressure fell by 20 mm Hg, or to 90 mm Hg or below, in about 75% of patients
  (Indications, p3).

## Covariates
- **Renal failure or dialysis:** patients may need smaller doses (Precautions 1(e), p5). No quantitative
  adjustment.
- **Children under 12:** 0.2 mg/kg/day to start, usually 0.25-1 mg/kg/day, maximum 50 mg/day.
- **Adults (and children over 12):** 5 mg/day to start, then 10, 20 and 40 mg. The usual range is 10-40 mg/day,
  maximum 100 mg/day. Dose changes at least 3 days apart, or every 6 h with careful monitoring (p8).
- **Geriatric:** no specific data (p7).
- **Pregnancy:** Category C.

## Caveats
- A generic repackager's label with qualitative PK only: one half-life, no clearance or volume, no exposure table.
  It does not mention minoxidil sulfate.
- Concomitant diuretic and β-blocker therapy is required (Boxed Warning; Warnings 1-2), which confounds any BP
  response taken from clinical use.

## Used by stanpumpR
`inst/extdata/antihypertensiveRegistry.csv:89` (row `minoxidil__ir_tablet__oral__catalog__none__label`, from
`data-raw/antihypertensiveRegistry.R:398`): a descriptive-only catalogue row, not simulated.
- Active moiety: "minoxidil sulfate (active)". Analyte: "minoxidil (parent)".
- Limitations text: "Parent Tmax within 1 h and half-life about 4.2 h, but the BP nadir is at 2-3 h and the effect
  can persist about 75 h ... Sulfate formation and the slow response are missing."

## Verification against stanpumpR
- **"Parent Tmax within 1 h"**: label p2, "maximum within the first hour". **matches**
- **"Half-life about 4.2 h"**: label p2, average 4.2 h. **matches**
- **"BP nadir is at 2-3 h and the effect can persist about 75 h"**: label pp1-2. **matches**
- **"Parent concentration does not track duration"**: the label says the BP fall does not correspond closely to
  plasma concentration (p1). **matches**
- **Active moiety "minoxidil sulfate (active)" and "Sulfate formation ... missing"**: **cannot verify** from this
  source. The label never mentions minoxidil sulfate. It says that the main metabolite is the glucuronide and that
  known metabolites are much less active than minoxidil itself. Taken at face value that reads against calling a
  metabolite the active moiety. The claim needs a source other than this label.
- **Source type "official_label"**: this is a repackager's SPL of a generic ANDA product, not the innovator
  (Loniten) label. The content is the standard class text. **matches**, with that note.
