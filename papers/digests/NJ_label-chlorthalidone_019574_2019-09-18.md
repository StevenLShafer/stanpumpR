---
file: NJ_label-chlorthalidone_019574_2019-09-18.pdf
pmid: null
doi: null
citation: "THALITONE (chlorthalidone) tablets, for oral use. US prescribing information, NDA 019574 (Casper Pharma); revised 09/2019; FDA label 019574s017lbl.pdf, 2019-09-18; initial US approval 1988."
kind: label
drugs: [chlorthalidone]
source_read: "text layer, 10 PDF pages (form-feed chunks = pages; footers 'Page n of 10' agree). Read Highlights and contents (p1), 1-2 Indications and Dosage (p2-3), 5 Warnings (p3-4), 8 Use in Specific Populations (p5-6), 10-12 Overdosage, Description and Clinical Pharmacology including 12.3 (p6-8), 14 Clinical Studies (p8-9). Clean prose, no PK table. Rendered PDF page 8 (12.3 text and clinical-studies Table 1) to confirm the values recorded."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# Thalitone (chlorthalidone) US label, 2019

## Study
This is the US prescribing information for THALITONE, a chlorthalidone 15 mg and 25 mg tablet (Revised 09/2019;
Reference ID 4493110). It is the product named. Note that it is a specific PVP-enhanced formulation, not the
generic or Hygroton-type chlorthalidone tablet.
- **Indications:** hypertension in adults, and adjunctive therapy of edema.
- **Dosage (2.1-2.3, p2-3):**
  - THALITONE cannot be substituted with other chlorthalidone formulations.
  - Hypertension: start at 15 mg once daily with food, and after 2 weeks increase to 25 mg if needed. Doses above
    25 mg are not expected to lower pressure further.
  - Edema: 50-100 mg daily, or 100 mg on alternate days, up to 200 mg daily.
- **Description (11, p7):** chlorthalidone USP, racemic, MW 338.76.

## Model and parameters
Section 12.3 (p7-8) is descriptive only.
```yaml
parameters:
  - {name: relative bioavailability (THALITONE vs oral solution), value: "104-116", unit: "%", source: "12.3, p8", note: "PVP bioavailability enhancer"}
  - {name: protein binding, value: 75, unit: "%", source: "12.3, p8", note: "approximately, over 0.2-7.7 ug/mL"}
  - {name: mean plasma t1/2, value: "40-60", unit: h, source: "12.3, p8", note: "about 40 to 60 hours"}
  - {name: onset of diuresis, value: 2.6, unit: h, source: "12.2, p7", note: "mean; diuresis lasts up to 72 h"}
```
- **Elimination:** mainly as unchanged drug in urine.
- **Not given:** no Tmax, Cmax, AUC, clearance, volume or absolute bioavailability is stated.

## Pharmacodynamics and end points
- **Diuresis (12.2):** begins at a mean of 2.6 h and continues up to 72 h. It gives dose-related falls in
  potassium and rises in uric acid and glucose.
- **Trials (14, p8-9):** two 12-week trials in 222 patients with DBP 90-104 mm Hg compared THALITONE 15 mg (n=71),
  standard chlorthalidone 25 mg (n=75) and placebo (n=76). Table 1 (p8) gives the standing SBP change from week
  2 to 12: -11.77 mm Hg on THALITONE against -1.89 on placebo (p<0.001), and -11.73 on standard 25 mg (p=0.98
  against THALITONE).
- No concentration-effect data and no ke0.

## Covariates
- **Renal:** substantially renally excreted, so use caution in renal impairment and the elderly (8.5).
- **Pediatrics:** not established (8.4).
- No quantitative covariate effects.

## Caveats
- The values describe a PVP-enhanced formulation that the label says is not interchangeable with other
  chlorthalidone tablets.
- The half-life is a range with no study details.

## Used by stanpumpR
Lines at 8baa733, found by content:
- `docs/antihypertensives.md:766-794` (section 10.2a, "Checked instead against the labels"). Table rows:
  - 774: chlorthalidone time to peak, model 1.4 h against label 1.5-6 h.
  - 775: terminal half-life, model 32 h against label 40-60 h.
- The FDA URL of this label is not in the repo at 8baa733. The registry's chlorthalidone catalog row cites a
  DailyMed SPL instead (`NJ_dailymed-3f0f3ee0`, Hemiclor).

## Verification against stanpumpR
- **Half-life 40-60 h** (`docs/antihypertensives.md:775`): the label says the mean plasma half-life is about 40 to
  60 hours (12.3, p8). **matches**
- **Time to peak 1.5-6 h** (`docs/antihypertensives.md:774`): this label states no Tmax or time to peak plasma
  concentration anywhere. Its only time-course statement is the onset of diuresis, a mean of 2.6 h, lasting up to
  72 h. The Hemiclor SPL (`NJ_dailymed-3f0f3ee0`) gives no Tmax either. The 1.5-6 h attributed to "the labels"
  is not in either archived chlorthalidone label. **discrepancy** (unsupported attribution; the source of
  1.5-6 h is unknown)
- **Model values** (derived): the model's typical-female time to peak is 1.3 h single-dose and 1.3 h at steady
  state, and its terminal t½ is 32 h (see `PMID_26632101.md`). The docs give 1.4 h, which may be a different
  reference subject. Against this label's 40-60 h the model's half-life is short, as the docs say.
- **Protein binding:** the label's 75% contradicts the chlorthalidone help page's description of the drug as
  "highly bound" (`inst/help/drugs/chlorthalidone.md:20`). That page cites no label for it. Noted only.
