---
file: NJ_dailymed-0a66f606.pdf
pmid: null
doi: null
citation: "FOSINOPRIL SODIUM tablets USP, 10, 20 and 40 mg, for oral use. US prescribing information, Solco Healthcare LLC (manufactured by Zhejiang Huahai), ANDA205670. DailyMed setid 0a66f606-5e6f-473d-8a1e-9ef316adb2e2; text revised 06/2017, SPL revised 10/2021; downloaded 2026-10-21."
kind: label
drugs: [fosinopril]
source_read: "text layer, 25 PDF pages (25 form-feed chunks = pages). Old-format label with no numbered sections or table of contents. Read Description and Clinical Pharmacology incl. Pharmacokinetics and Metabolism and Pharmacodynamics (pp1-5), Indications, Contraindications, Warnings (pp5-8), Precautions incl. Impaired Renal and Liver Function, Pregnancy, Nursing Mothers, Pediatric and Geriatric Use (pp8-12), Overdosage and Dosage and Administration (pp17-18), How Supplied and SPL product data (pp18-25). Adverse-reaction tables (pp13-16) skimmed. Every number used is from clean prose in the text layer; no page image was needed."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# Fosinopril sodium tablets (Solco), US label 2017/2021

## Study
This is a generic US label for fosinopril sodium tablets, 10, 20 and 40 mg (Solco/Huahai, ANDA205670). The text was
revised 06/2017 and the SPL listing 10/2021. Page 1 is fosinopril sodium, as named (the SPL header misspells it
"fosinopirl"). Fosinopril is the ester prodrug, hydrolysed by esterases to the active diacid fosinoprilat. The
fosinopril sodium MW is 585.65.

**Dosage (D&A, pp17-18):**
- Hypertension: start at 10 mg once daily. The usual range is 20-40 mg, and some patients respond further to 80 mg.
  If the trough response is inadequate, consider splitting the dose.
- Children over 50 kg: 5-10 mg once daily.
- Heart failure: 10 mg (5 mg if renal failure or vigorous diuresis), up to 40 mg once daily.
- Renal impairment: the usual dose, because hepatobiliary elimination compensates.

## Model and parameters
The label gives descriptive values for fosinoprilat (Clinical Pharmacology, pp2-3). It has no compartmental model.
```yaml
parameters:
  - {name: absolute absorption of fosinopril, value: 0.36, unit: fraction, source: "p2", note: "averaged 36% of an oral dose"}
  - {name: Tmax (fosinoprilat), value: 3, unit: h, source: "p2", note: "approximately 3 h, independent of dose"}
  - {name: protein binding (fosinoprilat), value: 99.4, unit: "%", source: "p2"}
  - {name: CL (IV fosinoprilat, healthy), value: "26-39", unit: mL/min, source: "p3", note: "mean range across two studies"}
  - {name: terminal t1/2 (IV fosinoprilat, healthy), value: 12, unit: h, source: "p3", note: approximately}
  - {name: effective t1/2 for accumulation (hypertensives), value: 11.5, unit: h, source: "p3", note: "repeated dosing"}
  - {name: effective t1/2 (heart failure), value: 14, unit: h, source: "p3"}
  - {name: terminal t1/2 (children 6-16 y), value: "11-13", unit: h, source: "p3", note: "N=20, 0.3 mg/kg solution"}
  - {name: fraction of plasma radioactivity as fosinoprilat, value: 0.75, unit: fraction, source: "p2", note: "glucuronide 20-30%, p-hydroxy 1-5%"}
  - {name: dialysis clearance, value: "2 (HD), 7 (PD)", unit: "% of urea clearance", source: "p3"}
```
- Elimination: IV fosinoprilat is eliminated about equally by liver and kidney. After oral dosing, about half the
  absorbed dose is in urine and the rest in faeces.
- Proportionality: AUC and Cmax are directly proportional to dose after single and multiple doses.
- Renal impairment (CrCl 10-80 mL/min/1.73 m²): clearance is not appreciably different. In end-stage disease
  (< 10), total body clearance is about half of normal (p3).
- Cirrhosis: apparent clearance is about half, and AUC about doubles (p3, p9).
- Elderly men aged 65-74 show no significant difference.

## Pharmacodynamics and end points
- Serum ACE activity was at least 90% inhibited 2-12 h after 10-40 mg. At 24 h it remained 85%, 93% and 93%
  suppressed at 10, 20 and 40 mg (p3).
- BP fell within 1 h and reached its peak at 2-6 h. The effect lasted 24 h (p4).
- At 20-80 mg once daily, trough seated or supine BP was 8-9/6-7 mmHg lower than placebo. The trough is about 50-60%
  of the peak diastolic and about 80% of the peak systolic response.
- In 252 children given 0.1, 0.3 or 0.6 mg/kg, the reductions were similar across doses.
- The label gives no concentration-effect model.

## Covariates
End-stage renal disease and hepatic cirrhosis each roughly halve fosinoprilat clearance. Heart failure lengthens the
effective t1/2 to 14 h. Age has no effect. The label gives no equations.

## Caveats
- Internal inconsistency on renal impairment:
  - p3 says clearance is not appreciably different at CrCl 10-80 but about half in ESRD.
  - p9 says impaired renal function decreases total clearance and about doubles AUC.
  - D&A (p18) says clearance is about 50% slower, then says it does not differ appreciably at any degree of renal
    insufficiency, including ESRD.
- The PK values are descriptive means without study sizes, except the pediatric N=20.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:59` (at 8baa733): `fosinopril__tablet__oral__catalog__none__label`.
  - Moiety: fosinoprilat, `official_label_descriptive` / `descriptive_only`.
  - Limitation text: "Prodrug identity only; parent and active-moiety PK and BP are missing." It then records that
    fosinopril was not pursued (out of clinical use, maintainer's decision 2026-10-20) and that Gehr 1993 fails the
    four-number identifiability check.
  - The row is generated from `data-raw/antihypertensiveRegistry.R:120`.

## Verification against stanpumpR
- Moiety fosinoprilat, oral tablet: correct. **matches**
- "Prodrug identity only; parent and active-moiety PK and BP are missing": this undersells the label, which gives
  descriptive fosinoprilat PK and BP response data.
  - PK given: IV CL 26-39 mL/min, terminal t1/2 about 12 h, effective t1/2 11.5 h, 36% absorption, Tmax about 3 h,
    and the renal and hepatic effects.
  - BP given: onset, peak at 2-6 h, and trough-to-peak ratios.
  - The label lacks what the registry needs (a compartmental vector and a concentration-BP model), so read as
    "missing as a model" the row is right. Read literally it is not.
  - **discrepancy (wording, minor)**: the label carries active-moiety PK and BP descriptors.
- Not pursued, and the Gehr 1993 arithmetic: this comes from outside the label (see `PMID_8112372.md`). **cannot
  verify from this label**
