---
file: NJ_dailymed-3822068f.pdf
pmid: none
doi: none
citation: "TERAZOSIN capsules USP (terazosin hydrochloride) 5 mg, Bryant Ranch Prepack repackaging of Jubilant Cadista ANDA075317 (marketing start 12/20/2004); prescribing information, patient leaflet Revised 08/12, SPL Revised 11/2025, DailyMed SPL setid 3822068f-f160-4493-a207-4d75dad6af73, archived 2026-10-21. https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=3822068f-f160-4493-a207-4d75dad6af73"
kind: label
drugs: [terazosin]
source_read: "text layer, all 22 pages (one form-feed chunk per page). Read: Description p.1; Clinical Pharmacology (pharmacodynamics BPH and hypertension p.1-6, Pharmacokinetics p.6); Indications p.6; Warnings p.6-7; Precautions incl. Pregnancy, Nursing, Pediatric p.8-11; Adverse Reactions p.11-15 (tables scanned); Overdosage p.16; Dosage and Administration p.16-17; How Supplied p.17; patient leaflet p.17-20; SPL product data p.21-22. Old-format label: no numbered sections and no Geriatric Use subsection. Page image: p.1 (identity)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# Terazosin capsules (Bryant Ranch / Jubilant Cadista) prescribing information, DailyMed

## Study
Not a study: a generic terazosin hydrochloride capsule label (5 mg capsules, 1000-count bottle, NDC 72162-2183-0),
old-format text describing 1, 2, 5 and 10 mg capsules; indications BPH and hypertension. The file is what its
name says (image p.1). It is a capsule label, not a tablet label.

## Model and parameters
No model. Pharmacokinetics (p.6):
- Capsules essentially completely absorbed; minimal hepatic first-pass; nearly all circulating drug is parent.
- Peak plasma about 1 h after dosing; taken immediately after meals, extent minimally changed but time to peak
  delayed by about 40 min.
- Half-life approximately 12 h; mean 14.0 h at >= 70 years vs 11.4 h at 20-39 years; oral plasma clearance 31.7%
  lower at >= 70 years.
- Protein binding 90-94%, constant over the clinical range.
- About 10% of an oral dose excreted unchanged in urine and about 20% in faeces; overall about 40% in urine and
  60% in faeces. Renal impairment has no significant effect; hemodialysis removes about 10%.
- Molecular weight of terazosin hydrochloride 423.93 (p.1).
Dosing (p.16-17): always start 1 mg at bedtime. BPH: step up to 2, 5 or 10 mg once daily, 10 mg usually needed,
20 mg limited data. Hypertension: usual 1-5 mg once daily, up to 20 mg/day; doses over 20 mg gave no further
effect, over 40 mg not studied; 12- or 24-h interval by response.

```yaml
parameters:
  - {name: F_capsule, value: "essentially complete", unit: fraction, source: "Pharmacokinetics p.6", note: "no number given"}
  - {name: tmax, value: 1, unit: h, source: "Pharmacokinetics p.6", note: "about; +40 min after meals"}
  - {name: t_half, value: 12, unit: h, source: "Pharmacokinetics p.6", note: "approximately"}
  - {name: t_half_age_ge70, value: 14.0, unit: h, source: "Pharmacokinetics p.6", note: mean}
  - {name: t_half_age_20_39, value: 11.4, unit: h, source: "Pharmacokinetics p.6", note: mean}
  - {name: protein_binding, value: "0.90-0.94", unit: fraction, source: "Pharmacokinetics p.6"}
  - {name: MW_terazosin_HCl, value: 423.93, unit: g/mol, source: "Description p.1"}
```

## Pharmacodynamics and end points
Hypertension (p.4-5): blood pressure falls gradually within 15 min of an oral dose. At the end of the dosing
interval (usually 24 h), supine responses 5-10 mmHg systolic and 3.5-8 mmHg diastolic greater than placebo.
Peak response at 2-3 h during chronic dosing is more than about twice the trough; no dose-response over 5-20 mg.
In the first 3 h after dosing 12.5% had a large orthostatic fall vs 4% on placebo. Normotensive BPH men: SBP/DBP
-3.3/-2.2 vs placebo -0.1/+0.4 mmHg; hypertensive (DBP > 90) -14.4/-15.1 vs -5.8/-7.1 (p.4). First-dose
syncope: 3 of 14 subjects at 2.5-7.5 mg single doses, all events within 90 min (p.7). No therapeutic plasma
range. Not an opioid.

## Covariates
Age (half-life and clearance); renal impairment has no significant effect; food delays tmax.

## Caveats
Descriptive values with no n, assay or study design; the patient leaflet is dated 08/12. The label gives no
numerical absolute bioavailability.

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:119: catalog record `terazosin__ir_tablet__oral__catalog__none__label`,
  formulation "immediate-release tablet", analyte terazosin, plasma, source_type official_label, descriptive only;
  limitations "Label nearly complete absorption, Tmax about 1 h, half-life about 12 h", plus a remark on the
  fitted healthy-volunteer SBP/DBP records (rows 120-121). Built at data-raw/antihypertensiveRegistry.R:380-384.

## Verification against stanpumpR
- **Nearly complete absorption**: "essentially completely absorbed" (p.6). **matches**.
- **Tmax about 1 h**: p.6. **matches**.
- **Half-life about 12 h**: p.6. **matches**.
- **Formulation "immediate-release tablet"**: this SPL is for capsules. The label states the PK for capsules.
  **discrepancy** (formulation label in the record id and formulation field).
- **Healthy-volunteer single 2 mg dose SBP/DBP records, PK vector not reported**: not from this label.
  **cannot verify** here.
