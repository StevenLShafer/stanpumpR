---
file: NJ_label-trandate_018716_2024-04-26.pdf
pmid: null
doi: null
citation: "FDA label. Labetalol hydrochloride tablets, for oral use (NDA 018716, Trandate). Alvogen, Inc. PLR label revised 4/2024 (FDA action 2024-04-26; Reference ID 5370606). https://www.accessdata.fda.gov/drugsatfda_docs/label/2024/018716s036lbl.pdf"
kind: label
drugs: [labetalol]
source_read: "text layer, whole label (11 pages: Highlights, sections 1-17 incl. 2 Dosage and Administration, 8 Use in Specific Populations, 12.1-12.3 Clinical Pharmacology); page images rendered and read for PDF pages 1, 5 and 9"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: matches
---

# Trandate (labetalol tablets) label 2024 (NDA 018716)

## Study
Not a study: US prescribing information for oral labetalol hydrochloride tablets 100, 200 and 300 mg under NDA
018716, distributed by Alvogen. The PDF itself names the product only as "Labetalol Hydrochloride Tablets"; the
brand name TRANDATE does not appear in its text (PDF p1, page image). Revised 4/2024 (Highlights, PDF p1). Oral
only; the label gives no IV labetalol dosing. Adverse-reaction data: placebo-controlled trials of 3-4 months,
labetalol n = 227, placebo n = 98 (Table 1, PDF p5).

## Model and parameters
No compartmental model or clearance/volume values. Section 12.3 (PDF p9-10, p9 confirmed on page image):
- tmax 1 to 2 h after oral dosing.
- Absolute oral bioavailability 25% vs IV infusion (first-pass); tablets vs oral solution 100%; increased with
  food; increased in hepatic impairment and by cimetidine (no numbers).
- Linear peak levels over oral doses 100 to 3,000 mg.
- Protein binding about 50%.
- Plasma half-life after oral dosing about 6 to 8 h; steady state by about day 3.
- Mainly glucuronide conjugation; 55-60% of a dose in urine (conjugates or unchanged) in 24 h; dialysis removes
  <1%.
- Half-life not altered in renal or hepatic impairment (PDF p10); elimination reduced in elderly (8.5, PDF p7).

```yaml
parameters:
  - {name: tmax_oral, value: "1 to 2", unit: h, source: "12.3 PDF p9"}
  - {name: F_oral_absolute, value: 25, unit: "%", source: "12.3 PDF p9", note: "vs IV infusion"}
  - {name: protein_binding, value: 50, unit: "%", source: "12.3 PDF p9", note: approximately}
  - {name: t_half_oral, value: "6 to 8", unit: h, source: "12.3 PDF p9", note: "about"}
  - {name: urinary_recovery_24h, value: "55 to 60", unit: "% of dose", source: "12.3 PDF p9", note: "conjugates or unchanged"}
  - {name: alpha_beta_ratio_oral, value: "1:3", unit: ratio, source: "12.1 PDF p8", note: "alpha:beta blockade, estimated"}
  - {name: alpha_beta_ratio_iv, value: "1:7", unit: ratio, source: "12.1 PDF p8"}
  - {name: MW_HCl, value: 364.9, unit: g/mol, source: "11 PDF p8"}
```

## Pharmacodynamics and end points
- **No therapeutic or toxic plasma concentration range is stated.** Dosing is titrated to blood pressure.
- Antihypertensive effect, and the reduction in exercise tachycardia at 2 h, correlate linearly with log plasma
  concentration (12.2, PDF p9); no slope or EC50 given.
- Peak effect of single oral doses within 2 to 4 h; duration at least 8 h after 100 mg and more than 12 h after
  300 mg; steady-state BP response with twice-daily dosing within 24 to 72 h (PDF p9).
- About 70% of maximal beta-blocking effect present 5 h after 400 mg orally, about 40% at 8 h (PDF p9).
- Postural hypotension 2%, most likely 2-4 h after a dose (PDF p9).
- Dosing (2.1, PDF p2): start 100 mg twice daily, increase by 100 mg twice daily every 2-3 days, maintenance
  200-400 mg twice daily. Highlights (PDF p1) also give severe hypertension 1,200-2,400 mg/day and elderly
  100-200 mg twice daily, citing sections 2.2 and 2.3 that are absent from the full prescribing information.
- Halothane 3% or above should not be combined with IV labetalol for controlled hypotension (7.3, PDF p5).
- Not an opioid; no MEAC, ke0 or potency ratio.

## Covariates
Qualitative: elderly (reduced elimination, lower maintenance doses), hepatic impairment (higher bioavailability),
food, cimetidine.

## Caveats
- Internal inconsistencies in this PDF: Highlights cite sections 2.2 and 2.3 not present in the full PI; section 6
  refers to a Table 2 of dose-related adverse reactions that is not in the document (PDF p5); end matter reads
  "Revised: XX/XXXX" (PDF p11) while the Highlights say 4/2024.
- Oral-tablet label; IV labetalol PK (the perioperative use) is not covered.

## Used by stanpumpR
- `inst/extdata/therapeuticRanges.csv:189` — registry source for the labetalol row (no therapeutic range;
  titrated to effect).

## Verification against stanpumpR
- `inst/extdata/therapeuticRanges.csv:189`: labetalol, plasma, lower/upper/typical 0, ng/mL, band_status
  `none_established`; source text "no therapeutic concentration range; titrated to effect (label read: TRANDATE
  tablets, rev. 04/2024)". Label: no concentration range anywhere; dosing titrated on blood pressure (2.1);
  revised 4/2024 (PDF p1, page image). Matches. Minor: the PDF does not print the brand name TRANDATE; it is the
  NDA 018716 label, so the attribution is to the right application. Verdict: **matches**.
