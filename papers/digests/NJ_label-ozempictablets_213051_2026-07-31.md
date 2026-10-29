---
file: NJ_label-ozempictablets_213051_2026-07-31.pdf
pmid: none
doi: none
citation: "RYBELSUS (semaglutide) tablets and OZEMPIC (semaglutide) tablets, for oral use, joint US prescribing information, NDA 213051 (Novo Nordisk), revised 1/2026, Reference ID 5737308. Drugs@FDA posting https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/213051Orig1s030lbl.pdf (archive date 2026-07-31)."
kind: label
drugs: [semaglutide, semaglutideOral]
source_read: "text layer, 38 pages: Highlights (p1), contents (p2), 1-2 Indications and Dosage and Administration incl. 2.3 Table 1 and 2.4 (p3-4), 3 Dosage Forms, 8 Use in Specific Populations (p10-12), 10-11 Overdosage and Description (p12-13), 12.1-12.3 and 12.6 Clinical Pharmacology (p13-15), 14.1 (p16); Medication Guide and carton art (p27-38) skimmed. Pages 4 (2.3-3) and 14 (12.3) confirmed from rendered images"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Rybelsus and Ozempic tablets label, 1/2026: two oral semaglutide formulations

Identity note. The PDF is the joint label of both oral semaglutide products (Rybelsus 3, 7, 14 mg and the new
Ozempic tablets 1.5, 4, 9 mg), Reference ID 5737308, "Revised: 1/2026" (p1, p29). It is byte-identical (same
SHA-256) to the archive's `NJ_label-rybelsus_213051_2026-07-31.pdf`. The 2026-07-31 in the file name is the
archive's date and is not printed in the document. The repository cites the DailyMed set id of the same label
(`NJ_dailymed-27f15fac`), not this Drugs@FDA URL; no repo file names NDA 213051 or this URL.

## Study
Not a study: a prescribing label. Indication (1): glycaemic control in adults with type 2 diabetes, and MACE risk
reduction in those at high CV risk. Efficacy of the Ozempic tablets is established on the Rybelsus trials (14.1).

## Model and parameters
No population model. Dosing and PK facts:
- **2.1**: Rybelsus and Ozempic tablets are "not substitutable on a mg-to-mg basis". One tablet daily, fasting, with
  up to 4 oz (120 mL) of water, then wait at least 30 min before food, drink or other oral drugs; a missed dose is
  skipped.
- **2.2**: Rybelsus 3 mg days 1-30 (not effective for glycaemic control), 7 mg days 31-60, then 7 or 14 mg. Ozempic
  tablets 1.5 mg days 1-30 (not effective), 4 mg days 31-60, then 4 or 9 mg.
- **2.3, Table 1** (p4, rendered): no switching during days 1-30; afterwards Rybelsus 7 mg = Ozempic tablets 4 mg and
  Rybelsus 14 mg = Ozempic tablets 9 mg, starting the other product the next day.
- **2.4**: from Ozempic injection 0.5 mg weekly, one week after the last injection start Rybelsus 7 or 14 mg or
  Ozempic tablets 4 or 9 mg.
- **3 / 11**: Rybelsus tablets oval, with magnesium stearate, microcrystalline cellulose, povidone and SNAC; Ozempic
  tablets round, with magnesium stearate and SNAC only. The SNAC amount per tablet is not stated. Molecular weight
  4113.58 g/mol (11).

```yaml
parameters:
  - {name: Css mean Rybelsus 7 mg, value: 6.7, unit: nmol/L, source: "12.3, p14", note: "estimated mean steady state, T2D"}
  - {name: Css mean Rybelsus 14 mg, value: 14.6, unit: nmol/L, source: "12.3, p14", note: "estimated mean steady state, T2D"}
  - {name: F Rybelsus 3/7/14 mg, value: "0.4-1", unit: "%", source: "12.3, p14", note: "estimated absolute bioavailability"}
  - {name: F Ozempic tablets 1.5/4/9 mg, value: "1-2", unit: "%", source: "12.3, p14", note: "estimated absolute bioavailability"}
  - {name: tmax, value: 1, unit: h, source: "12.3, p14", note: "approximately, both tablets"}
  - {name: time to steady state, value: "4-5", unit: weeks, source: "12.3, p14", note: "daily oral dosing"}
  - {name: V (absolute), value: 8, unit: L, source: "12.3, p15", note: "approximately, T2D; >99% albumin bound"}
  - {name: CL (absolute), value: 0.04, unit: L/h, source: "12.3, p15", note: "approximately, T2D"}
  - {name: t1/2, value: 1, unit: week, source: "12.3, p15; 10", note: "approximately; present ~5 weeks after last dose"}
  - {name: urinary intact, value: 3, unit: "% of absorbed dose", source: "12.3, p15"}
  - {name: molecular weight, value: 4113.58, unit: g/mol, source: "11, p12-13"}
```
12.3 (p14, rendered): steady-state AUC0-24 and Cmax showed no clinically significant differences between Rybelsus
3, 7, 14 mg and Ozempic tablets 1.5, 4, 9 mg respectively, in healthy subjects. Exposure is dose proportional.
Absorption is mainly gastric and higher with 50 mL than 240 mL of water and with a longer post-dose fast (15-120
min tested); 50 vs 120 mL made no clinically significant difference.

## Pharmacodynamics and end points
12.2 describes semaglutide injection 1 mg weekly in T2D (fasting glucose -29 mg/dL (22%), 2-h postprandial -74
mg/dL (36%), 24-h mean -30 mg/dL (22%) vs placebo); no concentration-effect relationship is given. Thorough QT at
4-fold the maximum tablet exposure: no relevant QTc prolongation. Levothyroxine total T4 AUC +33% with semaglutide
tablets.

## Covariates
12.3 Specific Populations: no clinically significant PK differences by age (>= 18), sex, race, ethnicity, body
weight 40-188 kg, upper GI disease, hepatic impairment (Child-Pugh mild to severe) or renal impairment (to ESRD).
8.6 / 8.7: no dose change in renal or hepatic impairment. 8.2: breastfeeding not recommended (SNAC in milk).
8.3: stop at least 2 months before a planned pregnancy.

## Caveats
- Label summary values ("approximately", "estimated"); no study sizes or variability for the PK numbers.
- Bioavailability ranges are given per product, not per strength.
- The CL and V in 12.3 are absolute (IV-referenced) values in T2D.

## Used by stanpumpR
- `inst/help/models/antidiabetics.md:33` (heading "A product is not a molecule") and `:35`: the newer tablets the
  2026 label says are not interchangeable mg for mg with Rybelsus are not modelled.
- Related, not in the context file: `R/drugs_semaglutideOral.R:78-80` and `inst/help/drugs/semaglutideOral.md:29`
  (Ozempic tablets 1.5, 4, 9 mg, "bioequivalent to 3, 7 and 14 mg", not this drug); `antidiabeticRegistry.csv:6`
  (cites the DailyMed copy; see that card).

## Verification against stanpumpR
- **antidiabetics.md:35**, "the newer tablets the 2026 label says are not interchangeable mg for mg with Rybelsus":
  the label (revised 1/2026) says the two are not substitutable on a mg-to-mg basis (Highlights; 2.1). **matches**
- **semaglutideOral.R:78-80 / semaglutideOral.md:29**: 1.5, 4, 9 mg strengths and not mg-for-mg substitutable
  match (3; 2.1). "Bioequivalent to 3, 7 and 14 mg": 12.3 reports no clinically significant differences in
  steady-state AUC and Cmax between those strength pairs in healthy subjects, and Table 1 (2.3) maps 7 to 4 and 14
  to 9 mg; the label does not use the word "bioequivalent". "each with 300 mg SNAC" (line 78) is not stated in this
  label. **matches**, with the 300 mg SNAC unverifiable here.
- The higher absolute bioavailability of the Ozempic tablets (1-2% vs 0.4-1%) supports the registry's "may not
  inherit its F" (`antidiabeticRegistry.csv:6`).
