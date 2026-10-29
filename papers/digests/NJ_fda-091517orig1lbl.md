---
file: NJ_fda-091517orig1lbl.pdf
pmid: null
doi: null
citation: "Repaglinide Tablets USP, 0.5 mg, 1 mg and 2 mg (generic; distributed by Breckenridge Pharmaceutical, manufactured by Standard Chem. & Pharm. Co.). Prescribing information and container labels, issued 03/2015. FDA Drugs@FDA label file 091517Orig1lbl.pdf (application 091517, an ANDA by its number; the label does not print it)."
kind: label
drugs: [repaglinide]   # registry-only; not a stanpumpR library drug at 8baa733
source_read: "text layer (pdftotext -raw re-extraction for reading order) plus page images of all 5 PDF pages; PK table on PDF p4 read from a zoomed render"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# Repaglinide Tablets USP (Breckenridge generic) label, 2015

## Study
Old-format (unnumbered) US prescribing information for a **generic** repaglinide tablet, not Novo Nordisk's
Prandin. PDF p1-3 are bottle labels (0.5, 1 and 2 mg; NDC 51991-853/854/855-01, Breckenridge, Standard Chem.,
Taiwan). PDF p4-5 are the two sides of the package insert. Sections read: Description, Clinical Pharmacology
(Mechanism, Pharmacokinetics, Special Populations, Drug-Drug Interactions, Clinical Trials) on p4; Indications,
Contraindications and Precautions starting on p4; Precautions (drug interactions, Pregnancy, Nursing, Pediatric,
Geriatric), Adverse Reactions, Overdosage, Dosage and Administration and How Supplied on p5. "Issued: 03/2015" (p5).

Repaglinide is a meglitinide insulin secretagogue (K-ATP channel closure in beta cells), MW 452.6.

## Model and parameters
No compartmental model. PK table (PDF p4, column 1, rendered and zoomed):

- Healthy subjects, IV: CL 38 ± 16 L/hr; Vss 31 ± 12 L; absolute oral bioavailability 56 ± 9% (mean ± SD).
- Patients with type 2 diabetes, multiple-dose parallel dose-proportionality study, dosed preprandially with three
  meals: AUC0-24 hr 68.9 ± 154.4 (0.5 mg), 125.8 ± 129.8 (1 mg), 152.4 ± 89.6 (2 mg), 447.4 ± 211.3 ng/mL*hr (4 mg);
  Cmax0-5 hr 9.8 ± 10.2, 18.3 ± 9.1, 26.0 ± 13.0, 65.8 ± 30.1 ng/mL; Tmax0-5 hr means 1.0-1.4 (SD 0.3-0.5) hr;
  t1/2 means 1.0-1.4 hr (individual range 0.4-8.0 hr).
- Prose (p4): Tmax within 1 hour; half-life about 1 hour; protein binding > 98%; food lowers mean Cmax 20% and AUC
  12.4% with unchanged Tmax; oral clearance unchanged over 0.5-4 mg (linear); no accumulation.
- Variability: intra- and inter-individual CV of AUC 36% and 69%; therapeutic-range AUC 69-1005 ng/mL*hr, up to
  5417 ng/mL*hr in dose escalation without apparent adverse consequence.
- Elimination: complete metabolism (CYP2C8 and 3A4 to M2, then M1; acyl glucuronide M7), OATP1B1 uptake substrate;
  after 14C dose about 90% faeces, 8% urine, 0.1% of dose urinary parent; metabolites inactive.

```yaml
parameters:
  - {name: CL, value: 38, unit: L/h, source: "PDF p4 PK table", note: "mean ± SD 16; IV, healthy subjects"}
  - {name: Vss, value: 31, unit: L, source: "PDF p4 PK table", note: "mean ± SD 12; IV, healthy subjects"}
  - {name: F, value: 0.56, unit: fraction, source: "PDF p4 PK table", note: "absolute bioavailability, mean ± SD 9%"}
  - {name: t_half, value: "1.0-1.4", unit: h, source: "PDF p4 PK table", note: "range of dose-group means, T2D, oral; individual 0.4-8.0 h"}
  - {name: Tmax, value: "1.0-1.4", unit: h, source: "PDF p4 PK table", note: "range of dose-group means, T2D, oral"}
  - {name: protein_binding, value: ">0.98", unit: fraction, source: "PDF p4 Distribution", note: "plasma and albumin"}
```
(derived: CL/Vss gives 31/38 x ln 2 = 0.57 h, shorter than the oral 1.0-1.4 h; Vss-based, not a terminal value,
and the label gives no IV half-life, so this is no contradiction.)

## Pharmacodynamics and end points
No concentration-effect model. Glucose-lowering effect is dose-proportional over 0.25-4 mg with meals (4-week
trial, n = 138). Efficacy end points are FPG, PPG and HbA1c (p4 tables: e.g. 3-month titration, placebo n = 33 vs
repaglinide n = 66, HbA1c change +1.1 vs -0.6%). Hypoglycaemia 16% of 1228 repaglinide patients vs 20% glyburide
and 19% glipizide in 1-year trials. No therapeutic or toxic concentration range is stated.

## Covariates
Females: AUC 15-70% higher (T2D, 0.5-4 mg), no dose change. Age ≥ 65: no significant PK difference. Renal: AUC and
Cmax similar for CrCl > 80 and 40-80 mL/min (56.7 vs 57.2 ng/mL*hr; 37.5 vs 37.7 ng/mL), higher for CrCl 20-40
(98.0 ng/mL*hr, 50.7 ng/mL); not studied below 20 mL/min. Hepatic (12 healthy vs 12 chronic liver disease): AUC
91.6 vs 368.9 ng/mL*hr, Cmax 46.7 vs 105.4 ng/mL. Interactions (p4): gemfibrozil 8.1-fold AUC, t1/2 1.3 to 3.7 h;
gemfibrozil + itraconazole 19-fold AUC, t1/2 6.1 h; itraconazole 1.4-fold; cyclosporine AUC 2.5-fold, Cmax 1.8-fold;
clarithromycin AUC +40%; trimethoprim AUC +61%; deferasirox AUC 2.3-fold; rifampin decreases (32-80% depending on
timing). Gemfibrozil co-administration is contraindicated.

## Dosing (Dosage and Administration, p5)
Within 15 min (0-30 min) before each meal, 2-4 times daily. Start 0.5 mg per meal if untreated or HbA1c < 8%;
1 or 2 mg if previously treated and HbA1c ≥ 8%. Double up to 4 mg per meal at intervals of at least a week;
range 0.5-4 mg; maximum 16 mg/day. Severe renal impairment: start 0.5 mg. Overdose: up to 80 mg/day for 14 days
gave few effects beyond hypoglycaemia.

## Caveats
A generic label rather than the reference product's (Prandin, Novo Nordisk); a generic label normally carries the
reference product's clinical pharmacology text, but that was not checked against a Prandin label here. The 03/2015 file carries FDA's "may not be the latest" banner. Means ± SD only;
no population PK, no IV time course, no absorption rate constant. The 0.5 mg AUC SD (154.4) exceeds its mean
(68.9), consistent with the wide variability the label states.

## Used by stanpumpR
- `inst/extdata/antidiabeticRegistry.csv:70` (built by `data-raw/antidiabeticRegistry.R:275-277`): registry row
  `repaglinide`, product "Prandin", PO, tier core, US; `spec_pk_status` and `pk_status` PK-CONSTRAINT, PD-TEMPLATE,
  pd_endpoint "meal-linked secretion", `stanpumpr_drug` NA, `structure_verified` FALSE, source_url this file, note
  "IV Vss and CL only."

## Verification against stanpumpR
- **Product identity: discrepancy (minor).** The row names "Prandin", but the linked document is the Breckenridge /
  Standard Chem. generic "Repaglinide Tablets, USP" (container labels PDF p1-3; distributor p5). The registry uses
  only the IV Vss and CL, which a generic label would normally copy from the reference product, but the source as
  linked is not a Prandin label.
- **PK content, "IV Vss and CL only": discrepancy (minor, understated).** Vss 31 L and CL 38 L/h are confirmed
  (PDF p4 table, healthy IV). The label also gives absolute bioavailability 56 ± 9%, Tmax about 1 h and a mean
  half-life of 1.0-1.4 h with dose-group AUC/Cmax, so the note understates what the source offers for a constraint.
  It is still true that there is no fitted model or IV time course, which supports PK-CONSTRAINT and leaving the
  drug unimplemented.
- **Route PO, tier, unimplemented status, PD-TEMPLATE with meal-linked secretion: matches** (oral tablet taken
  with meals; insulin release glucose-dependent; no PD model in the label).
