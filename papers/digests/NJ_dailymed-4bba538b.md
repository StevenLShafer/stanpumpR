---
file: NJ_dailymed-4bba538b.pdf
pmid: null
doi: null
citation: "SOLIQUA 100/33 (insulin glargine and lixisenatide) injection, for subcutaneous use. US prescribing information, Sanofi-Aventis U.S. LLC. BLA208673. DailyMed setid 4bba538b-cf7c-4310-ae8f-cb711ed21bcc; revised 3/2026; downloaded 2026-10-21."
kind: label
drugs: [insulinGlargine, lixisenatide]
source_read: "text layer, 49 PDF pages (form-feed chunks = pages). Read: Highlights (p1-2), contents (p2-3), 1-2 Indications, Dosage and Administration incl. Tables 1-2 (p4-7), 8 Use in Specific Populations (p16-19), 10 Overdosage (p19-20), 11 Description (p20), 12.1-12.3 and 12.6 Clinical Pharmacology (p20-24), 14.1-14.2 opening (p25), 16 How Supplied (p29-30), SPL product data (p49). Table 1 (dose table) is clean in the text layer; no page rendered. Medication Guide and Instructions for Use not read."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# SOLIQUA 100/33 (insulin glargine + lixisenatide) US label, 3/2026

## Study
This is the current US prescribing information for the fixed-ratio pen of insulin glargine 100 units/mL and
lixisenatide 33 mcg/mL (Sanofi; initial US approval 2016, BLA208673). PDF page 1 is the Soliqua label, so there is
no source file problem.

**Dosage (section 2, p4-7):**
- SC once daily, within the hour before the first meal. The pen delivers 15-60 units.
- The maximum is 60 units, which is 60 units of glargine plus 20 mcg of lixisenatide.
- Starting dose:
  - 15 units (15 U + 5 mcg) if naive to basal insulin or GLP-1 RA, on a GLP-1 RA, or on less than 30 U of basal
    insulin.
  - 30 units (30 U + 10 mcg) if on 30-60 U of basal insulin.
- Titrate by ±2-4 units weekly on fasting glucose (Table 2: 2 U ≙ 0.66 mcg, 4 U ≙ 1.32 mcg).
- Not IV and not by insulin pump (2.5). Do not mix with other insulins.
- **Table 1** gives n units = n U glargine + n/3 mcg lixisenatide, from 15 (5 mcg) to 60 (20 mcg).

## Model and parameters
Section 12.3 (p21-22) is descriptive and gives no population model.
```yaml
parameters:
  - {name: lixisenatide tmax (combination), value: "2.5-3.0", unit: h, source: "12.3, p21", note: "median range"}
  - {name: lixisenatide Cmax vs separate co-administration, value: "-22 to -34", unit: "%", source: "12.3, p21", note: "lower in the combination; AUC comparable"}
  - {name: lixisenatide t1/2 terminal, value: 3, unit: h, source: "12.3, p22", note: "approximately, multiple dose, T2D"}
  - {name: lixisenatide CL/F, value: 35, unit: L/h, source: "12.3, p22", note: "about; multiple dose, T2D"}
  - {name: lixisenatide protein binding, value: 55, unit: "%", source: "12.3, p21"}
  - {name: glargine exposure vs glargine alone, value: "86-101", unit: "%", source: "12.3, p21", note: "no pronounced peak"}
  - {name: "lixisenatide Cmax increase, mild/moderate/severe renal impairment", value: "60 / 42 / 83", unit: "%", source: "12.3, p22", note: "vs healthy N=4; AUC +34 / +69 / +124%"}
```

Other points from 12.3:
- **Glargine** forms the active metabolites M1 (21A-Gly-insulin) and M2 in the SC depot.
- **Lixisenatide** is eliminated by glomerular filtration and proteolysis. Age, weight, sex and race had no
  meaningful effect on its PK in population analyses.
- **Gastric-emptying interactions:**
  - Acetaminophen Cmax fell 29-31% and tmax was delayed 1.75-2 h when given 1 or 4 h after lixisenatide.
  - Oral contraceptive Cmax fell 20-52%.
  - Atorvastatin, warfarin, digoxin and ramipril showed Cmax falls or tmax delays with AUC mostly unchanged.
  - The advice is to take oral drugs 1 h before or 11 h after a dose.
- **Molecular weights:** glargine 6.063 kDa, lixisenatide 4.8585 kDa (11).

## Pharmacodynamics and end points
- Lixisenatide reduced fasting glucose by 33.8 mg/dL against placebo, postprandial glucose AUC0-300min by
  387 mg·h/dL, and postprandial glucagon AUC by 15.6 h·pmol/L at 20 mcg (12.2).
- The combination does not change glargine's PD. No QTc effect at 1.5 times the dose.
- No therapeutic concentration range is given.
- **Antibodies (12.6):** 21-26% anti-glargine and about 43% anti-lixisenatide after 30 weeks. Glycaemic response
  was attenuated in the 2.4% with the highest titres.

## Covariates
- Renal: lixisenatide exposure rises with renal impairment (above). The label advises against use in ESRD
  (eGFR < 15).
- Hepatic impairment not studied.
- Geriatric: no overall differences, but dose conservatively.
- Pediatric: not established.

## Caveats
- Descriptive label PK only. It gives no ka, V or covariate equations for either component.
- Lixisenatide Cmax in the combination is lower than with lixisenatide given alone, so an Adlyxin model would
  overstate the peak from Soliqua.

## Used by stanpumpR
- `inst/extdata/antidiabeticRegistry.csv:47` (line confirmed at 8baa733): key `soliqua_100_33`, SC, PK-CONSTRAINT,
  PD-TEMPLATE, `pd_endpoint` "none", `csii` FALSE, no stanpumpR drug, `structure_verified` FALSE.
  - Notes: "n steps = n U glargine + 0.33n mcg lixisenatide; components not both implemented; no pump."
  - Generated by `data-raw/antidiabeticRegistry.R:202-205`.

## Verification against stanpumpR
- **"n steps = n U glargine + 0.33n mcg lixisenatide":** Table 1 gives n/3 mcg (for example 30 units = 10 mcg,
  60 units = 20 mcg). 0.33n is the label's own rounding, as in Table 2's 0.66 mcg per 2 units, and is off by at
  most 0.2 mcg at 60 units. **matches**
- **"No pump" (`csii` FALSE):** 2.5 says not via an insulin pump. **matches**
- **SC, no PD endpoint, PK only as a constraint:** consistent with a label that gives no model. **matches**
- **"Components not both implemented":** imprecise repo-state wording, not a conflict with the label.
  - At 8baa733 both `R/drugs_insulinGlargine.R` and `R/drugs_lixisenatide.R` exist.
  - But the glargine model is the Basaglar fit. The Lantus-type registry row (`antidiabeticRegistry.csv:38`) says
    "product equivalence mapping required".
  - The label's 22-34% lower lixisenatide Cmax in the combination argues against reusing the Adlyxin model as is.
- **Side note, not a cited location:** the implemented lixisenatide CL/F (38.6 L/h at the median CLCR,
  `R/drugs_lixisenatide.R:101`) is close to the label's "about 35 L/h".
