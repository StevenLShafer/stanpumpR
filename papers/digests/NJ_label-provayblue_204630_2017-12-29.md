---
file: NJ_label-provayblue_204630_2017-12-29.pdf
pmid: none
doi: none
citation: "PROVAYBLUE (methylene blue) injection USP, for intravenous use, US prescribing information, Provepharm (distributed by American Regent); NDA 204630 (s005), revised 12/2017 (FDA Reference ID 4201611; posted 2017-12-29). https://www.accessdata.fda.gov/drugsatfda_docs/label/2017/204630s005lbl.pdf"
kind: label
drugs: [methylene blue]
source_read: "text layer, all 9 pages (label 'Page n' = PDF page n): Highlights and contents (p1), 2 Dosage and Administration (p2), 8 Use in Specific Populations (pp. 6-7), 10-12 Overdosage, Description, Clinical Pharmacology 12.1-12.3 (pp. 7-8); PDF p7 (12.3) rendered and read"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master ca8ce42
verification: matches
---

# ProvayBlue label 2017: methylene blue injection

## Study
Regulatory label (PLR format), revised 12/2017; accelerated approval for acquired methaemoglobinaemia in adults
and children. Product: 50 mg/10 mL (5 mg/mL) single-dose ampule; methylene blue (methylthioninium chloride)
MW 319.86 g/mol (Description, p7). The PK numbers come from a 2 mg/kg dose given as a 5-minute infusion (the
same dose and rate as the thorough QT study in 12.2); the label does not give the study's population, n, body
weight, sampling duration, matrix of the Cmax/AUC or assay.

## Model and parameters
No compartmental model. Section 12.3 (PDF p7, rendered), all after 2 mg/kg IV over 5 min:

```yaml
parameters:
  - {name: Cmax, value: 2917, unit: ng/mL, source: "12.3, PDF p7", note: "mean, CV 39%"}
  - {name: AUC, value: 13977, unit: "ng h/mL", source: "12.3, PDF p7", note: "mean, CV 21%; AUC type (0-t or 0-inf) not stated"}
  - {name: Vss, value: 255, unit: L, source: "12.3 Distribution, PDF p7", note: "mean +/- SD 58; absolute litres, no weight given"}
  - {name: t_half, value: "approximately 24", unit: h, source: "12.3 Elimination, PDF p7"}
  - {name: protein_binding, value: "approximately 94", unit: "%", source: "12.3 Distribution, PDF p7", note: "in vitro"}
  - {name: blood_to_plasma, value: "5.1 +/- 2.8 at 5 min; plateau 0.6 at 4 h", source: "12.3 Distribution, PDF p7", note: "concentration-dependent partitioning into blood cells"}
  - {name: urine_unchanged, value: "approximately 40", unit: "%", source: "12.3 Excretion, PDF p8; 8.6, PDF p6"}
  - {name: CL_derived, value: 10.0, unit: L/h, source: "derived: 2 mg/kg x 70 kg / 13.977 mg h/L", note: "assumes 70 kg and AUC to infinity; not printed"}
```

Metabolism (p8): CYP1A2, 2C19, 2D6 in vitro, mainly UGT conjugation (UGT1A4, 1A9); azure B formed, parent to
metabolite AUC ratio above 6:1, 8-fold less potent. P-gp substrate. Dosing (2.1, p2): 1 mg/kg IV over 5-30 min;
repeat up to 1 mg/kg after 1 h if methaemoglobin stays above 30% or symptoms persist; dilute in 50 mL D5W, not
saline.

## Pharmacodynamics and end points
Exposure-response unknown (12.2, p7). Thorough QT study at 2 mg/kg over 5 min: no effect on QT, PR or QRS.
Toxicity: hypotension, wheezing above 3 mg/kg; cumulative 7 mg/kg or more causes nausea, chest pain, mild
methaemoglobinaemia; 20 mg/kg or more severe haemolysis and death (10 Overdosage, p7). Serotonin syndrome with
serotonergic drugs (boxed warning). Not an opioid.

## Covariates
Renal impairment: about 40% excreted renally, monitor for an extended period (8.6, p6); hepatic impairment:
monitor longer (8.7). No exposures in impairment, no pregnancy PK (8.1 is developmental toxicity only).

## Caveats
Four disposition numbers only (Cmax, AUC, Vss, half-life); no population, weight, assay or sampling schedule; AUC
type not stated. Matrix inferred as plasma from the protein-binding and blood-to-plasma statements, not stated
for Cmax/AUC.

## Used by stanpumpR
- inst/extdata/vasoactiveRegistry.csv:43 (same line at ca8ce42; data-raw/vasoactiveRegistry.R:352-358) - row
  methylene_blue__label__provayblue2017, readiness needs_full_text: the 12.3 numbers, clearance about 10 L/h at
  70 kg, and "Four numbers cannot fix the six parameters of the three compartments".
- vasoactiveRegistry.csv:42 (Peter 2000 row) - "the ProvayBlue label's clearance (about 10 L/h) sides with
  Walter-Sack".
- AGENTS.md:90 - "methylene blue's ProvayBlue label is read and its four numbers cannot fix three compartments".

## Verification against stanpumpR
- Registry :43 numbers - Cmax 2917 ng/mL (CV 39%), AUC 13 977 ng h/mL (CV 21%), 2 mg/kg over 5 min, Vss 255 +/- 58
  L, half-life about 24 h, binding about 94%, blood-to-plasma 5.1 at 5 min to 0.6 at 4 h, about 40% unchanged in
  urine: **matches** 12.3 (PDF p7-8).
- "At 70 kg the clearance is about 10 L/h (140 mg / 13.977 mg h/L)": arithmetic **matches** (10.0 L/h, derived);
  70 kg is the repo's assumption, the label gives no weight, and it treats the AUC as AUC to infinity.
- "Four numbers cannot fix the six parameters of the three compartments": **matches**. The label's disposition
  information is exactly Cmax, AUC, Vss and one half-life; a three-compartment model has six parameters (V1, V2,
  V3, CL, Q2, Q3), so any split of Vss is a choice. (Binding, blood-to-plasma ratio and urinary fraction do not
  constrain the plasma disposition.) Note the four numbers could roughly fix a TWO-compartment model (CL from AUC,
  V1 from Cmax, Vss, terminal half-life); the repo's point rests on the curve needing three, from Peter 2000.
- "This label revision gives no renal-impairment exposures (only 'monitor longer')": **matches** 8.6 (p6).
- AGENTS.md:90: **matches**.
