---
file: NJ_dailymed-b5f4fed2.pdf
pmid: null
doi: null
citation: "LOPRESSOR (metoprolol tartrate) tablets, for oral use, 12.5 mg. US prescribing information, Validus Pharmaceuticals LLC; NDA218698 (marketing start 08/11/2025); Highlights revised 11/2025, SPL revised 1/2026. DailyMed setid b5f4fed2-369c-4808-a682-8a5b8cfdbb4f; downloaded 2026-10-21."
kind: label
drugs: []
source_read: "text layer, all 16 PDF pages (form-feed chunks = pages). Read Highlights (p1), contents (p2), 2 Dosage and Administration and 3-5 (pp3-5), 6 Adverse Reactions (pp5-6), 7 Drug Interactions (p6), 8 Use in Specific Populations (pp6-9), 10 Overdosage and 11 Description (p9), 12.1-12.3 and 12.5 Clinical Pharmacology (pp10-11), 14 Clinical Studies (p12), 16 How Supplied and SPL data (pp13-16). Rendered page 1 to confirm identity. No PK table in the label."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# LOPRESSOR (metoprolol tartrate) 12.5 mg tablets, Validus, 2025

## Study
This is the current US label for LOPRESSOR 12.5 mg film-coated tablets (Validus, NDA218698, marketed from
08/2025). Page 1 is that product, so there is no source file problem.

**This label is myocardial infarction only.** Its single indication (1.1) is to reduce cardiovascular mortality in
haemodynamically stable adults after MI. It carries no hypertension or angina indication, although it is linked
from the antihypertensive registry.

**Dosage (2.1, p3):**
- Start at 50 mg orally every 6 h, or 25 mg every 6 h for 48 h if not tolerated.
- Titrate to heart rate and blood pressure, preferably with or after meals.
- The maximum maintenance dose is 100 mg twice daily.

**Strength basis (11, p9):** 12.5 mg of metoprolol tartrate is equivalent to 9.76 mg of metoprolol. The tartrate
is a 2:1 salt, MW 684.82.

## Model and parameters
Section 12.3 (pp10-11) gives descriptive values only, with no compartmental model and no CL.
```yaml
parameters:
  - {name: F (immediate release), value: 50, unit: "%", source: "12.3, p10", note: "'about 50%'; presystemic metabolism is saturable, so exposure rises more than dose-proportionally"}
  - {name: V, value: "3.2-5.6", unit: L/kg, source: "12.3, p10", note: "'reported' range"}
  - {name: albumin binding, value: 10, unit: "%", source: "12.3, p10", note: "about 10%"}
  - {name: elimination t1/2, value: "3-4", unit: h, source: "12.3, p11", note: "mean; 7-9 h in CYP2D6 poor metabolizers"}
  - {name: urinary recovery, value: 95, unit: "% of dose", source: "12.3, p11", note: "<5% of an oral dose unchanged in extensive metabolizers"}
  - {name: CYP2D6 poor metabolizer prevalence, value: "8 (Caucasian), 2 (most others)", unit: "%", source: "12.3, 12.5, p11"}
  - {name: "beta1 effect 30-80% of max", value: "30-540", unit: nmol/L, source: "12.2, p10", note: "plasma metoprolol; about 8-23% reduction in exercise heart rate"}
  - {name: beta2 blockade threshold, value: 300, unit: nmol/L, source: "12.2, p10", note: "beta1 selectivity diminishes above this plasma concentration"}
```

## Pharmacodynamics and end points
- **Exercise heart rate (12.2):** the relation between plasma metoprolol and reduction in exercise heart rate does
  not depend on formulation. 30-540 nmol/L corresponds to 30-80% of maximal beta1 effect, about 8-23% reduction.
  Beta2 blockade increases above 300 nmol/L.
- **Other effects:** heart rate, cardiac output, exercise systolic pressure, isoproterenol tachycardia and reflex
  orthostatic tachycardia are reduced.
- **Resting pressure:** the label gives no resting blood-pressure concentration-effect relation.
- **CYP2D6 interactions (7.3, 12.3):**
  - Strong inhibitors (quinidine, fluoxetine, paroxetine, propafenone) double concentrations.
  - Quinidine 100 mg tripled S-metoprolol and doubled the half-life.
  - Propafenone raised steady state 2- to 5-fold.

## Covariates
- **CYP2D6 phenotype:** poor metabolizers have several-fold higher concentrations.
- **Hepatic impairment:** concentrations are likely to rise substantially; start low (8.6).
- **Renal failure:** no clinically significant change in availability or half-life, and no dose change (8.7).
- **Geriatric:** slightly higher concentrations, not clinically relevant.

## Caveats
- The half-life and F are summary statements with no study or n.
- The label says the drug is a racemate with stereoselective, phenotype-dependent metabolism, but gives no
  enantiomer-specific numbers.
- Excretion (12.3, p11) contains a garbled sentence: "less than 5% of an oral dose and the rest is excreted...".
  The meaning, under 5% unchanged, is clear from the IV label.

## Used by stanpumpR
`inst/extdata/antihypertensiveRegistry.csv:87` (generated from `data-raw/antihypertensiveRegistry.R:262-266`),
record `metoprolol__tartrate_ir_tablet__oral__catalog__none__label`:
- metoprolol tartrate, R- and S-enantiomers, oral immediate-release tablet, plasma;
- descriptive only, no stanpumpR drug;
- limitations: "CYP2D6- and enantiomer-dependent F about 50%, terminal half-life 3-4 h", with the fitted records
  noted as exercise systolic pressure and exercise heart rate, not resting BP.

## Verification against stanpumpR
- **F about 50%:** 12.3 says about 50%, with saturable presystemic metabolism. **matches**
- **"CYP2D6- and enantiomer-dependent":** the label says CYP2D6 is the main pathway and that oral metoprolol shows
  stereoselective, phenotype-dependent metabolism. It does not state F per enantiomer, so "enantiomer-dependent F"
  is a fair reading rather than a printed figure. **matches**
- **Terminal half-life 3-4 h:** 12.3 gives a mean elimination half-life of 3-4 h. **matches**
- **No resting BP model:** the label's only concentration-effect statement concerns exercise heart rate (12.2).
  That fits the registry's point that available fitted endpoints are exercise measures. **matches**
- **Note:** this label's indication is MI only, not hypertension. The registry row claims no indication, so this
  is not a discrepancy, but the record's label is not an antihypertensive one.
