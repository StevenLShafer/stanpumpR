---
file: NJ_fda-215904orig1s000clinpharmr.pdf
pmid: null
doi: null
citation: "FDA CDER Office of Clinical Pharmacology. Integrated Clinical Pharmacology Review, NDA 215904 (ZTALMY, ganaxolone oral suspension 50 mg/mL; Marinus Pharmaceuticals), 505(b)(1) NME, submitted 20 July 2021; reviewers Samala R, Li D, Bewernitz M, Bhattaram A; signed 9-10 March 2022 (Reference ID 4950301). accessdata.fda.gov 215904Orig1s000ClinPharmR.pdf; archived 2026-10-21."
kind: review
drugs: []
source_read: "text layer, all 38 PDF pages (PDF page = printed page + 1; PDF 1 is the CDER cover, PDF 38 the signature page): contents PDF 3-4; 1 Executive Summary PDF 5-9; 2 Summary incl. 2.1 PK and 2.2 dosing PDF 9-12; 3.2 general PK PDF 14-16; 3.3 review questions PDF 17-23; 4.1 bioanalysis PDF 24-25; 4.2 population PK PDF 26-34; 4.3 exposure-response PDF 37. Tables 3, 4 and 5 are images: PDF pages 27, 28 and 33 checked on rendered images; PDF 1 and 2 rendered to confirm identity."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: partly-verified
---

# FDA clinical pharmacology review of ganaxolone (Ztalmy), NDA 215904, 2022

## Study
An FDA review, not a study. Indication: seizures of CDKL5 deficiency disorder (CDD), patients 2 years and older;
pivotal trial 1042-CDD-3001 (median 28-day major motor seizure reduction 30.7% GNX vs 6.9% placebo, p = 0.0036;
Table 1, PDF 17). OCP recommends approval (PDF 5).

Population PK (4.2, PDF 26-34): the applicant's main report pooled 694 samples from 146 subjects (PDF 26) from
1042-DDI-1001 (healthy adults, 32 planned; 200 or 600 mg single doses with itraconazole or rifampin; rich sampling
to 168 h), 1042-0900 (30 children with PCDH19 epilepsy incl. 7 CDD, up to 63 mg/kg or 1800 mg/day) and 1042-CDD-3001
(100 planned, sparse samples at weeks 5, 9, 17) (Table 3, PDF 27). Assay LC-MS/MS, LLOQ 2 ng/mL (QSP 199-1503) or
1 ng/mL (QSP 199-0506) (4.1, PDF 24). The supplemental adult popPK report (study 1042-0600) was not reviewed:
data file and software (Kinetica 5.0) unavailable, and its ka 4620 /h was judged implausible (PDF 26).

## Model and parameters
Two compartments, first-order absorption with a lag, relative bioavailability F, and a maximum absorbed dose;
allometric exponents fixed at 0.75 (CL, Q) and 1 (V2, V3), normalised to 70 kg; exponential IIV; additive and
proportional residual error; weight a covariate on the maximum absorbed dose (PDF 27). The equation for the cap and
for the weight effect is not printed. Final estimates, Table 4 (PDF 28, rendered; from applicant report p35 Table 10):

```yaml
parameters:
  - {name: CL, value: 430, unit: L/h, source: "Table 4, PDF p28", note: "typical, 70 kg; apparent (oral data only); 95% CI 381-479"}
  - {name: V2, value: 823, unit: L, source: "Table 4, PDF p28", note: "95% CI 471-1174"}
  - {name: V3, value: 8854, unit: L, source: "Table 4, PDF p28", note: "95% CI 6756-10950; bootstrap median printed as 228 (see Caveats)"}
  - {name: Q, value: 228, unit: L/h, source: "Table 4, PDF p28", note: "95% CI 194-262; bootstrap median printed as 8847 (see Caveats)"}
  - {name: ka, value: 0.34, unit: 1/h, source: "Table 4, PDF p28", note: "95% CI 0.28-0.40"}
  - {name: tlag, value: 0.18, unit: h, source: "Table 4, PDF p28", note: "95% CI 0.16-0.19"}
  - {name: max_absorbed_dose, value: 266, unit: mg, source: "Table 4, PDF p28", note: "95% CI 208-323"}
  - {name: weight_effect_max_absorbed_dose, value: 0.81, unit: unitless, source: "Table 4, PDF p28", note: "95% CI 0.40-1.2; functional form not printed"}
  - {name: IIV_V2, value: 74, unit: "% (as printed)", source: "Table 4, PDF p28", note: "CI 0-114"}
  - {name: IIV_ka_healthy, value: 8.9, unit: "% (as printed)", source: "Table 4, PDF p28", note: "healthy volunteers only"}
  - {name: IIV_F, value: 75, unit: "% (as printed)", source: "Table 4, PDF p28", note: "CI 60-88"}
  - {name: sigma_prop, value: 0.35, unit: fraction, source: "Table 4, PDF p28", note: "additive 0.01 ng/mL fixed"}
  - {name: t_half_terminal, value: 34, unit: h, source: "2.1 and 3.2, PDF p9 and p15", note: "review summary value; method not stated"}
```

Other PK (2.1 and 3.2, PDF 9-16): Tmax 2-3 h; high-fat meal raises Cmax 3-fold and AUC 2-fold (take with food);
protein binding >84% at 50 ng/mL and >99% at 500 ng/mL; 200 to 600 mg single doses (3-fold) gave 2.4-fold Cmax and
3.6-fold AUC0-inf (PDF 15); metabolised by CYP3A4/5, 2B6, 2C19, 2D6; mass balance 55% faeces (2% unchanged), 18% urine
(none unchanged); M47 (sulfate), M60b and M18 exceed parent exposure; breast-milk AUC about 4 times plasma.

Derived check (mine): with Table 4's CL, V2, Q, V3 the disposition rate constants are 0.809 and 0.0166 /h, terminal
half-life 41.7 h and Vss 9677 L, against the review's "about 34 hours" (PDF 9, 15). The review does not say where 34
h comes from (NCA or model); the two are not reconciled in the document.

Simulated steady state at 63 mg/kg/day tid, max 600 mg tid (Table 5, PDF 33, rendered; mean per age bin): 2 to <6 y
(14.8 kg, 312 mg) AUC24 3903 ng·h/mL, Cmin 85, Cmax 247 ng/mL; 6 to <12 y (22.6 kg, 475 mg) 3998, 84, 269; 12 to <18 y
(36.1 kg, 600 mg) 4106, 84, 293; >=18 y (35.1 kg, 600 mg) 4100, 84, 292. These are model simulations, and OCP asked for
them to be removed from the label (PDF 33).

## Pharmacodynamics and end points
No PK/PD model. The applicant's exposure-response report (safety and efficacy, study 3001) was not reviewed (4.3,
PDF 37). Efficacy end point: 28-day major motor seizure frequency. No therapeutic concentration given.

Interactions: rifampin 600 mg qd lowered GNX AUC 68% and Cmax 57% (n = 16); itraconazole raised AUC 17%, Cmax
unchanged (n = 16) (PDF 18-19); grapefruit juice (n = 4, tablet, 1998 data) Cmax 2-fold, AUC 2.6-fold, judged
unreliable (PDF 18). GNX 400 mg bid did not change oral midazolam exposure (n = 17) (PDF 20).

Dosing (PDF 5-6, 10): weekly titration; <=28 kg 6, 11, 16, 21 mg/kg tid (max 63 mg/kg/day); >28 kg 150, 300, 450,
600 mg tid (max 1800 mg/day).

## Covariates
Only weight, on the maximum absorbed dose, plus the fixed allometry. Concomitant enzyme inducers are not in the main
model (only 12 of 82 on EIAEDs); the unreviewed supplemental model had carbamazepine/phenytoin on CL (PDF 31-32).
Age, sex and race: no clinically relevant effect after weight, by the reviewer's graphical check (PDF 34-36). Hepatic
and renal impairment studies were ongoing (PDF 7-8).

## Caveats
- The reviewer found the model may not be reliable in children: the pediatric pcVPC over-predicted 0-3 h and
  under-predicted 4-5 h, attributed to sparse sampling and large variability (PDF 31-32).
- Table 4's bootstrap medians for V3 (228) and Q (8847) sit outside their own bootstrap CIs (6557-10991 L and
  197-267 L/h) and equal the other parameter's model estimate: on the rendered page the two values appear
  transposed in the source table. The model estimates and CIs are consistent.
- CL is apparent (all data oral, F relative); absolute bioavailability is not given. IIV units are not stated in the
  table; they are taken as %.
- The cap's equation and the weight effect form are not printed, so the model cannot be fully reproduced from this
  document alone.

## Used by stanpumpR
- `inst/extdata/antiseizureRegistry.csv:19` (at 8baa733): moiety Ganaxolone, "oral suspension (Ztalmy)",
  `US_SEIZURE_LABELED`, `proposed_unestimated`, dose basis "ganaxolone base", analyte "ganaxolone (apparent, oral)",
  model "2C first-order with lag, capped absorption", `pk_only`, `stanpumpr_drug` NA, this review as source; notes
  "FDA challenged the paediatric predictive checks; full model not release-ready; UK F~13% is non-US." Built at
  `data-raw/antiseizureRegistry.R:139-143`. Ganaxolone is not a library drug (no other hit in R/, inst/help, tests at
  8baa733).

## Verification against stanpumpR
- Structural model "2C first-order with lag, capped absorption": the review's final model is two compartments,
  first-order absorption with lag and a maximum absorbed dose (PDF 27). **matches**.
- Analyte "apparent, oral": all data are oral and F is relative (PDF 27-28). **matches**.
- Note "FDA challenged the paediatric predictive checks": the reviewer judged pediatric performance possibly
  unreliable and asked for the simulation statements to be removed (PDF 31-33). **matches**.
- Status `US_SEIZURE_LABELED`: approved for CDD seizures from 2 years (PDF 5). **matches** (approval itself is
  outside this document, which recommends it).
- Evidence tier `proposed_unestimated`, defined at `data-raw/antiseizureRegistry.R:32` as "a structure is proposed,
  constants missing": the review prints a full set of point estimates (Table 4, PDF 28). What is missing is the
  equation of the absorption cap and its weight effect, and a validated pediatric fit. The tier is defensible only in
  that sense; the wording "constants missing" overstates it. **partly matches**; flagged for the maintainer.
- "UK F~13%": not in this document. **cannot verify** here.
