---
file: NJ_fda-nda22023_022023_1-22023S017-fosaprepitant-clinpharm-bpca.pdf
pmid: none
doi: none
citation: FDA Office of Clinical Pharmacology Review, NDA 22023/S-017, EMEND for injection (fosaprepitant dimeglumine), pediatric patients 6 months and older (Merck Sharp & Dohme; submitted 10/3/2017; Reference ID 4242452, 2018; reviewers E. Shang, J. Earp, I. Kim, G. Burckart). https://www.accessdata.fda.gov/drugsatfda_docs/pediatric/261_22023S017-fosaprepitant-clinpharm-bpca.pdf
kind: review
drugs: [aprepitant, fosaprepitant]
source_read: text layer, 116 pages; read the summary and recommendations (pp 8-15), 3.2 General PK (label excerpt, Table 10), 3.2.1 observed pediatric PK (pp 22-25), 4.3 Pharmacometrics Review (pp 85-113); Table 44 (final popPK model) read from its page image (PDF p 90); Tables 42-43 not rendered
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# FDA 2018: clinical pharmacology review of pediatric fosaprepitant (NDA 22023 S-017)

## Study
Efficacy supplement extending IV fosaprepitant to CINV in patients 6 months to 17 years, by exposure matching to
adults (single-day regimen) and to pediatric oral aprepitant (three-day regimen). PopPK data: 316 pediatric
subjects (PONV and CINV) from studies P097 (adolescents, oral 125/80/80 mg capsules), P134 (adolescents 115 mg IV
then 80 mg oral suspension; single oral doses 6 mo-12 y), P148 (PONV, oral) and P029 (single-dose IV dose ranging,
all ages). Minimum weight enrolled 6.80 kg. NONMEM 7.3, FOCE; ADVAN4/TRANS4; log10-transformed concentrations,
log-additive error. Fosaprepitant doses converted to aprepitant by 534.44/614.4 in the control files.

## Model and parameters
Two-compartment, first-order absorption with lag; allometric WT/70 with exponent 0.75 on clearances and 1 on
volumes. Table 44 (final model, typical values, sponsor's Population PK report Table I-5):
```yaml
parameters:
  - {name: CL, value: 5.38, unit: L/h, source: "Table 44", note: "RSE 6.7%; x (WT/70)^0.75 x (Dose/80)^-0.253; IIV 60.7%"}
  - {name: V2, value: 47.8, unit: L, source: "Table 44", note: "central; RSE 10.3%; x (WT/70) x (Age/8)^-0.205; IIV 58.8%"}
  - {name: Q, value: 35.6, unit: L/h, source: "Table 44", note: "RSE 23.9%; x (WT/70)^0.75; IIV 72.2%"}
  - {name: V3, value: 37.9, unit: L, source: "Table 44", note: "peripheral; RSE 11.8%; x (WT/70); IIV 61.6%"}
  - {name: ka, value: 0.319, unit: 1/h, source: "Table 44", note: "RSE 37.2%; IIV 103.6%; x exp(0.369) suspension P134, x exp(0.821) P148"}
  - {name: Tlag_capsule, value: 0.938, unit: h, source: "Table 44", note: "suspension 0 fixed"}
  - {name: F1, value: 0.918, unit: fraction, source: "Table 44", note: "relative oral bioavailability; IIV 55.1%"}
  - {name: EDTA_CL, value: -0.295, unit: "log effect", source: "Table 44", note: "CL x exp(-0.295) for reduced-EDTA formulation (P029)"}
  - {name: residual, value: 0.159, unit: log10, source: "Table 44"}
```
Derived: Vss = 47.8 + 37.9 = 85.7 L at 70 kg; CL at an 80 mg dose 5.38 L/h; terminal t1/2 not stated.
Adult label excerpt (Table 10, p 20-21): 150 mg fosaprepitant over 20 min in healthy adults gives aprepitant AUC
37.4 (+/- 14.8) mcg.h/mL and Cmax 4.2 (+/- 1.2) mcg/mL; fosaprepitant below 10 ng/mL within 30 min of the end of
infusion; aprepitant > 95% protein bound; Vd about 70 L; terminal t1/2 about 9-13 h; CYP3A4 (minor 1A2, 2C19).
Observed pediatric data: 150 mg IV over 30 min in adolescents gave aprepitant AUC0-24 30400-30800 ng.h/mL and Cmax
3500-5870 ng/mL, median Tmax 0.5 h (p 22). Fosaprepitant Cmax (Table 4): healthy adults 115 mg/15 min 5635 +/- 1544
ng/mL (C15min); 3 mg/kg/60 min 2756, 3034, 1654 ng/mL at 6 mo-<2, 2-<6, 6-<12 y; 150 mg/30 min 1310 ng/mL at 12-17 y.
Approved regimens (Tables 5, 6): single day 150 mg IV over 30 min (12-17 y), 4 mg/kg over 60 min (2-<12 y, max
150 mg), 5 mg/kg over 60 min (6 mo-<2 y, max 150 mg); three-day 115 mg IV (12-17 y) or 3 mg/kg (max 115 mg) on day 1,
then oral aprepitant 80 mg or 2 mg/kg. IV on days 2-3 rejected: Cmax about 2-fold oral; 8-16 h infusions would be
needed.

## Pharmacodynamics and end points
Exposure-response in P029 (complete response, delayed phase) explored graphically against AUC, Cmax and C24
(Figures 25-27); no concentration-effect model, no ke0. Not an opioid; no MEAC or potency ratio.

## Covariates
Weight (allometric, fixed exponents), age on V2 ((Age/8)^-0.205), dose on CL ((Dose/80)^-0.253, i.e. nonlinear),
formulation on ka, EDTA level on CL. Sex and race not clinically meaningful.

## Caveats
Sponsor's model, accepted without reviewer re-analysis; VPC under-predicts concentrations after 24 h (sparse
C24/C48/C72 sampling); the dose-on-CL term makes the model dose dependent; ka poorly estimated (RSE 37%, IIV 104%).

## Used by stanpumpR
- R/drugs_aprepitant.R:65 - reference list: "FDA clinical pharmacology review, fosaprepitant NDA 22023 S-017, 2018."
- inst/help/drugs/fosaprepitant.md:11 - the FDA review "fitted a larger two-compartment model, which was read for
  context; its parameters are not mixed with Nijstad's."

## Verification against stanpumpR
- help :11: the review's model is two-compartment (Table 44) with 9 structural/covariate terms, against Nijstad's
  one compartment; the repo uses none of its values. **matches.**
- Dose basis (fosaprepitant.md:7, drugs_aprepitant.R:68-71): 534.44/614.40 = 0.870 mg aprepitant per mg
  fosaprepitant, conversion taken as instantaneous. The review uses the same 534.44/614.4 factor and reports
  fosaprepitant negligible within 15-30 min of the end of infusion. **matches.**
- For context (not a claim the repo makes): the review's Vss 85.7 L (derived) is close to Nijstad's 86.8 L at 70 kg;
  its CL 5.38 L/h close to Nijstad's 5.83 L/h; the review's (WT/70)^0.75 / (WT/70) scaling is the same form.
- R/drugs_aprepitant.R:65 reference string: no reviewer names or Reference ID; year 2018 is right. **matches.**
