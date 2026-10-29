---
file: NJ_label-concerta_021121_2026-04-20.pdf
pmid: none
doi: none
citation: CONCERTA (methylphenidate HCl) extended-release tablets, CII (Janssen). Prescribing information, NDA 021121 (021121s054lbl), FDA label dated 2026-04-20 (Reference ID 5779265). https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/021121s054lbl.pdf
kind: label
drugs: [methylphenidate]
source_read: text layer (Highlights, contents, 2 Dosage and Administration, 9.2 Abuse incl. human abuse potential studies, 10, 11 Description, 12 Clinical Pharmacology in full). Table 7 is scrambled in the text layer; values assigned by row order (Cmax, Tmax, AUCinf, t½), which the magnitudes make unambiguous.
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
revised: 2026-10-27 (added 9.2 human abuse-potential study results, PDF p. 18)
---

# Concerta label 2026: OROS methylphenidate

## Study
Not a study: US prescribing information for Concerta 18, 27, 36, 54 mg extended-release tablets (ADHD,
6-65 y). 12.3 cites a single-dose study in 36 healthy adults (Table 7) and other healthy-adult and
adolescent studies; assay not stated (Table 7 reports "methylphenidate", i.e. total, not enantiomer-specific).

## Model and parameters
No compartmental model, and no description of the tablet's internal structure (no mention of an
overcoat or osmotic core in this edition). The only release statement is in 9.2 (human abuse potential
studies, label p. 18): 22% of the total methylphenidate is available for immediate release and the
remaining 78% for extended release over 24 hours.

12.3 Absorption: initial maximum at about 1 h, then rising concentrations over the next 5-9 h, then a
gradual fall; mean Tmax across doses 6-10 h. Once daily Concerta and IR three times daily give no
clinically significant difference in exposure in adults.

Table 7 (label p. 21), mean ± SD, healthy adults:
| | Concerta 18 mg single dose (n=36) | IR methylphenidate 5 mg x 3 every 4 h (n=35) |
|---|---|---|
| Cmax (ng/mL) | 3.7 ± 1.0 | 4.2 ± 1.0 |
| Tmax (h) | 6.8 ± 1.8 | 6.5 ± 1.8 |
| AUCinf (ng·h/mL) | 41.8 ± 13.9 | 38.0 ± 11.0 |
| t½ (h) | 3.5 ± 0.4 | 3.0 ± 0.5 |

```yaml
parameters:
  - {name: IR_fraction, value: 0.22, unit: fraction, source: "9.2 Abuse (HAP studies)", note: "available for immediate release; 0.78 extended release over 24 h"}
  - {name: Cmax_18mg, value: 3.7, unit: ng/mL, source: "Table 7", note: "total MPH, mean"}
  - {name: Tmax_18mg, value: 6.8, unit: h, source: "Table 7", note: mean}
  - {name: AUCinf_18mg, value: 41.8, unit: ng·h/mL, source: "Table 7", note: mean}
  - {name: t_half, value: 3.5, unit: h, source: "Table 7; 12.3 Distribution", note: "mean; 'about 3.6 h' up to 144 mg/day"}
  - {name: dose_adjusted_AUC, value: "36.7 (M) / 37.1 (F)", unit: ng·h/mL, source: "12.3 Specific Populations", note: "dose basis not stated"}
```

Other 12.3 statements: no accumulation on once-daily dosing; d-MPH Cmax and AUC dose-proportional
18-54 mg while l-MPH rises more than proportionally; l-isomer about 1/40 of d-isomer concentrations;
total MPH and PPAA dose-proportional 54-144 mg/day; no food effect in patients; biexponential decline.
Metabolism: de-esterification to PPAA (little or no activity); ~90% of radioactivity in urine, PPAA ~80%
of dose. Paediatric: CL/F 58% higher in adolescents than in younger children, partly body weight. No
renal or hepatic impairment data.

Dosing (2.3, 2.4): start 18 mg once daily (adults 18 or 36 mg); ranges 18-54 mg (6-12 y), 18-72 mg not
exceeding 2 mg/kg/day (13-17 y), 18-72 mg (adults); titrate by 18 mg weekly. Conversion from IR 5/10/15/20
mg two or three times daily to 18/36/54/72 mg each morning.

## Pharmacodynamics and end points
12.2: d-isomer more active than l-isomer; exposure-response and time course of response "not fully
characterized". No therapeutic concentration range, no ke0.

Abuse-related subjective PD (9.2, Studies of Abuse Potential with CONCERTA, PDF p. 18; dose-level comparisons, no
concentrations; distinct from ADHD efficacy, which 12.2 leaves uncharacterized): two placebo- and active-controlled
crossover HAP studies of single oral doses in recreational CNS stimulant users; response = maximum effect in the
first 8 h.
- Study 1 (n=40): Concerta 108 mg and IR MPH 60 mg gave statistically similar Drug Liking and Abuse Potential
  responses, both above placebo; Concerta 108 mg gave less Euphoria than IR MPH 60 mg.
- Study 2 (n=49; Concerta 108 and 54 mg, IR MPH 90 and 50 mg, placebo): the actives exceeded placebo on Drug
  Liking; IR MPH gave greater Drug Liking than Concerta at similar doses (50 vs 54 mg, 90 vs 108 mg), which the label
  attributes to extended release; Concerta 108 mg and IR MPH 50 mg did not differ on Drug Liking or Euphoria.
- The label says the clinical significance of these differences is unknown.

## Covariates
Age/weight in children (CL/F), sex (no difference). No equations.

## Caveats
- The 22% figure appears only in the abuse-potential section, as the immediate-release share; the label
  does not say how it is delivered (overcoat) in this edition.
- "Over 24 hours" describes availability, not the in-vivo absorption window.
- Table 7 concentrations are total methylphenidate; l-MPH is about 1/40 of d-MPH, so total is close to d.

## Used by stanpumpR
- `R/drugs_methylphenidate.R:60-61`: 22% of each Concerta dose at the time of dosing, "the drug overcoat,
  the label's content fraction", absorbed as the IR tablet; 78% from the osmotic core.
- `inst/help/drugs/methylphenidate.md:21`: same, "the label's figure".

## Verification against stanpumpR
- 22% fraction, `R/drugs_methylphenidate.R:60-61` and help line 21: label gives 22% for immediate release
  and 78% extended. Value **matches**. The attribution of the 22% to the overcoat is not stated in this
  label edition (correct pharmaceutically, but from elsewhere); wording only.
- 78% delivery window: code delivers the core from 2 to 15 h (fitted to Childress 2025), label says
  "over 24 hours". The code explains this is an in-vivo input rate, not release. Not a contradiction of
  the cited fact. **matches** (noted).
- Not a cited location, noted for audit: code Tmax 6.8 h (line 70) equals Table 7's mean 6.8 h after
  18 mg. Scaling the code's 54 mg figures linearly to 18 mg (derived: AUC 116 x 18/54 = 38.7 ng·h/mL,
  Cmax 8.9 x 18/54 = 3.0 ng/mL) gives AUC 7% and Cmax 20% below Table 7 (41.8, 3.7). The model is thus
  closer to this label's study than to Childress (−33% AUC), which supports the code's remark (lines 78-85)
  that the Childress shortfall is between studies.
