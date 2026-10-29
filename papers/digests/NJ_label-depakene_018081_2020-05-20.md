---
file: NJ_label-depakene_018081_2020-05-20.pdf
pmid: none
doi: none
citation: "DEPAKENE (valproic acid) capsules and oral solution, US prescribing information, AbbVie; NDA 018081 (s071) and 018082 (s054), revised 5/2020 (FDA Reference ID 4610951; posted 2020-05-20). https://www.accessdata.fda.gov/drugsatfda_docs/label/2020/018081s071,018082s054lbl.pdf"
kind: label
drugs: [valproate, ethosuximide]
source_read: "text layer, 47 pages (one form-feed chunk per PDF page): Highlights and contents (pp. 1-2), 2.1 Epilepsy and 2.2 (pp. 5-7), 7.1-7.2 drug interactions (pp. 20-24), 8.1 Pregnancy (pp. 25-27, scanned for PK statements), 12.2-12.3 (pp. 32-34); PDF p. 22 (7.2, the ethosuximide paragraph) rendered and read"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master ca8ce42
verification: matches
---

# Depakene label 2020: valproic acid capsules and oral solution

## Study
Regulatory label (PLR format), revised 5/2020. Products: capsules 250 mg valproic acid; oral solution
equivalent to 250 mg valproic acid per 5 mL as the sodium salt (Highlights, section 3). Indications: complex
partial and absence seizures (monotherapy and adjunctive). The label's PK and interaction statements summarise
small studies (n = 6 to 16 per interaction) without citing them.

## Model and parameters
No compartmental model. Section 12.3 (PDF pp. 32-33), all for total valproate unless stated:

```yaml
parameters:
  - {name: CL_total, value: 0.56, unit: "L/h/1.73 m2", source: "12.3 Elimination, PDF p33", note: "mean plasma clearance, total valproate; patients not on enzyme inducers"}
  - {name: V_total, value: 11, unit: "L/1.73 m2", source: "12.3 Elimination, PDF p33"}
  - {name: CL_free, value: 4.6, unit: "L/h/1.73 m2", source: "12.3 Elimination, PDF p33"}
  - {name: V_free, value: 92, unit: "L/1.73 m2", source: "12.3 Elimination, PDF p33"}
  - {name: t_half, value: "9 to 16", unit: h, source: "12.3 Elimination, PDF p33", note: "monotherapy, oral 250-1000 mg"}
  - {name: free_fraction, value: "about 10% at 40 mcg/mL to 18.5% at 130 mcg/mL", source: "12.2 and 12.3 Protein Binding, PDF pp. 32-33"}
  - {name: urine_unchanged, value: "<3%", source: "12.3 Metabolism, PDF p33", note: "30-50% as glucuronide; beta-oxidation >40%"}
```

Equivalent oral doses of Depakote products and Depakene capsules deliver equivalent valproate (12.3 Absorption).
Total concentration rises less than proportionally with dose (saturable binding); unbound kinetics linear.
CSF about the unbound concentration (about 10% of total). Neonates under 10 days: half-life 10-67 h against 7-13 h
beyond 2 months; children 3 months-10 years: weight-normalised clearance 50% higher than adults; elderly (68-89 y
vs 22-26 y): intrinsic clearance -39%, free fraction +44%; unbound clearance by sex 4.8 vs 4.7 L/h/1.73 m2;
cirrhosis free clearance -50% (n = 7), acute hepatitis -16% (n = 4), half-life 12 to 18 h; renal failure
(CrCl < 10 mL/min) unbound clearance -27%, haemodialysis lowers levels about 20% (12.3 Special Populations,
PDF pp. 33-34). No pregnancy PK statement in 8.1.

Interactions with valproate as perpetrator (7.2, PDF pp. 21-24), each as the label gives it:
- **Ethosuximide**: single 500 mg ethosuximide with valproate 800-1600 mg/day, healthy volunteers n = 6:
  ethosuximide half-life +25%, total clearance -15% (PDF p22, rendered).
- Lamotrigine: half-life 26 to 70 h (+165%), steady state, 10 healthy volunteers (p22).
- Phenobarbital: valproate 250 mg BID x 14 d, n = 6 normal subjects, 60 mg phenobarbital: half-life +50%,
  plasma clearance -30% (p23).
- Phenytoin: valproate 400 mg TID, n = 7: phenytoin free fraction +60%; total clearance and apparent V +30%;
  free clearance and V -25% (p23).
- Carbamazepine -17%, carbamazepine-10,11-epoxide +45% in epileptic patients (p22); diazepam free fraction +90%
  (n = 6); amitriptyline clearance -21%, nortriptyline -34%; lorazepam clearance -17% (p24).
Perpetrators on valproate (7.1, p20-21): rifampin oral clearance +40%; felbamate peak 86 to 115 mcg/mL;
aspirin free fraction 4-fold; carbapenems lower valproate markedly; cholestyramine -14%.

## Pharmacodynamics and end points
Therapeutic range in epilepsy commonly 50 to 100 mcg/mL total valproate (12.2, PDF p32; 2.1, PDF pp. 5-6,
"usually accepted therapeutic range"). Thrombocytopenia probability rises at total trough above 110 mcg/mL
(women) or 135 mcg/mL (men) (2.1, PDF p5). Maximum 60 mg/kg/day. Not an opioid; no MEAC, ke0 or potency ratio.

## Covariates
Age, hepatic and renal disease, enzyme inducers; no equations.

## Caveats
Interaction figures are from n = 6-16 single studies with no citations, means only. The repo's context note is
right that "the valproic acid label" in the repo does not name the product; the Depakote (NDA 018723, 2026)
label carries the identical ethosuximide paragraph (its PDF p25), so either label fits the citation.

## Used by stanpumpR
- docs/antiseizure-interactions-review.md:114 (at ca8ce42, same line) - "The valproic acid label and Mattson &
  Cramer also report inhibition" of ethosuximide by valproate.
- data-raw/antiseizureInteractions.R:392 (notes of the valproate -> ethosuximide row) - same statement. Not in
  the context file; found by grep at ca8ce42.
- The applied row itself is inst/extdata/antiseizureInteractions.csv:35 (CL x0.85, from Pisani 1984; the label is
  not its source).

## Verification against stanpumpR
- antiseizure-interactions-review.md:114 and antiseizureInteractions.R:392, "the valproic acid label also
  reports inhibition": **matches** - 7.2 states valproate inhibits ethosuximide metabolism (CL -15%, half-life
  +25%, n = 6), the same magnitude as the applied CL x0.85 (Pisani 1984, 11.2 to 9.5 mL/min). The label's study
  design (single 500 mg ethosuximide, valproate 800-1600 mg/day, n = 6) matches Pisani's, so the label is very
  likely the same study, not independent support.
- Context only (not cited by the repo): the label's phenobarbital clearance -30% is close to the applied
  valproate -> phenobarbital CL x0.711 (antiseizureInteractions.csv); its lamotrigine half-life 26 to 70 h implies
  a larger inhibition (CL about x0.37, derived, if V is unchanged) than the applied fold 0.49 at the reference
  valproate concentration (Gidal 2003 Emax fit). Different studies and populations; not a discrepancy.
- The Zarontin label (NJ_label-zarontin_012380_2021-10-29) says valproate has been reported both to raise and to
  lower ethosuximide levels; the two labels disagree in direction, which the repo's "variable" wording reflects.
