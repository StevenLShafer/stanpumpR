---
file: NJ_label-vasostrict_204485_2023-04-12.pdf
pmid: null
doi: null
citation: "Par Pharmaceutical. VASOSTRICT (vasopressin injection; vasopressin in dextrose injection) for intravenous use, prescribing information. NDA 204485, revised 04/2023 (Reference ID 5157117). https://www.accessdata.fda.gov/drugsatfda_docs/label/2023/204485Orig1s030lbl.pdf"
kind: label
drugs: [vasopressin]
source_read: "text layer, all 11 PDF pages (PDF pages 1-9 = label pages 1-9 of 9; PDF pp10-11 carry only the FDA 'may not be the latest' banner). Read: Highlights and contents (p1), 1-2 Dosage and Administration (p2), 3-7 (p3-4), 8 Use in Specific Populations (p4-5), 10-11 Overdosage and Description (p5-6), 12.1-12.2 (p6), 12.3 Pharmacokinetics (p7), 13-16 (p7-9). No tables relied on; nothing rendered"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# Vasostrict label (NDA 204485, 04/2023): synthetic arginine vasopressin

## Study
Not a study: the US prescribing information for synthetic 8-L-arginine vasopressin, indicated to raise blood
pressure in adults with vasodilatory shock despite fluids and catecholamines (section 1, PDF p2). The document is
what its name says (Par, NDA 204485, revised 04/2023).

Dosing (2.2, PDF p2): post-cardiotomy shock start 0.03 U/min, septic shock 0.01 U/min; titrate by 0.005 U/min every
10-15 min; limited data above 0.1 (post-cardiotomy) and 0.07 U/min (septic); taper by 0.005 U/min/h after 8 h
without catecholamines. Strengths 20 U/mL vials (dilute to 0.1 or 1 U/mL), premixed 0.2, 0.4, 0.6, 1 U/mL.

## Model and parameters
No model. Section 11 (PDF p5-6) and 12.3 (PDF p7):

```yaml
parameters:
  - {name: potency, value: 530, unit: U/mg, source: "section 11, PDF p6", note: "One mg is equivalent to 530 units; MW 1084.23"}
  - {name: CL in vasodilatory shock, value: "9-25", unit: mL/min/kg, source: "12.3, PDF p7", note: "at 0.01-0.1 U/min"}
  - {name: apparent t1/2, value: "<= 10", unit: min, source: "12.3, PDF p7"}
  - {name: Vd, value: 140, unit: mL/kg, source: "12.3, PDF p7", note: "population not stated"}
  - {name: plasma protein binding, value: 0, unit: fraction, source: "12.3, PDF p7", note: "does not appear to bind plasma protein"}
  - {name: time to steady state, value: 30, unit: min, source: "12.3, PDF p7", note: "continuous infusion; linear over 10-200 microU/kg/min"}
  - {name: urinary excretion unchanged, value: 0.06, unit: fraction, source: "12.3, PDF p7"}
  - {name: pregnancy clearance factor, value: "about 4 (third trimester), up to 5 (term)", unit: fold, source: "12.3, PDF p7", note: "placental vasopressinase; back to baseline within 2 weeks of delivery"}
```

## Pharmacodynamics and end points
Pressor effect proportional to infusion rate, peaks within 15 min, fades within 20 min of stopping; no tachyphylaxis
(12.2, PDF p6). No bolus time to peak, no concentration-effect data. Interactions: indomethacin more than doubles the
offset time; tetra-ethylammonium raises the pressor effect 20% (12.3).

## Covariates
Pregnancy (above). Paediatric safety not established; geriatric: no differences identified (8.4-8.5).

## Caveats
No study details or sources for the PK numbers; the population for Vd and the 530 U/mg standard's basis are not
given.

## Used by stanpumpR
Context file: corroborating clearance 9-25 mL/min/kg; "peak within 15 minutes" is for an infusion (why tPeak is
provisional). At 8baa733: R/drugs_vasopressin.R:35-37, :83-84; inst/help/drugs/vasopressin.md:5;
inst/extdata/vasoactiveRegistry.csv:27. The repo names the label only as "Vasostrict", without NDA, date or URL.

## Verification against stanpumpR
- **R :35-37, help md:5, vasoactiveRegistry.csv:27** clearance 9 to 25 mL/min/kg in vasodilatory shock, which
  brackets 1.0 L/min at 70 kg (14.3 mL/min/kg, derived): **matches** 12.3.
- **R :83-84** "the label's 'peak within 15 minutes' is for an infusion": 12.2 states the peak in the context of an
  infusion's proportional pressor effect; **matches**.
- **R :41-42, :51-52, :130; help md:5, :9; tests :31-40** the conversion 1 mcU/mL = 2.5 pg/mL uses 400 U/mg (Davison
  1993's RIA standard). This label, which the same file cites, gives 530 U/mg for the marketed synthetic hormone,
  i.e. 1 mcU/mL = 1.89 pg/mL (derived). At 530 U/mg the Landry check becomes 10 x 1.89 + 3.1 = 22 pg/mL against about
  30 (derived), outside the test's 10% tolerance. **discrepancy** (the choice of potency standard is undocumented
  against the label).
- **R :51-52** "about 30% of vasopressin is bound" (Fabian): the label says it does not appear to bind plasma
  protein. Not cited by the repo for binding; conflict noted, no verdict.
- Not used by the repo: Vd 140 mL/kg (about 9.8 L at 70 kg, near Fabian's 8.5 L), and pregnancy clearance "about
  4-fold" in the third trimester, "up to 5-fold" at term, which is consistent with the repo's 5.07 (Davison).
