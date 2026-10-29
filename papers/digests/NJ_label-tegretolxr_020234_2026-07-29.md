---
file: NJ_label-tegretolxr_020234_2026-07-29.pdf
pmid: none
doi: none
citation: Tegretol (carbamazepine USP) tablets 200 mg, suspension 100 mg/5 mL, and Tegretol-XR (carbamazepine extended-release tablets) 100/200/400 mg, US prescribing information, NDA 020234 (Novartis), revised 7/2026, Drugs@FDA posting 2026-07-29 (Reference ID 5835028). https://www.accessdata.fda.gov/drugsatfda_docs/label/2026/020234s57lbl.pdf
kind: label
drugs: [carbamazepine]
source_read: text layer, 25 pages (old-format label); read Clinical Pharmacology / Pharmacokinetics, Pediatric Use, Dosage and Administration and its dosing table; skimmed warnings and interactions
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Tegretol / Tegretol-XR label 2026: carbamazepine

## Study
Combined label for conventional tablets, suspension and XR tablets. PK statements are summary values without study
design or n.

## Model and parameters
No compartmental model. Clinical Pharmacology, Pharmacokinetics paragraph:
```yaml
parameters:
  - {name: F_rel_XR, value: 0.89, unit: fraction, source: "Pharmacokinetics", note: "bioavailability of the XR tablet 89% compared to suspension"}
  - {name: tmax_suspension, value: 1.5, unit: h, source: "Pharmacokinetics", note: "chronic dosing"}
  - {name: tmax_tablet, value: "4-5", unit: h, source: "Pharmacokinetics", note: "chronic dosing"}
  - {name: tmax_XR, value: "3-12", unit: h, source: "Pharmacokinetics", note: "chronic dosing"}
  - {name: protein_binding, value: 76, unit: "%", source: "Pharmacokinetics", note: "CSF/serum 0.22, similar to the 24% unbound"}
  - {name: t_half_initial, value: "25-65", unit: h, source: "Pharmacokinetics", note: "single dose"}
  - {name: t_half_repeated, value: "12-17", unit: h, source: "Pharmacokinetics", note: "after autoinduction, complete in 3-5 weeks"}
  - {name: therapeutic_range, value: "4-12", unit: mcg/mL, source: "Pharmacokinetics; Pediatric Use", note: "usual adult therapeutic levels; total plasma carbamazepine; same in children"}
  - {name: observed_range, value: "0.5-25", unit: mcg/mL, source: "Pharmacokinetics", note: "no apparent relation to daily dose"}
```
The three forms "delivered equivalent amounts" of drug; suspension absorbed faster and XR slower than the tablet.
XR bid gives steady-state levels comparable to tablets qid; suspension tid comparable to tablets bid. CYP3A4 forms the
10,11-epoxide (equipotent in animal screens); epoxide/parent ratio falls with age in children (0.44 under 1 y to 0.18
at 10-15 y). 72% of 14C in urine, 28% feces, 3% unchanged. Dosing: epilepsy > 12 y start 200 mg bid (tablet or XR),
add up to 200 mg/day weekly, maintenance usually 800-1200 mg/day, max 1000 mg (12-15 y) / 1200 mg (> 15 y), 1600 mg
rarely; 6-12 y start 100 mg bid, maintenance 400-800 mg/day; < 6 y 10-20 mg/kg/day, below 35 mg/kg/day; trigeminal
neuralgia 100 mg bid start, max 1200 mg/day, maintenance 400-800. Same total daily dose when converting tablets to XR.

## Pharmacodynamics and end points
Therapeutic range 4-12 mcg/mL total plasma carbamazepine (above). No ke0 or concentration-effect model. Not an
opioid; no MEAC or potency ratio.

## Covariates
Age (children: faster epoxide formation, poor dose-concentration correlation); race and sex not systematically
evaluated; inducers and inhibitors of CYP3A4.

## Caveats
Old-format label with summary statistics only; "89% compared to suspension" sits next to a statement that the forms
deliver equivalent amounts, so the 0.89 is a point estimate from an unnamed study.

## Used by stanpumpR
- R/drugs_carbamazepine.R:41-42 (header) and :101 (code, `ER = list(ka_PO = 0.1 / 60, bioavailability_PO = 0.89,
  tlag_PO = 0)`) - XR relative bioavailability 0.89 against the suspension, as "mg PO ER".
- inst/help/drugs/carbamazepine.md:11 - "0.89 relative availability (the Tegretol-XR label)".

## Verification against stanpumpR
- 0.89 relative availability of the XR tablet against the suspension: label Pharmacokinetics paragraph, 89%
  compared to suspension. **matches** (applied as a fraction 0.89, correctly; the base model's IR forms carry 1.0,
  consistent with the label's equivalence of suspension and tablets).
- The XR ka (0.1/h) is not from this label, but is consistent with it (derived, 70 kg reference: CL/F 4.52 L/h,
  V/F 138 L, k 0.0328/h; steady-state tmax on 12-hourly dosing about 5.2 h for XR, inside the label's 3-12 h, and
  about 3.6 h for the IR ka 0.441/h, near the label's 4-5 h for tablets).
- Not a cited location, for context: the band 4-12 mcg/mL (drugs_carbamazepine.R:54, cited to ILAE/Patsalos 2008)
  is the same as this label's usual adult therapeutic range; the header's 21 h half-life (single model, maintenance
  patients) lies between the label's initial 25-65 h and autoinduced 12-17 h.
