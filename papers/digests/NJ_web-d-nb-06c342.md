---
file: NJ_web-d-nb-06c342.pdf
pmid: null
doi: 10.1208/s12248-016-9975-1
citation: "Cirincione B, Edwards J, Mager DE. Population pharmacokinetics of an extended-release formulation of exenatide following single- and multiple-dose administration. AAPS J (online 2016; DOI 10.1208/s12248-016-9975-1). Open-access copy at https://d-nb.info/1123334935/34"
kind: pk-model
drugs: [exenatide]
source_read: "text layer, all 10 PDF pages; pages 3 (equations, Fig. 1), 4 (Table I) and 6 (Table II) rendered. PDF header reads 'The AAPS Journal (2016)'; volume and pages not printed in this copy"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: discrepancy
---

# Cirincione 2016: population PK of extended-release (microsphere) exenatide, single and weekly doses

Document identity: the archived file is a journal article (AAPS J research article), deposited at the German
National Library (d-nb.info), not a regulatory document. It is the ER companion to Cirincione & Mager 2017
(Br J Clin Pharmacol, the IR model; cited here as "manuscript accepted").

## Study
- Model-building data from two phase II, randomised, double-blind, placebo-controlled trials (Fineman 2011, ref 18)
  (pp1-2):
  - Single dose of ER exenatide 2.5, 5, 7 or 10 mg SC; 23 sampling times over 12 weeks (rich 0.25-48 h, then
    weekly to week 11); 887 concentrations from 41 patients after exclusions (1054 from 47 collected).
  - Weekly 0.8 or 2 mg for 15 weeks, 22 samples during dosing plus 8 in a 12-week washout; 699 concentrations from
    23 patients (31 enrolled, 8 excluded for incomplete dosing records).
  - Total 1586 concentrations, 64 patients with type 2 diabetes.
- External validation: 119 patients, 408 concentrations, 2 mg weekly for 24 weeks (DURATION-5, ref 19), predose
  samples weeks 4, 8, 14, 20, 24.
- Demographics (Table I, p4, total n = 64): mean (SD) age 53.5 (9.78) y, range 30-72; weight 95.7 (21) kg, 59-155;
  eGFR 89.6 (23.2) mL/min/1.73 m2, 56-169; 27 (42.2%) female; renal group normal 23, mild 38, moderate 3.
  Multiple-dose patients were heavier (mean 106.9 kg, 70-155) than single-dose (89.5 kg, 59-131).
- Assay: two-site sandwich ELISA, exenatide-specific capture antibody, LLOQ 10 pg/mL (p2). BLQ 36.9%, 15.6%,
  19.8%, 14.2% of samples at 2.5, 5, 7, 10 mg single doses; 16.3% and 9.1% with weekly 0.8 and 2 mg (pp3-4);
  BLQ kept with Beal M3.
- NONMEM 7.2, Laplacian estimation (p3).

## Model and parameters
Two-compartment disposition with linear plus Michaelis-Menten elimination, **disposition fixed to the IR model**
(CL, Vmax, Km, VC, VP, CLd and their eGFR and weight covariates; p3). Absorption: three parallel inputs of the
bioavailable dose (Dose · frel): first-order (fraction f1, rate ka) and two transit series (fractions f2 and
f3 = 1 − (f1 + f2)) using the explicit transit-compartment input ktr · (ktr·t)^N · e^(−ktr·t) / N! with N! from
Stirling's approximation, so N need not be an integer; the last transit compartment empties at ktr (p3, Fig. 1).
The printed equation (p3) shows Dose · frel only on the first-order term; Fig. 1 applies frel to all three
paths. Elim = CL/VC + Vmax/(Km·VC + Ac). Covariates: CL = CLint · (eGFR/80)^0.838; VC = Vcint · (WT/84.8)^2.67
(Table II footnote).

Table II (p6), single-dose model and combined single- and multiple-dose model (final):

```yaml
parameters:
  - {name: ka, value: 2.98, unit: 1/day, source: "Table II p6", note: "single-dose model; RSE 14.7%, IIV 77.9%"}
  - {name: ka_combined, value: 3.85, unit: 1/day, source: "Table II p6", note: "combined model; RSE 11.2%, IIV 53.9%"}
  - {name: f1, value: 1.13, unit: "% of bioavailable dose", source: "Table II p6", note: "single-dose; IIV 83%"}
  - {name: f1_combined, value: 1.18, unit: "%", source: "Table II p6", note: "IIV 66.7%"}
  - {name: f2, value: 46.3, unit: "%", source: "Table II p6", note: "single-dose; combined 48.0"}
  - {name: f3, value: 52.6, unit: "%", source: "Table II p6", note: "derived 1-(f1+f2), not estimated; combined 50.8"}
  - {name: frel_single_dose_study, value: 10.2, unit: "% relative to IR", source: "Table II p6", note: "single-dose model; combined model 8.86 (IIV 43.8%)"}
  - {name: frel_multiple_dose_study, value: 15.5, unit: "% relative to IR", source: "Table II p6", note: "combined model only"}
  - {name: ktr1, value: 0.0872, unit: 1/day, source: "Table II p6", note: "single-dose; combined 0.105"}
  - {name: N1, value: 0.667, unit: transit compartments, source: "Table II p6", note: "single-dose; combined 0.570"}
  - {name: ktr2, value: 0.722, unit: 1/day, source: "Table II p6", note: "single-dose IIV 13.6%; combined 0.591 (IIV 17.2%)"}
  - {name: N2, value: 32.2, unit: transit compartments, source: "Table II p6", note: "single-dose; combined 26.1"}
  - {name: MTT1, value: 19.1, unit: day, source: "derived (N1+1)/ktr1, single-dose", note: "paper p6: about 19 days; combined (derived) 15.0 d"}
  - {name: MTT2, value: 46.0, unit: day, source: "derived (N2+1)/ktr2, single-dose", note: "paper p6: about 46 days; combined (derived) 45.9 d"}
  - {name: CLint, value: 110, unit: L/day, source: "Table II p6", note: "fixed from IR model, at eGFR 80 (= 4.58 L/h, derived)"}
  - {name: Vcint, value: 7.03, unit: L, source: "Table II p6", note: "fixed, at 84.8 kg"}
  - {name: Vc_weight_power, value: 2.67, unit: none, source: "Table II p6", note: fixed}
  - {name: CL_eGFR_power, value: 0.838, unit: none, source: "Table II p6", note: fixed}
  - {name: Km, value: 567, unit: pg/mL, source: "Table II p6", note: fixed}
  - {name: Vmax, value: 0.0372, unit: mg/day, source: "Table II p6", note: "fixed; single-dose column prints 0.037 (= 1.55 mcg/h, derived)"}
  - {name: CLd, value: 89.3, unit: L/day, source: "Table II p6", note: "fixed (= 3.72 L/h, derived)"}
  - {name: VP, value: 7.04, unit: L, source: "Table II p6", note: fixed}
  - {name: RV_single_dose_study, value: 0.684, unit: log SD, source: "Table II p6", note: "combined model; single-dose model 0.727"}
  - {name: RV_multiple_dose_study, value: 0.376, unit: log SD, source: "Table II p6", note: combined model}
```

Text (p6): only about 1% of the bioavailable dose is in the initial release; about 46% and 53% go through the two
transit series. Steady state predicted after 6-10 weeks (results, p6) or about 8-10 weeks (abstract), and
concentrations undetectable 10-12 weeks after stopping. Phase III external validation: median PE −1.85%, median
absolute PE 25% (p8).

## Pharmacodynamics and end points
None. No glucose, HbA1c or other PD model; the paper presents itself as a platform for later PD work. Prior
steady-state exposure-response work cited (ref 27) is not reproduced.

## Covariates
Only the fixed IR-model covariates (eGFR on linear CL, weight on VC). Covariates on the absorption parameters
were not assessed (small n; p4). Anti-drug antibodies not modelled (discussion p9).

## Caveats
- Disposition fixed to the IR model; frel (about 9-16%) is relative to IR, absorbing any misfit of the fixed
  disposition.
- frel differed between the two studies (8.86% vs 15.5%); the text calls the single-dose trial phase I on p6 but
  phase II in Methods (p2).
- Timing statements differ by section: second peak "about 2 weeks" (introduction, p2) vs "about 3 weeks"
  (abstract) and MTT1 about 19 days (p6); initial release "within the first 24 h" (p2).
- Transit compartments are an empirical description of microsphere hydration and erosion (authors, p9).
- 64 patients, IIV only on ka, f1, ktr2 and frel.

## Used by stanpumpR
- `inst/extdata/antidiabeticRegistry.csv:14` (row `exenatide_er_bydureon`, "Bydureon microspheres (studied)", SC):
  source_url for a registry-only row with pk_status PK-AUDIT, pd_status PD-CONSTRAINT, not implemented. Generated
  by `data-raw/antidiabeticRegistry.R:100-103` (8baa733).

## Verification against stanpumpR
- `antidiabeticRegistry.csv:14` at 8baa733: note "Three release paths with noninteger gamma transit; needs a
  convolution kernel." **Matches**: one first-order and two transit series, N1 0.667 and N2 32.2 (non-integer,
  Stirling gamma). PK-AUDIT (a model exists but is not run) is consistent with the paper. The same note's
  constraints (steady state after 6-7 weeks; release initial 48 h, about 2 weeks, about 7 weeks) are attributed to
  Fineman 2011 and Knop 2017, not this paper; this paper gives steady state at 6-10 (results) or 8-10 weeks
  (abstract) and an initial release within 24 h.
- **Discrepancy (help text, not a context location)**: `inst/help/drugs/exenatide.md:27` at 8baa733 says the
  extended-release products are not modelled and that "no published model of that release was available". This
  paper, the registry's own source for the Bydureon row, is a published population model of exactly that
  three-phase release. The statement should say the published model needs an engine stanpumpR lacks (non-integer
  transit input), not that none exists. The BCise row (`exenatide_er_bcise`) is a different vehicle and this paper
  does not cover it.
