---
file: NJ_page2010-brogren_abstract_2026-10-12.pdf
pmid: none
doi: none
citation: "Brogren J, Nyberg S. Transit compartment model useful for describing absorption of quetiapine XR and IR. PAGE 19 (2010) Abstr 1686 [www.page-meeting.org/?abstract=1686]; web page printed 2026-10-12."
kind: other
drugs: [quetiapine]
source_read: text layer (4-page web print of the abstract; cookie-banner text interleaved but the abstract is legible); the linked PDF poster was not in the archive
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Brogren 2010: transit-compartment absorption of quetiapine XR and IR (PAGE abstract)

## Study
Conference abstract (PAGE 2010, poster, Applications - CNS), AstraZeneca. Data from a randomised, double-blind
crossover study in healthy volunteers (n = 58, NCT00702676) titrating quetiapine XR and IR (design reported in
Datto 2009). Population PK in NONMEM VI. Doses, sampling and assay are not given in the abstract.

## Model and parameters
Two-compartment disposition with transit-compartment absorption (Savic) feeding directly into the central
compartment; separate MTT and number of transit compartments (NB) per formulation; other structural parameters
shared across formulations.

```yaml
parameters:
  - {name: MTT_XR, value: 4.65, unit: h, source: "abstract Results"}
  - {name: MTT_IR, value: 1.14, unit: h, source: "abstract Results"}
  - {name: NB, value: "not reported", source: "abstract", note: "estimated separately per formulation, values not given"}
  - {name: CL_F, value: 96.8, unit: L/h, source: "abstract Results", note: "shared by both formulations"}
  - {name: Vss, value: 515, unit: "L (printed as L/h)", source: "abstract Results", note: "unit misprinted in the abstract; V1, V2, Q not given"}
  - {name: IIV_F, value: "40-45", unit: "%", source: "abstract", note: "similar for both formulations"}
  - {name: IIV_MTT_IR, value: 45, unit: "%", source: "abstract"}
  - {name: IIV_MTT_XR, value: 23, unit: "%", source: "abstract"}
```

Finding: with one CL/F for both, XR gives similar AUC to IR, unlike the original noncompartmental analysis.

## Pharmacodynamics and end points
None. Not an opioid: no MEAC, ke0 or potency ratio.

## Covariates
None reported.

## Caveats
Abstract only: no transit number, no V1/V2/Q, no relative bioavailability value, no covariates; healthy
volunteers; Vss unit misprint.

## Used by stanpumpR
Context file lists no locations (not flagged as cited). A search of the checkout finds it cited as the reason
quetiapine XR is not offered: R/drugs_quetiapine.R:29-31, R/antipsychotics.R:25-26 and :77-86 (quetiapineXR
profile, modelStatus "blocked"), docs/antipsychotics.md:68-69, inst/help/drugs/quetiapine.md:7.

## Verification against stanpumpR
- **"gives CL/F and the mean transit times but not the transit number, Q/F or volumes" (R/antipsychotics.R:25-26;
  docs/antipsychotics.md:68-69; drugs_quetiapine.R:29-31; quetiapine.md:7)**: CL/F 96.8 L/h and MTTs 4.65 / 1.14 h
  are given; NB values, Q and the compartment volumes are not. **Matches**, with one nuance: the abstract does
  give a Vss (515, unit misprinted), so "no volumes" is strictly "no compartmental volumes"; a full parameter
  vector still cannot be built.
- **quetiapineXR profile "58 healthy volunteers", "PAGE abstract 1686" (R/antipsychotics.R:80-82)**: n = 58
  healthy volunteers; PAGE 19 (2010) abstract 1686. **Matches.**
- Context: Brogren's CL/F 96.8 L/h is higher than the implemented Zheng 2024 76.1 L/h (different population);
  not used by the code.
