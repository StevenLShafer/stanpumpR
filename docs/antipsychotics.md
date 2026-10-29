# Antipsychotics

Eight route-specific profiles in the drug library, two metabolites, and two
profiles that are registered but kept out of the library for lack of a complete
published model. Each model's header (`R/drugs_<name>.R`) holds the full
derivation; `R/antipsychotics.R` holds the registry of provenance and
pharmacodynamic endpoints (`antipsychoticProfiles()`,
`antipsychoticOccupancy()`).

## Design rules

- **One drug per parameter basis.** Apparent oral or IM parameters (`/F`) and
  absolute IV parameters are different drugs, as with amiodarone and
  amiodaroneIV: `haloperidol` / `haloperidolIV`, `droperidol` /
  `droperidolIM`. An apparent drug offers only its own route and carries
  bioavailability 1. `test-antipsychotics.R` checks every profile's units
  against its `parameterBasis`.
- **No occupancy targets.** MEAC and `endCe` are zero and no TCI units are
  offered: D2 occupancy is a receptor biomarker, and no validated clinical
  occupancy target is specified.
- **The band is a serum therapeutic reference range**, drawn where one exists
  for the analyte the row plots (`inst/extdata/therapeuticRanges.csv`,
  `test-therapeutic-ranges.R`), from Table 5 of the AGNP update 2026 (Hart
  2026): aripiprazole 120-270 ng/mL, olanzapine 20-40 ng/mL, quetiapine
  100-500 ng/mL, haloperidol 1-5 ng/mL (narrowed from 2017's 1-10). It is a
  trough at steady state, read against the plasma curve. Risperidone's
  20-60 ng/mL is for the active moiety, split between two rows by CYP2D6, so it
  is not drawn; haloperidolIV and the droperidols (not in the AGNP table) have
  none that applies to their use.
- **Occupancy is not plotted.** The Effect Site trace stays a concentration.
  Aripiprazole (Kim 2012) and both haloperidols (Lim 2013) have one, from a
  PET ke0 supplied directly; the rest are plasma only. `antipsychoticOccupancy()` turns a simulated driver
  concentration into occupancy with one of the profile's paired Emax/EC50
  fits, and refuses the haloperidol PANSS term and the profiles with no
  endpoint.
- **Metabolites are real states.** Risperidone and aripiprazole form
  `hydroxyrisperidone` and `dehydroaripiprazole` through the library's
  metabolite machinery. The whole parent apparent clearance feeds a metabolite
  whose parameters are divided by `F x fmet`, so the metabolite drugs have no
  dosing units.

## Verification status

The values came from an implementation brief and were then checked against
primary text, as far as the network allowed (2026-10-09).

| Drug | Checked in primary text | Still to check |
|---|---|---|
| quetiapine | Zheng 2024 full text: all PK and the weight equations; Nord 2011: 1369 nmol/L, Emax fixed | none |
| olanzapine | Sun 2021 Table 3: all values; Vp and Q carry no covariates; Methods: the power form of the age term, referenced to 36 y | Kapur 1998 EC50 10.3 ng/mL (abstract gives occupancy by dose only) |
| risperidone | Størset 2024 text and Table 2: structure, ka, serum, allele activities, NFIB, both age terms, phenotype anchors 4.2 / 27.4 / 50.6 L/h, V/F 333 L, Vm 96 L, CLm 8.0 L/h; Emax 100% / EC50 8.2 ng/mL (Lindauer 2025) | Emax 88% / EC50 4.9 ng/mL |
| aripiprazole | Kim 2008 abstract: structure, ka, V/F **192 L** (the brief said 193), intermediate CL about 60% of normal; Kim 2012 abstract: EC50 8.63 ng/mL | genotype-class CL/F, CLm/fm, Vm/fm, the formation convention, ke0 0.725 /h, Emax |
| haloperidol | abstract only: two compartments, 122 subjects; full texts of Park 2006 (CYP2D6 0.77), Lim 2013 (ke0 0.213/h, Ce50 0.791 ng/mL) and Kim 1996 (rifampin, documented only) | every Pilla Reddy parameter (indirectly supported by Li 2022's citation of CL/F 88, total V 3169 L) |
| haloperidolIV | Li 2022 text: CL 51.7 L/h, V 1490 L, IIV 29.9%, IV route | CRP function (not applied) |
| droperidol | Lehmann 1988 full text (Fig. 1 curves, Tables I-III); checked against Gupta 1992, Fischler 1986, Cooper 2018 and Grunwald 1993 full texts | none |
| droperidolIM | Foo 2016 abstract: CL/F, Vc/F, ka, doses, half-lives | Q/F and Vp/F (consistent with the half-lives) |

The Gründer 2008 aripiprazole alternative in the brief (Emax 95%, EC50
20 ng/mL on parent + dehydro) is **not** supported by its abstract (EC50 5 to
10 ng/mL, serum aripiprazole) and was not implemented.

## Known weaknesses

- **IV droperidol** (Lehmann 1988): fitted to the three dose groups' mean
  curves rather than individual data. It reproduces Gupta's 24 h infusion
  plateau (3.2 ng/mL) and Fischler's Vss; Fischler's clearance is higher.
  Scaled to children it overstates clearance and volume about 2.5-fold
  against Grunwald 1993. Cooper 2018, used first, was replaced.
- **Aripiprazole CYP2D6**: poor and ultrarapid metabolisers were not studied
  and take the nearest studied clearance.
- **Risperidone ultrarapid**: extrapolated as three functional alleles.
- **Olanzapine**: smoking, race, rifampin, hepatic and renal effects are not
  applied (no Patient Profile field), so the curve is a nonsmoker's.

## Not registered

- **quetiapineXR**: Brogren and Nyberg 2010 give CL/F and the mean transit
  times, but not the transit number, Q/F or the volumes.
- **chlorpromazine**: Chetty 1994's accessible report has no structural model
  or coefficients, and Yeung 1993 gives summary statistics only.

Both appear in `antipsychoticProfiles()` with `modelStatus = "blocked"` and no
drug, so nothing invented reaches the plot.

## Deferred alternatives

Vermeulen 2007 (risperidone), Zhang 2024 (aripiprazole, parent only), Zang
2021 with its corrected Table 3 (olanzapine), de Greef 2011 (olanzapine
occupancy), and the PANSS model of Pilla Reddy 2013 (needs its placebo and
dropout equations).
