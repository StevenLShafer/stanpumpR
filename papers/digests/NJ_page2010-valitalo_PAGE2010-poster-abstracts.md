---
file: NJ_page2010-valitalo_PAGE2010-poster-abstracts.pdf
pmid: none
doi: none
citation: "Välitalo P, Kumpulainen E, Manner M, Laisalmi M, Lehtonen M, Hooker A, Ranta V-P, Kokki H. Plasma and cerebrospinal fluid pharmacokinetics of naproxen in children. Poster abstract, PAGE 2010 (Poster: Applications - CNS), PAGE 2010 Poster Abstracts booklet pp. 89-90. https://www.page-meeting.org/wp-content/uploads/page/page2010/PosterPAGEAbstracts2010.pdf"
kind: other
drugs: [naproxen]
source_read: "partial: text layer of the 341-page PAGE 2010 poster abstract booklet; table of contents and the one cited abstract (Välitalo, booklet pp. 89-90) only. The other abstracts were not digested (a Björnsson/Simonsson naproxcinod/naproxen dental pain abstract, booklet p. ~45 of text, is present but not cited from this file)."
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# Välitalo 2010: plasma and CSF naproxen PK in children (PAGE poster abstract)

## Study
- Children undergoing lower-body surgery under spinal anaesthesia; 53 enrolled, PK estimated in 51 aged
  3 months to 13 years (abstract text; the later paper says 3 months to 12 years).
- Dose: naproxen oral suspension 10 mg/kg, single dose.
- Data: 270 total plasma, 52 unbound plasma and 52 CSF concentrations.
- Software: NONMEM VI 2.0, PsN 2.3.2; case-deletion diagnostics flagged two individuals (Cook score > 1;
  the criterion text is truncated in the text layer).
- Assay not described in the abstract. Weight, sex distribution, sampling times not given.

## Model and parameters
Two-compartment model with first-order absorption for plasma; CSF described with an intercompartmental
clearance QCSF and an uptake factor UPTK. Clearance linear in body weight (exponent 1); age had no effect
after weight. Values below are typical values scaled to 70 kg as stated in the Results paragraph
(no table in the abstract).

```yaml
parameters:
  - {name: CL/F, value: 0.59, unit: L/h, source: "Results", note: "typical, scaled to 70 kg, linear weight"}
  - {name: V1/F, value: 7.4, unit: L, source: "Results", note: "central, scaled to 70 kg"}
  - {name: V2/F, value: 4.4, unit: L, source: "Results", note: "peripheral, scaled to 70 kg"}
  - {name: UPTK, value: 7.8, unit: none, source: "Results", note: "CSF uptake factor, 20% RSE"}
  - {name: fu, value: 0.14, unit: "%", source: "Results", note: "unbound fraction in plasma, 7.9% RSE"}
```
Not reported in the abstract: Q/F, ka, QCSF value, IIV, residual error, lag time.

## Pharmacodynamics and end points
No PD end point. No ke0, t½ke0, tPeak, MEAC or potency ratio. The only CNS finding is that CSF
concentrations were markedly greater than unbound plasma concentrations (UPTK 7.8), attributed to protein
binding in CSF. (Naproxen is an NSAID, not an opioid; the opioid audit fields do not apply.)

## Covariates
Body weight on CL/F, linear (exponent 1), normalised to 70 kg; the abstract says the volumes are also
"scaled to 70 kg" but does not state their exponent. Age not significant after weight.

## Caveats
- Preliminary conference abstract; the final analysis (Välitalo 2012, J Clin Pharmacol) reports different
  estimates (CL/F 0.62, V1/F 8.2, V2/F 4.3 L per 70 kg) after further modelling.
- Children having surgery under spinal anaesthesia, not healthy volunteers.
- Some individuals had remarkably low concentrations; outlier handling not fully described.
- Scaling from paediatric data (median ~20 kg) to 70 kg with linear weight is an extrapolation.

## Used by stanpumpR
- `docs/nsaid-models.md:52` — cited only to say that the earlier PAGE 2010 poster's estimates are
  preliminary, differ from the 2012 paper, and were not merged (naproxen).

## Verification against stanpumpR
- `docs/nsaid-models.md:52`: the doc says the poster's estimates are preliminary and differ from Table II of
  the 2012 paper, and were not merged. The poster gives CL/F 0.59 L/h, V1/F 7.4 L, V2/F 4.4 L per 70 kg
  versus the doc's 2012 values 0.62, 8.2, 4.3; they do differ. `R/drugs_naproxen.R` uses the 2012 values and
  does not use the poster. **matches.**
- Aside (outside this source's location): `R/drugs_naproxen.R:12` calls the 53 children "healthy"; this
  abstract describes children undergoing lower-body surgery under spinal anaesthesia (the 2012 paper's own
  description should decide this).
