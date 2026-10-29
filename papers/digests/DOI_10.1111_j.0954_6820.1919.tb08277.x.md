---
file: DOI_10.1111_j.0954_6820.1919.tb08277.x.pdf
pmid: none
doi: 10.1111/j.0954-6820.1919.tb08277.x
citation: Widmark EMP. Studies in the concentration of indifferent narcotics in blood and tissues. Acta Med Scand 1919;52:87-164.
kind: methods
drugs: []
source_read: text layer (all 78 pages; OCR garbles equations and tables) plus page images of pp117-119 for the infusion equations
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: partly-verified
---

# Widmark 1919: acetone kinetics, one-compartment elimination, steady state under continuous supply

## Study
Experimental and theoretical study of acetone as a model "indifferent narcotic" (Lund). Blood acetone by the
author's iodometric micromethod (mean error of a single estimation about 0.02 per mille, p109). Rabbits
(about 1.6-3.1 kg; subcutaneous, intraperitoneal, intravenous under urethane, oral, rectal; typically
0.35 or 0.70 mg/g) and the author himself (E.W., 73 kg; 10-16 g acetone orally or rectally) plus a second
subject (B.J.). Sampling over up to about 30 h. Also re-analyses Nicloux's chloroform, ether and methyl alcohol
blood data.

## Model and parameters
- Elimination phase is log-linear: c_t = c_0 e^(-alpha t), fitted by least squares on ln c; the fall is
  proportional to the existing concentration (first order, "monomolecular reaction"), pp102-110.
- "Reduced bodily volume" m = a_0 / c_0 (dose / back-extrapolated initial concentration): the apparent volume
  of distribution of a single compartment (pp112-115).
- Constant supply (p117, read from page image): with B mg per unit time, at steady state B dt = A alpha dt, so
  A = B / alpha and C = B / (alpha m). Suggested routes: "permanent intravenous injection" or drop-clyster.
- Intermittent dosing every p minutes (p119): body amount oscillates between a maximum
  D_t = p alpha A / (1 - e^(-alpha p)) and minimum D_t e^(-alpha p); mean A = B / (p alpha), same mean as
  continuous supply at the same rate.
- Loading plus maintenance (p120, Exp 11): small periodic doses alone take a very long time (theoretically
  never) to reach equilibrium; a larger first dose followed by smaller periodic doses holds a roughly constant
  concentration. Rabbit 3135 g: 1097 mg (0.35 mg/g) s.c., then 53.3 mg every 30 min; last six samples averaged
  0.358 per mille; alpha back-calculated 0.00177 /min.
- No explicit time-course equation for the approach to steady state (1 - e^(-alpha t)) during a constant
  infusion is given; only the steady state and the loading-dose remedy.

```yaml
parameters:
  - {name: alpha_human_EW, value: 0.000648, unit: 1/min, source: "Exp 10, Table 17 / p116", note: acetone, rectal, 73 kg}
  - {name: m_human, value: [57, 64], unit: L, source: "Table 22", note: "m/weight 0.74-0.80, mean 0.77"}
  - {name: alpha_rabbit, value: [0.00080, 0.00216], unit: 1/min, source: "Tables 23-24", note: "0.00080 in a starving rabbit"}
  - {name: m_over_weight_rabbit, value: 0.90, unit: L/kg, source: "Table 21", note: "range 0.82-0.99"}
  - {name: exhalation_rate_constant_human, value: 0.000316, unit: 1/min, source: p130, note: "V 6 L/min, m 57 L, partition 0.003"}
  - {name: steady_state, value: "C = B/(alpha m)", unit: "", source: p117}
```

## Pharmacodynamics and end points
No concentration-effect measurements. Qualitative: narcosis depends on concentration in sensitive tissue; rapid
IV or inhalational input causes temporary "cumulation" in blood and well-perfused tissue (brain) above the
equilibrium value, so less drug is needed for narcosis and recovery is faster if stopped before equilibrium
(pp159-161) — an early statement of distribution/redistribution. Rabbit IV "time of partition" about 60 min
(p145). Chloroform and ether fall too fast in the first 15-30 min after narcosis, attributed to diffusion into
tissues. Not an opioid: no MEAC, ke0 or potency.

## Covariates
None modelled; m/weight varies (more fat, gut contents in rabbits); alpha varies widely between animals.

## Caveats
Acetone, not an anaesthetic; mostly rabbits; one or two human subjects; single-compartment treatment that the
author himself shows fails in the first hour after IV injection. OCR text is unreliable for numbers.

## Used by stanpumpR
- inst/help/history.md:11 — "Widmark described the accumulation of a drug during a constant infusion in 1919,
  for a single compartment."
- inst/help/references.md:27 — citation.

## Verification against stanpumpR
- history.md:11: the paper does treat continuous supply in a single compartment (m) with first-order loss and
  derives the steady state C = B/(alpha m), plus intermittent dosing and loading-plus-maintenance. It does not
  derive the accumulation curve over time; it states only that equilibrium is approached slowly. "Described
  the accumulation ... during a constant infusion" is a fair paraphrase if read as "steady state of"; strictly
  the time course is not in the paper. **Matches in substance (minor overstatement).** The help also omits the
  loading-dose idea (Exp 11), which is the more direct TCI antecedent — an omission, not an error.
- references.md:27: author, title, journal, year 1919, vol 52, pages 87-164 agree with the PDF (pp87-164).
  **Matches.**
