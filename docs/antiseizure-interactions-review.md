# Antiseizure PK interactions — open questions and review targets

A working checklist for finishing the antiseizure drug-interaction model
(`R/antiseizureInteractions.R`, PR #563). These are the points flagged for a
careful, adversarial review before the model is considered settled. Nothing
here is a defect in what is committed; they are the deliberate assumptions and
the evidence gaps that a reviewer should press on. Drafted 2026-10-24.

## Review sequence

1. Obtain the full-text PDFs below and re-derive every number currently read
   from an abstract.
2. Add the cenobamate magnitudes from the XCOPRI label (§7 / §12.3).
3. Run the adversarial reviews (ChatGPT, Gemini) against this file's assumptions.
4. From all of the above, prepare the question list for the clinical reviewer.

## Full text to obtain, then re-derive (currently abstract-sourced)

Each of these supplies a number the model applies; only the abstract was read.
The registry marks them `to_obtain = "yes"`. The full text should confirm the
point estimate **and** give the uncertainty (SE/RSE/CI), which the abstract
usually omits.

- **Graves 1998** (PMID 9545146) — carbamazepine CL/F, the library's own model:
  phenytoin ×1.42, phenobarbital/felbamate ×1.17, the dual term ×1.62. Confirm
  the equation, the IIV and that the dual term is what the additive-excess rule
  should reproduce (it gives 1.59 against the reported 1.62).
- **Rivas 2008** (PMID 18641550) — lamotrigine inducer and valproate factors.
  Confirm the exponential coefficients and get the SEs; confirm the
  "two or more inducers" ×2.37 term, which anchors the combination rule.
- **Gidal 2003** (PMID 15013050) — the valproate→lamotrigine EC50 (≈5.6 mcg/mL)
  and max inhibition (~65%). The one measured concentration dependence.
- **Odani 1996** (PMID 8924916) — phenytoin Vmax/Km and the zonisamide Km ×1.16.
- **Jiao 2004** (PMID 15153086) — the only pop-PK with a carbamazepine-epoxide
  clearance term (valproate); equations are in the full text only.
- **Perucca 1980** (PMID 6777108) — the valproate→phenytoin split: how much is
  albumin displacement (applied, lowers total) vs metabolic inhibition
  (not applied, raises free). Needed to decide assumption 3 below.
- **Falcao 2012** (PMID 22171585), **Samara 1998** (PMID 9701378) — eslicarbazepine
  and tiagabine inducer factors.

## Cenobamate (XCOPRI label, pending upload)

Currently two `recorded` (not applied) rows: cenobamate → phenytoin and
→ lamotrigine. The label (§7 Drug Interactions, §12.3 Pharmacokinetics) gives
the fold-changes. On upload:
- promote those two rows to `applied` with the label's magnitudes and
  `ec50_basis = "product label (XCOPRI)"`;
- add phenobarbital (label reports an increase) and, if clobazam is ever added
  to the library, clobazam / N-desmethylclobazam;
- cross-check the already-applied cenobamate → carbamazepine (+15%, Vashi 2023)
  against the label's own number.
The mechanism is settled (cenobamate inhibits CYP2C19, induces CYP3A4/2B6); only
the magnitudes are label-sourced.

## Assumption 1 — a categorical factor modelled as saturable in concentration

Almost every source gives a **categorical** covariate ("CL ×1.94 in patients on
phenytoin"), measured at ordinary maintenance doses. The model turns each into a
saturable function of the perpetrator's own plotted concentration,
`fold(C) = 1 + (Fmax−1)·C/(EC50+C)`, calibrated so `fold(Cref)` equals the
published factor at the perpetrator's typical band value `Cref`. Where no EC50 is
measured (everywhere except valproate→lamotrigine), **EC50 defaults to the lower
edge of the perpetrator's reference band**.

Review questions:
- Is a concentration-dependent form defensible at all, given the sources are
  categorical? The alternative is a step function (full effect whenever the
  perpetrator is present at any level), which is what the population models
  literally fitted.
- Is "EC50 = lower band edge" the right default? Across the band it moves the
  effect by about −25% to +10% around the published factor. A reviewer may
  prefer EC50 = 0 (effect flat at the published factor for any C > 0) or a
  mechanism-based Ki where one exists.
- For **induction**, the perpetrator concentration drives enzyme turnover, so
  the effect already lags; adding concentration dependence on top may
  double-count. Consider whether induction should use a fixed target factor
  (step) with only the turnover providing the time course.

## Assumption 2 — additive-excess vs multiplicative combination of inducers

Multiple inducers of one victim combine as `E = 1 + Σ(Eᵢ − 1)` (excess adds);
inhibitions multiply; a combined induction then multiplies any inhibition.
Chosen because Rivas's own two-inducer term (×2.37) is closer to additive-excess
(×2.54) than to multiplicative (×3.10), and Grasela 1999 found a second inducer
added only ~13%.

Review questions:
- Is additive-excess right across all victims, or only lamotrigine (where the
  data are)? Enzyme induction is often sub-additive (shared CYP pools saturate);
  multiplicative would over-predict triple therapy.
- Autoinduction is combined with a second inducer by counting the second
  inducer's excess in units of the **uninduced** clearance (so carbamazepine +
  phenytoin reproduces Graves's ×1.42 on the induced state). Confirm this is the
  intended semantics with the full Graves equation.

## Assumption 3 — total vs free phenytoin with valproate (clinical)

The library plots **total** phenytoin. Valproate both displaces phenytoin from
albumin (lowers total, applied as Km ×0.615) and inhibits its metabolism (raises
free, **not** applied — not quantified in the abstract). So the model shows total
falling while the clinically important free concentration does not. This is
documented in the help page and the scenario, but it is the sharpest clinical
trap in the group.

Review questions:
- Is showing a falling total, with a help-page caveat, acceptable — or does the
  model need to plot free phenytoin (or both) when valproate is co-dosed?
- Perucca 1980's full text (above) may let us quantify the metabolic-inhibition
  arm and show free phenytoin rising, which would be more faithful.

## Not modelled (by design) — confirm each is acceptable

- Metabolites: carbamazepine-10,11-epoxide (valproate raises it via epoxide
  hydrolase), N-desmethylclobazam.
- Interactions with drugs outside the antiseizure group (oral contraceptives,
  rifampicin, azoles).
- Any pharmacodynamic interaction (the drugs are plasma-only in the library).
- The small/ambiguous induction **of** phenytoin by carbamazepine/phenobarbital
  (recorded, Yukawa 1990): phenytoin levels rise or fall with carbamazepine, and
  the pooled co-medication term is not cleanly per-perpetrator.

## Questions for the clinical reviewer

To be assembled after the full-text re-derivation and the adversarial reviews.
Seeded by the three assumptions above and the "not modelled" list.
