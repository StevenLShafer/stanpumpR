# Antiseizure PK interactions — open questions and review targets

A working checklist for finishing the antiseizure drug-interaction model
(`R/antiseizureInteractions.R`, PR #563). These are the points flagged for a
careful, adversarial review before the model is considered settled. Nothing
here is a defect in what is committed; they are the deliberate assumptions and
the evidence gaps that a reviewer should press on. Drafted 2026-10-24.

## Two behavioural requirements (Steven Shafer, 2026-10-25)

**R1. With "Include antiseizure drug interactions" on, the model reflects the
change in clearance.** *Status: met.* `applyAntiseizureInteractions()` multiplies
each victim's elimination clearance (`cl1`) by the time-varying fold and
re-simulates through the exact event engine; phenytoin, on the saturable
engine, takes Vmax/Km multipliers. All 24 linear-victim interactions are
clearance multipliers; the 2 phenytoin interactions act on Km. Verified in
`test-antiseizure-interactions.R`.

**R2. If the model tracks FREE drug and the interaction is protein binding, it
reflects the change in free fraction.** *Status: not yet needed, but the
architecture should be made explicit — see "The free-fraction question" below.*
No antiseizure drug in the library is plotted as free drug today (all are
total), so the literal condition is never triggered. But the one protein-binding
interaction we have (valproate→phenytoin) deserves the principled treatment R2
describes, and Perucca 1980's full text now lets us give it.

## The free-fraction question (R2) — analysis and recommendation

The pharmacology, laid out so it can be checked:

- **A drug plotted as TOTAL, restrictive hepatic clearance.** Displacement
  raises the free fraction `fu`. For a linear drug, total clearance
  `CL_total = fu · CL_int`, so `fu↑` ⇒ `CL_total↑` ⇒ **total falls**; but free
  steady state `Css_free = dose_rate / CL_int` is **unchanged** by displacement
  alone. For phenytoin (saturable), the same fact appears as `Km_total =
  Km_free / fu`, so `fu↑` ⇒ `Km_total↓`, Vmax unchanged ⇒ **total falls, free
  unchanged**. The model encodes valproate→phenytoin as `Km × 0.615`, which is
  exactly this. **So the current handling is pharmacologically correct for the
  total curve**, and it is the correct expression of a binding change on the
  total scale.
- **A drug plotted as FREE.** A pure displacement does **not** change free
  steady state, so an `fu` change should produce **no change** in a free curve.
  Only a change in intrinsic clearance (metabolic inhibition/induction) moves
  free. The model has no free-drug antiseizure output, so nothing to do yet.
- **The gap that matters clinically.** Valproate does two things to phenytoin:
  (1) displacement (applied, lowers total) and (2) **metabolic inhibition**,
  which Perucca 1980 now quantifies — free intrinsic clearance fell ~18%
  (Cl_i 348→286), so **free phenytoin rose in all 7 subjects**. The model does
  not apply (2), so it shows total falling and implies free is flat, when free
  actually rises. A clinician watching total drop could wrongly raise the dose.

**Recommendation.** Make the mechanism explicit in the registry and engine:
tag each interaction as `clearance` (acts on CL_int / Vmax) or `binding` (acts
on `fu`), and let the engine translate each correctly for whatever the victim
plots:

| Victim plotted as | `binding` (fu ↑) does | `clearance` (CL_int ↓) does |
|---|---|---|
| total | total ↓ (CL_total↑, or Km_total↓) | total ↑ |
| free | nothing (free flat) | free ↑ |

Then, to make valproate→phenytoin faithful, add phenytoin's metabolic-inhibition
arm (Vmax × ~0.82 from Perucca's free-CL −18%) **and** optionally plot free
phenytoin, so the user sees total fall while free rises. This is the single
most clinically important refinement in the whole model and is likely Prof.
Fan's sharpest concern. It is a deliberate, bounded piece of work (a free-drug
output for phenytoin plus a protein-binding model), not a quick edit, so it is
proposed here rather than slipped in.

## Coverage audit (2026-10-25)

Checked every selectable antiseizure drug as victim and perpetrator:

- **Covered as victim:** phenytoin, valproate, lamotrigine, carbamazepine,
  phenobarbital, topiramate, tiagabine, lacosamide, eslicarbazepine, cenobamate
  (pending), zonisamide-on-phenytoin only.
- **Gaps (selectable victims with no applied interaction):**
  - **Zonisamide as victim of inducers** — real (t½ 55→30 h with inducers;
    Willmore 2000). Silva 2025 (PMID 39848412), which is *already the library's
    zonisamide model source*, carries an inducer-drug-load covariate on CL/F.
    Read its full text and apply — self-consistent with the model. **Fillable now.**
  - **Ethosuximide as victim of inducers and valproate** — real (CYP3A4
    substrate). Needs a quantified factor; candidate PMIDs 12102641, 9421101,
    7512905, 3146222, and the Patsalos & Perucca 2003 review.
  - Pentobarbital, levetiracetam: correctly left out (levetiracetam is renally
    cleared and interaction-free; pentobarbital is an acute IV agent).
- **Cenobamate as perpetrator** — now quantified from the XCOPRI label, pending
  implementation (below).
- **Cenobamate as victim — a conflict to resolve.** The applied row uses Vashi
  2023's pop-PK covariate (carbamazepine ×1.15 on cenobamate CL/F). The XCOPRI
  label's *dedicated* DDI study found carbamazepine **not** significant on
  cenobamate, and **phenytoin** lowered cenobamate exposure 27–28% (CL ×1.38).
  The label should win: replace the carbamazepine→cenobamate row with
  phenytoin→cenobamate ×1.38.

## Cenobamate (XCOPRI label read 2026-10-25) — numbers and why not yet applied

From NDA 212839 §12.3 (cenobamate 200 mg/day as perpetrator): phenytoin AUC
**+84%** (Cmax +70%), phenobarbital AUC **+37%**, carbamazepine AUC **−23%**,
lamotrigine **−21% to −52%** across cenobamate 100–400 mg/day; valproate,
lacosamide and levetiracetam not significantly changed. All four magnitudes are
in the registry as `recorded`. They are **not yet applied** because cenobamate
is harder than the others and should be done deliberately:

1. **No therapeutic band.** The saturable `fold(C)` needs a reference
   perpetrator concentration; cenobamate has none. Use the model's steady-state
   exposure at 200 mg/day (the DDI study dose) as `c_ref`, and — uniquely — the
   lamotrigine **21% at 100 / 52% at 400 mg/day** span as a genuine two-point
   concentration anchor.
2. **Phenytoin is the saturable victim.** +84% total maps to Vmax × ~0.83 at the
   band midpoint, but the effect is operating-point dependent (larger near
   saturation) — arguably more correct, but it needs stating and testing.
3. **The victim conflict** above must be resolved in the same pass.

I can implement all of this on your go-ahead; I held it back so the reference-
concentration choice and the saturable mapping get the adversarial review rather
than landing unreviewed.

## Review sequence

1. Obtain the full-text PDFs below and re-derive every number currently read
   from an abstract.
2. Add the cenobamate magnitudes from the XCOPRI label (§7 / §12.3).
3. Run the adversarial reviews (ChatGPT, Gemini) against this file's assumptions.
4. From all of the above, prepare the question list for the clinical reviewer.

## Full text obtained and re-derived (2026-10-24)

Six of the eight download-list papers were read in full on 2026-10-24; every
applied number they supply was **confirmed exactly**, and the SEs/CIs are now
in the registry. Findings that bear on the assumptions below:

- **Graves 1998** (PMID 9545146) — carbamazepine, the library's own model.
  Table 4 (95% CI): PHT ×1.42 (1.33–1.51), PHB/FBM ×1.17 (1.10–1.24), the dual
  term ×1.62 (1.44–1.80), all 0/1 indicators. The dual term is a **fitted
  interaction term**, a direct combination-rule data point (see assumption 2).
  **Valproate was tested and NOT significant on parent CBZ CL/F** (Table 3) —
  supports leaving valproate→carbamazepine unapplied.
- **Rivas 2008** (PMID 18641550) — lamotrigine. Table 3 (SE%): VPA −0.713
  (7.66%), PHT 0.663 (10.22%), PHB/PRM 0.588 (8.66%), CBZ 0.467 (**29.6%**, the
  least precise), ≥2 inducers 0.864 (12.17%). Multiplicative in the exponentials;
  induction × inhibition balance near monotherapy (supports the model's
  multiply rule). The ≥2-inducer term ×2.37 is the second combination data point.
- **Gidal 2003** (PMID 15013050) — Table 2: Emax 64.9% (SE 7.2, CI 50.1–79.7),
  EC50 5.6 mcg/mL (SE 2.3, CI 0.8–10.4). Confirms the applied EC50; raises a
  calibration question (assumption 1).
- **Odani 1996** (PMID 8924916) — Table 2: θ_ZNS 1.16 (CI 1.07–1.25); Vmax 9.80,
  Km 9.19, weight power 0.463 confirm the library's phenytoin model. Zonisamide
  inhibits phenytoin metabolism competitively.
- **Perucca 1980** (PMID 6777108) — the quantified total-vs-free split
  (assumption 3): fu 9.6%→15.6%, total CL +30% but **free intrinsic CL −18%**
  (Cl_i 348→286), so free phenytoin rises. Single-dose IV, non-saturating.
- **Jiao 2004** (PMID 15153086) — Table 5: VPA reduces CBZ-epoxide CL ×0.693
  (0.615–0.771). Table 4: VPA (>18 mg/kg) on parent CBZ ×1.44, an **increase**
  (contradicts Graves's null); PHT ×1.26, PB ×1.23 (differ from Graves's US
  values). Epoxide not plotted, so still `recorded`.

**Samara 1998** (PMID 9701378, tiagabine) was also read in full on 2026-10-24:
central CL/F 21.4 L/h induced vs 12.8 noninduced (+67%, CV 28%), and inducers
explicitly **not additive** (a third combination data point; assumption 2).

**Cenobamate XCOPRI label** (NDA 212839) — obtained and read 2026-10-25; the
perpetrator magnitudes are now in the registry (pending application).

## Additional papers needed (verified against PubMed, 2026-10-25)

Ranked by what each unblocks:

1. **Falcao 2012** (PMID 22171585, eslicarbazepine, CNS Drugs) — confirms the
   three applied eslicarbazepine inducer factors. Not accessible to the author;
   being requested via Stanford DocXpress. Until then those rows stay
   abstract-sourced.
2. **Silva 2025** (PMID 39848412, Eur J Pharm Sci, zonisamide) — the inducer
   drug-load factor to apply inducers→zonisamide. It is already the library's
   zonisamide model source, so the full text is likely in hand; read the INDDL
   coefficient. [doi 10.1016/j.ejps.2025.107023]
3. **An ethosuximide inducer/valproate source** — to apply inducers→ethosuximide
   and valproate→ethosuximide. Candidates to screen: PMIDs 12102641, 9421101,
   7512905, 3146222.
4. **Patsalos & Perucca 2003, Lancet Neurol, Part I** (PMID 12849151, AED–AED
   interactions) — the authoritative consensus review to **cross-check every
   applied magnitude** against before the model is finalized. [doi
   10.1016/s1474-4422(03)00409-5]. (Part II, PMID 12878435, covers AED–other
   drugs and is less relevant here.)
5. **Willmore 2000** (PMID 11200305, Drugs Aging) — a secondary cross-check for
   zonisamide's t½ 55→30 h with inducers. [doi 10.2165/00002512-200017060-00002]

Lower priority (would upgrade `recorded`/abstract rows, not change applied
numbers): Bertilsson 1986 (PMID 3734137, CBZ autoinduction), Teixeira-da-Silva
2020 (PMID 32707172, phenobarbital), Wu 2025 (PMID 40960184, lacosamide),
Marchand 2010 (PMID 20084538, rufinamide), Giraud 2006 (PMID 16415114,
stiripentol).

For the free-phenytoin extension (R2), if pursued: a phenytoin albumin-binding
model (Winter–Tozer / Sheiner–Tozer) and valproate's concentration-dependent
(saturable) binding, to drive `fu` for both the displacement and a free output.

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
- **Data point (valproate→lamotrigine), now settled from full text:** Gidal
  2003's directly fitted Emax is 64.9% max inhibition (fold→0.351). Anchoring
  the model's Fmax to Rivas's categorical 0.49 at valproate 75 mcg/mL with
  EC50 5.6 gives only 54.8% max (Fmax 0.452). At valproate 75, Gidal's Emax
  predicts fold 0.40 vs Rivas's 0.49. The two disagree (Gidal single-dose
  volunteers vs Rivas TDM patients). **Decision for review:** adopt Gidal's
  Emax directly for this one interaction (we have Emax, EC50 and their SEs), or
  keep the Rivas-anchored value? This is the only interaction with a measured
  concentration-response, so it is the test case for the whole saturable form.

## Assumption 2 — additive-excess vs multiplicative combination of inducers

Multiple inducers of one victim combine as `E = 1 + Σ(Eᵢ − 1)` (excess adds);
inhibitions multiply; a combined induction then multiplies any inhibition.
Chosen because Rivas's own two-inducer term (×2.37) is closer to additive-excess
(×2.54) than to multiplicative (×3.10), and Grasela 1999 found a second inducer
added only ~13%.

**Two data points, now from full text:**
- **Graves 1998 (carbamazepine victim):** the dual inducer term is a fitted
  interaction indicator = 1.62 (CI 1.44–1.80). Additive-excess predicts 1.59,
  multiplicative 1.66 — additive is marginally closer (|Δ| 0.03 vs 0.04).
- **Rivas 2008 (lamotrigine victim):** the ≥2-inducer term = 2.37 (SE 12.17%).
  For PHT+CBZ, additive-excess predicts 2.54 and multiplicative 3.10 — additive
  is much closer but still **over-predicts by ~7%** (2.54 vs 2.37). Rivas's term
  is sub-additive relative to our rule.
- **Samara 1998 (tiagabine victim):** the strongest sub-additivity signal — the
  authors state inducers are "not additive since the CL values on the average
  were similar when one or more inducing AEDs were administered concurrently."
  Table 2: carbamazepine alone 24.6 L/h vs carbamazepine+phenytoin 27.1 L/h
  (barely higher), where phenytoin alone is 28.0. So two inducers ≈ one. The
  tiagabine model captures this because it uses a single inducer indicator, not
  a per-drug stack; but it means our additive-excess rule for lamotrigine likely
  over-predicts two-inducer induction.

Taken together (three victims): additive-excess beats multiplicative on all,
but every one shows induction is **sub-additive** — slightly for Graves,
~7% for Rivas, strongly for Samara. A reviewer may prefer a sub-additive rule
(e.g. cap the combined excess, scale it down, or use a saturating combination).

Review questions:
- Is additive-excess right across all victims, or only lamotrigine/carbamazepine
  (where the data are)? Enzyme induction is often sub-additive (shared CYP pools
  saturate); multiplicative would over-predict triple therapy.
- Should the combined induction be scaled down to match Rivas's 2.37 exactly
  (our 2.54 is ~7% high), or is the current additive-excess close enough given
  the SEs?
- Autoinduction is combined with a second inducer by counting the second
  inducer's excess in units of the **uninduced** clearance (so carbamazepine +
  phenytoin reproduces Graves's ×1.42 on the induced state). Confirmed against
  the full Graves equation (the single terms are 0/1 indicators on CL/F).

## Assumption 3 — total vs free phenytoin with valproate (clinical)

The library plots **total** phenytoin. Valproate both displaces phenytoin from
albumin (lowers total, applied as Km ×0.615) and inhibits its metabolism (raises
free, **not** applied — not quantified in the abstract). So the model shows total
falling while the clinically important free concentration does not. This is
documented in the help page and the scenario, but it is the sharpest clinical
trap in the group.

**Now quantified from Perucca 1980 full text:** valproate raised the free
fraction 9.6%→15.6% and total clearance ~30%, but the **free intrinsic
clearance FELL ~18%** (Cl_i 348→286, p<0.01) and free Vd fell, so unbound
phenytoin was higher in all 7 subjects. So the real picture with valproate is:
plotted total falls, active free **rises**. The model currently shows only the
first half. We now have the numbers to add the second (free CL ×0.82 alongside
the displacement). Caveat: single-dose IV, non-saturating; the authors expect
the effect larger at steady state under saturation.

Review questions:
- Is showing a falling total, with a help-page caveat, acceptable — or should
  the model plot free phenytoin (which rises ~) alongside the total when
  valproate is co-dosed? We now have Perucca's free-clearance factor (×0.82) to
  do it.
- This is likely Prof. Fan's sharpest concern: a clinician seeing total
  phenytoin drop into "subtherapeutic" after adding valproate could wrongly
  increase the dose. The free level is the guide.

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
