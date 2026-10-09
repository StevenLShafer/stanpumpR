# Temazepam in stanpumpR: how the model was derived

No two-compartment pharmacokinetic model of temazepam has been published.
stanpumpR's model is therefore **derived**. It is one two-compartment model
fitted by least squares to the published mean data of two intravenous
studies. Oral absorption is then added from a third study and the label.

Steven L. Shafer reviewed the derivation and decided to keep the model as it
stands (2026-10-09). This document records the method so that the fit can be
checked and reproduced.

| | |
|---|---|
| Model code | `R/drugs_temazepam.R` |
| Fit script | `data-raw/temazepam-fit.R` (base R; `Rscript data-raw/temazepam-fit.R` from the repository root, about 2 s) |
| Tests | `tests/testthat/test-drugs-temazepam.R`, including a check that the shipped constants are the minimum of the objective below |
| Help page | `inst/help/drugs/temazepam.md` |
| Source check | `docs/sedatives-verification.md` |

Drafted with Claude Code at the request of Steven L. Shafer, 2026-10-09.

---

## 1. Why a derived model

- **The specification's model could not reach the observed peaks.** The
  ChatGPT specification that started this work proposed a one-compartment
  reduction of Ochs 1984: 20 mg orally in 10 young adults, V 1.45 L/kg, CL
  2.33 mL/min/kg, half-life 8.6 h. Thirty milligrams spread through 1.45 L/kg
  gives at most about 296 ng/mL. The Restoril label reports a mean peak of
  865 ng/mL (range 666–982).
- **The decline is biphasic.** The label gives a distribution half-life of
  0.4–0.6 h, so a second compartment is needed.
- **No paper reports one.** No two-compartment parameter set was found.
  van Steveninck 1994 fitted two compartments to each subject but published
  only each subject's end points: peak, AUCs and half-life.
- **Two intravenous studies report usable means.**
  - van Steveninck 1994 (part II): a 30-min infusion, end points tabulated.
  - Halliday 1987: a 20-s injection, means plotted for 2 h.

  Oral data cannot separate absorption from distribution, so the
  disposition is fitted to these intravenous data alone.

## 2. The data

### van Steveninck AL et al., Clin Pharmacol Ther 1994;55:546-555 (part II)

Nine healthy volunteers (four men, five women, 18–24 years) were each
infused at 0.8 mg/kg/h for up to 30 minutes, about 0.4 mg/kg. Each was
studied on two occasions six months apart. Samples were taken from the other
arm for 24 hours and assayed by HPLC. Each subject was fitted with two
compartments. Table I lists each subject's end points with their means; the
text gives the mean weights. The fit uses the average of the two occasions:

| | Occasion 1 | Occasion 2 | Used |
|---|---|---|---|
| Weight, kg (text) | 66 | 67 | 66.5 |
| Dose, mg | 26.1 ± 5.4 | 25.6 ± 6.2 | 25.85 |
| Infusion, min | 29 ± 2 | 28 ± 3 | 28.5 |
| Cmax, ng/mL | 964 ± 167 | 1028 ± 173 | 996 |
| AUC 0–3 h, µg·h/mL | 1.4 | 1.4 | 1.4 |
| AUC 0–8 h, µg·h/mL | 2.9 | 2.7 | 2.8 |
| AUC 0–∞, µg·h/mL | 6.4 | 5.9 | 6.15 |
| Half-life, h | 10.7 | 10.4 | 10.55 |

The paper takes Cmax from the measured concentrations. It computes AUCs by
the trapezoid rule, and extrapolates AUC 0–∞ as the last concentration
divided by the terminal rate constant.

### Halliday NJ et al., Br J Anaesth 1987;59:465-467

- **Subjects:** eleven fasting young volunteers, mean 22 years and 68 kg.
- **Doses:** each received 20 mg intravenously over 20 s in two research
  solutions, 90% propylene glycol and 40% sodium salicylate, on separate
  occasions. Each also took a capsule and an elixir.
- **Sampling:** venous blood from the other arm at 0, 5, 10, 15, 30, 60, 90
  and 120 min.
- **Results:** the means are plotted in Figure 1, without SDs and without a
  table.

The two intravenous curves did not differ, so their average was read from
the figure by eye. No digitising software was used, and the readings are
approximate:

| min | 5 | 10 | 15 | 30 | 60 | 90 | 120 |
|---|---|---|---|---|---|---|---|
| ng/mL | 1250 | 1010 | 910 | 810 | 625 | 500 | 420 |

### The two studies disagree

Simulated at the same dose and weight, Halliday's concentrations are about
1.6 times van Steveninck's over the first hours. Neither paper explains the
difference. Several factors differ between the studies, and any could
contribute; nothing here tests which:
- the assay laboratory
- the vehicle
- the injection rate
- protein binding, which van Steveninck part I found to change with free
  fatty acids

A model fitted to either study alone therefore misdescribes the other.

## 3. The model fitted

Two compartments with first-order elimination from the central compartment,
in **per-kilogram** parameters: V1 and V2 in L/kg, CL and Q in L/h/kg. Both
studies dosed by weight or reported a mean weight, and per-kg parameters let
each be simulated at its own mean weight. The rate constants are k10 = CL/V1,
k12 = Q/V1 and k21 = Q/V2.

Each study is simulated as it was given, with no dose normalisation:

- **van Steveninck:** a constant-rate infusion of 25.85 mg over 28.5 min at
  66.5 kg. The five end points are computed the way the paper defines them:
  - Cmax: the highest concentration, which is at the end of the infusion.
  - AUC 0–3 h and AUC 0–8 h: the trapezoid rule on a 0.5-min grid.
  - AUC 0–∞: dose / (CL × weight).
  - Half-life: ln 2 / β, where β is the smaller of the two exponents.
- **Halliday:** an infusion of 20 mg over 20 s at 68 kg. The concentrations
  are taken at the seven sampling times.

The solution is exact. The script uses the eigen-decomposition of the
amount matrix; the shipped engine uses its closed-form coefficients; the
tests check that the two agree to 1e-8.

## 4. The objective and the fit

The objective is the sum of squared log ratios, model over observed:

```
SS = Σ(5 van Steveninck end points) [ln(model / observed)]²
   + Σ(7 Halliday points)          [ln(model / observed)]²
```

- **Log ratios** put a 10% miss on a peak, an AUC and a half-life on the same
  footing, whatever their units.
- **Each of the 12 terms has equal weight.** No study is preferred. With
  seven points against five, Halliday contributes slightly more terms. The
  effect of that choice is tested in section 6.

Minimisation:
- Nelder-Mead (`stats::optim`) on the logarithms of the four parameters,
  which keeps them positive, with `reltol = 1e-12`.
- Restarted from its own answer until the objective stops falling.
- Started from the fit to van Steveninck alone, which was itself started
  from generic values (V1 0.3, V2 0.7 L/kg; CL 0.063, Q 0.6 L/h/kg).

## 5. Result

| Parameter | Per kg | At 70 kg (switch off) |
|---|---|---|
| V1 | 0.2784 L/kg | 19.49 L |
| V2 | 0.5231 L/kg | 36.62 L |
| CL | 0.0661 L/h/kg (1.10 mL/min/kg) | 4.63 L/h |
| Q | 0.1115 L/h/kg | 7.81 L/h |

The constants are rounded to 4 decimals. The objective is 0.258164 at the
optimum and 0.258165 at the rounded values.

Derived quantities: half-lives 0.88 h and 10.8 h; Vss 0.80 L/kg.

**Against van Steveninck** (model / observed):

| Cmax | AUC 0–3 h | AUC 0–8 h | AUC 0–∞ | Half-life |
|---|---|---|---|---|
| 1208 / 996 = **1.21** | 1976 / 1400 = **1.41** | 3164 / 2800 = **1.13** | 5881 / 6150 = **0.96** | 10.8 / 10.55 = **1.02** |

**Against Halliday** (model / read):

| min | 5 | 10 | 15 | 30 | 60 | 90 | 120 |
|---|---|---|---|---|---|---|---|
| model | 1004 | 953 | 905 | 778 | 587 | 456 | 366 |
| ratio | **0.80** | 0.94 | 0.99 | 0.96 | 0.94 | 0.91 | 0.87 |

The fit splits the difference between the two studies in the first hours:
- It sits above van Steveninck's early exposure (Cmax 1.21, AUC 0–3 h 1.41).
- It sits below Halliday's (0.87–0.99 from 10 min on).
- It matches van Steveninck's total exposure and half-life within 4%.

The 5-min point is the worst fitted (0.80). A 5-min venous sample after a
20-s injection is the one most affected by mixing, which a mammillary model
does not describe.

**Plausibility against data not used in the fit:**
- **Clearance**, 1.10 mL/min/kg, lies within the oral literature:
  - 1.03 (Ochs 1986)
  - 1.02 in women and 1.35 in men (Divoll 1981)
  - 1.59 (Greenblatt 1984)
  - 2.33 (Ochs 1984)
- **Distribution half-life**, 0.88 h, is close to the 1.03 h Müller fitted
  after morning oral doses. Drake found 0.5 h; the label says 0.4–0.6 h.

## 6. Alternatives tested

| Fit | V1 | CL | Q | V2 | Half-lives (h) | Halliday ratios | 20 mg oral peak, 70 kg |
|---|---|---|---|---|---|---|---|
| van Steveninck alone | 0.2787 | 0.0626 | 0.4014 | 0.6025 | 0.31, 10.5 | 0.56–0.81 | 391 ng/mL at 41 min |
| **Joint, equal weights (shipped)** | 0.2784 | 0.0661 | 0.1115 | 0.5231 | 0.88, 10.8 | 0.80–0.99 | 545 at 55 min |
| Joint, Halliday weight × 0.5 | 0.2905 | 0.0658 | 0.1273 | 0.5342 | 0.83, 10.8 | — | 517 at 55 min |
| Joint, Halliday weight × 2 | 0.2695 | 0.0659 | 0.1025 | 0.5105 | 0.91, 10.7 | — | 566 at 56 min |

V1 and V2 are in L/kg; CL and Q in L/h/kg. The oral column uses the absorption
in section 7; it was not part of any fit.

- **van Steveninck alone** reproduces its own five end points within 1.5%.
  However:
  - It puts Halliday's 30–120 min concentrations 35–44% low.
  - Its 20 mg oral peak of 391 ng/mL is below most of the oral studies (section 7).
  - Its distribution half-life, 0.31 h, is shorter than the label's 0.4–0.6 h.

  It was not used.
- **The weighting** barely moves clearance: 0.0658–0.0661 L/h/kg. It moves
  V1 by ±4% and Q by about ±14%. The 20 mg oral peak changes by about ±5%,
  much less than the twofold spread between oral studies. Equal weighting
  was kept.
- **One compartment** (the specification's Ochs reduction) cannot reach the
  oral peaks (section 1).

## 7. The oral route (not fitted)

**Absorption** is added to the fitted disposition from sources outside the fit:

- **Bioavailability 0.92.** The Restoril label reports "minimal (8%) first
  pass metabolism".
- **Absorption half-life 0.38 h, no lag.** Müller 1987 gave 20 mg in a soft
  gelatin capsule to 12 men at 09:00 and 22:00. The fitted absorption
  half-life was 0.38 h in the morning and 0.53 h at night. The morning value
  is used: it matches the premedication setting and the daytime conditions
  of the intravenous studies.

**Checks** in the reference patient (70 kg, 170 cm, 35-year-old man). None of
these data were used in the fit:

| Dose | Model | Observed |
|---|---|---|
| 20 mg, peak | 545 ng/mL at 55 min | Müller, morning: 510 at 1.02 h; night: 362 at 1.67 h. Drake 1991 (n = 24): 617–708 at 30–40 min |
| 20 mg, 2 h | 411 ng/mL | Halliday's capsule: about 370 and still rising |
| 30 mg, peak | 818 ng/mL | Label: 865 (666–982) at 1.5 h. Greenblatt 1984: 560 at 2.0 h |
| 30 mg nightly, day 7, 9 h after the dose | 217 ng/mL | Label (days 2–7): 260 ± 210 |
| 30 mg nightly, day 7, 24 h after the dose | 82 ng/mL | Label (days 2–7): 75 ± 80 |

Single-dose peaks vary twofold between studies with formulation and time of
day. The model's peaks sit within that range.

The intravenous route is not offered. Both of Halliday's solutions caused
"an unacceptably high incidence of venous thrombosis", and neither became a
product. The intravenous data set only the disposition beneath the oral route.

## 8. Body size

The infusion was dosed per kilogram and the parameters are per kilogram, so
the rate constants are fixed.
- **Fat-free-mass switch off:** volumes and clearances all scale with
  weight / 70.
- **Switch on (the default):** volumes scale with fat-free mass relative to
  the 70 kg, 170 cm reference male, and clearances with that ratio ^ 0.75
  (`docs/weight-adjustment.md`).

The reference man gets the same values either way.

## 9. Limitations

- **The model is fitted to means, not to individuals.** It describes a
  typical curve, carries no variability, and does not estimate the
  parameters' uncertainty.
- **Halliday's points were read by eye** from a printed figure.
- **The two source studies disagree** by about 1.6-fold. The model lies
  between them, 21–41% above van Steveninck's early exposure and up to 20%
  below Halliday's.
- **Not modelled:**
  - sex: clearance about 25% lower and half-life longer in women (Divoll 1981)
  - the hard capsule's slower absorption
  - night-time dosing
  - protein binding that varies with free fatty acids
  - the glucuronide, which is inactive
- **No effect site.** van Steveninck found proteresis, not hysteresis, so
  only the plasma concentration is plotted (header of `R/drugs_temazepam.R`).

## References

- van Steveninck AL et al. Effects of intravenous temazepam. II. Clin Pharmacol Ther 1994;55:546-555. https://doi.org/10.1038/clpt.1994.68
- van Steveninck AL et al. Effects of intravenous temazepam. I. Clin Pharmacol Ther 1994;55:535-545. https://doi.org/10.1038/clpt.1994.67
- Halliday NJ et al. Br J Anaesth 1987;59:465-467. https://doi.org/10.1093/bja/59.4.465
- Müller FO et al. Eur J Clin Pharmacol 1987;33:211-214. https://doi.org/10.1007/BF00544571
- Drake J et al. J Clin Pharm Ther 1991;16:345-351. https://doi.org/10.1111/j.1365-2710.1991.tb00324.x
- Greenblatt DJ et al. J Pharm Sci 1984;73:399-401. https://doi.org/10.1002/jps.2600730329
- Divoll M et al. J Pharm Sci 1981;70:1104-1107. https://doi.org/10.1002/jps.2600701004
- Ochs HR et al. J Clin Pharmacol 1984;24:58-64. https://doi.org/10.1002/j.1552-4604.1984.tb01814.x
- Restoril (temazepam) prescribing information.
