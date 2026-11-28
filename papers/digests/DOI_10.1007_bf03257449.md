---file: DOI_10.1007_bf03257449.pdf
pmid: none
doi: 10.1007/BF03257449
citation: de Groot ANJA, Vree TB, Hekster YA, van den Biggelaar-Martea M, van Dongen PWJM, van Roosmalen J. Variation in bioavailability of oral methylergometrine in healthy male volunteers. Drug Investigation 1994;8(6):345-351.
kind: pk-model
drugs:
- methylergonovine
source_read: 'page images (PDF pages 2, 4, 5 rendered and read: Tables I-IV, Fig 1, Table II; pages 4 and 5 re-rendered on 2026-11-27 at higher magnification to read Table II''s footnote markers, Table IV in full per subject, and Figure 1''s peak region -- the text layer interleaves the two-column table rows and offsets Table IV''s values by one row, so it must not be transcribed; remaining pages text layer)'
edition: published
digest_by: Claude (unattended digest_run.py), 2026-11-26; reworked by hand on 2026-11-27 at Steven L. Shafer's direction -- the closed form for V1, the median/log-OLS refit, the Figure 1 digitisation and the oral deconvolution
checked_against: stanpumpR master 8e491545 (re-checked against 6a5bff47 on 2026-11-27 -- still no methylergonovine model)
verification: not-applicable
open_questions:
- '[check] Table II''s mean AUC for the six men, 4.82 ug/L*h, is NOT the mean of Table III''s six values (4.74, 8.11, 2.96, 4.07, 4.05, 3.63), which is 4.5933. Its printed SD of 1.82 IS their sample SD (1.8195) to the digit, so the six values and the SD agree with each other and the mean does not -- which points at the printed mean rather than at the individuals. de Groot 1995 prints 4.59 for the same six men (27.56/6), supporting that reading (found by the methylergonovine session, 2026-11-27). Use the Table III values; treat Table II''s 4.82 as suspect. Note the consequence for the over-determination check below: dose/AUC is 33.09 L/h on the Table III mean, not the 31.54 that 4.82 gives.'
- '[check] Table II''s footnote b (Model-independent) marks t_abs and footnote c (1-compartment model) marks MAT. Footnote b is contradicted by the paper''s own numbers: t_abs reproduces the printed tmax through the 1-compartment formula tmax = tlag + ln(ka/k)/(ka-k) to three decimals (subject 1, 0.995 against 0.99; subject 4, 0.401 against 0.40), so it is the fitted absorption half-life, not a model-independent quantity. Footnote c is right, and MAT is 1-compartment-derived through MRT_po. Do not take t_abs as a model-free input parameter on the strength of that footnote.'
- '[check] The dose each subject actually received is recoverable as CL x AUC from Table III, and for five of six gives 138.0 to 147.1 ug (mean 143.4, 94.3% of the nominal 152), matching the paper''s own measurement of the IV solution at 93.3 +/- 6.5% of claimed content. Subject 5 gives 87.1 ug, 57% of nominal and 5.6 SD from the others, so one of his printed CL 21.5 L/h and AUC 4.05 ug/L*h must be in error; Vss = CL x MRT holds for him as for all six, which ties the error to the AUC rather than the clearance. The unit-disposition refits above are unaffected, being in unit-dose coefficients.'
- '[check] The abstract (PDF p1) gives CL 34.1 ± 9.7 L/h after IV, but Table II (PDF p4, printed 348) and the Results text (p5) give 32.2 ± 11.80 L/h, and the mean of the six Table III values is 32.2. Which is printed in error cannot be settled from this PDF; use the table value.'
- '[check] RESOLVED 2026-11-27 by digitising Figure 1. Subject 1''s 60-minute oral point reads 1.417 ug/L on the figure, against the text''s 1.2 and Table IV''s 1.00. Table IV''s value is not an observation: the paper''s own 1-compartment fit, run at the printed ka and k, predicts tmax 60 min and Cmax 1.033, which is what Table IV prints to rounding. So the table gives the model''s peak, the text gives something closer to the measured one, and both understate the figure. Digitisation uncertainty, measured on the IV arm of the same figure, is about 10%, which covers the text''s 1.2 but not the table''s 1.00.'
- '[check] The printed mean set is over-determined and does not agree with itself. Dose/AUC gives CL 31.54 L/h on Table II''s mean AUC of 4.82 and 33.09 on Table III''s own mean of 4.5933 (see the AUC check above, which makes 4.82 the suspect figure), Table II gives 32.2, the abstract 34.1, and Vss/MRT 31.78; and CL x MRT = 72.45 L against the printed Vss 71.5 L. Expected of means taken parameter by parameter over six individual fits, but it means no four of the printed means are mutually consistent.'
- '[decision] For Steven L. Shafer, and the only thing on this card that needs him. The paper''s printed means are a two-stage average and describe no subject; the arithmetic, the refit and the validation are in *What this paper actually supports* below. Three candidate parameter sets exist. (1) The printed means closed: V1 32.60, V2 38.90, Vss 71.50 L, CL 32.20 L/h, t1/2 11.4 min and 1.85 h. (2) The mean of the six individually closed sets: V1 20.34, Vss 71.52 L, CL 32.16 L/h, t1/2 5.5 min and 1.84 h. (3) The two-stage refit through the median curve, which this card recommends, its grid anchored at the model''s own t = 0 intercept and then running from the study''s first observation: V1 16.88, V2 58.87, Vss 75.75 L, CL 32.28 L/h, Q 127.2 L/h, t1/2 3.70 min and 1.886 h. Note that the refit''s V1 moves 32% with where the grid starts (16.12 L from 1 min, 20.65 from 5), because the grid''s start is the simulated study''s first sample time and V1 is fixed by the first sample after a bolus. No candidate determines V1 to better than about 30%, and none supports a claim about the DIRECTION of any error in it; CL is firm at 32.3-32.9 L/h across every candidate and method, and is the parameter to stand behind. Which set? And is methylergonovine to be built at all from a six-man study whose bioavailability ranges 22 to 136%? The oral input does not depend on that choice: among the four shapes tried, a zero-order input released over 55 min after a 15.4 min lag is the one selected on seven digitised points from one subject, with F the group median of 83.7%, and stanpumpR already has the engine for it. Treat it as the best-supported shape rather than a settled one -- strong against the published first-order ka, not a proof that no other shape fits.'
---

# de Groot 1994: oral methylergometrine bioavailability in men

## Study
Six healthy men (Table I, PDF p2 / printed 346): age 24, 53, 27, 49, 26, 35 y; weight 75, 79, 79, 73, 72, 70 kg; height 173-194 cm; normal liver and renal function. Randomised crossover, 2 weeks apart: oral 0.125 mg methylergometrine maleate tablet (Methergin, = 0.095 mg base) after a standard breakfast, and IV 0.200 mg maleate (= 0.152 mg base) over 1 min after a similar breakfast. Venous plasma (lithium heparin), samples at 0, 10, 20, 30, 60, 90, 180, 270, 360, 450, 540 min, plus 3 and 5 min after IV. Assay: HPLC with fluorescence detection (ex 315 / em 430 nm); quantitation limit 150 ng/L in plasma (120 ng/L in water); intraday CV 3%, interday 5%. Software MW\Pharm: 2-compartment after IV, 1-compartment with lag time after oral. IV solution content measured at 93.3 ± 6.5% of claimed (n=20); the dose used in the analysis is the nominal base dose.

## Model and parameters
Table II (PDF p4 / printed 348), mean ± SD, all doses as free base; concentrations are plasma (total, no protein-binding data).

```yaml
parameters:
  - {name: CL_IV, value: 32.2, unit: L/h, source: "Table II", note: "mean, SD 11.80, absolute (not per kg); abstract says 34.1 ± 9.7"}
  - {name: Vss_IV, value: 71.5, unit: L, source: "Table II", note: "mean, SD 25.9, absolute; model-independent (AUMC)"}
  - {name: t_half_alpha_IV, value: 0.19, unit: h, source: "Table II", note: "mean, SD 0.27, range 0.04-0.74 per Table III; 142% CV"}
  - {name: t_half_beta_IV, value: 1.85, unit: h, source: "Table II", note: "mean, SD 0.28"}
  - {name: MRT_IV, value: 2.25, unit: h, source: "Table II", note: "mean, SD 0.44"}
  - {name: AUC_IV, value: 4.82, unit: "ug/L*h", source: "Table II", note: "mean, SD 1.82, for 152 ug base"}
  - {name: F_oral, value: 84.9, unit: "%", source: "Table II", note: "mean, SD 37.2, 43.8% CV; individual range 22.2-136.0 (Table IV)"}
  - {name: tlag_oral, value: 0.33, unit: h, source: "Table II", note: "mean, SD 0.09, range 0.24-0.50"}
  - {name: t_half_abs_oral, value: 0.08, unit: h, source: "Table II", note: "mean, SD 0.08, range 0.010-0.23"}
  - {name: t_half_beta_oral, value: 2.08, unit: h, source: "Table II", note: "mean, SD 0.43, range 1.62-2.69"}
  - {name: Cmax_oral, value: 0.77, unit: "ug/L", source: "Table II", note: "mean, SD 0.34, for 95 ug base"}
  - {name: tmax_oral, value: 0.68, unit: h, source: "Table II", note: "mean, SD 0.22"}
  - {name: MAT_oral, value: 0.87, unit: h, source: "Table II", note: "mean, SD 0.72, model-independent"}
  - {name: CL_oral, value: 31.1, unit: L/h, source: "Table II", note: "mean, SD 10.3, calculated assuming CL equal after IV and oral (F derived)"}
  - {name: Vss_oral, value: 94.4, unit: L, source: "Table II", note: "mean, SD 38.9"}
```

Individual IV values (Table III, PDF p5 / printed 349), subjects 1-6: t1/2alpha 0.076, 0.74, 0.04, 0.09, 0.14, 0.041 h; t1/2beta 1.84, 1.96, 1.63, 2.31, 1.52, 1.88 h; CL 30.4, 17.65, 49.7, 33.9, 21.5, 39.8 L/h; Vss 69.9, 44.8, 92.7, 99.1, 36.5, 86.1 L. Individual oral values are in Table IV (F 103.6, 22.2, 81.9, 80.0, 85.4, 136 %). Not reported: V1, V2, rate constants k10/k12/k21 or intercepts; clearance or volume per kg. Population approach not used (individual fits, then means). The IV vs PO t1/2beta, CL and Vss differences are not significant (p 0.31, 0.50, 0.11).

## What this paper actually supports

Established 2026-11-27 and recorded here rather than in the open questions, because it is settled
arithmetic and measurement rather than a question. All of it is reproducible from the printed tables
plus Figure 1.

### The two-compartment model closes, and uniquely
V1 is not printed. It does not need to be: CL, Vss and both half-lives are four non-redundant
parameters, and from `alpha + beta = k10 + k12 + k21`, `alpha*beta = k10*k21`, `CL = k10 V1`,
`Vss = V1 + V2` and `Q = k12 V1 = k21 V2`, the cross terms collapse to `k12 + k21 = alpha*beta*Vss/CL`,
giving

    V1 = CL / ((alpha + beta) - alpha*beta*Vss/CL)

which is LINEAR in 1/V1, so the admissible root is unique -- no quadratic, no branch to choose. This is
`docs/antihypertensives.md` section 4's rule (CL + Vss + BOTH half-lives closes the system) in closed
form. The feasible region is bounded too: `V1(CL) = CL^2/((alpha+beta)*CL - alpha*beta*Vss)` has a
minimum, so with Vss 71.5 L and t1/2 0.19/1.85 h no clearance whatever gives a V1 below 24.16 L.

### The printed means describe no subject
Table III prints t1/2alpha, t1/2beta, CL and Vss PER SUBJECT, so every man's model closes individually:
V1 = 19.03, 39.26, 12.40, 27.21, 13.66 and 10.50 L (V1/Vss 27, 88, 13, 28, 37 and 12%). Their mean is
20.34 L; closing the MEAN set instead gives 32.60 L. The damage is confined to the distribution phase,
because t1/2alpha ran 0.04 to 0.74 h with SD 0.27 against a mean of 0.19 (142% CV): CL, Vss and
t1/2beta survive averaging to within 3, 3.5 and 4% of the refit, and t1/2alpha does not (11.4 min
against 3.55), carrying V1 with it. Against the geometric mean of the six closed curves the mean set
sits -30% at 3 min and +28% at 20 min.

### The refit, and why it is fitted this way
Shafer's method: simulate each subject from his own closed model, sample at log-spaced times, take the
MEDIAN at each time, and fit a polyexponential. Fit the medians, not the whole cloud -- least squares
over the cloud lets subject 2's outlying alpha torque the model off its centre (unweighted CL 29.3 L/h,
V1 16.4, Vss 56.7; 1/y weighted CL 36.0, V1 18.6, Vss 90.5). Fit them on the LOG scale by ordinary
least squares: that is scale-free, has no variance model to misidentify, and is in the same currency as
MDPE and MDAPE, which are defined on the ratio measured/predicted. Extended least squares was tried,
and its variance power is not identifiable from noise-free medians -- the objective falls monotonically
over theta 0 to 1.5 with no interior minimum, because simulated medians have no residual variance for
it to be identified from -- so theta would have to be fixed at 1 on physical grounds; log-OLS avoids
the question. Grid SPACING is not the constraint: 2.0-fold to 1.15-fold took the points below 10 min
from 4 to 17 and moved CL 0.5% and t1/2alpha not at all. Grid START is, and badly -- see the table
below, which is the single largest uncertainty on this card.

The grid's START matters, and far more than its spacing -- found by the methylergonovine session on
2026-11-27 and reproduced here. Simulating from 1 min extrapolates each man's model into a region
where the study has no data (its first IV sample is at 3 min; the infusion ends at 1 min), and that
is exactly where the six differ most. Over starts of 1 to 5 min and both spacings:

| grid starts at | V1 | V2 | Vss | CL | Q | t1/2alpha | t1/2beta |
|---|---|---|---|---|---|---|---|
| 1 min | 16.12 | 58.50 | 74.62 | 32.28 | 127.4 | 3.55 min | 1.862 h |
| 2 min | 17.16 | 58.80 | 75.96 | 32.29 | 125.0 | 3.81 min | 1.893 h |
| **3 min (first observation)** | **18.32** | **58.56** | **76.88** | **32.45** | **123.7** | **4.05 min** | **1.903 h** |
| 5 min | 20.65 | 57.39 | 78.03 | 32.91 | 123.3 | 4.45 min | 1.892 h |

V1 moves 32% across that range and t1/2alpha 25%, while Vss moves 5% and CL 2% -- the same two
quantities the two-stage averaging damaged, and for the same reason. So the refit does not determine
V1 to better than about 30% either, and the card says so rather than quoting a single figure.

The reason is not numerical (Steven L. Shafer, 2026-11-27). **V1 is a fiction and clearance is real.**
At time zero the drug has not left the injection site, so the true concentration is zero and the true
V1 infinite; what a study reports is where its own curve extrapolates back to an intercept that does
not exist, and that depends on when it drew its first sample. The grid's start IS the first sample
time of the simulated study, so moving it runs a different experiment. Clearance has no such
dependence -- it is the dose over the AUC -- which is why CL sits at 32.3 to 32.9 L/h across every
candidate set and fitting method tried here while V1 ranges 16 to 33, and why **no direction should
be claimed for a bias in V1**. See *Comparing sources: only clearance is real* in `AGENTS.md`.

That start-dependence largely dissolves once the grid carries **the model's own concentration at time
zero** (Steven L. Shafer, 2026-11-27): for a bolus that is dose/V1, the intercept the curve
extrapolates back to. It is not a physical concentration, but it is intrinsic to the mammillary model
being fitted and it is where V1 lives. Anchored that way, V1 runs 16.03 to 16.88 L over starts of 1 to
10 minutes -- a 5% spread against 31% without it -- and settles on the median of the six subjects'
own V1 (16.35 L), which is what anchoring to the median intercept must give.

**Recommended: CL 32.28 L/h, V1 16.88, V2 58.87, Vss 75.75 L, Q 127.2 L/h, t1/2 3.70 min and 1.886 h**
(k10 1.9120, k12 7.5363, k21 2.1613 /h; admissible, and MDPE -1.29% with MDAPE 3.26% against the
median curve). The grid is t = 0 at the model's intercept, then the study's first observation at
3 min, 1.5-fold to 540 min. CL lands on the paper's printed 32.2.

Scored by Varvel, Donoho and Shafer 1992 (PMID 1588504, `papers/digests/PMID_1588504.md`),
PE = 100 (measured - predicted) / predicted, against all six subjects' simulated concentrations:
MDPE +1.4% and MDAPE 25.3%, against the mean parameter set's +3.9% and 33.5% and proportionally
weighted least squares on the whole cloud at +19.8% and 30.0%. Two cautions on those metrics. MDAPE
floors near 25% for every sensible candidate because it is measuring the spread of six men whose CL
ranges 17.65 to 49.7 L/h rather than the estimator, and its ranking changes with the pooling, so MDPE
is the discriminating statistic here. And minimising MDAPE directly is not the answer: it reaches
19.1% by writing subject 2 off (his MDAPE 110%) and pays MDPE +11%. No two-compartment model describes
subject 2 alongside the other five.

### Checked against the only real data in the paper
Figure 1 carries subject 1's actual concentrations -- 10 filled markers (IV) and 7 open (oral). It was
digitised by rendering PDF page 4 at scale 10, finding the open circles as enclosed white interiors
(fill-holes minus the ink, filtered to round components of marker size) and the filled ones by a
morphological opening with a 9-pixel disk, which erases the curves and the rings and leaves the solid
disks. The axes calibrate on the four y decades (508 px each, confirmed by the minor-tick pattern
matching a log decade at 0.9, 0.8 ... 0.2) and the eight x ticks (81.4 px/h); every marker was then
matched to the protocol's nominal sampling time, which removes the x-axis reading error entirely.

Subject 1's unit disposition function, closed from his four Table III numbers alone (V1 19.03 L,
V2 50.87, Q 109.4 L/h, k10 1.597, k12 5.749, k21 2.151 /h), reproduces his OWN digitised IV points at a
**median ratio of 1.102 and MDAPE 10.5%** when simulated at the dose he actually received -- 144.1 ug,
recovered as CL x AUC, the paper having measured its solution at 93.3 +/- 6.5% of the claimed 152
(the methylergonovine session's point, 2026-11-27; at the NOMINAL 152 the agreement reads better,
1.044 and 9.5%, but the delivered dose is the right basis and is what the authors' own CL and AUC
imply). Either way the closure is confirmed to about 10%, which is the digitisation's own accuracy.
So the closed-form V1 recovery reproduces the measured points, not merely the arithmetic it came from.

**What this check is and is not.** It is not independent data. Figure 1 plots the very samples that
subject 1's Table III values were fitted to, so the comparison asks whether the closure recovers the
authors' own fitted model, mediated by their own data -- the ~10% is digitisation error plus the
rounding of the printed summaries, not a model validated against anything new. That is exactly what
is needed to license the deconvolution below, which only requires subject 1's own disposition to be
right. It says nothing about the model's fitness beyond these six men.

A **second check of the same kind** was supplied by the methylergonovine session on 2026-11-27: de
Groot 1995, the companion, plots man no. 6 of these same six, and reading that figure against his
closure from Table III (V1 10.50 L, this card's arithmetic, not read off the figure) gives a median
read/model ratio of 0.98 and MDAPE 3.7%. The session was right to say this is not independent data
either, for the same reason -- 1995 plots the samples those 1994 values were fitted to.
Note that 1995's men ARE these men -- identical demographics and table values -- so the two papers
are not independent studies of the drug, and a clearance agreeing between them is not a cross-check
(a mistake this card's author made and the methylergonovine session caught).

### The oral input: comparing candidate shapes, and why the published ka is not a measurement
Only F is model-independent in Table IV. The printed tmax values (59, 32, 35, 24, 54 and 40 min) are
not sampling times (0, 10, 20, 30, 60, 90, 180 min): they are computed from the 1-compartment fit, as
`tmax = tlag + ln(ka/k)/(ka-k)` confirms to three decimals (subject 1, 0.995 against 0.99; subject 4,
0.401 against 0.40). MRT_po is the fit's too (subject 1, `tlag + 1/ka + 1/k` = 2.909 h against the
printed 2.91), so MAT, which is `MRT_po - MRT_iv - tlag` exactly for all six, inherits it. F alone
reproduces non-compartmentally from the printed AUCs, 6/6.

With subject 1's UDF held fixed, four input shapes were fitted to his 7 oral points by OLS on the log
concentrations:

| input | SSlog | AIC | MDAPE | tmax | Cmax |
|---|---|---|---|---|---|
| lagged zero-order (3p) | 0.0592 | -27.40 | 6.32% | 70 min | 1.404 |
| lagged first-order (3p) | 0.3204 | -15.59 | 9.55% | 45 min | 0.935 |
| lagged Weibull (4p) | 0.0548 | -25.95 | 8.40% | 56 min | 1.275 |
| two parallel first-order (5p) | 0.3145 | -11.72 | 9.48% | 44 min | 0.941 |

**Among those four, a zero-order input wins: F 105.4%, released over 55 min after a 15.4 min lag.**
That is a selection within the family tried, on seven points from one subject, not a proof that no
other shape fits; what it does establish is that the published first-order ka is not supported by
the data. F carries the
digitisation's scale uncertainty -- the IV arm of the same figure reads about 10% high against the
delivered dose -- so it is consistent with anything from roughly 96 to 105%, which brackets Table
IV's own 103.6% for this subject. The SHAPE does not depend on that scale. It reaches Cmax 1.404
at 70 min against the observed 1.417 at 60 min. A lagged first-order input cannot reach the peak at all
-- its own maximum is 0.935 ug/L. The Weibull buys nothing for its fourth parameter, and two parallel
paths collapse to one (ka1 = ka2 = 1.753 /h), so the data support neither. The zero-order residuals
(+2.2, -13.2, +12.6, +14.2, -6.3, -4.9, -1.8%) sit at the level of the digitisation error measured on
the IV arm of the same figure, so the data cannot distinguish it from truth.

So the paper got the lag right (0.24 h printed, 14 min in the text, 0.256 h deconvolved) and F right
(103.6% against 105.4%); what these data do not support is the input's KIND. Its t1/2abs of 0.23 h
asserts a first-order ka of 3.0 /h, and no first-order input fits these points -- the best one misses
the observed peak by 34%, its own maximum being 0.935 ug/L against 1.417. That looks like the whole
group's artifact visible in the one subject whose data can be seen: a first-order input cannot make a
peak this sharp on this disposition, so the
1-compartment fit drove ka to 3.0, 11.6, 21.0, 23.1 and 69.3 /h across the six to compensate -- 0.010 h
is a 36-second absorption half-life for a tablet taken after breakfast. The flip-flop signature
confirms it and is directional: ordered by MAT, the three fastest absorbers have oral t1/2beta 5 to 12%
BELOW their own intravenous value and the three slowest have it 16, 40 and 56% ABOVE. The paper's own
test, p = 0.31 on IV against PO t1/2beta, averages that pattern away.

### For the library
Build the oral form with stanpumpR's existing `oralZeroOrder = list(duration, lag)`
(`R/oral-zero-order.R`, as carvedilol and metformin ER use it), NOT a ka. A uniform input has mean
duration/2, so the group's MAT values imply releases of roughly 0.4 to 3.8 h; subject 1's MAT of 0.37 h
implies 0.74 h against the 0.92 h actually fitted. F is the group median, 83.7%. Clearance and the
volumes are ABSOLUTE, not per kg, and Table I gives the six weights (70-79 kg, mean 74.7, ages 24-53,
all men), so a cohort anchor per `docs/weight-adjustment.md` sits close to the reference man and moves
little.

## Pharmacodynamics and end points
None. No effect measure (uterine pressure, blood pressure change) was modelled; blood pressure did not change after either route. No ke0, t1/2ke0, tPeak, EC50, MEAC or effective concentration, and no potency ratio. Not an opioid. Side effects after IV: nausea in 1 subject, a "weird" sensation in 3. Pharmacodynamics in non-pregnant women is announced as future work.

## Covariates
None analysed. Only men; weights in Table I only. No pregnancy or sex comparison here (a companion women's study is cited as in preparation).

## Caveats
n = 6 healthy men, none pregnant or postpartum (the clinical target). Very large IIV in oral F (22-136%, with F above 100% in one subject, attributed to assuming equal CL for IV and oral) and in CL (17.7-49.7 L/h) and Vss (36.5-99.1 L). The distribution phase is very fast (t1/2alpha 0.04-0.74 h) with sampling at 3 and 5 min only early on, so the alpha phase and V1 are poorly determined (subject 3's fitted t1/2alpha is 2.4 min, shorter than the gap to the first sample after a 1-min infusion, so his alpha rests on one or two points; V1 is recoverable arithmetically -- see open_questions -- but is the weakest quantity in the paper); alpha was not detected after oral dosing. Plasma assay quantitation limit 150 ng/L. Oral doses taken after breakfast (food effect not tested). The abstract and Table II disagree on CL (34.1 vs 32.2 L/h); the oral sampling-time sentence reads "after ergometrine administration" (apparent slip). The authors state that earlier reports of rapid, complete oral absorption were not confirmed.

## Used by stanpumpR
The context file lists no repo locations, `cited_in_repo` is false, and no drug or model uses it.

## Verification against stanpumpR
No R/drugs_methylergonovine.R, drugDefaults_global.csv row or help text mentioning methylergonovine/methylergometrine/ergometrine exists in master 8e491545 (searched whole checkout by name and keyword). Only papers/pending-work.csv lines 79-81 and docs/literature-search-2026-11-16.md line 623 mention it: they list this paper as wanted for V1/Vss, clearance per kg and weights, and oral ka/lag/F. Findings relevant to that request: Vss 71.5 L (the unverified figure mentioned there is confirmed, Table II), no V1 reported, CL and Vss are absolute (L/h, L) with subject weights 70-79 kg in Table I (mean of six weights 74.7 kg, derived), oral F 84.9 ± 37.2% with a 22-136% range, lag 0.33 h, t1/2abs 0.08 h. Verdict: not-applicable (nothing in the repo to compare), with the open questions above.
