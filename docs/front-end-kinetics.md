# Front-end (recirculatory) pharmacokinetics

Claude Code, 2026-11-19, at the request of Steven L. Shafer: "In case someone
uploads a paper with front-end PK into the single paper review ... even if we
never adopt this into the library for a drug, for teaching PK principles the
engine should be built, tested, and validated."

Code: `R/frontEndKinetics.R` (the engine), `R/frontEndModels.R` (the
papers' models), `R/paper-review-front-end.R` (the Single Paper Review's
use of it). Tests: `tests/testthat/test-front-end-kinetics.R`,
`tests/testthat/test-paper-review-front-end.R`, with points digitised from
the papers' figures in `tests/testthat/fixtures/front-end/`.

## What front-end kinetics is

A mammillary model puts an intravenous dose into a central volume that
mixes at once. It does not: the dose travels with the venous return to the
right heart, through the lungs, out through the left heart and the arteries,
and only then recirculates. Arterial concentrations in the first minutes
oscillate (a first-pass peak, a trough, a recirculation peak), and during an
infusion arterial blood carries unmixed drug from upstream on top of the
mixed, recirculated concentration. Henthorn, Avram and Krejcie developed the
methodology (indocyanine green for the intravascular space, antipyrine or
inulin for water spaces, the drug itself, sampled every few seconds).

The oscillations matter little to an effect: an effect site is a low-pass
filter (time constant 1/ke0, minutes) and the oscillation's period is a
circulation time (under a minute), so the brain sees the smoothed arterial
rise. What front-end kinetics does decide is the size of the central volume,
and with it a loading dose, the first minutes of a target-controlled
infusion, the dependence of the early concentrations on cardiac output, and
the arterial-venous difference. `test-front-end-kinetics.R` ("an effect
site filters the oscillations but not the central volume") shows both halves
on the dog thiopental model after a 100 mg bolus:

| ke0 (/min) | Arterial: recirculatory vs model 3, largest gap / peak | Ce peak: recirculatory / model 3 / model 1 (mcg/mL) |
|---|---|---|
| 0.25 | 3.3 | 8.81 / 8.61 / 6.05 |
| 0.5 | 3.3 | 13.59 / 13.52 / 9.03 |
| 1 | 3.3 | 19.56 / 19.85 / 12.65 |

Model 3 (a mammillary model whose VC is the recirculatory VC + VND) misses
the arterial curve by three times its own peak, yet its effect-site peak by
2.3% or less at every ke0 from 0.25 to 1/min; model 1 (VC from samples at 1 min
and later, 3.37 L) misses the effect-site peak by a third. The error that
reaches the effect site is the central volume, not the oscillation. The
qualification is timing: the effect sites differ in their first fraction of
a minute, the recirculatory one waiting for the circulation (the largest
instantaneous gap is 31% of the peak at ke0 0.5/min, at the onset). With the
provisional delay shape used before Figure 3 was fitted (a single 15-deep
central chain), the first pass did get through at ke0 1/min (a 17% higher
peak): how much the brain sees of the first pass depends on how dispersed
the pulmonary transit is. The published central shape (Krejcie 1996a, below)
settles it: the first pass reaches a fast effect site (at ke0 1/min model 3
misses the peak by 23%), not a slow one.

## The engine

Two model families compile to one linear system, dx/dt = A x + B r(t),
observed through y = O x:

- **recirculatory**: cardiac output through one or more parallel central
  delay elements (each a chain of n identical compartments, so its
  transit-time density is Erlang, as in Krejcie 1996's parallel Erlang
  functions), then parallel peripheral circuits fed from arterial blood
  (nondistributive delay chains and flow-limited tissue volumes), elimination
  from arterial blood, the venous return re-entering the central elements.
  The flows must balance (the pathways and ClE sum to cardiac output, within
  1%); a model that creates or loses blood is refused.
- **mixing**: Henthorn 2018's structure: an unmixed first-pass volume
  cleared by the total pharmacokinetic flow into the central compartment of a
  mammillary model; arterial = central + unmixed; an optional arm compartment
  linked like an effect site, venous = F PC C_arm + (1 - F) C_central.
- **mammillary**: the ordinary model in the same frame, for comparison and
  for designing infusions.
- **compartmental**: any linear model, as named compartments, one-way
  clearances between them, a dose site and named observations
  (`frontEndCompartmental()`), for structures that are neither of the two
  families (Olofsen 2010's venous chain).

A delay element, central or peripheral, may be a series of segments, each a
chain of n identical compartments with its own volume: Henthorn 1992's
initial volume, time lag and sampling volume are three (n = 1, a long chain,
n = 1). A pure delay is a long chain, Krejcie 1996b's own reading of it (n to
infinity); SAAM's delays were not pure (Krejcie 1994's model curve for dog 2
starts before its tabulated lag). A pathway may carry an elimination from
its last compartment, either on top of its outflow or taken out of it (`clearance_in_flow`: Henthorn 1992's ICG
from the slow circuit, eq. 11; Henthorn 1999's ketamine from the fast
tissue). When ClE is not one of the flows summed to cardiac output
(`elimination_in_flow = FALSE`), it is extracted from the arterial blood
feeding the pathways (Upton 2004 eq. 2b). Until 2026-11-19 that branch added
the elimination on top of the flows and so created drug; no test had
exercised it, and Upton 2004 found it.

An infusion is constant or decays exponentially, which is what a BET
target-controlled regimen is (`frontEndBET()`), so the inputs are themselves
states and the augmented system is autonomous between events. Its exact
solution over a step is the matrix exponential, from `expmPade()` (the gas
engine's), cached on the step length. Nothing is integrated numerically, in
keeping with the rest of the library; a chain of n identical compartments
has one eigenvalue repeated n times, which the closed-form coefficient
engines cannot carry (see `R/advanceTransitChain.R`). Any system can take an
effect site (`frontEndAddEffectSite()`, one more linear state).

Pharmacokinetic flow is cardiac output times the blood:plasma ratio
(`frontEndPkFlow()`, Stec 1979), so it exceeds cardiac output for a drug that
partitions into red cells.

## Validation

### The mathematics

| Check | Result |
|---|---|
| A central delay element's first pass against `dgamma()` | within 1e-4 of the peak |
| Mass balance (boluses, constant and decaying infusions) | 1e-9 of the dose |
| Steady state, recirculatory: Ca = R0 / ClE | 1e-9 |
| Steady state, mixing (Henthorn eq. 2): Ca = R0/Cl + R0/ClE, Cv from eq. 4 | 1e-9 |
| The mammillary model against an independent eigen-decomposition | 1e-10 |
| A BET regimen holds its own model at the target | 1e-9 |

### Henthorn 2018 (ketamine, arterial and venous)

Henthorn TK, Avram MJ, Dahan A, Gustafsson LL, Persson J, Krejcie TC,
Olofsen E. Anesthesiology 2018; 129:260-70. Table 2 is read from the page
image (its text extraction scrambles the columns): VC 44.93, V2 136.58, V3
377.97 L; Cl2 3.17, Cl3 0.65, ClE 1.23 (S) and 1.06 (R) L/min; karm0 0.18
/min; pharmacokinetic flow 7.59 L/min; Vunmixed 1 L (fixed); F 0.04.

**Figure 6** (a simulation of 7 mg S(+)-ketamine hydrochloride over 30 min,
five curves) was digitised pixel by pixel from a 400-dpi rendering (axis
calibration from the tick marks: 3 to 30 ng/mL is 536 px; 300 min is 861 px).
From Table 2 alone the engine reproduces every curve within an RMS log error
of 8%, once three things the paper leaves open are settled. Each was settled
by fitting the figure, not chosen:

1. **The dose basis is ketamine base.** A free fit of a dose factor returns
   0.856; the hydrochloride-to-base factor is 237.72/274.19 = 0.867. The
   paper's parameters therefore take doses as base, and "7 mg ketamine
   hydrochloride" is 6.07 mg.
2. **VC 44.93 L is the S(+) enantiomer's.** Eq. 9 links the two
   (VC,S = VC,R ClE,S / ClE,R) without saying which was printed; a free fit
   returns 45.2 L for the S(+) simulation, so R(-)'s is 38.7 L.
3. **The arm link is driven by the mixed central concentration**, as eq. 4's
   shunted blood is. Driven by arterial blood (unmixed drug included), the
   arm curve misses by 25% at the table's values and by 13% even with the
   other parameters refitted; and its peak then falls at the end of the
   infusion, where the figure's comes two minutes after it.

The lavender "arm compartment" curve of Figure 6 is the tissue concentration
PC x C_arm (PC = V2/VC = 3.04), as the paper's methods say.

**The Supplemental Digital Content** (http://links.lww.com/ALN/B718, the PML
code of the final model) confirms decisions 2 and 3 and settles R(-)'s arm:

- the code estimates R(-)'s VC (`Vcb`) and makes S(+)'s `Vcb Cls/Clr`;
  Table 2's 44.93 L is S(+)'s, as the figure said (reading it as R(-)'s, with
  S(+)'s scaled up to 52.1 L, misses Figure 6's arterial curve by 5.5%
  rather than 2.3%);
- the arm is driven by the central concentration, `deriv(Carmr = karm0 *
  (Cr - Carmr))`;
- PCarm is V2/VC of each enantiomer (3.04 for S(+), 3.53 for R(-));
- the arm rate constant scales with ClE as VC does. Table 2's 0.18 /min is
  read as S(+)'s, like its VC; Figure 6 cannot tell (arm curve 9.0% at 0.18,
  9.9% at 0.209), so R(-)'s is 0.155.

The code's `fixef()` values (VC,R 38.76, V2 101.5, V3 98.5, Cl3 1.13 L/min,
total flow 8.33) differ from Table 2: in PML they are starting values. Table
2 is used. [open question for Shafer: whether the supplement's fixef values
are the final estimates of some other run]

One residual is **not** explained: the figure's orange "unmixed" line sits at
28.8 ng/mL, 7% above infusion rate / total flow (26.8 ng/mL in base, 30.7 in
salt). Neither basis gives it; a flow of 7.05 L/min (base) or 8.10 (salt)
would. [open question for Shafer: how Figure 6's unmixed line was computed]

**Table 3** (three-compartment models fitted separately to arterial and
venous data give VC 15.4 and 54.2 L) is reproduced in kind: fitted to the
final model's own samples at the study's times, VC is 9.8 L from the
arterial and 46.8 L from the venous. The paper's were fitted to both
enantiomers' real data, so only the direction and size of the gap are
tested.

The paper's text says the canine erythrocyte:plasma ratio is 1.44 and its
true-cardiac-output arithmetic (7.59 to 6.5 L/min at a haematocrit of 0.45)
needs 1.37, the value its methods give for ketamine in dogs; recorded, not
used.

### Avram and Krejcie 2003 (thiopental in dogs, target-controlled infusions)

Avram MJ, Krejcie TC. Anesthesiology 2003; 99:1078-86, Table 1 (means of
five dogs; the recirculatory parameters are Avram, Krejcie and Henthorn, J
Pharmacol Exp Ther 2002; 302:594-600).

The paper's experiment: BET infusions to 20 mcg/mL for 60 min, each designed
with one of four three-compartment models and simulated with the
recirculatory model; the "infusion delta AUC" is the area between the result
and the target. From Table 1's mean parameters:

| Model | Paper (mean of 5 dogs) | Engine (mean parameters) |
|---|---|---|
| 1, samples from 1 min | 162.0 | 166.3 |
| 2, from the recirculation peak | 124.6 | 126.8 |
| 3, VC fixed to VC + VND | 17.3 | 18.6 |
| 4, simulated 2-min infusion | -3.79 | -1.6 |

The paper's values are means of per-dog results, so agreement within a few
per cent is all that can be asked; the order and the size of the overshoot
are the paper's finding.

**The delay elements' shape.** Neither the 2003 nor the 2002 paper prints
the central chains' shape or the nondistributive chain's length (Avram 2002:
"the number of compartments needed in a delay varied between 2 and 30"). The
central shape is published in Krejcie 1996a Table 4 (J Pharmacol Exp Ther
278:1050-7, the "general model" the later papers cite): two parallel chains
of 24 and 16 compartments, the fast one carrying 0.66 of cardiac output for
antipyrine (k 86.4 and 41.7 /min) and 0.72 for ICG (92.8 and 43.2)
(`FRONT_END_KREJCIE1996_CENTRAL`); Avram 2002 shows thiopental's central
circulation is antipyrine's within 5%. The nondistributive chain's length
was chosen per dog and never printed (Henthorn 1999: "determined
iteratively"). With the central shape fixed, the one published thiopental
history (Avram 2002 Figure 4 = Avram 2003 Figure 3, dog 5, 31 points) was
fitted with that dog's cardiac output, VC, nondistributive flow and volume
and fast tissue volume free:

| Nondistributive chain | 1 | 2 | 3 | 5 | 10 | 20 |
|---|---|---|---|---|---|---|
| RMS log error | 8.4% | 10.5% | 14.7% | 19.3% | 23.4% | 24.7% |

So `FRONT_END_ND_CHAIN` is 1, from one dog. The fitted dog is physiological
(cardiac output 2.00 L/min, VC 0.73 L, nondistributive flow 0.73 L/min and
volume 0.39 L, fast tissue 7.5 L), where the shape inferred before the
papers arrived needed 1.15 L/min and a 434 L fast tissue. The shape decides
the first two minutes and does not touch the infusion results: the delta AUC
table above is the same to the first decimal under either shape.

**The effect site.** With the published central shape the first pass is
sharper (157 mcg/mL at 0.33 min after 100 mg), and how much of it reaches an
effect site depends on ke0. Peak effect-site concentration after a 100 mg
bolus, mcg/mL (models 3 and 1 driven by their own central compartments):

| ke0 (/min) | recirculatory | model 3 | model 1 |
|---|---|---|---|
| 0.25 | 8.89 | 8.61 (-3%) | 6.05 (-32%) |
| 0.5 | 14.42 | 13.52 (-6%) | 9.03 (-37%) |
| 1 | 25.72 | 19.85 (-23%) | 12.65 (-51%) |

The teaching point holds where the effect site is slow against the
circulation: the central volume, not the oscillation, decides the peak. For
a fast effect site the first pass itself arrives.

### Avram 2002 and Henthorn 1999 (the Northwestern dog models)

`frontEndAvram2002()` builds Table 2's ICG, antipyrine and thiopental models
(five dogs, 1.5% halothane, cardiac output 3.00 L/min); the thiopental one is
Avram 2003's recirculatory model. `frontEndHenthorn1999Ketamine()` builds
Table 2's model 4 for S(+) and R(-) ketamine (cardiac output 3.58 L/min; the
enantiomers share every distribution parameter; ClE 0.66 and 0.49 L/min
from the fast tissue, part of its clearance). Checks: the steady-state
volume from the arterial moments (dose AUMC / AUC^2) equals each table's Vss
(2.83, 33.00, 53.12 L) within 0.5%, and AUC = dose / ClE where ClE is from
arterial blood.

### Henthorn 1992 (the minimal model of circulatory mixing of ICG)

Henthorn TK, Avram MJ, Krejcie TC, Shanks CA, Asada A, Kaczynski DA. Am J
Physiol 1992; 262:H903-10, Table 1 (five dogs, per dog). Built with
segments: initial volume, lag and sampling volume in series; fast and slow
circuits each a lag and a volume; ICG eliminated from the slow circuit's
flow. Figure 3 (dog 2, 25 points digitised at 400 dpi:
`fixtures/front-end/henthorn1992-figure3-dog2.csv`):

- The paper's own curve rises 0.20 min after injection; dog 2's tabulated
  lag is 0.47 L / 1.89 L/min = 0.25 min. The figure's time axis runs about
  0.04 min (one 3-second sample) ahead of the parameters.
- From the Table 1 row as printed: RMS log error 29% unshifted, 15% with the
  0.04-min offset (lags as chains of 50 to 100). The recirculation peak still
  comes about 0.04 min late; moving every value by at most half its last
  printed digit (0.005) with a 0.028-min offset gives 11.7%.
- Krejcie 1994 prints a different set for the same dog (its Figure 6: VC
  lag 0.39 L, ClF 0.49, ClS 1.41 L/min), and Table 1's Vss for dog 2 (2.02
  L) is not the sum of its volumes (2.10 L).

### Upton 2004 (the two-compartment recirculatory model)

Upton RN. Br J Anaesth 2004; 92:475-84. A lung and a body joined by cardiac
output, clearance acting on arterial blood entering the body. The engine
reproduces the paper's closed forms exactly: recirculating rate constants
2.2128 and 0.12051 /min (eq. 21); steady states R0/Q_Cl arterial and
(R0/Q_Cl)(1 - Q_Cl/Q_CO) mixed venous (Table 1); Figure 3's peak of 6.30
mg/L (the text's "approximately 6.5" is loose); Figure 4's arterial peaks of
30.2, 23.9 and 19.7 mg/L at cardiac outputs of 3, 6 and 9 L/min, and its
venous peaks (1.62 at 2.28 min, 3.43 at 1.49, 4.10 at 1.19). Eq. 13b as
printed is missing the minus sign in its exponent.

### Olofsen 2010 (arterial and venous morphine-6-glucuronide)

Olofsen E et al. Anesth Analg 2010; 111:626-32, Table 1. A different
arterial-venous link from Henthorn 2018's: the arterial model's elimination
flow passes through a venous compartment exchanging with a second, so venous
equals arterial at steady state (the `compartmental` type). Table 1's VV1 of
0.05 L is CLA1 x kV0; CLA1 / kV0 = 0.404 L is what the model's own
definitions give, and it reproduces the Discussion's venous-site half-lives
of 1.4 and 33 min (1.37 and 31.6), where 0.05 L gives 0.17 min. Arterial
exceeds venous early and falls below it later, as Figure 3 shows. Body
weights are not reported (the test's dose assumes 70 kg).

### Kuipers 2001 (rocuronium in patients)

Kuipers JA et al. Anesthesiology 2001; 94:47-55, Tables 2, 3 and 6 (means of
15 patients; cardiac output 3.86 L/min; ke0 0.129 /min, Ce50 876 ng/mL,
gamma 8.75). Its Vss from the arterial moments is the table's 17.29 L. The
central chains' shape and the chains' lengths are not printed; the dogs'
ICG shape stands in.

### Read, not built

- **Krejcie 1994** (antipyrine in dogs): its tissues are fed from both blood
  circuits by rate constants the paper does not print (only their sums); it
  contradicts itself on where elimination sits (text: the slow circuit;
  Figures 4 and 6: the fast tissue); its dog 2 differs from Henthorn 1992's.
  The `compartmental` type can carry it once the split is assumed.
- **Kuipers 1999** (alfentanil in pigs): means over different subsets of
  pigs (13 for the tissues, 17 otherwise) whose clearances sum to 3.60
  against a mean cardiac output of 3.33 L/min, and no central shape.
- **Upton & Ludbrook 2005** (propofol, standard man): flows that do not sum
  to cardiac output (5.786 against 5.7 L/min), "extraction" undefined,
  cerebral blood flow falling with brain concentration (nonlinear), and a
  Figure 9 washout the Table 1 parameters cannot reproduce. Its equations
  are in its reference 4.
- **Masui 2010**: a comparison on openTCI data; it does not print the Upton
  model's parameters and needs the individual dosing records.
- **Levitt 2004** (the arm): its parameters are in a supplement
  (1472-6904-4-2-S1.doc) that is not here, and the arm is driven by a whole
  PKQuest body model.

## The Single Paper Review

The extraction schema has a `front_end` block (`paperReviewFrontEndSchema()`):
the model type (`none`, `recirculatory`, `mixing`, `other`), the total flow,
the unmixed volume, the central delay elements, the peripheral circuits, the
arm, each quantity with its printed units; and every figure series has a
`sampling_site`. A "mixing" model's mammillary part is the ordinary
parameters. `paperReviewFrontEndChecks()` builds the model at the reference
patient (no covariates), reports what was built and every assumption it had
to make, and compares each concentration figure's series with the engine's
concentration at that series' site, in the report section "Front-end
kinetics", with its plots beside the mammillary model's in Figures. A reply
without the block (the example, the fixtures, any review made before) has no
front-end model and is not told so.

Not yet done: the review's own simulator tab still draws only the mammillary
model.

## Papers still wanted

1. Avram MJ, Krejcie TC, Niemann CU, et al. The effect of halothane on the
   recirculatory pharmacokinetics of physiologic markers. Anesthesiology
   1997; 87:1381-93: the 1.5% halothane dogs of Avram 2002, perhaps with the
   delay elements' lengths.
2. Krejcie TC, Avram MJ, Gentry WB, et al. A recirculatory model of the
   pulmonary uptake and pharmacokinetics of lidocaine based on analysis of
   arterial and mixed venous data from dogs. J Pharmacokinet Biopharm 1997;
   25:69-90: arterial and mixed venous sampling in one model.
3. Upton & Ludbrook 2005's reference 4 (the propofol model's equations), and
   Levitt 2004's supplement 1472-6904-4-2-S1.doc (the arm worksheets).
4. Henthorn TK, Avram MJ, Krejcie TC. Intravascular mixing and drug
   distribution: the concurrent disposition of thiopental and indocyanine
   green. Clin Pharmacol Ther 1989; 45:56-65 (in humans).
5. Krejcie TC, Avram MJ. What determines anesthetic induction dose? It's the
   front-end kinetics, Doctor! Anesth Analg 1999; 89:541-4 (teaching).

Received and read on 2026-11-19: Henthorn 1992, Krejcie 1994, Krejcie 1996a
and 1996b, Henthorn 1999, Kuipers 1999 and 2001, Avram 2002, Upton 2004,
Upton & Ludbrook 2005, Levitt 2004, Masui 2010, Olofsen 2010, and Henthorn
2018's Supplemental Digital Content.
