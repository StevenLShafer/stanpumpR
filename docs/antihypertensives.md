# Antihypertensive models: audit of the handoff specification, and what was implemented

Claude Code, 2026-10-17, at the request of Steven L. Shafer, from his handoff
specification *Build evidence-gated antihypertensive PK/PD models* (ChatGPT,
evidence reviewed through 2026-10-16).

This document is the review the request asked for, and the record of what the
review changed. The short version: the specification's **architecture is
right and its scepticism is well placed**, several of its individual claims
are wrong or overstated, one of its central "unavailable" verdicts is wrong in
the useful direction, and its porting plan could not be followed as written
because the sources it names are unreachable from this environment.

---

## 1. What the specification gets right

Four judgements in it are correct and are now built into the code.

**The endpoint is part of the model identity.** Mean pressure, resting
systolic, upright systolic, exercise systolic, exercise heart rate, the heart
rate rise caused by infused isoproterenol, ACE inhibition, urinary sodium, a
vascular-resistance proxy and the probability of crossing a pressure threshold
are different outputs, and their coefficients do not substitute for one
another. Checking the literature confirms this is the dominant problem in the
class, not an edge case: of the records with a usable fitted response, most
fitted something that is not a resting blood pressure.

**Product and route are part of the model identity.** Propranolol
immediate-release against long-acting, each loop diuretic oral against
intravenous, the spironolactone tablet against the study suspension,
standalone canrenone against canrenone formed from spironolactone, racemic
sustained-release verapamil against the controlled-onset product whose model
is the S-enantiomer. Each is now a separately keyed record.

**Independent evidence fields beat one "validated" flag.** A drug can have a
complete fitted pharmacokinetic model and no blood-pressure model at all, and
the common failure mode is a reader seeing the drug listed and assuming the
rest. The registry's `pk_evidence`, `pd_evidence`, `execution_status`,
`bp_simulation_enabled` and `external_validation_status` are implemented as
the specification defines them.

**A reported half-life is not permission to invent a clearance and a volume.**
Correct, and section 4 shows how far that principle reaches: in three of the
four cases examined it is decisive, and in one it is not.

---

## 2. The access problem, which changes the plan

The specification's implementation sequence asks for the Tsai azilsartan and
chlorthalidone model, the Hwang carvedilol model and the Tatipalli
spironolactone model to be ported "from one internally consistent source at a
time". Two of those three cannot be done from this container. The Hwang
carvedilol model **can**, and was: it is open access in PubMed Central, the one
channel that works here, and section 8 records the port and what its full text
settled. This section was written before that was discovered, and the first
lesson of it is that "unreachable" deserves re-testing per paper rather than
per publisher.

`WebFetch` fails DNS resolution for every host, and the egress proxy returns
a policy denial for Wiley, PubMed, PubMed Central, NCBI E-utilities, MDPI,
Springer, Google Scholar and the rest. The one working literature channel is
the PubMed MCP server, which serves bibliographic records, abstracts, and
PubMed Central full text where a machine-readable copy exists.

So the sources split three ways:

| Reachability | Sources | Consequence |
|---|---|---|
| Full text read | Hammer 2008 paediatric fenoldopam; **Hwang 2023 carvedilol**; Tatipalli 2021 spironolactone (narrative only, tables stripped) | Numbers verified, or structure verified with the table missing |
| Abstract only | Ericsson 1999 clevidipine; Bailey 2002 clevidipine; Campanero 2008 terazosin; Donnelly 1993 amlodipine; Sum 1983 esmolol; Tsai 2016 azilsartan/chlorthalidone; Van Wart 2013 hydrochlorothiazide and furosemide | Whatever is in the abstract is verified; the parameter tables are not |
| Unreachable | everything else in the ledger | The handoff's transcription stands unchecked |

Two cautions about that table, both learned the hard way. A paper can be **in**
PubMed Central and still have no full text there: Ericsson 1999 is PMC2014189
and the server returns its abstract with an empty body, so deposition is not
readability. And the "abstract only" row is worth more than it sounds --
reading those abstracts systematically, rather than treating the whole Wiley
set as lost, verified five numbers and supplied two the handoff never had.
Section 9 records which, and the rule it suggests: read the abstract before
asking anyone for the paper.

Every number is therefore labelled in
`inst/extdata/antihypertensiveParameters.csv` with how it was checked:
`primary_full_text`, `primary_abstract`, `derived_here` or
`handoff_unverified`. Of the 125 catalogued numbers, 89 are verified or
derived from verified numbers -- 56 from a primary full text, 25 from a
primary abstract and 8 derived here -- and 36 are not. When this section was first
written the counts were 56 of 101 verified; the three papers that have since
arrived moved nine numbers out of `handoff_unverified` and added twenty-four
more that were verified on the way in. Every one of the remaining 36 belongs
to a record the package does not execute, and a test asserts that **no record
this package executes depends on an unverified number**.

This is not a complaint about the environment. It is the reason the
implemented surface is three drugs and four endpoint relations rather than the
six or seven ports the specification sketches, and it is the single thing to
fix before the next round: library copies of a handful of PDFs would move more
records than any amount of further work here. **Section 9 says which, in what
order, and what each one unblocks.**

---

## 3. Errors and overstatements found in the specification

Listed in descending order of how much they would have cost.

**3.1 The azilsartan and chlorthalidone port is not ready, and `ka` is why.**
The specification presents a complete two-compartment vector for each drug and
puts the pair at the head of the porting queue. The abstract confirms the
two-compartment first-order structure, azilsartan `CL/F` 1.47 L/h and `V1/F`
3.98 L, chlorthalidone `CL/F` 4.13 L/h and `V1/F` 62.1 L. It does not contain
`ka`, `Q`, `V2/F`, the metabolite block, either EC50, any confidence interval,
or the exposure metric driving the response model, and none of those numbers
appears in any other reachable source. Absorption governs the entire shape of
an oral curve, so a port missing `ka` is not a port. Three further problems
sit underneath: the abstract states age is a retained covariate worth about
20% of exposure, so a typical-value vector is not the published model; the
abstract calls the maximum responses **predicted**, not estimated, which is a
weaker claim than the specification makes; and the matrix for chlorthalidone
is unstated, which matters because it partitions heavily into erythrocytes, so
a `V1/F` of 62.1 L cannot be interpreted until plasma, serum or whole blood is
pinned down. Status: `requires_source_extraction`.

**3.2 Clevidipine's healthy-volunteer vector is NOT unavailable.** The
specification says "individual compartment volumes and Q `TBD` here" and
leaves the record unimplemented. This is the one place where its caution
costs something real. A two-compartment model has four free parameters, and
Ericsson 1999 reports four quantities: clearance, steady-state volume, and
both exponential half-lives. That closes the system, uniquely:

```
V1 = CL / (l1 + l2 - l1 l2 Vss / CL)
k21 = l1 l2 V1 / CL      V2 = Vss - V1      k12 = k21 V2 / V1      Q = k12 V1
```

giving V1 0.3689 L/kg, V2 0.1911 L/kg, Q 0.01637 L/min/kg, all positive, with
k21 between the two eigenvalues, which is exactly the condition for a
physically admissible mammillary model. The specification's own rule is
satisfied rather than broken: this is not a half-life converted into a
clearance, it is a fully determined linear system. Two checks support it. The
area under the curve per unit dose comes to 1/CL, as it must. And the fast
phase carries 81.7% of that area, against the paper's own statement of "more
than 80%" -- a quantity the derivation never uses, so it is a genuine
out-of-sample check. This is also the same identifiability test
`R/drugs_codeine.R` already applies and *fails*: codeine's distribution
half-life is not reported, so one compartment was used there. Clevidipine's
is. Implemented.

**3.3 Esmolol's record is formally determined and still unusable, and the
specification's reason is the wrong one.** It says clearance plus an apparent
volume do not determine the vector. In fact Sum 1983's abstract adds a fifth
fact the handoff omits: the apparent volume of 3.43 L/kg "was four times the
volume of the central compartment", which gives V1 directly and
over-determines the model. It still cannot be implemented, for a better
reason. The reported volume and the one implied by clearance over the terminal
rate disagree by 10% (3.43 against 3.78 L/kg), and the two defensible readings
of V1 give peripheral volumes a factor of 4.2 apart, 0.078 against 0.330 L/kg,
because `k12` is then only 1.7% to 7% of the eigenvalue sum -- a difference of
nearly equal large numbers. Formal identifiability is not enough; a
decomposition has to be numerically stable. Clevidipine's is (a 5%
perturbation of any input moves V1 by at most 4%, V2 by 10%, Q by 13%);
esmolol's is not. Status: `requires_parameter_fit`, with the arithmetic
recorded. For an anaesthesia audience this is the most valuable record still
missing, so it is the first one to settle from the full text.

**3.4 The Ericsson response coefficients could not be found anywhere, and
conflict with their neighbours.** The specification gives Emax 45.4% and EC50
12.1 nmol/L for the vascular-resistance proxy. Neither number appears in any
reachable copy of the paper or in any secondary source; the Australian
regulator's assessment of the same programme quotes 42% with an EC50 of 40
nmol/L in one place and 48.2% in another, and Schwieler's study in hypertensive
patients reports roughly 25 nmol/L with about a 50% reduction. One internal
consistency check does hold -- the paper reports mean pressure fell only about
10% even at the top dose rate, so a 45% maximum could only be on the ratio,
not on pressure -- but that argues about the endpoint, not the number.
Recorded as unverified and used nowhere.

**3.5 The spironolactone compartment labels are wrong in a way that would
invert the model.** The specification writes the parent's volumes as `V1/F`
and `V2/F`. In the paper, `V2` is the parent's **central** volume and `V3` the
peripheral; `V4` and `V5` are canrenone's. Anyone mapping "V2/F = 777 L" onto
the paper's `V2` would swap central for peripheral. Two further corrections:
canrenone's clearance is the paper's `CLM`, apparent in both F and the
**fixed** fraction metabolised, so writing it as a plain `CL/F` loses half the
conditioning; and the bootstrap intervals are 99% percentile intervals by the
paper's own words, not 95%. The specification is **vindicated** on the point
it flags as doubtful: the odd-looking `CL*(1-Fm) + CLM1*Fm` definition of
parent total clearance does appear to be the paper's own, not an invention.
What `CL` denotes inside that expression remains genuinely ambiguous in the
accessible text, which is a reason to read the model's differential-equation
block before porting, exactly as the specification says. One more thing the
specification does not mention and a port would need: **no covariates were
retained**, and the authors explicitly decline to endorse a weight
relationship from their narrow weight range, so a port must supply its own
size scaling.

**3.6 Paediatric fenoldopam's baseline covariate cannot be implemented, and
the volume is not the paper's.** The specification describes the model "with
baseline MAP covariate". The paper states baseline mean pressure significantly
improved the fit and then publishes no functional form, no coefficient and no
standard error -- the article has no pharmacokinetic or pharmacodynamic
parameter table at all. So the fitted model cannot be reproduced as published;
only the drug term can. Separately, the paper reports **no volume of
distribution**, so the 0.232 L/kg used here is `CL/kel`, derived and labelled
as such. Everything else the specification says about this record is confirmed
verbatim, including "approximate standard errors" as the paper's own words for
its uncertainties. Two minor corrections: 77 were enrolled but **76**
analysed, and the paper's own per-group counts are internally inconsistent
(the text sums to 76, Table 1 to 77), which is the source's error and should
not be "fixed".

**3.7 Terazosin's effect rate constant is attributed to a symbol and an
equation the paper does not use, and one of its numbers is not credible.** The
specification presents `dCe/dt = ke0(Cp - Ce)` with `ke0 = 0.40 h-1` as the
paper's. The paper's words are a "first-order rate constant" of an
"inhibitory Emax effect compartment model"; it neither prints the equation nor
uses the symbol. More importantly, 0.40 +/- 0.006 /h is a 1.5% coefficient of
variation on a rate constant fitted subject by subject in 24 people, alongside
35% on Emax and 14% on EC50 in the same subjects. A rate constant twenty times
more reproducible than everything fitted with it is not credible; either the
plus-or-minus is a standard error or there is a decimal error. This is a
defect in the **source**, faithfully transcribed by the specification, and no
test should be pinned to 0.006 until the full text settles it. The
specification's "29.9 appears twice" is **not** an error: the paper really does
print 29.9 for the systolic Emax in mmHg and the systolic EC50 in mcg/L, with
different uncertainties.

**3.8 Amlodipine's slope is right and is more dangerous than the
specification says.** Posture, pressure, placebo correction and the value are
all confirmed. Two things to add. The paper says "responsiveness, **for
example**, for erect systolic blood pressure", so other postures and pressures
were fitted and are not transcribed, and citing this as *the* amlodipine slope
without naming both would be wrong. And the concentration range over which it
was estimated is not in the abstract: a linear slope with no ceiling, used
outside the exposure a 5 mg daily dose produces, gives physiologically
impossible falls. The endpoint function therefore warns on every single
evaluation rather than only outside a range.

**3.9 The adult fenoldopam record is underdetermined for a reason the
specification does not give.** It reports the three numbers and says no
executable adult curve follows. True, and the sharper statement is that the
three are **mutually inconsistent in one compartment**: a clearance of 30.3
mL/min/kg with a volume of 582 mL/kg implies a half-life of 13.3 minutes
against the reported 9.8, so the volume is a multi-compartment summary and the
vector cannot be recovered from it.

**3.10 Small things.** Ericsson's 1.8 minutes is the paper's **initial**
half-life, not a "distribution" half-life, and the authors' companion paper
states that this phase is mainly elimination rather than distribution, so the
specification's label contradicts them. Bailey's 32.4 L is "a volume of
distribution" of unstated kind, so it must not be mapped to V1 or to Vss.
Bailey measured **plasma** while Ericsson measured **whole blood**, which the
specification notes in general terms but which deserves to be flagged on the
individual numbers, since no blood-to-plasma ratio is available to bridge them.
The nicardipine abstract's `V1` of 0.22 L for an adult is implausibly small on
its face, which is an additional reason to gate that record. The Tatipalli
paper's own parameter table appears to label two different rows `CLM1`, which
is a publication defect worth knowing about before transcribing from it.

---

## 4. The identifiability question, settled case by case

The specification's rule -- a reported half-life is not permission to set a
clearance and a volume -- is right, but it is a special case of a question with
a definite answer each time. A model with *n* free parameters needs *n*
independent reported quantities, and the decomposition must also be stable.

| Record | Model | Independent reported quantities | Verdict |
|---|---|---|---|
| Clevidipine, healthy (Ericsson) | 2C, 4 parameters | CL, Vss, 2 half-lives = **4** | Determined, stable, corroborated. **Implemented** |
| Fenoldopam, children (Hammer) | 1C, 2 parameters | CL, kel = **2** | Determined. **Implemented** |
| Esmolol (Sum) | 2C, 4 parameters | CL, Vz, V1 = Vz/4, 2 half-lives = 4, one redundant and one inconsistent | Formally closed, **ill-conditioned**. Not implemented |
| Hydralazine IV (Ludden) | 2C, 4 parameters | CL, Vss, terminal half-life = **3** (Varea is implied by the other two) | Underdetermined. Not implemented |
| Fenoldopam, adults (Frederickson) | >1C | CL, Vd, half-life, mutually inconsistent in 1C | Underdetermined. Not implemented |
| Clevidipine, postoperative (Bailey) | 3C, 6 parameters | CL, one volume of unstated kind, one half-life = **3** | Underdetermined. Not implemented |
| Nicardipine infusion (label) | 3C, 6 parameters | 3 half-lives, CL, non-compartmental Vd = 5, and the Vd is not a compartment | Underdetermined. Not implemented |
| Hydrochlorothiazide (Van Wart) | 2C, 4 parameters | the full table: CL, V1 97.3 L, V2 178 L, Q 17.2 L/h | **Determined, internally consistent and ported.** Was blocked instead by a Weibull input, discretised in section 12 |
| Furosemide (Van Wart) | 3C, 6 parameters | non-renal CL, renal CL, F = **3**, no half-life and no volume | Underdetermined. Not implemented |

The useful generalisation: for a two-compartment model, clearance and
steady-state volume plus **both** half-lives is enough and plus only the
terminal half-life is not. That is the difference between clevidipine and
hydralazine, and it is worth checking before concluding that any record is
unavailable.

The hydrochlorothiazide row is the clearest example of the rule paying off,
and of the limits of applying it to an abstract. When this section was
written, the two clearances and the two half-lives were all that could be
read, which left the system short by exactly one volume — and that reasoning
was right: the full text supplied V1 = 97.3 L and the system closed. The
micro constants from the complete table give eigenvalue half-lives of 1.576
and 14.80 h against the paper's own 1.6 and 14.8, so the model is internally
consistent as well as determined.

What the identifiability check could not see is that the obstacle lay
somewhere else entirely. Hydrochlorothiazide's absorption is a **Weibull**
function, not first order, and no count of parameters reveals that.
Sections 10.4 and 12 record what it costs and how it was got round:
**identifiability and representability are different questions, and this
section had only been asking the first.** The caution raised here was answered too: the
supplementary control stream has no bioavailability term at all, so every
volume and clearance in that model is apparent in F, exactly as carvedilol's
are.

---

## 5. What was implemented

**Six drugs, all plotting concentration**, in a new `Antihypertensives`
category:

| Drug | Matrix | Source | What it needed |
|---|---|---|---|
| `clevidipine` | whole blood | Ericsson 1999 (BJCP) | V1, V2 and Q derived from four reported quantities (section 4); externally validated in section 11 |
| `fenoldopamPediatric` | plasma | Hammer 2008 | nothing; named for its population |
| `carvedilol` | plasma | Hwang 2023 | the zero-order oral release mode of section 8 |
| `azilsartan` | plasma | Tsai 2016 | the full text, and a dose basis the paper never states (section 10.2) |
| `chlorthalidone` | plasma | Tsai 2016 | the full text; two of its four covariates deliberately left out (section 10.3) |
| `hydrochlorothiazide` | plasma | Van Wart 2013 | the full text, and the default-form pulse discretisation of section 12 |

The paediatric fenoldopam model is named for its population, following
`methylphenidatePediatric`, so it cannot be selected as an adult default;
there is deliberately no adult `fenoldopam` entry. **None of the six has an
effect site, a MEAC, or a blood-pressure output.** Tests pin both switch
positions of the fat-free-mass scaling, the published half-lives, and the
analytic fixtures the specification asks for: a bolus starting at dose over
central volume (38.73 ng/mL per mg in the reference man) and an infusion
approaching rate over clearance (19.68 ng/mL at 10 mg/h).

Worth noticing about that table: of the four drugs added after the first
pass, **three were blocked by the engine rather than by the evidence** — a
release mode, an input shape, and a dose basis that had to be recovered
arithmetically. None of the four brought a blood pressure with it.

**A registry of 106 model records** (`inst/extdata/antihypertensiveRegistry.csv`,
built by `data-raw/antihypertensiveRegistry.R`): 69 catalog records
establishing product and route identity from the official label, and 37 source
records, one per publication, population and endpoint. Ten are executable, one
is `complete_not_implemented` (section 11.2).
`bp_simulation_enabled` is `FALSE` on all 106.
`external_validation_status` is `none` on 105 of them and
`external_population` on exactly one, clevidipine (section 11).

**A parameter audit of 125 numbers**
(`inst/extdata/antihypertensiveParameters.csv`) with unit, matrix, population,
endpoint, fitted/fixed/derived status, uncertainty, the uncertainty type the
source itself names, the source URL, and the verification state of section 2.
Where a paper prints a plus-or-minus without saying what it is -- Campanero,
Donnelly, Liu, Lima -- the type is `not_reported`, never upgraded by
assumption. Where a number is missing it is `NA` with a note saying what is
missing and why it matters, never filled with a plausible value.

**Four evaluable endpoint relations** (`antihypertensiveEndpoint()` in
`R/antihypertensives.R`): paediatric fenoldopam mean pressure, terazosin
systolic and diastolic, amlodipine erect systolic. Each is evaluated against a
concentration the caller supplies, returns the **change** the drug term
predicts rather than an absolute pressure, and carries its population,
matrix, evidence grade and warnings. Every effect is stored as a positive
magnitude with an explicit direction, so a concentration at the EC50 gives
exactly half the fall -- `-13.5 mmHg` for paediatric fenoldopam, which the
tests check, since storing the published `-27` and subtracting it would raise
the pressure instead.

**Not implemented, deliberately:** no blood-pressure layer, no cosinor
baseline, no slow-adaptation state, no numerical ODE engine, and no
`simulateAntihypertensive()`. The specification asks for the scaffolding and
then requires every part of it to be disabled, because no record has a
complete dose-to-pressure chain with a published baseline. Building a disabled
engine would add a large untested surface whose only honest behaviour is to
refuse, and would invite exactly the mistake the gating exists to prevent. The
registry carries the coefficients as data; when a record earns a pressure, the
engine can be built around a real example rather than around a schema.

**Also untouched, as instructed:** the existing anaesthetic models and the
public `simCpCe()` contract. Suggest Dosing needed no change, because no
blood-pressure endpoint exists for it to optimise against.

---

## 6. What to do next, in order of value per unit of work

1. **Get the PDFs, in the order section 9 sets out.** That list replaces the
   one this item used to carry, which was assembled before the abstracts were
   read systematically and had its priorities wrong in two places. Each paper
   unblocks a named record and none needs new modelling. Two of the three
   Tier 1 papers have since arrived and are sections 10 to 12; **the Van Wart
   2013 furosemide paper is now the top of the list**, and the
   hydrochlorothiazide work makes it a better prospect than it was, since
   furosemide's input is a Weibull too and the machinery for one now exists.
   Hwang 2023 has come off the list entirely: it is open access in PubMed
   Central, which the PubMed server reaches even though the publisher's own
   host is blocked, and section 8 records what its full text settled.
2. ~~**Add a zero-order oral release mode.**~~ **Done**, 2026-10-17, and the
   guess above was right: it is an expansion, not a new solver. See section 8.
2a. ~~**Let `expandOralPulses()` pulse the default oral form.**~~ **Done**,
   2026-10-17, which is what hydrochlorothiazide needed. Also an expansion
   rather than a solver, and the pattern is now twice confirmed: **an input
   shape the engine lacks has so far always been a rewrite of the dose rows,
   never a new engine.** Section 12. The one case that genuinely needed a new
   engine was Michaelis-Menten elimination, and it needed one because it is
   not superposable — which is the test to apply next time.
3. **Settle esmolol.** It is the antihypertensive an anaesthesiologist
   titrates most often. Its obstacle is conditioning rather than access -- the
   abstract alone formally closes the system, as section 4 records -- so what
   the full text is wanted for is the individual-level data, which would allow
   a decomposition per subject instead of one from rounded summary half-lives.
4. **Leave the blood-pressure layer until a record earns it.** The nearest
   candidates are not the famous drugs but the ones whose chain is almost
   complete in a single source.

## 7. One scope note

The specification asks, before calling the library "all commonly used", for
the catalog to be compared against an intended formulary and a dated United
States utilisation list. That comparison has **not** been done: the catalog is
the specification's own list, 69 products across the classes it names, and it
should not be described as complete coverage of any formulary. Doing that
comparison needs a formulary to compare against, which is a question for the
clinician rather than the programmer.

---

## 8. The zero-order oral release mode, and carvedilol (2026-10-17)

Added at the user's request, after the work above. Two separate things
happened: a source turned out to be reachable, and the engine gained a
capability.

### 8.1 The source was verifiable after all

Section 2 put Hwang 2023 among the unreachable papers, because the publisher's
host is blocked here. It is open access and deposited in PubMed Central as
PMC10244018, which the PubMed server does serve, so its full text was read and
Tables 2 and 3 transcribed from it.

Every one of the handoff's carvedilol numbers is **confirmed**: apparent
clearance 153 L/h, apparent volumes 440 and 754 L, apparent intercompartmental
clearance 41.3 L/h, release duration 0.383 h, lag 0.215 h, the clearance
multiplier (the paper fits the effect as 0.328, so the multiplier is
1 − 0.328 = 0.672) and the inhibitory concentration of 16.5 ng/mL. The
relative standard errors and bootstrap intervals are now recorded alongside
them. This is the one record in this work whose transcription could be checked
in full, and it was right throughout.

Three corrections to the handoff's *description*, none to its numbers:

| Handoff said | The paper says |
|---|---|
| "healthy Korean men, single 20 mg" | **21** healthy Korean men, a single **12.5 mg** dose then once-daily **25 mg** |
| clearance multiplier 0.672 for the `*10/*10` group | the fitted parameter is 0.328 for the group the paper calls intermediate metaboliser 2; the allele names could not be read from the machine-readable text, so the registry names the group rather than the diplotype |
| nothing about covariates | age, height and weight were all screened and **none** was significant, so a port must supply its own body-size convention |

The paper also reports the clearance effect as 32.8% in its results and
abstract and 32.7% in its discussion, a small internal inconsistency worth
knowing before quoting either.

### 8.2 The engine capability, which needed no new solver

A zero-order oral fit releases the dose at a constant rate over a duration
instead of with a rate constant, and the first-order extravascular path cannot
represent that. The guess in section 6 was that this would be an expansion
rather than a solver, and that proved right.

The package already classifies a rate unit carrying the PO route word
(`mg/day PO`, which amiodarone uses) as an infusion on the drug's apparent oral
parameters, with neither an absorption constant nor a bioavailability applied.
A zero-order oral dose is exactly that infusion, of finite length. So
`R/oral-zero-order.R` rewrites each such dose into the rate rows that deliver
it, in the same place and the same way that `expandScheduledDoses()` and
`expandOralPulses()` rewrite theirs, and the dose then reaches the
already-tested infusion path. A drug declares
`oralZeroOrder = list(duration = <minutes>, lag = <minutes>)`.

One subtlety was worth getting right rather than discovering later. The
infusion rate on the time line is a **step** function: `simulationTimeGrid()`
sums the rate rows falling at one instant and then holds the latest value. Two
releases that overlap, which happens whenever the dosing interval is shorter
than the release, therefore cannot be written as two independent pairs of rows,
because the second pair's start would *replace* the first release instead of
adding to it and silently lose drug. The expansion computes the total rate
active at every breakpoint instead, and a test pins that the integrated rate
still equals the dose given.

The mode is refused alongside an active metabolite, several oral formulations,
saturable oral or sublingual absorption, parallel systems, Michaelis-Menten
elimination, an osmotic or molar dose basis, pulsed oral release, or an active
first-order oral depot. Each of those reads or rewrites the oral dose rows
itself, and a zero-order release leaves no row flagged `PO` for them to act
on, so saturable absorption in particular would silently do nothing, a
declared `ka_PO` would simply never be applied, and a pulse -- which
`expandOralPulses()` turns into ordinary `PO` rows before this expansion sees
them -- would be released at a constant rate rather than with its own
absorption. Only a *positive* `ka_PO` or `ka_PO2` is a depot: a model writing
`ka_PO = 0` to say it has none is not declaring two absorption models, and
`tlag_PO` alone opens no depot, so neither is refused. The check runs before
the other absorption checks so that a drug declaring both is told which
combination is the problem.

One combination the expansion refuses itself, rather than `getDrugPK()`,
because it is a property of the dose table and not of the model: a standing
oral **rate** row given at the same time as a release. The rows the expansion
writes include the zero that ends the last release, and on a step function
those would replace the standing rate rather than add to it, so the simulation
would quietly lose the infusion. Totalling the two would be the fuller answer
and no drug needs it, so the combination errors rather than being computed
wrongly. A drug dosed only by an oral rate is untouched, which is how
amiodarone's `mg/day PO` keeps working.

### 8.3 Verification

- The simulated curve matches an **independent** closed-form solution of the
  two-compartment constant-rate input, written from the micro constants rather
  than taken from the engine, to 2 parts in 10^14.
- The area under the curve equals dose over apparent clearance, which is the
  handoff's own fixture for an apparent model: the full dose arrives, with no
  second multiplication by F.
- The peak falls exactly at the end of release, as a constant input into a
  linear system requires, and nothing is absorbed before the lag.
- Overlapping, simultaneous, zero and text-valued doses each behave, and the
  scheduled expansion composes with it, so `mg PO qd` releases every repeat.

### 8.4 What is still not modelled for carvedilol

The **CYP2D6 effect** is fitted, verified and deliberately not applied. The
paper splits intermediate metabolisers into two groups and finds reduced
clearance in only one of them, while the Patient Profile has a single
`intermediate` value that cannot separate them; applying the reduction to every
intermediate metaboliser would impose a third less clearance on the patients
the source found normal. It is recorded in the audit as an unapplied covariate.

The **pharmacodynamic endpoint** is the heart-rate rise caused by an injected
dose of isoproterenol, modelled as competitive antagonism. Its coefficients are
complete and verified (baseline 60.4 bpm, isoproterenol ED50 0.685 mcg, maximum
effect 30.7 bpm, carvedilol IC50 16.5 ng/mL), but predicting it needs that
challenge dose as a second input, which the package has no way to express. So
the record is catalogued rather than offered, and its `execution_status` is
`descriptive_only` with `pd_evidence` of `primary_fitted_full` — the one place
in this registry where the evidence is complete and the obstacle is purely that
the endpoint is not expressible here. It is not a blood pressure, and no
antihypertensive in the library plots one.

The **active metabolite** 4'-hydroxyphenyl carvedilol is thirteen times more
potent than the parent at about a tenth of its concentration. The source
measured metabolic ratios below 0.1 and modelled the parent alone; this model
follows it, so the metabolite's contribution is absent rather than zero. The
plotted analyte is **racemic** parent carvedilol, while beta-blocking activity
belongs to the S enantiomer, so an S-enantiomer estimate from another study
(the paper mentions 7.7 ng/mL) refers to a different quantity.

---

## 9. Which papers to obtain, and what each one unblocks (2026-10-17)

Derived from the registry rather than from recollection, and after reading
every abstract that PubMed serves. Ordered by what each PDF produces, not by
how many unverified numbers it carries. Two entries moved from where the
earlier list in section 6 put them, and both moves are noted.

### The structural fact that explains the whole list

This container's outbound proxy refuses Wiley, PubMed, PMC and MDPI at the
tunnel. The one literature route that works is the PubMed MCP server, which
serves **PubMed Central full text**. That is the whole explanation of which
records are verified and which are not: anything deposited in PMC with a
full-text body has been read, and anything behind Wiley has not. Hwang 2023
(carvedilol) was read that way after being written off as unreachable, which
is why section 8 exists. So the request to a human is narrow and specific:
**the Wiley papers.**

One trap worth recording, because it cost a false assumption: a paper can be
*in* PMC and still have no full text there. Ericsson 1999 is PMC2014189, and
the PubMed server returns its abstract with an empty body. Being in PMC is not
the same as being readable.

### Tier 1 -- each produces a new drug curve

**Both of the first two arrived on 2026-10-17 and section 10 records what
they settled. They are kept here, struck through, because the reasoning that
ranked them is worth checking against the outcome: it was right about
Tsai and right about what hydrochlorothiazide was missing, and wrong about
hydrochlorothiazide being implementable once it had it — which cost one
engine extension rather than one paper, and is section 12.**

| Paper | Identifier | What is missing | What it yields |
|---|---|---|---|
| ~~**Tsai 2016**~~ **RECEIVED** | PMID 26632101, J Clin Pharmacol 2016;56:988-98, doi 10.1002/jcph.684 | ~~`ka`, `Q`, `V2/F` for both drugs~~ | **Both drugs ported.** Section 10.1 |
| ~~**Van Wart 2013**, hydrochlorothiazide~~ **RECEIVED** | PMID 24123104, Biopharm Drug Dispos 2013;34:527-39, doi 10.1002/bdd.1863 | ~~one volume, V1 or Vss~~ — supplied, and the real obstacle was a Weibull input | **Ported**, once `expandOralPulses()` accepted the default oral form. Sections 10.4 and 12 |
| **Van Wart 2013**, furosemide | PMID 24151207, doi 10.1002/bdd.1874 | the volumes and the Weibull parameters | furosemide |

**Tsai 2016** is the largest single block, 11 unverified numbers across five
records. The abstract already pins azilsartan's CL/F at 1.47 L/h and V1/F at
3.98 L, chlorthalidone's at 4.13 L/h and 62.1 L, and the two Emax values at
-15.6 and -23.9 mmHg. What it does not print is `ka`, and `ka` governs the
shape of the entire oral curve: it appears in no accessible text or search
result, which is why neither drug can be built from the abstract. Chlorthalidone
is the bigger prize of the two, being a first-line thiazide-type diuretic with
nothing in the library.

**Hydrochlorothiazide** is new to this list and goes near the top, because
section 4's identifiability rule says it is short by exactly one number. Its
two clearances and both half-lives are in the abstract and have now been
verified there; a single volume closes it. Before spending the effort, note
that the oral input is a **Weibull** function, a shape this package's engine
does not have, so part of the work is judging how far a first-order or
zero-order approximation sits from it -- which needs the paper's figures, not
only its table.

**Furosemide** is the same group and the same obstacle. Its intravenous input
is zero order, which the package already represents as an infusion, so the
injection route is the nearer of its two to being portable.

### Tier 2 -- completes a record or settles a question

- **Ericsson 1999, the BJCP one** (PMID 10336577, Br J Clin Pharmacol
  1999;47:531-8, doi 10.1046/j.1365-2125.1999.00933.x). Clevidipine's own
  implemented source, still read only from its abstract. **Promoted to the
  top of Tier 2 on 2026-10-17**, by a result rather than by a guess: the third
  gain listed here -- checking the derived V1, V2 and Q -- arrived from the
  *other* Ericsson 1999 paper instead (section 11), and the check passed to
  1.1%. That makes the remaining gap sharper, not smaller. The derivation is
  now validated while its four INPUTS are still abstract-only, which leaves
  clevidipine the one live curve in this library whose own parameters have
  never been read in a full text. The other two gains stand: the five PK
  numbers rise from `primary_abstract` to `primary_full_text`, and the Emax
  and EC50 of the MAP-over-HR resistance proxy become available.
- **Bailey 2002** (PMID 11981147, Anesthesiology 2002;96:1086-94, doi
  10.1097/00000542-200205000-00010). A second, independent clevidipine dataset
  in cardiac surgery patients: three compartments, V 32.4 L, CL 4.3 L/min,
  early half-life 0.6 min, and C50 of 9.7 mcg/L for a MAP fall of 10% or more
  and 26.3 for 20% or more. Wanted as an **external validation target** for the
  healthy-volunteer model, and for the logistic slope. It would still not plot
  a pressure: its endpoint is the probability of crossing a threshold.
- **Sum 1983** (PMID 6617063, Clin Pharmacol Ther 1983;34:427-34, doi
  10.1038/clpt.1983.193). Esmolol. This is the second entry that moved, and it
  moved **down** from the earlier list and then back up to here, so the
  reasoning is worth stating. Its abstract prints both half-lives (2.03 and
  9.19 min), clearance 285 mL/min/kg, and an apparent volume of 3.43 L/kg that
  it states is four times the central volume -- which formally closes a
  two-compartment system, so the record is not blocked for want of numbers.
  What it is blocked by is conditioning: that half-life pair with that
  clearance puts V2 and Q on a knife edge where rounding in the printed
  summaries swings them widely. A PDF cannot fix conditioning, which is why the
  first instinct was to drop this paper. But the full text's value is not its
  summary table: it is the **individual-level data**, which would allow a
  decomposition per subject rather than one from rounded means, and that is a
  different and better input. Worth obtaining, for that reason and not the
  obvious one.
- **Tatipalli 2021** (PMC8228864), the tables and Figure 1. The handoff's
  labelling here is wrong in a way that would invert the model: it calls the
  paper's V2 and V3 "V1/F and V2/F" when V2 is the parent's central volume.

### Tier 3 -- not worth obtaining

- **PAGE nicardipine 2007.** Six unverified numbers, the second-largest block,
  and deliberately skipped: the specification itself said not to port it from
  partial anchors, and the abstract and the poster disagree on clearance.
- **Campanero 2008** and **Donnelly 1993.** These would settle whether a
  printed plus-or-minus is a standard error, a standard deviation or a
  coefficient of variation. They change a metadata field, not a number.

### What reading the abstracts settled without any PDF

Three numbers that the handoff supplied unverified are printed in their own
abstracts and match exactly, so they are now `primary_abstract` rather than
`handoff_unverified`: hydrochlorothiazide's non-renal clearance of 2.44 L/h and
renal clearance of 18.3 L/h, and furosemide's bioavailability of 0.591. The
supporting ladders match too -- hydrochlorothiazide's renal clearance of 10.5,
5.47 and 2.70 L/h and furosemide's of 4.67, 3.11, 1.95 and 1.17 L/h, across
creatinine clearances of 80, 50 and 30 and of 120, 80, 50 and 30 mL/min. Two
further numbers, hydrochlorothiazide's two half-lives, were in the abstract and
not in the handoff at all; they are now recorded, and they are what moved that
record up Tier 1. The lesson for the next registry entry is to read the
abstract before asking anyone for the paper.

---

## 10. What the two Tier 1 papers settled (2026-10-17)

Steven L. Shafer supplied the full texts of Tsai 2016 and Van Wart 2013, the
two papers section 9 ranked first, together with their supplementary
material. Between them they moved 19 numbers from `handoff_unverified` to
`primary_full_text`, added two drugs, and replaced one guessed obstacle with
a real one. The ledger stands at 77 verified of 113, and nine model records
are executable rather than seven.

### 10.1 Tsai 2016: both drugs port, and the handoff was right throughout

Every one of the handoff's azilsartan and chlorthalidone numbers is confirmed
by Tables 1 and 2, including the five that section 9 called decisive —
azilsartan's `ka` of 0.195 /h, its fixed `Q` and `V2/F`, and chlorthalidone's
`ka`, `Q` and `V2/F`. Two details the registry had flagged as unsettled are
settled in the handoff's favour: azilsartan's `Q` and `V2/F` really are
**fixed**, confirmed by Table 1's own footnote and their NA bootstrap
intervals, while every one of chlorthalidone's disposition parameters is
**estimated**. Chlorthalidone's matrix, which the registry called
uninterpretable, is **plasma**.

Both are now drugs: `R/drugs_azilsartan.R` and `R/drugs_chlorthalidone.R`,
concentration only, with help pages and tests.

### 10.2 The dose basis azilsartan's paper never states

Azilsartan medoxomil is a prodrug and the model is of azilsartan. Nothing in
the paper says which mass its doses are in, and the difference is 20% — not a
rounding, a fifth of the curve.

The paper's own simulated exposures settle it. For a linear model AUC is dose
over clearance, so the mass that entered the model is AUC × CL/F:

| Dose of AZL-M | Simulated AUC | Implied mass | Dose × 456.45/568.52 |
|---|---|---|---|
| 20 mg | 10 919 ng·h/mL | 16.05 mg | 16.06 mg |

A match to 0.04%. The doses are azilsartan, converted from the medoxomil by
molecular weight. The same check discriminates against the marketed potassium
salt, whose ratio would be 0.752 and does not fit. The 40 and 80 mg points
imply 0.797 and 0.783 rather than 0.803, drift consistent with those AUCs
being population summaries over the covariate distribution rather than the
typical-value AUC; the 20 mg point, where that term is smallest, pins the
constant.

The port therefore divides every apparent volume and clearance by the ratio,
which puts them on a per-milligram-of-medoxomil basis and leaves every rate
constant, half-life and time to peak exactly as published. A dose is entered
as it is prescribed.

**The whole chain is checked end to end.** Running the implemented drugs at
the source's own reference subject reproduces all ten exposures the paper
simulated: AUC within 0.4%, 0.3% and 2.1% at 20, 40 and 80 mg of azilsartan
medoxomil and within 0.2% and 2.3% at 12.5 and 25 mg of chlorthalidone; the
steady-state peaks on once-daily dosing within 0.8% to 3.9% and 2.5% to 8.4%.
Those are population means against a typical subject, so agreement at a few
percent is the most that can be asked, and it is what the tests pin.

### 10.2a A caveat that only an outside check reveals

Both drugs reproduce the paper's own exposures, which says the transcription
is right but cannot say whether the model extrapolates. Checked instead
against the labels:

| | Model | Label |
|---|---|---|
| Azilsartan, time to peak | 3.0 h | 1.5–3 h |
| Azilsartan, terminal half-life | **26 h** | **11–12 h** |
| Chlorthalidone, time to peak | 1.4 h | 1.5–6 h |
| Chlorthalidone, terminal half-life | 32 h | 40–60 h |

Azilsartan's terminal half-life is more than twice the labelled value, and the
reason is structural rather than a slip. Sampling was sparse and confined to
one dosing interval — troughs at weeks 4 and 6, one sample 1 to 4 h after a
dose and two more 4 to 12 h after — so nothing in the dataset observes the
drug beyond 24 h. That is why the authors **fixed** Q and V2/F instead of
estimating them, and those two fixed numbers are exactly what set the terminal
slope. The 26 h is an extrapolation from fixed parameters, not a fitted
half-life.

The practical reading: both curves are trustworthy across a dosing interval,
which is the window they were fitted in and where every simulated exposure
matches; azilsartan's tail is not evidence about its terminal phase. The drug
header and help page say so.

This is worth generalising. Reproducing a source's own published outputs, as
section 10.2 does, tests the transcription and nothing else — a model can
match every number a paper prints and still extrapolate badly outside the
window it was fitted in. The cheap external check is the label.

### 10.3 The one number in Tsai that a reader cannot use

Chlorthalidone's volume equation is printed as

> V<sub>d</sub>/F = 62.1 L · (1 + Sex · 1.19) · 2.41 · (BMI − 30.9 kg/m²)

which cannot be read as written: as a product it gives a volume of **zero** at
the reference body mass index of 30.9 and a negative volume below it. The
surrounding text says the volume increased *linearly* with body mass index, so
an additive term is the likely intent — but likely is not published, and the
supplementary material holds only a demographics table and a diagnostic
figure, with no control stream to settle it. At the reference every candidate
reading gives 62.1 L, so the typical patient is unaffected; the term is left
out rather than guessed, and the drug's help page says so.

The cystatin C term on clearance is left out for an unrelated and firmer
reason: **the Patient Profile has no cystatin C field, and serum creatinine is
not a substitute.** The authors chose cystatin C precisely because it is
cleared only by glomerular filtration; feeding creatinine into a term fitted
to cystatin C would invent a covariate the source never estimated.

One transcription slip worth recording: Supplemental Table S1 prints the
cystatin C units as mg/dL, while the equation, Figure 2 and the observed
median of 0.73 are all mg/L. 0.73 mg/dL would be ten times a normal value.

### 10.4 Van Wart 2013: the volume was there, and the obstacle was elsewhere

Section 4 reasoned that this record was short by exactly one volume. That was
right, and the full text supplies it: **V1 = 97.3 L, V2 = 178 L, Q = 17.2
L/h**, with renal clearance 18.3 × (CL<sub>CR</sub>/120)<sup>1.38</sup> and
non-renal clearance 2.44 L/h. The micro constants reproduce the paper's own
half-lives of 1.6 and 14.8 h to three figures, so the model is internally
consistent. The supplementary **NONMEM control stream** confirms every theta
and shows there is no bioavailability term, so the parameters are apparent.

**One discrepancy.** Table 2 prints V<sub>c</sub> = 97.3 L; the control
stream's `$THETA` prints 97.5, while its other seven thetas match Table 2
exactly. The table is taken as the final estimate. The difference is 0.2% and
moves no curve, but a reader comparing the two should know it is there.

**The real obstacle is the input shape.** Absorption is a Weibull function,

> dA<sub>po</sub>/dt = −(1 − e<sup>−(k<sub>a</sub>·TSLD)<sup>γ</sup></sup>) · A<sub>po</sub>

with γ = 2.84, not 1. The depot's rate constant is therefore time-varying,
rising from zero towards 1/h over about 1.5 h, and the package's
extravascular path cannot represent it. No parameter count reveals this,
which is the lesson: **identifiability and representability are different
questions, and this registry had only been asking the first.**

How much that costs was measured, against a numerical integration of the
exact system, using the discretisation standard MDMA set. **These are the
scoping figures, superseded by section 12:** each was fitted at one renal
function with the depot rate constant free, and the drug that shipped is
fitted across the paper's whole clearance ladder with that constant fixed.

| Approximation | Peak error | Max error over the curve | AUC ratio |
|---|---|---|---|
| One lagged first-order depot | −1.14% | 12.15% | 0.99986 |
| Two lagged depots (`ka_PO2`) | −0.31% | 2.96% | 0.99999 |
| Five lagged pulses | −0.03% | 0.52% | 1.00001 |

Even as a scoping exercise two things were already clear. Lagged fractions
reach the fidelity MDMA's zero-order depot was held to (0.01% at the peak,
0.53% over the curve), and the fitted rate constant converges to 1.0 /h — the
Weibull's own asymptotic value, which is evidence the approximation has found
the real structure rather than overfitting. Both survived the proper fit;
what did not survive was the pulse count, which went from five to six once
the criterion covered every patient rather than one.

So hydrochlorothiazide was **one small engine extension away, not blocked**:
`expandOralPulses()` is keyed by a formulation word in the dose unit, and an
immediate-release tablet has none, so it needed to accept the default oral
form. That extension and the drug are now both in, and section 12 is what
they came to. The numbers in the table above were measured at one renal
function and with the depot rate constant fitted; section 12 replaces them
with figures measured across the paper's whole clearance ladder, which is the
form the drug's test pins.

## 11. Clevidipine's derived volumes, checked against a measurement (2026-10-17)

Section 5 implemented clevidipine by *deriving* V1, V2 and Q from the four
quantities Ericsson's 1999 *Br J Clin Pharmacol* population analysis reports.
The derivation is unique and the drug header argues for it at length, but an
argument is not a measurement, and the registry's
`external_validation_status` field had been `none` on every record in it.

It is now `external_population` on one of them, because of a **second**
Ericsson 1999 paper, in a different journal:

> Ericsson H, Fakt C, Höglund L, Jolin-Mellgård Å, Nordlander M, Sunzel M,
> Regårdh CG. Pharmacokinetics and pharmacodynamics of clevidipine in healthy
> volunteers after intravenous infusion. *Eur J Clin Pharmacol*
> 1999;55:61–67 (doi 10.1007/s002280050594).

A different study, not a reprint: **eight** volunteers rather than
twenty-five, **one hour** of infusion rather than twenty minutes, blood
sampled frequently out to **thirty-two hours** rather than sparsely within a
dosing period, with a tritiated tracer dose and a mass balance. And its
Table 1 prints, as measurements, the very quantities the implemented record
has to derive: **V1, k10, both bolus intercepts, and the fast phase's share
of the area under the curve.**

It is worth saying what this paper does *not* contain, since its title
promises pharmacodynamics: no concentration-effect model of any kind. Blood
pressure and heart rate are reported as time courses and group means — mean
arterial pressure fell from 87 to 77 mmHg at the highest rate and returned to
pre-dose values within fifteen minutes of stopping — and nothing relates
either to a concentration. Its `pd_evidence` is `proposed_unfitted`, and
those two pressures live in the record's limitations field rather than the
parameter audit, because an observed group mean is not a parameter.

That turns the derivation into something testable. Apply it to *this* study's
own CL, Vss and two half-lives — 0.142 L/min/kg, 0.58 L/kg, 1.6 min and
15.5 min — and compare what comes out against what the same table reports:

| Quantity | Derived from its four numbers | Its own reported value | Difference |
|---|---|---|---|
| V1 | 0.3561 L/kg | 0.36 L/kg (SD 0.12) | **−1.1%** |
| k10 | 0.3988 /min | 0.40 /min (SD 0.05) | **−0.3%** |
| Bolus intercept, initial phase | 99.01% | 98.9% (SD 0.8) | +0.1% |
| Fast phase's share of AUC | 91.1% | 92.0% (SD 4.3) | −0.9% |
| Bolus intercept, terminal phase | 0.99% | 1.1% (SD 0.8) | −9.7% |

Four of the five land within about a percent. The fifth is the terminal
intercept, where −9.7% *relative* is 0.13 of that mean's own standard
deviation: a quantity whose between-subject SD is 73% of its mean cannot
discriminate at that resolution, and it is in any case the complement of the
initial intercept rather than an independent statement. V1's agreement is
**0.09 of the standard error** of the reported mean (0.12/√8 = 0.042 L/kg).

A second, weaker check falls out of the same table. The implemented record's
own derived V1, from the *other* study's numbers, is 0.369 L/kg — 2.5% from
this study's measured 0.36, in different subjects under a different infusion.

So the method that fills in clevidipine's missing volumes recovers a measured
V1 when a measured V1 exists to recover. That is the strongest claim this
registry can currently make about any of its records, and it is why
clevidipine is the one row whose validation field is not `none`.

### 11.1 Why the validating study is not the implemented one

It would be tempting to switch to the paper with the richer sampling. The
library does not, for a reason worth recording: the twenty-five-subject
analysis covers a **400-fold range of dose rates** (0.12 to 48
nmol/min/kg) and found concentration linear across all of it, while this one
is a single rate in eight people. For a drug given as an infusion titrated
over a wide range, the dose-range coverage is worth more than the tail.

The estimates do differ, and in a direction that is itself informative:

| | BJCP, 25 subjects (implemented) | EJCP, 8 subjects (validation) |
|---|---|---|
| CL | 0.121 L/min/kg | 0.142 L/min/kg |
| Vss | 0.56 L/kg | 0.58 L/kg |
| Initial half-life | 1.8 min | 1.6 min |
| Terminal half-life | **9.5 min** | **15.5 min** |

The terminal half-life is the one that moves, and it moves the way sampling
duration predicts: the study that followed the drug for thirty-two hours saw
a longer tail than the study that did not follow it past a dosing period.
That is the same lesson azilsartan's 26-hour tail teaches in section 10.2a,
arriving from the opposite direction — and it says plainly which part of the
implemented clevidipine curve is the weaker part. It is recorded in the
drug's header.

### 11.2 A word the registry needed

This paper could not be filed. It is a complete, verified, primary-fitted
model, so `descriptive_only` would deny that it has a vector; but the library
deliberately does not plot it, so `ready_for_source_reproduction` — which
this registry's tests read as "is an implemented drug" — would promise a
second clevidipine curve.

`execution_status` therefore gained a fifth value,
**`complete_not_implemented`**: a model read in full and verified, whose
numbers may be used to *check* an implemented record and are never an input
to one. The gap it fills is not a one-off. Most of these drugs have several
published models, and a registry that can only say "usable" or "descriptive"
has nowhere to put the good paper it chose against. Its test asserts both
halves: that such a record carries fully verified parameters, and that none
of them reaches a curve.

## 12. Hydrochlorothiazide, and discretising an input shape (2026-10-17)

Section 10.4 left hydrochlorothiazide verified in full and unimplementable,
blocked not by a missing number but by a missing input **shape**: a Weibull
oral absorption with a shape parameter of 2.84, whose depot rate constant
rises with time where `ka_PO` is a constant. It is now a drug, and getting
there needed one small engine change and one decision about what accuracy to
hold an approximation to.

### 12.1 The engine change

`expandOralPulses()` already gave a dose as fixed fractions at fixed delays —
that is how Adderall XR's two bead populations are modelled. It was keyed by
a **formulation word** in the dose unit (`mg PO XR`), and an immediate-release
tablet has none. So `oralPulses` gained a reserved key,
`ORAL_PULSE_DEFAULT_FORM` (`"default"`), which matches every plain oral row
and leaves its unit exactly as entered.

The code is a dozen lines. The distinction it introduces is worth more than
the code, and `R/oral-pulses.R` now states it: **the same mechanism is doing
two different jobs.**

| | A formulation's pulses | The default form's pulses |
|---|---|---|
| What they are | a model of a real product's release | a discretisation of a published curve |
| Evidence | the beads exist; the unit names the product | the source's own input function |
| Example | Adderall XR (Tulloch 2002) | hydrochlorothiazide's Weibull |
| What the header owes | the product and its bioequivalence data | the shape stood in for, and the measured cost |

A block may not declare both at once: an `mg PO XR` row is also an oral row,
so which pulses applied would depend on the order the block was written in.
The validator refuses it rather than resolving it by declaration order.

One other row the default form has to leave alone, which is easy to miss: a
standing oral **rate**, `mg/day PO` (amiodarone's unit). It is an oral row
with no formulation word, so the naive match would catch it — and splitting a
constant rate into fractions at delays would turn one infusion into several
starting at different times, which is not what it means. A rate has no
release to discretise; it is already continuous. It is skipped rather than
refused, because the pulses emit amount rows and so coexist with a rate row
correctly, unlike the zero-order release of section 8, whose terminating zero
would replace it. No drug in the library declares both today; the guard is
there so that the first one to try does not get a silently wrong curve.

### 12.2 What to fix and what to fit

The depot rate constant is **not fitted**. The bracket multiplying the depot
amount, 1 − e<sup>−(k<sub>a</sub>t)<sup>γ</sup></sup>, is dimensionless and
tends to 1 as *t* grows, and the source writes the equation in hours, so
**one per hour is the exact asymptotic rate constant of the source's own
Weibull depot** — a property of the published model rather than of any fit to
it.

Fitting it freely gives 0.982, 0.993 and 0.996 /h at two, three and four
pulses — it converges there, which is the evidence that fixing it forces
nothing. What fixing it costs, at four pulses, is a tenth of a percent: the
worst point on the curve moves from 0.92% to 1.02% of the peak and the peak
itself from −0.04% to +0.06%. So it is fixed at 1.0 /h and only the fractions
and delays are fitted, which removes a number from the model and replaces it
with a property of the source.

Two choices about the fit itself turned out to matter more than the number of
pulses.

**Fit the curve you plot, not the input.** Fitting the pulses to the Weibull's
*input rate function* is the more obviously principled thing to do — the
input is a property of the release alone, so the fractions cannot absorb a
disposition difference. It is also substantially worse: five pulses fitted to
the input rate leave 2.25% of peak error in plasma, where five fitted to
plasma leave 0.61% — and that comparison is if anything kind to the rate fit,
since it had the depot rate constant free where the plasma fit had it fixed. The reason is that the central compartment integrates the
input, so a least-squares fit to the rate spends its effort matching the
spiky rises of a sum of lagged exponentials against a smooth curve, errors
that then partly cancel in the convolution. The error that matters is in the
plotted concentration, so that is the criterion.

**Fit every patient at once.** One pulse set is stored as a constant and has
to serve everyone, so it is fitted to the plasma curves at all four
creatinine clearances the paper tabulates (120, 80, 50 and 30 mL/min), each
normalised by its own peak, rather than to a typical patient. That it works
is itself the evidence the discretisation is describing the release: the
residual error is **essentially flat across a sevenfold range of clearance**,
which it would not be if the fractions had absorbed something that belongs to
disposition.

### 12.3 What it costs, measured

Against a midpoint integration of the exact three-state Weibull system,
pooled fit, ka fixed at 1.0 /h, as a percentage of the peak concentration:

| Pulses | Peak error | Worst point on the curve | AUC ratio |
|---|---|---|---|
| 2 | +0.26% | 3.75% | 1.00002 |
| 3 | +0.11% | 1.63% | 1.00001 |
| 4 | +0.06% | 1.02% | 1.00000 |
| 5 | +0.04% | 0.61% | 1.00000 |
| **6** | **+0.02%** | **0.45%** | **1.00000** |

Six is what shipped, because six is where the worst point on the curve comes
inside the **0.53%** that MDMA's zero-order discretisation was held to — and
it does so at *every* renal function, the worst of the four being 0.45% and
the best 0.38%. Five would have been 0.61%, outside it. The worst point falls
on the rising limb, where the pulse structure is still visible; by the peak
the error is 0.02% and the area is exact to a thousandth of a percent.

The drug's test checks this directly: it writes out the three-state Weibull
integration independently of the package and compares it with what
`simulateDrugsWithCovariates()` returns, at all four renal functions. It does
*not* check the fractions. If a later change to them or to
`expandOralPulses()` degrades the curve, the test fails on the curve, which
is the thing worth protecting.

### 12.4 What the drug inherits from its source

Renal function is the entire covariate model — the paper fitted no body-size
covariate at all, so weight reaches the drug only through Cockcroft-Gault,
which contains it. That makes the **Serum creatinine** field the most
consequential input on the page for this drug, and makes the usual rule bite
harder than usual: a blank field is an assumed normal creatinine for age and
sex, so it models a patient with **no** renal impairment. The help page and
the drug header both say so.

The non-renal clearance of 2.44 L/h carries no covariate and acts as a floor.
At the highest creatinine the Patient Profile accepts, the renal term has
fallen roughly fiftyfold while total clearance has fallen about sixfold; the
half-life lengthens a long way and never without limit. The paper's own
figures agree — a terminal half-life of 14.8 h at a creatinine clearance of
120 mL/min, 29 h at 50 and 42 h at 30.

### 12.5 One part of the source left on the table

Van Wart fits plasma and urine jointly — 137 urinary amounts beside 823
plasma concentrations — and the paper's own stated conclusion is that the
model predicts the **rate of urinary excretion of the drug** from dosing
history and creatinine clearance. That is not implemented, because the
program has no urinary-amount output to draw it in.

Worth recording for whoever wants it, with two observations. First, leaving
it out costs no accuracy in what *is* plotted: the urine compartment
accumulates what renal clearance has already removed from plasma, so it is a
sub-partition of the elimination in `cl1` rather than a pathway competing
with it, and the plasma curve is bit-for-bit what the full model gives.
Second, it is a genuinely separate feature rather than a missing parameter —
an output channel, not a model term — and the one drug class where it would
earn its keep is this one, since a diuretic's urinary recovery is closer to
its effect than its plasma concentration is. Furosemide's record (section 9)
is a plasma-and-urine fit too.

Ten of the registry's 106 records are now executable. None of them plots a
blood pressure.
