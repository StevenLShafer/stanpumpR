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
time". That cannot be done from this container.

`WebFetch` fails DNS resolution for every host, and the egress proxy returns
a policy denial for Wiley, PubMed, PubMed Central, NCBI E-utilities, MDPI,
Springer, Google Scholar and the rest. The one working literature channel is
the PubMed MCP server, which serves bibliographic records, abstracts, and
PubMed Central full text where a machine-readable copy exists.

So the sources split three ways:

| Reachability | Sources | Consequence |
|---|---|---|
| Full text read | Hammer 2008 paediatric fenoldopam; Tatipalli 2021 spironolactone (narrative only, tables stripped) | Numbers verified, or structure verified with the table missing |
| Abstract only | Ericsson 1999 clevidipine; Bailey 2002 clevidipine; Campanero 2008 terazosin; Donnelly 1993 amlodipine; Sum 1983 esmolol; Tsai 2016 azilsartan/chlorthalidone | Whatever is in the abstract is verified; the parameter tables are not |
| Unreachable | everything else in the ledger | The handoff's transcription stands unchecked |

Every number is therefore labelled in
`inst/extdata/antihypertensiveParameters.csv` with how it was checked:
`primary_full_text`, `primary_abstract`, `derived_here` or
`handoff_unverified`. 39 of the 95 catalogued numbers are verified or derived
from verified numbers; 56 are not. A test asserts that **no record this
package executes depends on an unverified number**.

This is not a complaint about the environment. It is the reason the
implemented surface is two drugs and four endpoint relations rather than the
six or seven ports the specification sketches, and it is the single thing to
fix before the next round: library copies of about six PDFs would move more
records than any amount of further work here.

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

The useful generalisation: for a two-compartment model, clearance and
steady-state volume plus **both** half-lives is enough and plus only the
terminal half-life is not. That is the difference between clevidipine and
hydralazine, and it is worth checking before concluding that any record is
unavailable.

---

## 5. What was implemented

**Two drugs, both plotting concentration.** `clevidipine` (whole blood, ng/mL,
`R/drugs_clevidipine.R`) and `fenoldopamPediatric` (plasma, ng/mL,
`R/drugs_fenoldopamPediatric.R`), in a new `Antihypertensives` category. The
paediatric fenoldopam model is named for its population, following
`methylphenidatePediatric`, so it cannot be selected as an adult default;
there is deliberately no adult `fenoldopam` entry. Neither drug has an effect
site, a MEAC, or a blood-pressure output. Tests pin both switch positions of
the fat-free-mass scaling, the published half-lives, and the analytic fixtures
the specification asks for: a bolus starting at dose over central volume
(38.73 ng/mL per mg in the reference man) and an infusion approaching
rate over clearance (19.68 ng/mL at 10 mg/h).

**A registry of 105 model records** (`inst/extdata/antihypertensiveRegistry.csv`,
built by `data-raw/antihypertensiveRegistry.R`): 69 catalog records
establishing product and route identity from the official label, and 36 source
records, one per publication, population and endpoint. Six are executable.
`bp_simulation_enabled` is `FALSE` on all 105 and
`external_validation_status` is `none` on all 105.

**A parameter audit of 95 numbers**
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

1. **Get six PDFs.** Tsai 2016 (J Clin Pharmacol 56:988-998) for `ka`, `Q`,
   `V2/F`, the metabolite block, the covariate equations and the matrix; Sum
   1983 for esmolol's individual-level volumes; Bailey 2002 for the logistic
   slope and the kind of the 32.4 L volume; Ericsson 1999 for the response
   coefficients; Campanero 2008 for the uncertainty type and the oral
   parameters; Tatipalli 2021's table and differential-equation block. Each
   unblocks a named record, and none needs new modelling.
2. **Add a zero-order oral release mode.** Carvedilol's fit is complete and
   blocked only by the engine: a lagged zero-order input is representable as a
   short infusion of the apparent dose, so the natural home is an expansion in
   `R/routes.R` alongside `expandOralPulses()`, not a new solver. That one
   addition also opens the door to several extended-release products.
3. **Settle esmolol.** It is the antihypertensive an anaesthesiologist
   titrates most often, and the only obstacle is one paper's individual data.
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
