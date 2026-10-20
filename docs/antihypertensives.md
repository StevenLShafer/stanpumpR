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
recorded.

**Settled, 2026-10-19, and not from this source.** The drug an anaesthesiologist
titrates most often is now in the library as `esmolol`, under *Vasoactive and
resuscitation* rather than here, from de Bruijn 1987 (*Anesthesiology*
66:323-326): whole-blood esmolol in nineteen anaesthetised cardiac surgical
patients, two compartments fitted by NONLIN with the means printed in Tables 3
and 4, and the beta-phase volume as an independent check. So the conclusion of
this section stands -- Sum 1983's abstract cannot be decomposed -- and the
lesson generalises past it: **when a record is blocked by conditioning rather
than by access, the answer may be a different study rather than the full text of
this one.** A second source with four independent numbers beats a first source
with three, however much better the first source's population looks. Sum 1983's
record stays `requires_parameter_fit` because the arithmetic in it is worth
keeping; nothing is waiting on it.

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

**A registry of 127 model records** (`inst/extdata/antihypertensiveRegistry.csv`,
built by `data-raw/antihypertensiveRegistry.R`) over 81 drugs: 90 catalog
records establishing product and route identity, of which 66 were written with
an official label open, 3 with another source document open (a journal paper
for azilsartan medoxomil, a regional registration for canrenone, a regional
monograph for urapidil), and 21 assert ingredient identity only because no
document could be retrieved at all (section 13), and 37 source records, one
per publication, population and endpoint. Ten are executable, one is
`complete_not_implemented` (section 11.2).
`bp_simulation_enabled` is `FALSE` on all 127.
`external_validation_status` is `none` on 126 of them and
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
3. ~~**Settle esmolol.**~~ **Done**, 2026-10-19, and by the move section 3.3
   now records: not from Sum 1983's full text but from de Bruijn 1987, a
   different study with four independent numbers instead of three. It is in
   the library as `esmolol`, filed under *Vasoactive and resuscitation*.
4. **Leave the blood-pressure layer until a record earns it.** The nearest
   candidates are not the famous drugs but the ones whose chain is almost
   complete in a single source.

## 7. One scope note

The specification asks, before calling the library "all commonly used", for
the catalog to be compared against an intended formulary and a dated United
States utilisation list. That comparison has **not** been done: the catalog
began as the specification's own list, 69 products across the classes it
names, and it should not be described as complete coverage of any formulary.
Doing that comparison needs a formulary to compare against, which is a
question for the clinician rather than the programmer.

**Partly closed on 2026-10-18.** Section 13 enumerates the five classes the
registry exists for — angiotensin-receptor blockers, ACE inhibitors,
calcium-channel blockers, beta blockers and diuretics — adding 21 records and
arguing each deliberate exclusion. The inventory it is measured against is
the approved ingredients **as section 13.1 assembles them**, which no FDA
register was reachable to confirm (13.2): so a molecule's absence from those
five classes is a claim this program makes, not yet one the Orange Book or
DailyMed has agreed to. It is still not a formulary comparison, and the
paragraph above still stands for every other class.

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
the real structure rather than overfitting. Both survived the proper fit. What did not
survive was the pulse count, nor the accuracy figures: the count went from
five to six once the criterion covered every patient, and then to **eight**
once the error was measured at the pulse times rather than between them, which
is section 12.3.

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

### 12.3 What it costs, measured — and the measurement that was wrong

**This section replaces a wrong one.** Its first version reported six pulses at
0.45% of peak and called the standard met. That number came from evaluating
the error on a uniform 0.02 h grid, and it was wrong by a third: the true
figure for that pulse set is **0.64%**, outside the standard. The error is
worth more than the correction.

**Where a discretisation's error actually lives.** Each lagged depot starts
abruptly, so the approximation has a **kink at every pulse time**, and that is
where it deviates most from the smooth exact curve. A uniform grid steps over
the kinks and reports whatever it happens to sample between them. The app does
not: `simulationTimeGrid()` puts a point at every dose time, so a user sees
precisely the worst points. **The figure being published was the one not being
measured.** CodeRabbit found it on PR #532, from the other end — it noticed the
test allowed 1.5% where the doc promised 0.5%, and tightening the bound to the
promise is what made the real error visible. A bound looser than the claim does
not test the claim.

**Two corrections to the method.** First, the error is measured at the pulse
times. Second — and this is the subtler one — the delays *move during the fit*,
so no fixed grid is safe, not even a dense one: a grid built from the starting
delays goes stale as they shift. The objective therefore rebuilds its grid for
every candidate (a uniform backbone plus a neighbourhood of that candidate's
own pulse times), with the exact system integrated once per renal function at
h = 1/4000 and interpolated, which is legitimate because the exact curve is
smooth — only the approximation kinks. And the objective is the **maximum**,
not the sum of squares, because the maximum is what the standard limits.

**Both of MDMA's figures are held**, each normalised by its own limit so the
fit cannot buy one with the other. That matters: a curve-only minimax at seven
pulses reaches 0.343% on the curve but lets the peak drift to 0.063%, six times
MDMA's figure. Minimising the worse *ratio* prevents that trade.

The honest ladder, measured on the package's own output grid:

| Pulses | Worst point on the curve (limit 0.53%) | Peak (limit 0.01%) | Verdict |
|---|---|---|---|
| 6 | 0.593% | 0.006% | **fails the curve**, even fitted minimax |
| 7 | 0.516% | 0.008% | passes with 3% of headroom — too thin to defend |
| **8** | **0.344%** | **0.0061%** | **both met, 35% and 39% headroom** |

Eight is what shipped. The worst point sits at 0.82 h in every patient, and the
area is exact to a thousandth of a percent. The fit's own estimate (0.341% and
0.0064%) and the package measurement (0.344% and 0.0061%) now agree, which is
the cross-check that the grid problem is gone — under the old method they
disagreed by a third and nothing noticed.

The drug's test checks this directly: it writes out the three-state Weibull
integration independently of the package and compares it with what
`simulateDrugsWithCovariates()` returns, at all four renal functions, and it
does *not* check the fractions. Its bounds **are** the documented standard,
0.53% and 0.01%, not a loose fence around the current numbers — which is the
one change that would have caught this before review.

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

Ten of the registry's 127 records are now executable. None of them plots a
blood pressure.

---

## 13. Closing the five classes, and a weaker kind of citation (2026-10-18)

Section 7 said the catalog was the specification's own list of 69 products and
should not be called complete coverage of anything. This section closes that
for the five classes the registry exists to enumerate — angiotensin-receptor
blockers, ACE inhibitors, calcium-channel blockers, beta blockers and
diuretics — and is explicit about the one thing that got worse in doing it.

### 13.1 What was missing

Comparing the 60 molecules already in the registry against the FDA-approved US
systemic products in those five classes left 19 molecules, 21 records.

**The list compared against was not retrieved.** No FDA register is reachable
from here (13.2), so the set of approved ingredients was assembled from
knowledge of the products rather than read off a register, and it carries the
same caveat as the rows themselves: it is a claim, not a transcription. That
matters most at the edges — an ingredient approved only in a fixed
combination, an old product withdrawn before electronic listing — so the
enumeration is written down somewhere executable rather than only in prose.
`test-antihypertensive-registry.R` holds the five class lists and fails if a
member is missing, which makes them a *checkable* assertion for anyone who can
reach a register: correct the list, and the test says what the registry lacks.

The 21:

| Class | Added |
|---|---|
| ARB | eprosartan |
| ACE inhibitor | moexipril |
| Dihydropyridine CCB | isradipine, levamlodipine, nisoldipine |
| Beta blocker | acebutolol, betaxolol, carteolol, penbutolol, pindolol, timolol |
| Thiazide / thiazide-type | chlorothiazide (oral **and** intravenous), methyclothiazide, hydroflumethiazide, trichlormethiazide, polythiazide, bendroflumethiazide, quinethazone |
| Loop | ethacrynic acid (oral) and ethacrynate sodium (intravenous) |

The registry is now 127 records over 81 `drug` values, of which 90 are catalog
rows and 10 are executable. The 81 exceeds 60 + 19 because a salt is its own
`drug` string, as `metoprolol tartrate` and `metoprolol succinate` already
were: 19 counts molecules, 81 counts the field. Every one of the five classes is enumerated, which means
something it did not mean before: **a molecule's absence from the registry is
now a claim**, not a gap — either it is not an approved US systemic
antihypertensive, or it is one of the exclusions argued in 13.3.

Three of the new rows are there to prevent a specific error rather than to fill
a hole:

- **levamlodipine** is a separate record from amlodipine on purpose. It is the
  (S)-enantiomer, and the racemate's published parameters are not the
  enantiomer's. Whether the racemate's disposition is carried by the (S) form
  is exactly the question a source would have to answer, so reading
  amlodipine's records across to it is the mistake the separate row exists to
  make visible.
- **acebutolol** has an active metabolite, diacetolol, that accumulates. That
  makes it a two-moiety problem of the kind cefazolin and aspirin are elsewhere
  in this library, so a single-analyte curve would be the wrong *structure*,
  not merely an incomplete one.
- **chlorothiazide** and **ethacrynic acid** each get two rows, one per route,
  as furosemide already does. For chlorothiazide the intravenous salt is the
  only parenteral thiazide there is, and it would be the cleaner identification
  of the disposition if either route were ever fitted, because it asks no
  absorption question. For ethacrynate sodium the point is the **dose basis**:
  the salt is dosed and the acid circulates, so a future record owes a salt
  factor, as calcium gluconate and GHB oxybate do.

### 13.2 The citation is weaker, and it says so

Every one of the 69 earlier catalog rows was written with a source document
open — 66 a US official label and three something else, which is why the
honest contrast is *document opened* against *no document opened* rather than
label against no label: azilsartan medoxomil's row cites a journal paper,
canrenone's a regional registration, urapidil's a regional monograph. Each of
the 69 quotes the descriptors found in whatever it opened. These 21 opened
nothing. DailyMed
(`dailymed.nlm.nih.gov`) and the openFDA API (`api.fda.gov`) are both
unreachable from the environment this registry is built in — `curl` returns no
response at all, not a refusal — and `accessdata.fda.gov` does answer but
serves 404 on the Orange Book paths, so no approval or marketing record was
retrieved from there either.

There were three ways to handle that and only one of them is defensible.

1. Invent a setid so the new rows match the old ones. This is a fabricated
   identifier for a document nobody opened, and it is the single thing this
   registry cannot afford, because its whole value is that a reader can check
   any row against its source.
2. Write the rows with label descriptors recalled rather than read. Same
   failure, less visibly: a half-life nobody verified, indistinguishable in the
   CSV from 125 that were.
3. Record exactly what is known and make the difference machine-readable.

So these rows carry a new `source_type`, **`ingredient_catalog`**, and their
`pk_evidence` is `proposed_unfitted` rather than `official_label_descriptive` —
because the latter would assert descriptors nobody read. Their `source_url` is
a DailyMed **ingredient search**, a real resolvable URL that points at whatever
documents exist instead of naming one that was never opened. Each row asserts
four things: the ingredient exists as a US-approved systemic product, its
class, its route, its formulation. Its `matrix` is deliberately `NA`, because
a matrix is a source-specific property — clevidipine's is whole blood,
chlorthalidone's is plasma although most of the drug in blood is inside the
erythrocytes — and naming one for a row with no source would be the same kind
of unearned claim as a recalled half-life. It asserts nothing numeric, and
nothing about **current marketing**, because approval is not availability and several
of these are long-superseded products — oral carteolol and penbutolol most
doubtfully, and the older thiazides generally. `bendroflumethiazide` is
recorded as an ingredient although the surviving US product is a fixed
combination with nadolol, which is a dosing constraint rather than a
pharmacokinetic one: a combination tablet does not make a disposition
unidentifiable, it makes the dose non-independent.

Closing one of these rows is a small, well-defined job for anyone who can
reach DailyMed: open the label, replace the search URL with that document's
setid, move `source_type` to `official_label` and `pk_evidence` to
`official_label_descriptive`, and record what the label says.
`test-antihypertensive-registry.R` counts the two kinds — 69 and 21 — so the
remaining work cannot quietly disappear, and asserts that no
`ingredient_catalog` row carries a parameter, a setid, or a `stanpumpr_drug`.

That test is also the thing that stops the weaker rows drifting into the
stronger set by accident. It is the same lesson as section 12's: the registry's
protection is never the care taken when a row is written, it is the assertion
that fails when the row changes.

### 13.3 What is deliberately absent

An exclusion is a claim too, so each one is argued here rather than left as an
omission:

| Not a record | Why |
|---|---|
| **sotalol**, **landiolol** | Beta blockers whose US indications are arrhythmia, not hypertension. Esmolol is in the registry because its label and its perioperative use are about pressure as well as rate; these two are not |
| **nimodipine** | A dihydropyridine indicated for subarachnoid haemorrhage only. It lowers pressure; that is an adverse effect in its label, not an indication |
| **levobunolol**, **metipranolol**, and the ophthalmic products of betaxolol, timolol and carteolol | Ophthalmic only. Their systemic absorption is real and is a drug-interaction problem, not an antihypertensive model. The three oral products are in the registry and their rows say which product they are |
| **finerenone** | A nonsteroidal mineralocorticoid-receptor antagonist indicated for chronic kidney disease in type 2 diabetes, with **no** hypertension indication — unlike spironolactone and eplerenone, which both carry one and are both in the registry |
| **acetazolamide**, **methazolamide** | Carbonic-anhydrase inhibitors. Diuretics, not antihypertensives |
| **tolvaptan**, **conivaptan** | Vasopressin antagonists for hyponatraemia |

What section 7 still says stands: this closes the five classes against the
approved ingredients as 13.1 enumerates them, and it is **not** a comparison
against an intended formulary or a dated utilisation list. Enumerating what exists and knowing what is used are
different questions, and the second one is the clinician's.

And the number that matters has not moved. Of the 127 records as at section 13, 10 were executable,
`bp_simulation_enabled` is `FALSE` on every one, and **no drug in stanpumpR
plots a blood pressure.** All twenty-one new rows are a statement about what
the published evidence does not contain.

## 14. The renin-angiotensin drugs, and where each one actually stops (2026-10-20)

Eight drugs were requested: the three best-known angiotensin receptor blockers
(losartan, valsartan, olmesartan) and five ACE inhibitors (benazepril,
captopril, enalapril/enalaprilat, fosinopril, lisinopril). They did not come out
in two groups, implemented and not. They came out in **six distinct states**,
and the distinctions are the useful part of this section, because each one is a
different reason and so calls for a different next action.

| Drug | State | What stops it, or what did not |
|---|---|---|
| **lisinopril** | Implemented | Thomson 1989, full text. The library's first flip-flop model |
| **olmesartan** | Implemented | Yoshihara 2005, full text. Dose basis resolved from the paper's own words |
| **valsartan** | Implemented | Heo 2016, full text and supplement. Zero-order release, no renal term |
| **enalapril** | Blocked on an **engine**, not on evidence | Faisal 2019 is complete and verified. Repeated eigenvalues (section 14.3) |
| **losartan** | **Refuted**, not merely underdetermined | Lo 1995, full text read; it prints one area where two are needed (§15) |
| **captopril** | **Not pursued** — wrong structure | Out of clinical use, by the maintainer's decision; and saturable enzyme binding makes a linear fit the wrong structure, not an approximation |
| **fosinopril** | **Not pursued** — not identifiable | Out of clinical use, by the maintainer's decision; and its sources give three independent numbers where two compartments need four |
| **benazepril** | ✅ Implemented, 2026-10-20 | Kaiser 1989, full text. Derived; plotted as benazeprilat (§15) |

### 14.1 Three implementations, three different lessons

**Olmesartan's dose basis was derived from a sentence, not assumed.** Olmesartan
medoxomil is a prodrug 20% heavier than the olmesartan that is plotted, so a
model fitted to olmesartan concentrations could have been given its doses on
either basis and the wrong guess is a flat 20% error in every curve. The paper
settles it in its Model Evaluation section — the data were *"normalised to a
dose of olmesartan medoxomil 20mg"* — so the apparent parameters already carry
the ratio and a dose is entered as prescribed. **The general rule this is a case
of: for any prodrug, the dose basis is a thing to find in the text, never a
thing to infer from the analyte.** Azilsartan was the harder version of the same
question (section 10) and had to be settled by arithmetic on the paper's own
simulated exposures, because its text never said.

**Olmesartan also parameterises in a way that has to be reproduced rather than
improved.** The source fits apparent clearance together with the four rate
constants, so the central volume is not a parameter at all — it is clearance
over the elimination rate constant — and every covariate sits on clearance. The
consequence is that each patient's eigenvalues are identical and the entire
covariate model is a single multiplier on concentration: nothing about the
shape moves with age, weight, sex, renal function or disease. It would have been
easy to "fix" this by putting the covariates on clearance alone and holding the
volume at its typical value. That would be a different model from the published
one. The drug header, the help page and the test all state the property
explicitly, and the test pins that the eigenvalues are patient-independent.

**Valsartan's absorption shape is a finding, and so is its missing renal term.**
Heo 2016 fitted first-order absorption too and it lost by 162 objective-function
units on the same number of parameters, so the zero-order release mode built for
carvedilol (section 8) is used — **its second confirmation, and the second time
a source has rejected first-order absorption outright rather than merely
preferring something else.** The consequence is pinned rather than smoothed
over: a constant-rate input into a linear system peaks when the input stops, so
the predicted peak is at exactly 4.39 h against the label's 2 to 4 h, and the
authors themselves report that the model underpredicts the median peak. The
curve describes exposure well and the peak bluntly, and the help page says so.

The absent renal term matters more. Age, serum creatinine, smoking and drinking
were all screened and rejected; body weight was the only survivor. So the
signature deliberately does not name `creatinine`, and the test asserts that a
creatinine of 4 mg/dL changes nothing. **For a drug whose patients' kidneys are
often the reason they are taking it, a silence like that should be explicit
rather than merely true**: the population was 48 healthy young adults with a
mean creatinine of 0.81 mg/dL, so the model is not saying renal function is
irrelevant, it is saying this study cannot speak to it.

### 14.2 Losartan: the one study that identifies the system, and why its abstract is not enough

A PubMed screen returned 52 hits for losartan population pharmacokinetics and
122 for losartan with EXP3174, and **not one is a population model with a
printed parameter table.** For the most prescribed drug in its class that is a
surprising result, and it is worth recording as a result rather than as a failed
search: losartan's literature is overwhelmingly CYP2C9 phenotyping, drug
interactions and ethnic comparisons, which measure metabolic ratios rather than
fit disposition.

What the screen did find is better than a population model. Lo 1995 (*Clin
Pharmacol Ther* 58:641-9) gave 18 healthy men **intravenous losartan,
intravenous EXP3174, and oral losartan**. Separate intravenous arms for both
moieties make both their clearances and volumes absolute rather than apparent,
and the 14% conversion fraction it reports closes the parent-to-metabolite link
that no oral-only study can close at all. That design is exactly what a
two-moiety model needs, and it is why this is the one source to obtain rather
than one candidate among many.

Its abstract is still not enough, and the reason is the familiar one. Losartan's
reported clearance over its reported volume gives a half-life of 0.64 h against
the reported 2.1 h; EXP3174's gives 2.46 h against the reported 6.3 h. Neither
moiety is one-compartment on these numbers, so the quoted volumes belong to a
multi-compartment fit, and the abstract supplies three numbers per moiety where
two compartments need four — **the same arithmetic that rules out esmolol's
abstract (section 4) and fosinopril's (section 14.4).** The full text's
compartmental tables would settle it, and would also answer a question the
abstract raises: whether the oral conversion fraction differs from the
intravenous one, which first-pass metabolism to EXP3174 would imply.

### 14.3 Enalapril: the first case where the obstacle is an engine, and why discretising will not do

Faisal 2019 (*Front Pharmacol* 10:973) is complete, verified and not
implemented, which no other record in this registry has been. It fitted serum
**and urine** for both moieties simultaneously in 24 healthy adults, which makes
the system identifiable: bioavailability is a separately estimated parameter
(0.606, against the literature's 60%), so the volumes and clearances are
absolute, not apparent. The structure is enalapril in one compartment behind
**eight Erlang transit compartments**, its elimination splitting into a renal
and a metabolic path, feeding enalaprilat in two compartments behind **two more
transit compartments** that delay the metabolite's formation.

The obstacle is specific. **A chain of N identical transit compartments has one
eigenvalue repeated N times**, and every closed-form coefficient in this library
divides by a difference of two rate constants — which is why
`separateEigenvalues()` exists, nudging ties apart by a relative 1e-8. For N = 8
the partial-fraction coefficients would scale as 1/nudge⁷. That is not merely
inelegant: it is precisely the catastrophic cancellation of nearly equal large
numbers that section 3.3 **rejected esmolol's decomposition for**, so the
library's own standard forbids the nudge here.

Section 6's lesson still holds and points the right way. The system is linear
throughout, so it is superposable, so by that test it does **not** need a
numerical solver — what it needs is exact closed-form handling of a repeated
eigenvalue, and a **matrix-exponential advance** over the 13-state system is
that. `tests/testthat/test-compartments.R` already carries a dependency-free
scaling-and-squaring `expmTaylor()`, which shows one costs no new package.

It is worth being clear why the cheaper route is closed, because the cheap route
has won every previous time. The parent's input shape could have been
discretised into lagged fractional doses exactly as hydrochlorothiazide's
Weibull was (section 12). Convolution commutes, so that would be exact in the
limit. But the metabolite's own 2-transit delay means **the parent and the
metabolite need different input kernels from one dose table**, and the
architecture gives them the same one. So: *an input shape the engine lacks has
always been a rewrite of the dose rows — until the shape differs between a
parent and its metabolite.* That is the new boundary, and it is a sharper
version of the old rule rather than a contradiction of it.

**The paper's transit parameterisation is internally inconsistent, and it turns
out not to matter.** It quotes MTT = (N+1)/KTR, which assumes every step shares
one rate, while also defining KA as the separate rate out of the last transit.
Four readings were integrated by Runge-Kutta and **all four reproduce the
paper's own reported observations**: parent peak at 0.82 to 0.95 h against its
"almost 1 h", metabolite peak at 3.26 to 3.68 h against its "around 3 to 4 h",
with peak concentrations varying under 5%. That is the exact opposite of the
Yang 2022 valsartan case, where no reading worked at all, and the contrast is
the point: **an ambiguous parameterisation is a blocker only when the readings
disagree with the paper's own figures.** Testing them costs an afternoon and
turns a blocker into a documented rounding decision.

### 14.4 Two drugs not pursued, and why that is still a record

Captopril and fosinopril are out of clinical use, which is the maintainer's call
and the sufficient reason. But an unpursued drug that looks merely unexamined
invites someone to examine it again, so both rows now carry the evidential
reason beside the clinical one.

Captopril was the worst candidate in its class regardless: 58 unscreened
candidate sources, and saturable binding to converting enzyme strong enough that
a linear fit is the wrong *structure* rather than an approximation of the right
one. Fosinopril failed the section 4 identifiability check on the very papers
obtained for it — Gehr 1993 reports clearance, steady-state volume, mean
residence time and a terminal half-life, but **mean residence time is
steady-state volume over clearance**, so those are three independent numbers and
not four. Two compartments need four. It is the third drug in this document to
fail on that one identity.

### 14.5 Benazepril: the parent closes and the metabolite does not

A screen of the 25 benazepril/benazeprilat pharmacokinetic hits shows that the
Ciba-Geigy series — Kaiser, Waldmeier, Sioufi, 1989 to 1994 — *is* the primary
literature for this drug; everything else is a special-population comparison, an
interaction study or a review. **Kaiser 1989** (*Biopharm Drug Dispos*
10:365-76) is the one to obtain: 62 healthy men on a single 10 mg dose, plus
steady state in 15 more.

The abstracts are enough to do the identifiability check, and the answer splits
the two moieties:

**The parent closes.** One compartment for benazepril is determined by Sun
1994's printed AUC(0-4 h) of 140 ng·h/mL and the reported 0.6 h half-life —
CL/F about 71 L/h, V/F about 62 L — and `ka` near 2.7 /h follows from the
0.55 h time to peak against an elimination rate constant of 1.155 /h.

**The metabolite does not.** AUC = f<sub>m</sub> × dose ÷ CL is one equation in
two unknowns, and the 17% urinary recovery does **not** pin the formation
fraction: Waldmeier 1991's ¹⁴C mass balance shows a third of the radioactivity
leaving as glucuronides of both moieties, and the class review reports biliary
elimination for benazeprilat specifically. So the urinary fraction is a lower
bound on formation, not a measurement of it.

**And there is a structural caution that matters more than the arithmetic.**
Benazeprilat's elimination is biphasic with a 22.3 h terminal phase that carries
only a *minor amount* of the total — which is the signature of saturable binding
to converting enzyme, the same phenomenon the enalaprilat record already flags,
and **exactly the reason captopril was dropped** (§14.4). A two-compartment
linear fit to that tail would be the wrong *structure* rather than an
approximation of the right one. Kaiser 1989's fitted parameters over 62 subjects
would say whether the published fit avoids that or inherits it, and that is the
question to ask of the full text — not merely "what are the numbers".

### 14.6 Where the numbers stand

Of the 135 records as at section 14, 13 were executable, `bp_simulation_enabled` is `FALSE` on every
one, and **no drug in stanpumpR plots a blood pressure.** Valsartan's source
fitted one — an effect compartment with a maximum inhibition of 0.164, plus an
interaction term with amlodipine — and it is now a **gated** record of its own,
`valsartan__…__resting_sbp_and_dbp__heo2016`, which is *not* one of the four
evaluable relations and should not be read as one: only the Imax was
transcribed, so the relation cannot be evaluated against a concentration at
all. Two things about it are worth knowing before anyone extracts the rest. The
authors **fixed** that 0.164 as amlodipine's Imax as well, "assuming that the
amlodipine and valsartan monotherapy show similar efficacy" — an assumption,
not a measurement. And the monotherapy arm of the fit is a *naively pooled*
analysis of mean blood pressures read from two other papers, because this study
gave both drugs together and has no valsartan-alone arm, so the valsartan
response is identified from other people's published means rather than from
these subjects.

## 15. One missing print: why benazepril closes and losartan does not (2026-10-20)

Steven supplied the two full texts §14 asked for — Lo 1995 for losartan and
Kaiser 1989 for benazepril. Both turn out to be **entirely non-compartmental**:
trapezoidal areas, terminal rate constants by log-linear regression, no
compartmental model fitted anywhere in either paper. By §4's arithmetic both
should therefore have failed, because two compartments need four independent
numbers and a non-compartmental paper normally prints three — a clearance, a
volume and a terminal half-life.

One of them closed and the other was refuted, and the difference comes down to
**how many exponents each paper reports.**

> **A non-compartmental paper identifies a two-compartment model when it prints
> BOTH exponents** — and then any fourth number closes it. A clearance with a
> steady-state volume is one such number, which is how clevidipine was
> identified (§11). A total area with a *partial* area is another, and it is
> benazeprilat's: the difference of the two estimates the area under the
> terminal phase alone, so β times it gives the concentration at the split time
> and recovers the terminal intercept.

An earlier draft of this section stated that as an *if and only if* on the
partial area. That was wrong, and wrong against a record in this same document:
clevidipine has no partial area and is identified anyway. **The necessary
condition is both exponents, not the partial area.** What losartan lacks is not
a particular fourth number but any fourth number at all — Lo reports only the
terminal exponent, so there is nothing for a clearance or an area to combine
with.

Kaiser's Table 2 gives benazeprilat **two** half-lives, α 2.7 h and β 22.3 h,
and then two areas to close on. Lo's Table I gives each moiety **one** half-life
and one area. That is the reason one of these drugs is now in the library and
the other is not, and it is cheap to check before requesting a paper: *ask
whether the tables carry both exponents, and then what fourth number sits
beside them.*

### 15.1 Benazeprilat, derived and checked

From Kaiser's four numbers — t½α 2.7 h, t½β 22.3 h, AUC(0–24 h) 2649 and
AUC(0–∞) 3185 (pmol/g)·h — the tail area is 536, so C(24 h) = β × 536 =
16.66 pmol/g, which gives the β intercept and then the whole vector. All three
rate constants come out positive (k10 0.17666, k12 0.06598, k21 0.04517 /h).

**Two checks the derivation was not given.** Its implied apparent clearance
equals dose over area exactly. And its implied renal fraction is 20.0%, which
is the paper's own separate sentence that "about 20 per cent of the dose were
excreted as benazeprilat". A third: the effective input rate needed to put the
peak at the measured 1.5 h is 1.433 /h, within 24% of benazepril's own
elimination rate constant of 1.155 /h — the quantity that should rate-limit
formation, and not an input to the fit.

**Conditioning**, by §3.3's test. A 10% perturbation of the tail area moves V1
by 4.5%, V2 by 9% and Q by 13.5% — almost exactly clevidipine's sensitivity
(4%, 10%, 13% at 5%). The *structure* is as stable; the *input* is noisier,
because the tail is a difference of two measured areas whose standard errors
give it about ±25%, so V1 carries ±13%, V2 ±22% and Q ±37%. The apparent
clearance is untouched by the split, so **the area under the plotted curve is
firm and its distribution shape is the soft part** — which is the useful way to
state the limit of a derived vector.

**What it does not determine, and why that is fine.** The formation fraction
cannot be recovered, and the 16.8% urinary recovery does *not* supply it: that
figure is arithmetically CL<sub>r</sub> × AUC/dose and so already implied by the
clearance and the area (it reproduces to 16.6%). This was the trap flagged in
§14.5 from the abstract, and the full text confirms it. It does not matter for
the curve: an apparent parameterisation gives the correct concentration because
the same f<sub>m</sub> divides volume and dose, exactly as F does for an ordinary
oral drug, and all three rate constants are f<sub>m</sub>-free.

**The one acknowledged shortfall.** The model underpredicts Kaiser's reported
peak by 20% while reproducing AUC(0–∞) exactly and AUC(0–24 h) to 0.7%. The
probable cause is the statistic rather than the model: that Cmax is a *mean of
individual peaks* over 60 subjects whose times to peak ranged from 1 to 6
hours, and averaging peaks that occur at different times always exceeds the peak
of the averaged curve. It cannot be verified from the printed data, so it is
stated and pinned in the test at the value it actually has rather than hidden
behind a loose tolerance.

**External check.** Sun 1994 measured the same 10 mg dose at Cmax 257 ng/mL and
AUC(0–24 h) 1410 ng·h/mL against Kaiser's 195 and 1050 — ratios of 1.32 and
1.34, *the same factor on both*. Two independent studies therefore agree on the
shape of this curve and disagree on its height by a third. Recorded, not
reconciled: rescaling a primary fit by another study's number is fitting to a
result rather than to data.

### 15.2 Losartan: refuted rather than underdetermined

Lo's design is everything a two-moiety model needs — intravenous losartan,
intravenous EXP3174 *and* oral losartan in the same 18 men, so both moieties
have absolute parameters and the 14.3% systemic conversion closes the
parent-to-metabolite link no oral-only study can. It still cannot be built, and
the finding is stronger than "not identified".

**One compartment is refuted.** CL over V<sub>ss</sub> gives a half-life 3.5×,
3.3× and 2.5× shorter than the reported terminal values for the three records;
equivalently V<sub>z</sub>/V<sub>ss</sub> is 3.49, 3.32 and 2.48. There is a real
deep compartment.

**Two compartments are refuted too, against the paper's own oral data.** With
CL, V<sub>ss</sub> and β fixed at Lo's values, the admissible models form a
one-parameter family in V1. Sweeping it from 6 to 27.5 L — each member with the
Q that reproduces the measured β and the k<sub>a</sub> that reproduces the
measured oral t<sub>max</sub> of 1.0 h — predicts an oral Cmax of **29 to 147
ng/mL against the measured 296**. The best member is 2× low and most are 4 to
10× low. The mechanism is clear: β of 0.385 /h with a V<sub>ss</sub> of only
28.4 L forces α between 1.4 and 8.4 /h, and a distribution phase that fast
cannot hold a one-hour peak up.

Two further warnings against patching it. The paper's **two studies disagree
1.6-fold on V<sub>ss</sub>** (28.4 against 46.6 L) and **1.8-fold on the terminal
half-life** (1.8 against 3.2 h) — precisely the two numbers a two-compartment
fit leans on hardest. And per-subject oral bioavailability ranges from 12.1% to
66.6%.

What would unblock losartan is now narrow and specific, which is the useful
outcome: **a published compartmental fit for losartan or EXP3174 giving either a
distribution half-life or the exponential intercepts.** Everything else — both
clearances, both volumes, the conversion fraction, bioavailability and its
presystemic/systemic split — is pinned absolutely by this paper.

### 15.3 A word the parameter audit needed

`estimate_status` gained **`observed`**, for a measured summary statistic that
is not an estimate of any model parameter: a Cmax, a median t<sub>max</sub>, a
urinary recovery. Kaiser's table is the first case where such numbers are what
*check* a derived vector rather than what feeds it, and calling them `fitted`
would have hidden exactly the fact that makes them worth having — that the
derivation was never given them. The same reason `execution_status` needed
`complete_not_implemented` (§11).

### 15.4 Where the numbers stand

Of 135 records, **14** are executable and 160 parameters are audited: **91
checked against a primary full text**, 25 against an abstract, 8 derived here,
and 36 still carrying the handoff specification's numbers unverified.
`bp_simulation_enabled` is `FALSE` on every record, and **no drug in stanpumpR
plots a blood pressure.**

An earlier draft of this line said "124 verified against a primary full text".
That was wrong: 124 is the registry's count of *not-unverified*, which pools
three different provenances. It is the second time in this document's history
that a printed total was quoted without checking its composition — §13.2's "69
rows with the label open" was the first, and only 66 were. The builder now
prints the breakdown instead of one total, because the fix for that mistake is
not more care in reading, it is output that cannot be misread.
