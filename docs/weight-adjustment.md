# Weight adjustment in stanpumpR

stanpumpR scales most of its pharmacokinetic models to the patient's **fat-free
mass** rather than to total body weight. This document explains why, how the
scaling is anchored so that it agrees with per-kilogram dosing for a typical
adult, what it does to the predictions for everyone else, which models it
applies to, and how to turn it off. It is also the specification every new
model added to the library must follow.

---

## The short version

- Drug clearance tracks lean tissue, not fat. A 120 kg patient does not clear
  drug 1.7 times as fast as a 70 kg patient.
- stanpumpR therefore computes the patient's fat-free mass (FFM) from weight,
  height, age and sex using the model of Al-Sallami and colleagues (2015), and
  scales each model's **volumes by FFM / FFM<sub>ref</sub>** and its
  **clearances by (FFM / FFM<sub>ref</sub>)<sup>0.75</sup>**.
- FFM<sub>ref</sub> is the fat-free mass of the reference patient, a **70 kg,
  170 cm man**, which is 54.5 kg. His scaling factors are exactly 1, so he
  receives every model's published parameters unchanged, and a package insert's
  per-kilogram dose for him is unchanged too.
- The switch is the checkbox **Adjust weight to fat-free mass** in the Patient
  Profile panel. It is on by default. Turning it off restores the scaling each
  model used before, exactly.
- Doses typed in per-kilogram units (mg/kg, mcg/kg/min, ...) are **always**
  converted using total body weight, because that is what the clinician
  actually gives. The switch changes only the pharmacokinetics.

---

## Why total body weight misleads

Almost every pharmacokinetic model in the library was reported for a typical
adult, and most package inserts give doses per kilogram. Both implicitly assume
a patient of ordinary body composition: about 20 percent fat in a man, 25 to 30
percent in a woman. When the patient is heavier than that, the extra weight is
mostly fat. Fat is poorly perfused and metabolically quiet, so it contributes
little to drug clearance and, for most drugs, modestly to the volumes into which
the drug distributes.

Scaling a model linearly with total body weight therefore overpredicts
clearance in obese patients and recommends too much drug. Across many drugs,
fat-free mass has been found to be the body-size descriptor that best predicts
clearance (McLeay et al. 2012), and it is the descriptor Holford and colleagues
recommend for scaling pharmacokinetic parameters.

## Fat-free mass

Fat-free mass is total weight minus all fat. It is estimated from weight,
height and sex using the equations of Janmahasatian and colleagues (2005),
which were developed in adults ranging from 41 to 216 kg:

```
male    FFM = 9270 × WT / (6680 + 216 × BMI)
female  FFM = 9270 × WT / (8780 + 244 × BMI)
```

with WT in kg and BMI in kg/m². Al-Sallami and colleagues (2015) extended these
to children by multiplying by a sex-specific maturation function of age:

```
male    maturation = 0.88 + 0.12 / (1 + (age / 13.4)^-12.7)
female  maturation = 1.11 - 0.11 / (1 + (age / 7.1)^-1.1)
```

The maturation term is 1 in adults, so for an adult the Al-Sallami FFM is the
Janmahasatian FFM. The model was built on children aged 3 to 29 years plus the
adult data. Below 3 years it is an extrapolation. O'Hanlon and colleagues (2023)
extended it down to premature neonates, but their model needs postmenstrual
age, which stanpumpR does not collect, so stanpumpR uses the 2015 model
throughout and flags the extrapolation here.

This is the same fat-free mass that the Eleveld propofol and remifentanil
models already use internally. The James lean body mass formula that appears in
older models (Schnider propofol) is **not** used for this scaling: it is a
parabola in weight that turns downward above about 120 kg and gives nonsense
for heavy patients.

## How stanpumpR applies it

### The reference patient

Every scaled model is anchored to a reference patient: **70 kg, 170 cm, 35
years, male**. His fat-free mass by the formula above is **54.5 kg**. This
number is computed from the formula, never typed in, so the reference patient's
factors are exactly 1.

Anchoring matters. If the per-kilogram dose on a label were applied to
fat-free mass directly, the typical 70 kg man would receive 54.5 mg of a drug
labelled 1 mg/kg instead of the 70 mg the label intends: a 22 percent
underdose at the very patient the label was written for. Dividing by the
reference fat-free mass instead of by 70 kg keeps him at 70 mg and scales
everyone else from there.

### The scaling

For a patient with fat-free mass FFM:

| Parameter | Multiplier |
|---|---|
| Every volume (V1, V2, V3) | FFM / 54.5 |
| Every clearance (CL1, CL2, CL3) | (FFM / 54.5)<sup>0.75</sup> |

The 0.75 exponent is the allometric exponent for clearance. A consequence for
models that were published as rate constants with a per-kilogram V1 (ketamine,
etomidate, morphine and others) is that the rate constants are no longer fixed:
k<sub>10</sub> = CL/V now varies as (FFM / 54.5)<sup>-0.25</sup>. That is
intended. The rate constants were only ever fixed because the original study
scaled everything linearly with weight.

### Which models are scaled

| Scaled to fat-free mass | Not scaled (own covariates) |
|---|---|
| alfentanil, dexmedetomidine, etomidate, fentanyl, hydromorphone, ketamine, lidocaine, mannitol, methadone, midazolam, morphine, oliceridine, oxycodone, pethidine, remimazolam, rocuronium, sufentanil, codeine, hydrocodone (formation only), oxymorphone, tramadol, desmetramadol; the antibiotics cefazolin, clindamycin, cefalexin, ceftriaxone, vancomycin, metronidazole, gentamicin; the steroids hydrocortisone, methylprednisolone, dexamethasone, prednisolone, prednisone; and sugammadex, neostigmine, glycopyrrolate | **propofol** (Eleveld) and **remifentanil** (Eleveld, Kim) already contain the Al-Sallami or Janmahasatian fat-free mass as a covariate and are used as published. **oxytocin** was fitted in parturients, a population the reference male does not describe, and the formula has not been validated in pregnancy. **naloxone** (Dowling 2008) has its own lean-body-weight covariate on clearance, which is the Janmahasatian fat-free mass; its other parameters inherit the scaling. |

**Models with their own weight or renal covariates.** Several antibiotic and
reversal-agent models write a body-weight term into some parameters
(vancomycin's volumes, gentamicin's central volume, every sugammadex parameter)
or carry weight into a renal covariate through Cockcroft-Gault or body surface
area. With the switch on, those terms are evaluated at the **pharmacokinetic
weight**, 70 kg × FFM / FFM<sub>ref</sub>, so that the reference man receives the
published values and everyone else is scaled on lean rather than total weight;
parameters the source left without a size term take the library factors above.
With the switch off, total body weight enters every published term as written
and the size-free parameters are fixed. `R/renalFunction.R` holds the renal
estimators, which run at the patient's serum creatinine or, when none is
entered, an assumed normal one.

Each scaled model's source file (`R/drugs_<name>.R`) carries a comment stating
what it did before and what it does now.

### What the switch does

The checkbox **Adjust weight to fat-free mass** in the Patient Profile panel is
on by default. With it off, every model reverts to exactly the scaling it used
before fat-free mass was introduced:

| Model type | Behaviour with the switch off |
|---|---|
| Fixed published parameters (alfentanil, sufentanil, midazolam, oliceridine, oxycodone, mannitol, adult dexmedetomidine, ceftriaxone, methylprednisolone, dexamethasone, prednisolone, prednisone, glycopyrrolate) | no scaling at all |
| V1 per kilogram with fixed rate constants (ketamine, etomidate, morphine, methadone, hydromorphone, pethidine, lidocaine, rocuronium, neostigmine) | volumes and clearances both × weight / 70 |
| Allometric on total weight (fentanyl, remimazolam, infant dexmedetomidine, cefalexin, hydrocortisone, metronidazole on its adjusted body weight, clindamycin with its published 0.497 exponent) | volumes × weight / 70, clearances × (weight / 70)<sup>0.75</sup> (or the published exponent) |
| Own weight or renal covariates (vancomycin, gentamicin, sugammadex, cefazolin, naloxone) | the published equations on total body weight; size-free parameters fixed |

Propofol, remifentanil and oxytocin do not respond to the switch.

The switch exists for three reasons: to show, live, how far per-kilogram dosing
departs from fat-free-mass dosing in an obese or female patient; to regenerate
simulations and figures made before the change; and to reproduce a source
study exactly where it scaled on total weight. Simulation links saved before
the switch existed restore with it off, so their output is unchanged.

### Per-kilogram doses

A dose entered as 1 mg/kg for a 120 kg patient is converted to 120 mg. The
switch does not change that. The simulation then shows what 120 mg does to a
patient whose fat-free mass is 71 kg, which is the point: the concentrations
come out higher than the same per-kilogram dose produces in the reference man.

---

## Worked examples

All patients are 170 cm and 50 years old.

| Patient | FFM | Volume factor | Clearance factor | Total-weight factor |
|---|---|---|---|---|
| 70 kg man | 54.5 kg | 1.000 | 1.000 | 1.000 |
| 70 kg woman | 44.7 kg | 0.820 | 0.862 | 1.000 |
| 120 kg man | 71.1 kg | 1.305 | 1.221 | 1.714 |

The woman has the same weight as the reference man but 18 percent less
fat-free mass, so her volumes are 18 percent smaller and her clearances 14
percent lower. Per-kilogram dosing treats them identically. The 120 kg man has
71 percent more weight but only 31 percent more fat-free mass. Per-kilogram
dosing would scale his loading dose by 1.71; fat-free mass says 1.30.

### Choosing a dose from a per-kilogram label

The simulator applies the scaling to the pharmacokinetics, not to the dose you
type. If you want to use the scaling to choose a dose, think of it as a
**dosing weight**: the weight you would plug into the label's per-kilogram dose
to get the fat-free-mass-consistent amount.

- A **bolus** is sized by volume, so the dosing weight is 70 kg × (FFM / 54.5).
- An **infusion rate** is sized by clearance, so the dosing weight is
  70 kg × (FFM / 54.5)<sup>0.75</sup>.

| Patient | Dosing weight for a bolus | Dosing weight for an infusion |
|---|---|---|
| 70 kg man | 70 kg | 70 kg |
| 70 kg woman | 57 kg | 60 kg |
| 120 kg man | 91 kg | 85 kg |

For a drug labelled 1 mg/kg, the 120 kg man's bolus is 91 mg, not 120 mg.

---

## Limitations

- **Below 3 years** the Al-Sallami maturation function is extrapolated. The
  infant dexmedetomidine model is affected. The extrapolation is smooth and the
  factors are close to the weight / 70 values the model used before, but they
  are not validated.
- **Pregnancy.** The formula has not been validated in parturients, and
  pregnancy weight is mostly fat-free. Oxytocin is exempted for this reason; for
  other drugs in a pregnant patient, consider turning the switch off.
- **Peripheral volumes of lipophilic drugs.** The scaling applies the same
  fat-free-mass ratio to every volume. The deep peripheral volume of a lipophilic
  drug such as fentanyl tracks fat mass and is larger, not smaller, in an obese
  patient. Fat-free-mass scaling therefore underestimates V3 for such drugs in
  obesity. Clearance, which governs the maintenance rate and the eventual
  decline, is the better-supported part of the scaling.
- **Extreme covariates.** The Janmahasatian equations were developed between 41
  and 216 kg and BMI 17 to 70. Outside that range they are extrapolated.

---

## Requirement for new models

Every model added to the library must do one of two things:

1. **Include its own body-size covariate**, as the Eleveld models do. Document
   it in the source file and in `docs/adding-a-drug.md`'s table, and have the
   function accept and ignore `adjustToFFM`.
2. **Inherit this scaling.** Express the published parameters for the 70 kg
   reference, call `pkSizeFactors(weight, height, age, sex, adjustToFFM, ...)`,
   multiply volumes by `$volume` and clearances by `$clearance`, and pass the
   `legacyVolume` / `legacyClearance` arguments that reproduce the published
   scaling when the switch is off.

Either way the function signature is
`<name>(weight, height, age, sex, adjustToFFM = TRUE)` and the unit test pins
both switch positions. See `docs/adding-a-drug.md` for the procedure.

---

## References

- Al-Sallami HS, Goulding A, Grant A, Taylor R, Holford N, Duffull SB.
  Prediction of fat-free mass in children. *Clin Pharmacokinet* 2015;54:1169-78.
  [PMID 25940825](https://pubmed.ncbi.nlm.nih.gov/25940825/)
- Janmahasatian S, Duffull SB, Ash S, Ward LC, Byrne NM, Green B. Quantification
  of lean bodyweight. *Clin Pharmacokinet* 2005;44:1051-65.
  [PMID 16176118](https://pubmed.ncbi.nlm.nih.gov/16176118/)
- O'Hanlon CJ, Holford N, Sumpter A, Al-Sallami HS. Consistent methods for
  fat-free mass, creatinine clearance, and glomerular filtration rate to
  describe renal function from neonates to adults. *CPT Pharmacometrics Syst
  Pharmacol* 2023;12:401-12, with corrigendum 2024;13:181-2.
  [PMID 36794347](https://pubmed.ncbi.nlm.nih.gov/36794347/)
- McLeay SC, Morrish GA, Kirkpatrick CM, Green B. The relationship between drug
  clearance and body size: systematic review and meta-analysis of the literature
  published from 2000 to 2007. *Clin Pharmacokinet* 2012;51:319-30.
  [PMID 22439649](https://pubmed.ncbi.nlm.nih.gov/22439649/)
- Holford NHG, Anderson BJ. Allometric size: the scientific theory and extension
  to normal fat mass. *Eur J Pharm Sci* 2017;109S:S59-64.
  [PMID 28506869](https://pubmed.ncbi.nlm.nih.gov/28506869/)
