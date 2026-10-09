### The model

Alprazolam's parameters are from DeVane and colleagues (*Clin Pharmacol Ther* 1993;53:521-528), who took two random blood samples from each of 94 psychiatric inpatients taking alprazolam (70 men and 24 women, mean age 48) and fitted a one-compartment model with first-order absorption. The apparent clearance is 0.05 L/h/kg, the apparent volume 0.7 L/kg and the absorption rate 1.1/h, so the reference man has a clearance of 3.5 L/h, a volume of 49 L and a half-life of 9.7 hours (label: 11.2 hours, 6.3 to 26.9).

Sampling was sparse and mostly at steady state, which pins down clearance well but the volume and the absorption rate poorly. 1 mg by mouth peaks at 16.9 ng/mL, within the 12 to 22 ng/mL seen in volunteers, but at 2.7 hours rather than the observed 0.7 to 1.8 hours (Greenblatt and Wright, *Clin Pharmacokinet* 1993;24:453-471). The published absorption rate is kept. At steady state, each 1 mg a day adds 11.9 ng/mL, as observed (10 to 12).

### Oral only

There is no intravenous alprazolam product, and the model was fitted to oral data alone: its clearance and volume are divided by the bioavailability, which is about 0.92 (Smith and colleagues, *Psychopharmacology* 1984;84:452-456). Alprazolam is offered only as `mg PO`, with the daily schedules. The extended-release and orally disintegrating products are not represented.

### Covariates

- **Weight**: clearance and volume are proportional to it. With the [fat-free-mass switch](help:models/fat-free-mass) on, they use the pharmacokinetic weight; with it off, total body weight, as published.
- **Age**: clearance is 23 per cent lower above 60.
- **Sex**: clearance is 59 per cent higher in women, so a woman's steady-state concentration is 37 per cent lower than a man's on the same dose. **This term is disputed.** DeVane notes that a sex difference "has been sometimes observed ... but not consistently", and Greenblatt and Wright's review found that "most studies show that alprazolam pharmacokinetics are not significantly influenced by gender".
- DeVane's fourth term, 26 per cent lower clearance with two or more concurrent illnesses, has no input here and is not applied.

### Effect site

The effect-site rate constant is 0.144/min, an equilibration half-life of 4.8 minutes, from the EEG beta response after intravenous alprazolam in 9 healthy men (Venkatakrishnan and colleagues, *J Clin Pharmacol* 2005;45:529-537). It is supplied directly rather than carried as a time to peak effect, because it was measured against an intravenous curve that this oral model cannot reproduce. With oral absorption this slow, the effect site follows plasma within minutes.

### Typical concentrations

The shaded band, 20 to 40 ng/mL, is the steady-state range at which anxiety in panic disorder is best reduced (Greenblatt and Wright); higher concentrations may be needed to suppress the panic attacks themselves. For comparison, half-maximal impairment of card sorting and digit-symbol substitution occurred at 37 to 40 ng/mL in young men and 25 ng/mL in the elderly (Bertz and colleagues, *J Pharmacol Exp Ther* 1997;281:1317-1329). No time-until-threshold level is set.

### Where to be careful

CYP3A4 inhibitors raise alprazolam concentrations substantially; smoking lowers them. In obesity the volume is larger and the half-life about twice as long, with an unchanged clearance (Abernethy and colleagues, *Clin Pharmacokinet* 1984;9:177-183). The hydroxylated metabolites reach less than a tenth of the parent concentration and are not modelled. Alprazolam adds to the sedation and respiratory depression of opioids and other sedatives.
