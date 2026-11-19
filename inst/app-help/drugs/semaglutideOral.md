### The model

Overgaard and colleagues' population analysis of oral semaglutide in the PIONEER phase 3 trials (*Cell Rep Med* 2021): 10,527 concentrations from 2,345 adults with type 2 diabetes who took Rybelsus at home, as patients do. The disposition and the absorption rate are fixed from the clinical pharmacology model (Overgaard 2019 and 2021). The PIONEER data estimated the bioavailability and what changes it. The parameter values are in the paper's supplement (Supplementary Table 1 and the NONMEM code, Methods S1).

- **Disposition**, at 85 kg: clearance 0.039 L/h × (weight/85)<sup>1.01</sup>; intercompartmental clearance 0.304 L/h × (weight/85)<sup>1.01</sup>; central volume 3.59 L and peripheral volume 4.1 L, each × (weight/85)<sup>0.923</sup>. The half-life is about a week, so steady state takes four to five weeks of daily doses.
- **Absorption:** first order, 2.09/h. The bioavailability is 0.471% for a woman of 85 kg, × 0.814 in men, × (weight/85)<sup>0.374</sup>. Under half of one percent of each tablet reaches the circulation.

### Why this model: drugs don't work in patients who don't take them

A second model of the same tablets exists, from the same group: Overgaard and colleagues' analysis of six clinical pharmacology trials (*Clin Pharmacokinet* 2021). Every dose in those trials was taken on site, under supervision, exactly as the label says: fasting, with no more than 120 mL of water, at least 30 minutes before food. Its bioavailability is 0.795% at 85 kg. For the same patient it gives about twice the exposure of this model. At 14 mg daily, an 88 kg man reaches an average of 29.4 nmol/L on the supervised model and 13.6 nmol/L on this one. The PIONEER patients averaged 14.6 nmol/L (geometric mean).

Both models describe adults with type 2 diabetes taking the same product. Nothing about the patient tells a clinician which applies, so stanpumpR does not ask. It uses the outpatient model because what separates the two is how the tablet is taken, and patients at home do not take a drug the way a supervised study does. Doses are missed, and conditions as strict as Rybelsus's are not always kept. In ambulatory care, variable adherence rivals pharmacokinetics as the largest source of variation in drug response (Blaschke and colleagues, *Annu Rev Pharmacol Toxicol* 2012). [Cautions](help:cautions) gives the evidence. Overgaard and colleagues leave the cause open: "Whether these observations reflect real problems with drug absorption and/or compliance remains uncertain."

Two consequences:

- **This curve is still a trial's.** The PIONEER patients were enrolled in clinical trials, where adherence is better than in ordinary practice. A typical clinic patient may absorb less than this curve shows.
- **A patient who takes every dose exactly as labelled** absorbs about twice as much as this curve shows.

### Covariates

- **Sex:** men absorb 19% less (× 0.814).
- **Weight** acts on clearance, intercompartmental clearance, both volumes and bioavailability. A heavier patient absorbs a slightly larger fraction of the dose, but clearance rises faster, so exposure falls with weight.

Age, Black race, Hispanic ethnicity, renal function and dose were tested and not kept. Two covariates the paper kept are not applied, because the Patient Profile has no field for them:

- **Asian race:** bioavailability × 1.10.
- **Upper GI disease** (mostly gastro-oesophageal reflux): bioavailability × 1.13.

The curve is therefore for a patient with neither. The paper's corrections for two individual trials, PIONEER 3 and 5, are not applied either.

### Variability

Bioavailability varies 84% between patients, the largest variability of any drug in the library. With a half-life of a week, daily dosing averages out much of the day-to-day variation. The curve is the typical patient and does not show this spread.

### Product

Rybelsus tablets, 3, 7 and 14 mg (`mg PO`, `mg PO qd`). The 2026 Ozempic tablets (1.5, 4 and 9 mg) are not interchangeable with Rybelsus mg for mg and are not this drug. The weekly injection is [semaglutide](help:drugs/semaglutide), a separate model.

### What is not plotted

Concentration only, in ng/mL (1 nmol/L = 4.11 ng/mL). No glucose, HbA1c or body-weight effect is drawn. See [Antidiabetic models and their limits](help:models/antidiabetics).
