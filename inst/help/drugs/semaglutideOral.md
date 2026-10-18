### The model

Overgaard and colleagues' population analysis of oral semaglutide (*Clin Pharmacokinet* 2021). It used six clinical pharmacology trials, 376 subjects, almost all healthy, given 10 mg tablets co-formulated with 300 mg of SNAC, the absorption enhancer in Rybelsus. Every dose was taken on site, under supervision. The disposition is the two-compartment model fitted to intravenous and subcutaneous data (Overgaard 2019), fixed. The oral data estimated the absorption. The parameter values are in the paper's supplement (ESM Table 1).

- **Disposition**, at 85 kg: clearance 0.0348 L/h × (weight/85)<sup>1.01</sup>, 9% higher in type 2 diabetes; intercompartmental clearance 0.304 L/h × (weight/85)<sup>1.01</sup>; central volume 3.59 L and peripheral volume 4.1 L, each × (weight/85)<sup>0.923</sup>. The half-life is about a week: 158 h in healthy subjects, 145 h in type 2 diabetes. Steady state takes four to five weeks of daily doses.
- **Absorption:** first order, 2.09/h. The bioavailability is 0.795% at 85 kg × (weight/85)<sup>0.741</sup>. Under 1% of each tablet reaches the circulation.

The curve is for the label's dosing conditions: taken fasting, with up to 120 mL of water, at least 30 minutes before food. The paper models both conditions:

- **Fasting time after the dose:** bioavailability rises with it, to about 1.4% at 120 minutes. It is 40% lower when food comes after 15 minutes.
- **Water:** taken with 240 mL, bioavailability is 36% lower. It is the same from 50 to 120 mL.

The Patient Profile has no field for either condition, so neither is applied. Type 2 diabetes is assumed, since the product is indicated for it. A separate re-estimation in 55 patients found bioavailability 16% lower in diabetes, but not significantly, and the paper concludes it does not differ; that factor is not applied.

### Supervised trials, not the clinic

This model describes doses taken exactly as instructed. In the PIONEER phase 3 trials, where patients dosed themselves, the geometric mean steady-state concentration at 14 mg daily was 14.6 nmol/L (Overgaard, *Cell Rep Med* 2021). This model gives 29.4 nmol/L for an 88 kg patient: twice as much. The PIONEER re-estimate of bioavailability is in that paper's supplement and is not used here. Read the curve as what the tablet delivers under ideal conditions, not as a typical outpatient's exposure.

### Covariates

Body weight only. Hepatic and renal impairment, age 65 and over, and sex were tested and removed. Weight acts on every parameter. A heavier patient absorbs a larger fraction of the dose, but clearance rises faster, so exposure still falls with weight. The weight term on bioavailability is held to the oral trials' range, 51.1 to 122 kg.

### Variability

Bioavailability varies 53.5% between people and 137% from one dose to the next, the largest variability of any drug in the library. With a half-life of a week, daily dosing averages much of it out: within one person, steady-state exposure varies about 33%. The curve is the typical patient and does not show this spread.

### Product

Rybelsus tablets, 3, 7 and 14 mg (`mg PO`, `mg PO qd`). The 2026 Ozempic tablets (1.5, 4 and 9 mg) are not interchangeable with Rybelsus mg for mg and are not this drug. The weekly injection is [semaglutide](help:drugs/semaglutide), a separate model.

### What is not plotted

Concentration only, in ng/mL (1 nmol/L = 4.11 ng/mL). No glucose, HbA1c or body-weight effect is drawn. See [Antidiabetic models and their limits](help:models/antidiabetics).
