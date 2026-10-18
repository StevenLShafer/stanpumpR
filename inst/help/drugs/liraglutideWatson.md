### The model

Watson and colleagues' population analysis of liraglutide (*J Clin Pharmacol* 2010). They pooled four phase 1 studies: healthy volunteers given single doses of 1.25 to 20 µg/kg, with an intravenous arm, and people with type 2 diabetes. They then checked the model against once-daily dosing in 168 people with type 2 diabetes. It is a second model of the product that [liraglutide](help:drugs/liraglutide) (Røge 2015) also models; the two are separate drugs and are not mixed.

One compartment, apparent clearance 0.013 L/h/kg and apparent volume 0.16 L/kg; the elimination half-life is 8.5 hours. The absolute bioavailability, 0.51, is contained in these apparent values.

### Absorption: zero order, then first order

Absorption is "sequential dual absorption". For the first T0 hours the injection site releases liraglutide at a constant rate. After that, what remains is absorbed first order at kA. The type 2 diabetes values are used: T0 8.0 hours, kA 0.154/h. Healthy volunteers had a shorter constant phase and slower absorption (6.0 hours, 0.104/h). The constant phase carries kA·T0 / (1 + kA·T0) of the dose, 55%. The rest starts its first-order absorption at 8 hours, at exactly the rate the constant phase ended on.

A 1.2 mg dose in a 70 kg patient peaks at about 50 ng/mL (13 nmol/L) some 12 hours after the injection, as in the paper's Figure 1.

### Covariates

Clearance and volume per kilogram, at the pharmacokinetic weight with **Adjust weight to fat-free mass** ticked and at total weight with it unticked.

### What is not plotted

Concentration only, in ng/mL (1 nmol/L is 3.751 ng/mL). See [Antidiabetic models and their limits](help:models/antidiabetics).
