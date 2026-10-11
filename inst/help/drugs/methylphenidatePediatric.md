### What is plotted

The plasma concentration of **total methylphenidate** (both enantiomers, d and l), in ng/mL, in children and adolescents. The dose is racemic immediate-release methylphenidate (Ritalin), entered as the tablet strength (**mg PO**) or per kilogram (**mg/kg PO**). The bid and tid frequencies repeat the dose.

This is a different quantity from the [adult methylphenidate](help:drugs/methylphenidate) entry, which plots d-methylphenidate, the active enantiomer. Do not compare numbers between the two pages as if they measured the same thing.

### The model

Shader and colleagues (*J Clin Pharmacol* 1999;39:775-785) studied 273 children and adolescents aged 5 to 18 years (mean 11.1). All had ADHD and were doing well on stable twice- or three-times-daily immediate-release methylphenidate. Each child gave one blood sample at a known time after their usual doses, and everyone was assumed to be at steady state. The concentrations were fitted with a one-compartment model with first-order absorption, clearance proportional to body weight, and every child's dosing schedule written into the equations (printed in the paper's appendix).

- **Absorption: 1.19 per hour** (absorption half-life 35 minutes), with no lag. It could be estimated only from the twice-daily group, and was then held fixed.
- **Elimination: 0.154 per hour**, a half-life of 4.5 hours (95% confidence interval 3.1 to 8.1).
- **Apparent clearance: 90.7 mL/min/kg** (74.6 to 107).
- **Apparent volume: 35.3 L/kg.** This is not a separate estimate: it follows from the clearance divided by the elimination rate.

The dose goes in as the racemic tablet mass and is measured as total methylphenidate, so nothing is converted. A single 10 mg dose in a 40 kg child peaks at about 2 hours.

### Covariates

Weight is the model's own covariate: clearance and volume are both proportional to it. With the [fat-free-mass switch](help:models/fat-free-mass) off, the model sees total body weight, exactly as published; with it on, it sees the pharmacokinetic weight. Boys and girls had similar clearances (91.6 and 86.7 mL/min/kg), and Shader kept one model for both. Age matters only through weight.

### How well it fits

The fit explained 43 per cent of the variance in measured concentrations. As a check, on the three-times-daily group's mean dose (0.36 mg/kg per dose), the model's average concentration over a day at steady state is 8.3 ng/mL. That fits the 9.6 ng/mL averaged by the 16 children who were sampled twice, although a single timed sample is not exactly a daily average. The method was naive pooled, so there is no estimate of variability between children, and none is shown.

### Where to be careful

- **The half-life is probably too long.** Single-dose studies of immediate-release methylphenidate find 2 to 3.5 hours. Shader points out that with one sample per child the elimination phase was thinly sampled, and its confidence interval is wide. The model is therefore likely to overstate late-day concentrations and accumulation.
- **Immediate release only.** The children took IR tablets. Concerta and the other extended-release products are not represented here; Concerta's input was fitted on the adult entry and is not transferred to this one.
- **Adults** are an extrapolation of this model; the adult entry is the better choice for them.

### Effect site and typical range

None. No calibrated concentration-effect model exists for a classroom measure (SKAMP, PERMP) or for weekly symptom scores (ADHD-RS-IV), and no concentration is an established therapeutic threshold, so no band is drawn. Nothing on this plot is a dose recommendation.
