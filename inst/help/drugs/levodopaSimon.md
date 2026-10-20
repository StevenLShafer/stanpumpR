### The model

This is a second model of oral carbidopa-levodopa, beside [levodopa](help:drugs/levodopa). It comes from Simon and colleagues (*Eur J Clin Pharmacol* 2016), who studied 30 patients in Marseille with advanced disease (Hoehn and Yahr 3 or 4), all of whom had peak-dose dyskinesia. After a 12-hour washout each patient took a single dose of 150% of their usual morning dose (mean 380 mg) and was followed for 5 hours.

- **Plasma.** One compartment, with no lag: absorption 1.86 per hour, apparent clearance 36.6 L/h and volume 42.9 L. Both are scaled allometrically by weight, a scaling the authors fixed rather than estimated.
- **Effect site.** A single effect compartment, ke0 1.37 per hour (equilibration half-life 0.51 h), drives both the motor response and dyskinesia. It was fitted to the same patients on this plasma model.

A 200 mg tablet in a 75 kg patient peaks in plasma at about 50 minutes (2.3 mcg/mL) and at the effect site at about 100 minutes.

### Why there are two levodopa models

The library's main levodopa model combines Othman and Dutta's plasma model with Mao's effect site, two studies in advanced disease. Simon's is a single study in later, dyskinetic disease, and it differs where the literature says such patients differ:

- **Higher concentration needed.** Half the maximal motor improvement needs 1.41 mcg/mL here, against 0.81.
- **Steeper response.** The Hill coefficient is 4.26, against 2.5, so the response is close to all-or-none: patients switch from OFF to ON over a narrow range of concentration.
- **Dyskinesia threshold.** Dyskinesia has a half-maximal concentration of 6.28 mcg/mL, 4.45 times the motor value, which the authors propose as a cut-off for patients with dyskinesia.

Load the [wearing-off scenario](scenario:levodopa-wearing-off) and add levodopaSimon at the same dose to compare the two.

### Threshold and band

- **Threshold.** The time until threshold is measured to the motor EC50, 1.41 mcg/mL. After that point the effect site is below the concentration for half the maximal improvement in the UPDRS Part III score, which here was 72% of a baseline score of 31.4.
- **Band.** The AGNP reference range, 0.9 to 2.0 mcg/mL, the same as for levodopa.

### What is not in the model

- **The peak.** The authors' visual predictive check underpredicts the observed peak, and the observed peaks came later, at about 1 hour.
- **The terminal phase.** Sampling stopped at 5 hours, so the slow terminal phase that Othman's peripheral compartment describes is missing. Repeated doses are drawn with no accumulation beyond the single compartment.
- **Covariates.** None survived: not sex, age or disease duration.
- **The long-duration response.** The days-long response of early disease is outside the model.
