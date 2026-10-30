### The model

Apomorphine hydrochloride injection (Apokyn) is a dopamine agonist injected under the skin to rescue an OFF episode, typically 2 to 6 mg. Its model is the population PK/PD analysis in the FDA's pharmacometrics review of Apokyn (NDA 21-264, 2004). Six patients with Parkinson's disease each received a single injection on three days. Nasser and colleagues (*J Pharmacokinet Pharmacodyn* 2024) used the same numbers to compare the injection with the sublingual film.

- **Plasma.** One compartment, with absorption at 14.9 per hour (half-time 2.8 minutes), apparent clearance 191 L/h and volume 153 L. A dose peaks at about 11 minutes, at 5.2 ng/mL per mg, and the half-life is about 33 minutes.
- **Effect site.** The motor score (UPDRS Part III, baseline 24.3) falls as an inhibitory sigmoid Emax function of the effect-site concentration. The equilibration half-time is 7.8 minutes, the half-maximal concentration 10.7 ng/mL, and the Hill coefficient 3, so the response switches on and off steeply. The maximal effect was fixed at the whole score.

After 4 mg, the effect site peaks at about 24 minutes, at 16 ng/mL, enough to take the typical patient's motor score below half its baseline. Below about 2 mg the effect site never reaches the EC50.

The review reports that the effect begins within 10 minutes, peaks around 40 minutes and lasts about 90 minutes. Nasser's simulations, which include variability between patients, give 7 minutes to a clinically relevant response and 27 minutes to the maximal response after 4 mg. The sublingual film takes about four times as long to respond and twice as long to reach its maximum.

### Dosing and units

Enter the dose as **mg SC**, the label's mg of apomorphine hydrochloride. The review advises at least 90 minutes between doses, and the label starts at 1 mg in mild or moderate renal impairment.

### Threshold and band

- **Threshold.** The time until threshold is measured to the EC50, 10.7 ng/mL, which the review calls the therapeutic concentration "about 10 ng/mL".
- **Band.** 3.9 to 15.2 ng/mL, the minimum effective and minimum toxic (dyskinesia) concentrations measured during infusion by van Laar (1998), as the review quotes them. Van Laar's abstract gives a motor response at 4.7 ng/mL and dyskinesia at 8.5.

### What is not in the model

- **Population.** Six patients, so no covariate could be tested. The label states that clearance does not depend on age, sex or weight.
- **Clearance.** The non-compartmental clearance in the same study was 225 L/h, and healthy volunteers' was 280 to 370 L/h.
- **Renal and hepatic impairment.** Moderate renal impairment raised the peak by 50% and hepatic impairment by 25%; neither is modelled.
- **Metabolism and genotype.** No COMT metabolites were found in humans, so COMT inhibitors are not expected to interact. CYP metabolism is minor, and there are no genotype data.
- **Other forms.** The continuous subcutaneous infusion (Onapgo, APO-go) is not modelled. Nor is the sublingual film: its model (Agbo 2021) needs two transit compartments; see [Parkinson disease drugs](help:models/parkinsons).
