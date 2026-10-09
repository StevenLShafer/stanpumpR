Propofol and opioids are synergistic: a modest opioid concentration lets a much lower propofol concentration prevent the response to a stimulus. The **Interaction** panel shows this using the response-surface model of Bouillon and colleagues for laryngoscopy.

## The model

Bouillon et al. (*Anesthesiology* 2004;100:1353-1372) gave 20 healthy volunteers (10 men and 10 women, aged 20 to 43 years, weighing 50 to 120 kg) propofol and remifentanil in combination and modelled the probability of no response to two stimuli, shake and shout and laryngoscopy, as response surfaces over the two effect-site concentrations. stanpumpR uses the laryngoscopy surface, with the Bayesian predicted parameters from their Table 4:

| Parameter | Value |
|---|---|
| C50 remifentanil | 1.01 ng/mL |
| C50 propofol | 6.68 mcg/mL |
| Steepness, remifentanil | 0.72 |
| Steepness, propofol | 6.9 |
| Pre-opioid stimulus intensity (laryngoscopy) | 0.83 |

The opioid first reduces the intensity of the stimulus,

```
intensity = 0.83 × (1 - U^0.72 / (U^0.72 + (1.01 × 0.83)^0.72))
```

and propofol then has to overcome what is left:

```
P(no response) = Prop^6.9 / (Prop^6.9 + (6.68 × intensity)^6.9)
```

where U is the remifentanil-equivalent opioid effect-site concentration in ng/mL and Prop is the propofol effect-site concentration in mcg/mL. The panel plots the probability of **response**, 1 minus this, so that a low line is a well-anesthetised patient.

## Other opioids

The surface was fitted with remifentanil. Other opioids are converted to remifentanil equivalents through their MEAC: each opioid's effect-site concentration is divided by its own MEAC and multiplied by remifentanil's (1 ng/mL), and the results are summed. In terms of the [MEAC panel](help:models/meac), U is its *total opioid* line divided by 100: 150% MEAC of fentanyl is U = 1.5 ng/mL. This assumes that equal multiples of MEAC are equi-effective in reducing stimulus intensity, which is the same additivity assumption as the MEAC panel. The paper studied remifentanil only, so the conversion of other opioids is stanpumpR's extension, not part of the published model.

## The three curves

- **With both drugs**: the probability of response given the propofol and opioid actually present.
- **Propofol alone**: the probability if the opioid were absent, for comparison. The gap between the two curves is the opioid's contribution.
- **Opioid alone**: always 1. The model requires some propofol; an opioid alone does not abolish the response to laryngoscopy in it.

## What to look for

[The TIVA scenario](scenario:tiva-remifentanil-propofol) gives propofol 1.5 mg/kg with an infusion and remifentanil 1 mcg/kg with an infusion. Once the propofol bolus has redistributed, the *propofol alone* curve shows a high probability of response to laryngoscopy; the combined curve stays near zero while the infusions run. Turn the remifentanil infusion off (set its rate to 0 at time 0) and, once the remifentanil bolus wears off, the two curves come close together.

## Limits

This is one model of one stimulus in one population of healthy volunteers. It is not a depth-of-anesthesia measure, says nothing about hemodynamics or ventilation, and does not describe other hypnotics or other stimuli. The parameters are typical values; the paper reports substantial between-subject variability, especially in the steepness for propofol. Bouillon's paper also modelled tolerance of shake and shout (a second probability surface) and two continuous EEG measures, the bispectral index and approximate entropy; none of these is implemented. See [Cautions](help:cautions).

## Reference

Bouillon TW, Bruhn J, Radulescu L, Andresen C, Shafer TJ, Cohane C, Shafer SL. Pharmacodynamic interaction between propofol and remifentanil regarding hypnosis, tolerance of laryngoscopy, bispectral index, and electroencephalographic approximate entropy. *Anesthesiology* 2004;100:1353-1372.
