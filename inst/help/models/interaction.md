Propofol and opioids are synergistic: a modest opioid concentration lets a much lower propofol concentration prevent the response to a stimulus. The **Interaction** panel shows this using the response-surface model of Bouillon and colleagues for laryngoscopy.

## The model

Bouillon et al. (*Anesthesiology* 2004;100:1353-1372) gave volunteers propofol and remifentanil in combination and modelled the probability of no response to several stimuli as a response surface over the two effect-site concentrations. stanpumpR uses the laryngoscopy surface, with the Bayesian predicted parameters from their Table 4:

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

The surface was fitted with remifentanil. Other opioids are converted to remifentanil equivalents through their MEAC: each opioid's effect-site concentration is divided by its own MEAC and multiplied by remifentanil's (1 ng/mL), and the results are summed. This assumes that equal multiples of MEAC are equi-effective in reducing stimulus intensity, which is the same additivity assumption as the [MEAC panel](help:models/meac).

## The three curves

- **With both drugs**: the probability of response given the propofol and opioid actually present.
- **Propofol alone**: the probability if the opioid were absent, for comparison. The gap between the two curves is the opioid's contribution.
- **Opioid alone**: always 1. The model requires some propofol; an opioid alone does not abolish the response to laryngoscopy in it.

## What to look for

[The TIVA scenario](scenario:tiva-remifentanil-propofol) gives propofol 1.5 mg/kg with an infusion and remifentanil 1 mcg/kg with an infusion. The *propofol alone* curve shows a high probability of response to laryngoscopy throughout; the combined curve is low. Turn the remifentanil infusion off (set its rate to 0 at time 0) and the two curves coincide.

## Limits

This is one model of one stimulus in one population of healthy volunteers. It is not a depth-of-anesthesia measure, says nothing about hemodynamics or ventilation, and does not describe other hypnotics or other stimuli. Bouillon's paper also fitted surfaces for other end points (sedation, shake and shout, pressure algometry) that are not implemented. See [Cautions](help:cautions).

## Reference

Bouillon TW, Bruhn J, Radulescu L, Andresen C, Shafer TJ, Cohane C, Shafer SL. Pharmacodynamic interaction between propofol and remifentanil regarding hypnosis, tolerance of laryngoscopy, bispectral index, and electroencephalographic approximate entropy. *Anesthesiology* 2004;100:1353-1372.
