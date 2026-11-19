## What to look for

A 40-year-old woman, 160 cm and 150 kg (BMI 59), is given two target-controlled infusions: an effect-site target of 3 mcg/mL propofol and one of 4 ng/mL remifentanil, both set at the start and both set to 0 at sixty minutes. With **Minto / Schnider** ticked, each target is run by two pumps. One runs the library's model: Eleveld's for propofol and, because her BMI is over 30, Kim's for remifentanil. The other runs the Schnider propofol or Minto remifentanil model, the models most TCI pumps run.

Schnider's and Minto's models take their patient's size from the **James lean body mass**. In a woman of normal weight the two propofol pumps agree within 15 per cent. In this patient they do not:

- **Propofol: the Schnider pump gives nearly half again as much.** Over the hour it gives 1,327 mg against the Eleveld pump's 917. Its maintenance rate at thirty minutes is 0.125 mg/kg/min against 0.089. Its loading dose is 219 mg against 98, and the plasma line (dashed) spikes to 44 mcg/mL, against 12, before the effect site catches up.
- **Remifentanil: the Minto pump gives less.** It gives 679 mcg over the hour against the Kim pump's 927. Its loading dose is 65 mcg against 89.
- **Every pump still hits its own target.** All four effect-site lines rise to their target and hold there. The controller cannot tell that its model is wrong; it can only do what the model says it takes.

## Why: the James equation is a parabola

James's lean body mass (1976) is

```
men:   LBM = 1.10 weight - 128 (weight/height)²
women: LBM = 1.07 weight - 148 (weight/height)²
```

At a given height this is a quadratic in weight, a parabola that opens downward. Written as LBM = weight × (1.07 − 0.0148 × BMI) for a woman, or (1.10 − 0.0128 × BMI) for a man, it is easy to see what happens:

- lean body mass rises with weight, but more and more slowly;
- it **peaks at a BMI of 36.1 in women and 43.0 in men**, whatever the height;
- above that BMI it **falls** as weight rises, so a heavier patient is credited with less lean tissue;
- it reaches **zero at twice that BMI**, 72.3 in women and 85.9 in men, and is negative beyond.

<!-- generated: james-lbm-figure -->

For a woman of 160 cm, against the Al-Sallami fat-free mass that the library's models use:

| Weight (kg) | BMI | James LBM (kg) | Held at the peak (kg) | Al-Sallami FFM (kg) |
|---|---|---|---|---|
| 60 | 23 | 43.4 | 43.4 | 38.9 |
| 80 | 31 | 48.6 | 48.6 | 45.9 |
| 92.5 | 36 | 49.5 (peak) | 49.5 | 49.4 |
| 120 | 47 | 45.2 | 49.5 | 55.8 |
| 150 | 59 | 30.4 | 49.5 | 61.1 |
| 185 | 72 | 0.1 | 49.5 | 65.9 |
| 200 | 78 | −17.2 | 49.5 | 67.5 |

The fall is an artifact of the equation, not of physiology, and nobody saw it when the Schnider and Minto models were built. Bouillon and Shafer drew it in 1998, in an editorial on remifentanil in the obese, and showed that for every height there is a weight past which James's lean body mass falls.

**What the fall would do to the models.** Schnider's clearance falls with lean body mass, CL1 = 1.89 + 0.0456 (weight − 77) − 0.0681 (LBM − 59) + 0.0264 (height − 177). Past the peak the weight term and a falling lean body mass push in the same direction, and for this woman the raw equation would give a CL1 of 6.72 L/min at 150 kg, and 12.2 at 200 kg. Minto's V1, V2 and CL1 rise with lean body mass, so they would shrink: V1 would be 3.3 L at 150 kg and negative at 200 kg, where the model would have no meaning at all.

**What stanpumpR does about it: it holds the lean body mass at the peak.** Below the peak the two models use the equation exactly as published. Past it, they use the most the equation gives for the patient's height, 49.5 kg for this woman, whatever her weight: the dashed line in the figure and the fourth column of the table. Lean body mass then never falls as weight rises, and the bizarre downward trend never reaches a parameter. For this woman Schnider's CL1 is 1.73 L/min at 60 kg, 2.79 at 92.5 kg and 5.42 at 150 kg: still steep, because the weight term is the model's own and the central volume stays 4.27 L, but it is the weight term alone. A Schnider pump in effect-site mode carries the time to peak effect of 1.6 minutes, so the steep clearance also drives ke0 down, from 0.40 to 0.13 per minute, and the loading bolus up. Minto's V1 is 4.7 L at 92.5 kg and stays 4.7 L at 150 and 200 kg, so the Minto line is drawn at every weight the Patient Profile allows.

**What the pumps do about it.** The pump manufacturers' field safety notices limited the Schnider and Minto models to a BMI below 35 in women and 42 in men (Schnider and colleagues, 2016; see *Safety* in [The history of TCI](help:history)). These limits sit just below the peak of the parabola, 36 and 43, where the equation turns over. A patient above them cannot be given these models at all. Holding the lean body mass at the peak is what such a pump computes for the heaviest patient it accepts, continued without the fall.

**What Eleveld did instead.** Eleveld's propofol and remifentanil models scale to Al-Sallami's fat-free mass, which rises with weight at every weight, and his propofol model was fitted to pooled data from patients of 0.68 to 160 kg. Those pools are built primarily on the same Stanford data from which Schnider and Minto built their models, shared through the Open TCI initiative (see *The Open TCI initiative* in [The history of TCI](help:history)), with other investigators' studies added. So the Eleveld models are not a rival to Schnider's and Minto's: they are the same science, extended by more data. That is the case for running them in the pumps. (For remifentanil in an adult with a BMI of 30 or more the library uses Kim's model for obesity, which is why the remifentanil comparison in this patient is Minto against Kim.)

## Try next

- **Set the weight to 60 kg** (BMI 23). The two propofol pumps now give 545 and 473 mg over the hour, 15 per cent apart.
- **Set the weight to 90 kg** (BMI 35, the pumps' limit for a woman). The Schnider pump gives 732 mg against 634, already 15 per cent more, and its plasma spike is twice Eleveld's.
- **Set the weight to 200 kg** (BMI 78). James's lean body mass would now be negative; held at the peak, both lines are still drawn. The Schnider pump asks for a loading dose of 487 mg and gives 1,960 mg over the hour against Eleveld's 1,133; the Minto pump gives 679 mcg against Kim's 1,019.
- **Change the targets to *Plasma target*.** The loading doses then depend only on V1 and no longer on ke0, but at 150 kg the Schnider pump still gives 1,193 mg over the hour against 893.
- **Untick *Minto / Schnider*** and only the library's models remain, which is what a pump running Eleveld would show.

## Background

[The Minto and Schnider models](help:models/minto-schnider) gives both models and says where they are not defined. [Covariates and body size](help:models/covariates) gives the James and Al-Sallami equations side by side. [Scaling to fat-free mass](help:models/fat-free-mass) explains how the library scales its other drugs. [TCI: the same targets on Eleveld and on Schnider and Minto](scenario:tci-eleveld-vs-minto-schnider) is this scenario in a 70 kg man, where the models nearly agree.

Bouillon T, Shafer SL. Does size matter? *Anesthesiology* 1998;89:557-560.
