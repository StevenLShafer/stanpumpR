### The model

Propofol uses the **Eleveld general-purpose model** (Eleveld DJ, Colin P, Absalom AR, Struys MMRF. *Br J Anaesth* 2018;120:942-959). It was fitted to data pooled from 30 previously published studies: 1,033 individuals, from premature neonates to the elderly, from 0.68 to 160 kg, healthy volunteers and surgical patients, with and without opioids, sampled from arteries and veins. Its purpose was a single model that could be used across the whole population an anesthesiologist meets, replacing the age-specific and population-specific models that preceded it.

### Covariates

All four covariates are used.

- **Weight** scales V1 by a sigmoid (half-maximal at 33.6 kg), V2 linearly, and the clearances allometrically to the 0.75 power.
- **Age** reduces V2 and the elimination clearance exponentially in adults, matures the elimination clearance in infants as a sigmoid in post-menstrual age (half-maximal at about 42 weeks), and matures the slow intercompartmental clearance separately. This model does not read the **Gestational age at birth** field, so post-menstrual age is taken as 40 weeks plus 52 weeks for each year of age entered.
- **Sex** gives women a higher reference clearance (2.10 against 1.79 L/min at 70 kg and 35 years).
- **Height**, with weight, enters through the Al-Sallami fat-free mass, which scales V3.

Two of the published model's switches are fixed in the code: concentrations are predicted as **arterial**, and the ageing terms for V3 and clearance are applied as for a patient **receiving opioids**, which is the usual anesthetic case.

### Effect site

The time to peak effect, 1.6 minutes, is from Schnider and colleagues' 1999 study of propofol pharmacodynamics (*Anesthesiology* 1999;90:1502-1516). Eleveld's own pharmacodynamic model is not used. In that model the arterial ke0 is scaled to body weight, not to age; age acts instead on the delay of the measured effect and on the sensitivity to propofol. Here the drug library's single tPeak gives a ke0 that depends on the patient only through the disposition parameters.

### The Schnider model, beside it

The Schnider 1998 pharmacokinetic model (*Anesthesiology* 1998;88:1170-1182), the basis of most commercial propofol TCI pumps, is written out in the drug file but overwritten by Eleveld's before it is returned. It is computed on request: tick **Minto / Schnider** under Graph Options and the Schnider model is run beside this one on the same doses, drawn as a second line in the propofol panel (and, under a target-controlled infusion, as a second pump in the rate panel). The time until threshold stays on the Eleveld model. See [The Minto and Schnider models](help:models/minto-schnider) and [the comparison scenario](scenario:eleveld-vs-minto-schnider).

### Renal impairment

The model has no renal term, and none has been added. Rohilla and colleagues (*J Clin Pharmacol* 2026;66:e70268; [doi:10.1002/jcph.70268](https://doi.org/10.1002/jcph.70268)) fitted a three-compartment model with a creatinine-clearance covariate in 35 adults at stages 1 to 4 of chronic kidney disease: clearance 4.46 L/min × (Cockcroft-Gault creatinine clearance / 75)<sup>0.19</sup>, V1 6.21 L, V2 23.86 L, V3 84.57 L, Q2 2.79 and Q3 2.66 L/min, with a BIS ke0 of 0.11 /min. Its V1 agrees with Eleveld's (6.0 L for a 62-kg man), but its clearance is **three times** Eleveld's at the same patient (1.5 L/min), and its slowest half-life is 37 minutes, because blood was venous plasma and sampling ended when the patient opened their eyes, so the washout that sets the terminal phase was never seen. It is therefore not offered as a model: at the same infusion rate it would plot about a third of the Eleveld concentration. What it adds is the size of the renal effect. With the exponent 0.19 (95% confidence interval 0.003 to 0.41), clearance at a creatinine clearance of 20 mL/min is 22% below its value at 75, which fits the kidney's share of propofol clearance (up to about 30%, Takizawa 2005 and Hiraoka 2005). Whether to carry that ratio onto Eleveld's clearance is an open question for the maintainer.

### Typical concentrations

The shaded band is 2.5 to 4 mcg/mL, a typical range for maintenance of anesthesia with an opioid. Loss of consciousness in most adults occurs between 2 and 3 mcg/mL without opioid; the concentration at which patients wake is about 1 mcg/mL, which is the default recovery threshold.

### Where to be careful

The Eleveld model is the broadest in the library, but its extremes rest on the few studies that covered them. Predictions in neonates, in patients over about 90 kg, and in the very elderly carry more uncertainty than the plot shows. It describes a typical patient. Individuals differ from that patient considerably: the published between-subject variance of clearance is 0.265 on the log scale, a coefficient of variation of 55 per cent (Eleveld 2018, Table 2). That is the spread of real patients around the typical curve, which the plot does not show, and is separate from any uncertainty in the typical curve itself.
