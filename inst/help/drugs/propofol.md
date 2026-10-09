### The model

Propofol uses the **Eleveld general-purpose model** (Eleveld DJ, Colin P, Absalom AR, Struys MMRF. *Br J Anaesth* 2018;120:942-959). It was fitted to data pooled from 30 previously published studies: 1,033 individuals, from premature neonates to the elderly, from 0.68 to 160 kg, healthy volunteers and surgical patients, with and without opioids, sampled from arteries and veins. Its purpose was a single model that could be used across the whole population an anesthesiologist meets, replacing the age-specific and population-specific models that preceded it.

### Covariates

All four covariates are used.

- **Weight** scales V1 by a sigmoid (half-maximal at 33.6 kg), V2 linearly, and the clearances allometrically to the 0.75 power.
- **Age** reduces V2 and the elimination clearance exponentially in adults, matures the elimination clearance in infants as a sigmoid in post-menstrual age (half-maximal at about 42 weeks), and matures the slow intercompartmental clearance separately. The app has no gestational-age input, so post-menstrual age is taken as 40 weeks plus the age entered.
- **Sex** gives women a higher reference clearance (2.10 against 1.79 L/min at 70 kg and 35 years).
- **Height**, with weight, enters through the Al-Sallami fat-free mass, which scales V3.

Two of the published model's switches are fixed in the code: concentrations are predicted as **arterial**, and the ageing terms for V3 and clearance are applied as for a patient **receiving opioids**, which is the usual anesthetic case.

### Effect site

The time to peak effect, 1.6 minutes, is from Schnider and colleagues' 1999 study of propofol pharmacodynamics (*Anesthesiology* 1999;90:1502-1516). Eleveld's own pharmacodynamic model is not used. In that model the arterial ke0 is scaled to body weight, not to age; age acts instead on the delay of the measured effect and on the sensitivity to propofol. Here the drug library's single tPeak gives a ke0 that depends on the patient only through the disposition parameters.

### What is also in the file

The Schnider 1998 pharmacokinetic model (*Anesthesiology* 1998;88:1170-1182), the basis of many commercial propofol TCI pumps, is written out in the drug file but overwritten by Eleveld's before it is returned. It is kept for reference and could be revived as an alternative model.

### Typical concentrations

The shaded band is 2.5 to 4 mcg/mL, a typical range for maintenance of anesthesia with an opioid. Loss of consciousness in most adults occurs between 2 and 3 mcg/mL without opioid; the concentration at which patients wake is about 1 mcg/mL, which is the default recovery threshold.

### Where to be careful

The Eleveld model is the broadest in the library, but its extremes rest on the few studies that covered them. Predictions in neonates, in patients over about 90 kg, and in the very elderly carry more uncertainty than the plot shows. It describes a typical patient. Individuals differ from that patient considerably: the published between-subject variance of clearance is 0.265 on the log scale, a coefficient of variation of 55 per cent (Eleveld 2018, Table 2). That is the spread of real patients around the typical curve, which the plot does not show, and is separate from any uncertainty in the typical curve itself.
