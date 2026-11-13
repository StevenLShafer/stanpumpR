### A toxic metabolite

Norpethidine (normeperidine) is formed from [pethidine](help:drugs/pethidine) in the liver by N-demethylation. It is a weaker analgesic than pethidine but a central nervous system stimulant: tremor, myoclonus and seizures during repeated or prolonged pethidine dosing have been attributed to its accumulation (Kaiko and colleagues, *Ann Neurol* 1983;13:180-185). Its half-life is several times pethidine's, so it keeps rising for a day or more after pethidine has levelled off. It is drawn with its **label in red**: its curve adds to toxicity, not to analgesia, and it is not counted in the opioid MEAC.

### How this model was built

No pharmacokinetic model of norpethidine formed from pethidine has been published, and norpethidine has never been given to people, so its own volume and clearance have never been measured. This model was therefore **built from descriptive studies**, at Steven Shafer's request, in three steps.

**What can be known.** When a metabolite is only ever seen after the parent drug is given, its concentrations fix two things: how fast it is eliminated (the slope of its own decline) and the ratio of the amount formed to its volume. The fraction of pethidine that becomes norpethidine and norpethidine's volume cannot be separated. So the model forms norpethidine from **all** of pethidine's clearance, mole for mole (norpethidine 233.31 g/mol, pethidine 247.33 g/mol), and carries an **apparent** volume: the true volume divided by the fraction actually converted. The concentrations formed from pethidine are the same whatever the true split, which is also why norpethidine has **no dosing unit**: it appears only when pethidine is given. The same convention was used by the published model of lidocaine's [MEGX](help:drugs/MEGX).

**Step 1, the elimination rate.** Kuhnert and colleagues (*Am J Obstet Gynecol* 1985;151:406-409) followed norpethidine every 24 hours for three days after delivery in four mothers who had received pethidine in labour. Its half-life was **20.55 hours** (SD 3.61, range 17.3 to 24.8). The model is one compartment with that half-life; no study resolves a distribution phase.

**Step 2, the apparent volume.** The same paper reports norpethidine at delivery in eleven mothers who had received a mean of 79 mg of pethidine intravenously in a mean of 2.58 doses, the first 303 minutes and the last 150 minutes before delivery: **33.6 ng/mL** (SD 19.9). The library's own pethidine model was given that mean regimen (30.6 mg, 17.8 mg and 30.6 mg at 0, 76.5 and 153 minutes) and formed norpethidine from all of its clearance. At 303 minutes the body holds 36.8 mg of norpethidine, so the volume that gives 33.6 ng/mL is

> 36.8 mg / 0.0336 mg/L = **1,095 L** (apparent, at 70 kg)

and the apparent clearance is 1,095 L x ln 2 / 20.55 h = **36.9 L/h**. How the 2.58 doses are spread hardly matters (1,089 to 1,097 L). The pethidine model passes its own check on the same data: it gives 124 ng/mL of pethidine at delivery against Kuhnert's 129. The volume was calibrated with the library's pethidine, not with a model fitted to those mothers, because it is the number that makes *this* pethidine model form the norpethidine they had. Kuhnert's standard deviation (59% of the mean) is the uncertainty it carries.

**Step 3, a check it was not fitted to, and it disagrees.** Austin, Stapleton and Mather (*Br J Anaesth* 1981;53:255-258, read as an abstract) gave pethidine at an average 25 mg/h for two days after abdominal surgery and found blood norpethidine rising steadily at 0.013 mcg/mL per hour, about 0.62 mcg/mL by 48 hours. For 25 mg/h this model gives plasma norpethidine of 0.29 mcg/mL at 24 hours and 0.48 at 48 hours, or about 0.39 mcg/mL in blood (plasma runs about 25% above blood, Verbeeck and colleagues 1981). The model reaches only about **60%** of Austin's figure at 48 hours. Norpethidine is partly cleared by the kidney, and Kuhnert's mothers were young and had the raised filtration of pregnancy; Austin's patients were older and postoperative. Fitting the volume to Austin's figure instead would give about 680 L, which would raise every norpethidine curve by 60%. The model keeps the calibration to the paper that could be read in full and states the difference here: **over days of pethidine, this model may underpredict norpethidine accumulation.**

### Covariates

None from the sources. Norpethidine scales with body size exactly as pethidine does ([fat-free-mass scaling](help:models/fat-free-mass)), so that the pair stays on one scale.

### No band

No equilibration delay for its effect has been published, so the row plots plasma. No seizure threshold has been established: Kaiko related excitation to accumulation but gave no single concentration. So **no band is drawn**.

### Where to be careful

- **Renal impairment.** Norpethidine's renal clearance follows creatinine clearance and is lower in old patients (Odar-Cederlöf and colleagues 1985), and its renal clearance is about four times pethidine's (Pond and Kretschzmar 1981). No published study gives a relation that could be applied, so this model has **no renal term**, and in renal impairment or old age it underpredicts, possibly greatly. That is where norpethidine toxicity is most often reported.
- **Duration.** The model was calibrated over the first five hours, and its one check over two days suggests it underpredicts accumulation.
- **The parent.** Enzyme inducers (phenobarbital, phenytoin) increase norpethidine formation; those interactions are not modelled.
