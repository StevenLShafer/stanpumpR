### The model

Neostigmine's parameters are from Heier and colleagues (*Anesthesiology* 2002;97:90-95), who gave 70 mcg/kg over two minutes to seven anesthetized volunteers (six men and one woman, 23 to 39 years, 58 to 79 kg), each at normal temperature and at mild hypothermia, and sampled venous blood from 1 minute to 4 hours. It is a **three-compartment** population model (their Table 3, normal temperature): central volume 5.59 L, peripheral volumes 15.0 and 110 L, clearance 696 mL/min and intercompartmental clearances 1480 and 556 mL/min. The half-lives are 1.3, 16.5 and 256 minutes.

Until October 2026 the library used one patient's two-compartment fit from Calvey and colleagues (*Br J Clin Pharmacol* 1979;7:149-155), because Heier's parameter table had not been read. That fit had a central volume of 0.76 L, so its first minutes after a bolus were several times higher than these.

### What this means for the curve

There is no sample before 1 minute, and the authors distrust the central volume for that reason, so the first minute after a bolus is extrapolated. The largest peripheral volume is poorly estimated (standard error 67%) because sampling stopped at 4 hours: the slow terminal phase is the model's extrapolation, not a measurement. Hypothermia lowered the central volume by 38%; that is not applied, since there is no temperature in the Patient Profile. Seven volunteers, with no between-subject variability reported, cannot say how much a patient differs from this typical curve.

### Effect site

The time to peak effect, 4.6 minutes, is Heier's measured time to maximum antagonism of a vecuronium block, counted from the start of a two-minute infusion; used as a bolus tPeak it slightly understates ke0. No concentration-effect model is attached: the effect-site curve is a delayed concentration, not a train-of-four prediction, and nothing here accounts for the depth of block being reversed. The recovery threshold is 30 ng/mL, the bottom of the shaded band (30 to 300 ng/mL).

### Covariates

Size only. Heier tested weight and did not keep it, so the published values describe these adults; under the default [fat-free-mass scaling](help:models/fat-free-mass) the volumes follow fat-free mass and the clearances its 0.75 power, and unticking the box uses the published values for every patient.

### Where to be careful

Seven healthy young volunteers under isoflurane and nitrous oxide, so neither age nor illness is represented. The dose is nominal labelled milligrams of the methylsulfate, with no molar correction. Renal failure roughly doubles neostigmine's half-life and is not represented. Give [glycopyrrolate](help:drugs/glycopyrrolate) alongside to see the two curves together; their interaction is not modelled.
