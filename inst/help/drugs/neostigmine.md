### The model

Neostigmine's parameters are from Heier and colleagues (*Anesthesiology* 2002;97:90-95), who gave 70 mcg/kg over two minutes to seven healthy anaesthetised volunteers during a steady vecuronium block and fitted a **three-compartment** population model (NONMEM) to venous samples from 1 minute to 4 hours. At normal body temperature: central volume 5.59 L, peripheral volumes 15.0 and 110 L, clearance 0.696 L/min and intercompartmental clearances 1.48 and 0.556 L/min, giving half-lives of 1.3, 16.5 and 256 minutes. The model is not weight-related; Heier tested weight and did not keep it. Interindividual variability was not reported, the slowest volume is poorly estimated, and the authors distrust the central volume because no sample was taken before one minute. Mild hypothermia (below 34.5 °C) lowered the central volume by 38% and changed nothing else; that is not represented.

Until October 2026 the library used one patient's two-compartment fit from Calvey and colleagues (*Br J Clin Pharmacol* 1979;7:149-155), because only Heier's abstract had been read and no population model was thought to exist. Calvey's tiny central volume put the concentration just after a 3 mg bolus near 4000 ng/mL; Heier's model gives about 540.

### Effect site

Heier measured the time to maximum antagonism of the block (twitch height at the adductor pollicis) at a median of 4.6 minutes after the two-minute infusion began. With Heier's kinetics, an equilibration rate of 0.20/min puts the effect-site peak of that infusion at 4.6 minutes, and the same rate peaks 3.5 minutes after a bolus, which is the time to peak effect used here. No concentration-effect model is attached: the effect-site curve is a delayed concentration, not a train-of-four prediction, and nothing here accounts for the depth of block being reversed.

### Band

100 to 250 ng/mL, with 175 as the typical line, is the library's own orientation band, not a therapeutic range. It was set from simulated bolus doses of 0.03 mg/kg to 5 mg in a 70 kg patient: with Heier's kinetics the effect site peaks about 4 minutes after the dose, at 87 ng/mL after 0.03 mg/kg (2.1 mg) and 207 ng/mL after 5 mg. The recovery threshold is 30 ng/mL, which the effect site reaches about 20 to 45 minutes after those doses. Until October 2026 the band was 30 to 300 ng/mL.

### Covariates

None in the source. Under the default [fat-free-mass scaling](help:models/fat-free-mass) the volumes follow fat-free mass relative to the 70 kg reference man and the clearances its 0.75 power; with the switch off, Heier's values are used as published for every patient.

### Where to be careful

Seven young volunteers. Renal failure roughly doubles neostigmine's half-life and is not represented. Give [glycopyrrolate](help:drugs/glycopyrrolate) alongside to see the two curves together; their interaction is not modelled.
