### The model

Pethidine (meperidine) uses the kinetics of Björkman (*J Pharmacokinet Pharmacodyn* 2003;30:285-307), a physiologically based model of several opioids recast here as a three-compartment model with a central volume of 18.1 L at 70 kg and fixed rate constants.

### Covariates

Size only. By default, with *Adjust weight to fat-free mass* ticked, the volumes scale with the patient's fat-free mass relative to the 70 kg, 170 cm reference man and the clearances with that ratio to the 0.75 power, so height, age and sex enter through fat-free mass ([fat-free-mass scaling](help:models/fat-free-mass)). Björkman's parameters are for a 70 kg adult, with fixed rate constants: unticking the box scales the volumes and clearances linearly with weight instead.

### Effect site

The time to peak effect is 10 minutes.

### MEAC and typical concentrations

MEAC is 0.46 mcg/mL, with the shaded band at 0.8 to 2 MEAC, 0.37 to 0.92 mcg/mL. It is the mean minimum effective analgesic concentration Austin, Stapleton and Mather measured in nine patients given intramuscular pethidine after surgery (*Anesthesiology* 1980;53:460-466): 0.46 mcg/mL (SD 0.18), against 0.41 mcg/mL for the highest concentration still associated with severe pain. Their study was the origin of the MEAC concept; they found a steep concentration-analgesia relationship in each patient, with a 39 per cent coefficient of variation in MEAC between patients. Austin measured **whole blood**, and the library uses the number as an effect-site value without any conversion. Until October 2026 the MEAC was 0.25 mcg/mL, which was attributed to Austin but is not in the paper; its origin was not recorded.

### Where to be careful

Pethidine's active metabolite norpethidine, which accumulates with repeated dosing and in renal failure and causes seizures, is not modelled. Nor is the serotonergic interaction with monoamine oxidase inhibitors. The model is a pharmacokinetic description of a typical adult and nothing more.
