### The model

Rocuronium's parameters are from Plaud and colleagues (*Clin Pharmacol Ther* 1995;58:185-191), who modelled rocuronium's kinetics and its effect at the vocal cords and the adductor pollicis. It is a **two-compartment** model: a central volume of 0.056 L/kg with elimination and one peripheral compartment.

### Covariates

Size only. By default, with *Adjust weight to fat-free mass* ticked, the volumes scale with the patient's fat-free mass relative to the 70 kg, 170 cm reference man and the clearances with that ratio to the 0.75 power, so height, age and sex enter through fat-free mass. Plaud's model is per kilogram of total body weight, with fixed rate constants: unticking the box restores that, scaling the volumes and clearances linearly with weight.

### Effect site

The time to peak effect is 4.5 minutes, the population median time to maximum block at the adductor pollicis after 0.15 mg/kg in Cortínez and colleagues (*Br J Anaesth* 2007;99:679-685; range 2.0 to 7.7 minutes), whose ke0 was 0.19 to 0.20/min by three methods. Solved against Plaud's kinetics the library's ke0 is about 0.19/min, and Plaud's own adductor pollicis ke0 (0.168/min, present in the file as `k41` but unused) agrees. Until October 2026 the library used 2.2 minutes, the time of one illustrated subject in that paper, which implied a ke0 near 0.7/min. Onset at the vocal cords is faster than at the adductor pollicis, and is not what the curve shows.

### Typical concentrations and recovery

The shaded band is 1 to 2.2 mcg/mL and the default recovery threshold 1 mcg/mL. Here "effect" is a concentration, not a twitch height: the model does not include the sigmoid relationship between concentration and block, so the plot cannot show train-of-four counts. The threshold of 1 mcg/mL is roughly the concentration at which recovery of neuromuscular function begins, and *Time until threshold* is then the time to the start of recovery, not to full reversal. See [the rocuronium scenario](scenario:rocuronium-recovery).

### Where to be careful

Rocuronium's duration is prolonged by hepatic and renal dysfunction, by the inhaled agents (a pharmacodynamic potentiation) and by hypothermia, and shortened by sugammadex, none of which is modelled. [Sugammadex](help:drugs/sugammadex) can be given in the dose table, but only its own concentration is simulated: it does not change the rocuronium row. Plaud's patients were adults under propofol-opioid anesthesia.
