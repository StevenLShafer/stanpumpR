### The model

Clindamycin's parameters are from Bouazza and colleagues (*Br J Clin Pharmacol* 2012;74:971-977), a population analysis of total plasma concentrations in 50 adults with osteomyelitis treated intravenously and by mouth. The code uses the paper's **final table**, not its abstract, which prints an earlier vector: clearance 15.2 L/h at 70 kg, volume 66.2 L, one compartment, absorption rate constant 0.967/h, oral bioavailability 0.876. Half-time at 70 kg is 3.0 hours.

Because both routes were fitted together, 15.2 L/h is systemic clearance and 0.876 is absolute bioavailability, applied once and only to oral doses.

### Covariates

The source scaled clearance with total weight to the power 0.497 and left the volume fixed. Under the default [fat-free-mass scaling](help:models/fat-free-mass) the same 0.497 exponent is applied to the fat-free-mass ratio and the volume follows the library convention; with the switch off the published total-weight scaling is reproduced exactly.

### Effect site

None; only the plasma concentration is plotted.

### Typical concentrations

The shaded band is 1 to 4 mcg/mL total, around the source's working trough criterion of 2 mg/L. Clindamycin is about 85 per cent bound to alpha-1 acid glycoprotein, so free concentrations are roughly a sixth of what is plotted.

### Where to be careful

The intravenous product is clindamycin phosphate, an inactive prodrug hydrolysed with a half-time of about six minutes; treating the labelled dose as direct parent input is the source's approximation and shows only in the first minutes after a fast infusion. The active N-demethyl and sulfoxide metabolites are not modelled.
