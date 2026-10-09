### The model

Clindamycin's parameters are from Bouazza and colleagues (*Br J Clin Pharmacol* 2012;74:971-977), a population analysis of total plasma concentrations in 50 patients with osteomyelitis treated intravenously and by mouth. The code uses the paper's **final table**, not its abstract, which prints an earlier vector: clearance 15.2 L/h at 70 kg, volume 66.2 L, one compartment, absorption rate constant 0.967/h, oral bioavailability 0.876. Half-time at 70 kg is 3.0 hours.

Because both routes were fitted together, 15.2 L/h is systemic clearance and 0.876 is absolute bioavailability, applied once and only to oral doses.

### Covariates

The source scaled clearance with total weight to the power 0.497 and left the volume fixed. Under the default [fat-free-mass scaling](help:models/fat-free-mass) the same 0.497 exponent is applied to the fat-free-mass ratio and the volume follows the library convention; with the switch off the published total-weight scaling is reproduced exactly.

### Effect site

None; only the plasma concentration is plotted. *Time until threshold* is therefore timed on the plasma curve. That curve is **total** clindamycin (bound plus free), but it is free drug that acts on the organism, so the threshold is the total concentration at which the **free** concentration equals the MIC. The line shows how long, with no further dose, until free clindamycin falls below the MIC. The MIC is **0.5 mg/L**, the CLSI susceptible breakpoint for staphylococci. Because binding saturates, the threshold comes from the binding fit of Wulkersdorfer and colleagues (*J Antimicrob Chemother* 2021;76:2106-2113): free clindamycin is 0.5 mg/L when the total is about **5.2 mcg/mL** (free fraction about 0.10). The often-quoted free fraction of 0.15 is an average over a whole dose and would put the threshold too low, at 3.3 mcg/mL. When alpha-1 acid glycoprotein is raised, after surgery or with inflammation, the same free level needs more total drug. See *Time until threshold: free drug at the MIC* above.

### Typical concentrations

The shaded band is 1 to 4 mcg/mL total, around the source's working trough criterion of 2 mg/L. Clindamycin is bound to alpha-1 acid glycoprotein, and the binding saturates: about 90 per cent bound at these low levels, less at the peak after a dose. Free concentrations in the band are therefore roughly a tenth of what is plotted.

### Where to be careful

The intravenous product is clindamycin phosphate, an inactive prodrug hydrolysed with a half-time of about six minutes; treating the labelled dose as direct parent input is the source's approximation and shows only in the first minutes after a fast infusion. The active N-demethyl and sulfoxide metabolites are not modelled.
