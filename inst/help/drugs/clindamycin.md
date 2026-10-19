### The model

Clindamycin's parameters are from Bouazza and colleagues (*Br J Clin Pharmacol* 2012;74:971-977), a population analysis of total plasma concentrations in 50 patients with osteomyelitis treated intravenously and by mouth. The code uses the paper's **final table**, not its abstract, which prints an earlier vector: clearance 15.2 L/h at 70 kg, volume 66.2 L, one compartment, absorption rate constant 0.967/h, oral bioavailability 0.876. Half-time at 70 kg is 3.0 hours.

Because both routes were fitted together, 15.2 L/h is systemic clearance and 0.876 is absolute bioavailability, applied once and only to oral doses.

### Covariates

The source scaled clearance with total weight to the power 0.497 and left the volume fixed. Under the default [fat-free-mass scaling](help:models/fat-free-mass) the same 0.497 exponent is applied to the fat-free-mass ratio and the volume follows the library convention; with the switch off the published total-weight scaling is reproduced exactly.

### Effect site

None; only the plasma concentration is plotted. *Time until threshold* is therefore timed on the plasma curve. That curve is **total** clindamycin (bound plus free), but it is free drug that acts on the organism, so the threshold is the total concentration at which the **free** concentration equals the MIC. The line shows how long, with no further dose, until free clindamycin falls below the MIC. The MIC is **0.5 mg/L**, the CLSI susceptible breakpoint for staphylococci. Because binding saturates, the threshold comes from the binding fit of Wulkersdorfer and colleagues (*J Antimicrob Chemother* 2021;76:2106-2113): free clindamycin is 0.5 mg/L when the total is about **5.2 mcg/mL** (free fraction about 0.10). The often-quoted free fraction of 0.15 is an average over a whole dose and would put the threshold too low, at 3.3 mcg/mL. When alpha-1 acid glycoprotein is raised, after surgery or with inflammation, the same free level needs more total drug. See *Time until threshold: free drug at the MIC* above.

### Typical concentrations

Clindamycin is **bacteriostatic** against *Staphylococcus aureus* (the FDA label says so in those words): its MBC is many times the MIC, and serum at usual doses mostly does not kill it. The shaded band (3.1 to 9.9 mcg/mL total) is therefore built from the organisms clindamycin does kill: from 0.25 mg/L free, the one measured MBC of tonsillar streptococci and flora, to 1.6 mg/L free, which killed 80 per cent of susceptible *Bacteroides fragilis*. Clindamycin is bound to alpha-1 acid glycoprotein and the binding saturates, so each free MBC is placed on the total curve through the binding fit: about 90 per cent bound at the low edge, less at the top. For staphylococci the measure is the time-until-threshold line, free drug at the MIC. The band sits where the label's own levels lie: 600 mg every 8 hours gives a steady-state peak of 12.0 and a trough of 3.1 mcg/mL in a 70 kg adult (Table 1 of the Cleocin Phosphate label, revised 12/2021), and the label's continuous-infusion regimens hold serum above 4, 5 or 6 mcg/mL. The *Bacteroides fragilis* edge needs the label's higher doses, 1200 to 2700 mg a day, which it gives for proven or suspected *B. fragilis*. See *Shaded band: free drug at the MBC* above.

### Where to be careful

The intravenous product is clindamycin phosphate, an inactive prodrug hydrolysed with a half-time of about six minutes; treating the labelled dose as direct parent input is the source's approximation and shows only in the first minutes after a fast infusion. The active N-demethyl and sulfoxide metabolites are not modelled.
