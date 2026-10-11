### The model

Diclofenac kinetics are from Standing and colleagues (*Paediatr Anaesth* 2011;21:316-324), a pooled NONMEM analysis of 375 samples from 111 children aged 1 to 14 years given diclofenac intravenously, as an oral suspension and as suppositories, with published adult dispersible-tablet and suspension data added. Disposition is three-compartment. Per 70 kg: clearance 16.5 L/h, central volume 3.68 L, intercompartmental clearances 1.75 and 7.21 L/h, peripheral volumes 7.48 and 3.79 L. The terminal half-time at the reference man is about 1.9 hours. The concentrations are total plasma diclofenac.

### Routes

**mg PO** is the **dispersible tablet**. Its bioavailability, 0.35, is applied once to the whole dose, and the absorbed dose is then divided between two lagged first-order paths: 26% after 3.6 minutes with an absorption rate constant of 2.95 per hour, and 74% after 45 minutes at 2.23 per hour. stanpumpR carries both paths exactly, as two oral depots (the *Extravascular routes* table above). A 50 mg tablet peaks at about 0.8 mcg/mL at an hour in the reference man. The source's suspension (bioavailability 0.36, different paths) is not offered. Neither set of estimates applies to the **enteric-coated** tablet, the commonest adult form, whose absorption is delayed and erratic, or to suppositories. The enteric-coated tablet is a separate entry, [diclofenac EC](help:drugs/diclofenacEC), from a different source.

**mg** is intravenous diclofenac. Infusions are entered as **mg/hr** for the length of the infusion, since **mg** is a bolus.

### Where to be careful

The intravenous disposition rests on 65 concentrations from 10 children. The 70 kg intravenous curve is an allometric extrapolation, not an adult intravenous validation. An adult oral-only analysis (Bartels, a conference poster with 171 adults) reports apparent parameters only and cannot replace it; its enteric-coated arm is the separate [diclofenac EC](help:drugs/diclofenacEC) entry. The source's youngest child was a year old, so infants are an extrapolation. Body size follows the library's [fat-free-mass scaling](help:models/fat-free-mass); with the switch off, the published allometry on total weight.

There is no effect site and no shaded band. No analgesic concentration-effect relationship suitable for a typical range was found. The model has no CYP2C9 or CYP2D6 term, because none was fitted, and it does not represent hepatic impairment.
