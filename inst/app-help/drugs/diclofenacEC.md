### The model

Enteric-coated diclofenac kinetics are from Bartels, Armogida and Hamrén (Novartis), a poster at the Population Approach Group Europe meeting in Athens in 2010 ([PDF](https://www.page-meeting.org/wp-content/uploads/pdf_assets/3455-213_v5_ChristianBartels_final.pdf)). They pooled rich data from healthy volunteers given four oral products: immediate release (117 subjects), mixed release (21), slow release (21) and enteric coated (12, three times a day for 5 days). All four were fitted with one two-compartment disposition. The parameters are **apparent**, divided by the unmeasured bioavailability of the immediate-release product: CL/F 40.3 L/h, Vc/F 23.5 L, Q/F 10.6 L/h and Vp/F 21.3 L. The half-times are about 18 minutes and 1.9 hours. The concentrations are total plasma diclofenac.

### Why a separate drug

The [diclofenac](help:drugs/diclofenac) entry (Standing 2011) has a systemic disposition and the dispersible tablet, and its estimates do not apply to the enteric-coated tablet. This source is oral only, so joining the two would need a bioavailability carried between studies. Enteric-coated diclofenac is therefore its own drug, with every value from the one poster, like [amiodarone IV](help:drugs/amiodaroneIV). The two are plotted as separate lines, and a dose entered under one is not added to the other.

### Absorption

**mg PO** is the enteric-coated tablet, taken as labelled (usually diclofenac sodium; the poster does not name the salt). Its bioavailability relative to the immediate-release product, 0.784, is applied once to the dose. The whole dose is absorbed through one first-order path (0.503 per hour) after a lag of 0.932 hours. The source also passes it through two short transition compartments (100 per hour each), added to help NONMEM's fit. stanpumpR adds their mean transit time, 1.2 minutes, to the lag. A 50 mg tablet peaks at about 0.26 mcg/mL at 1.9 hours in the reference man.

### Where to be careful

Absorption of enteric-coated tablets is erratic. The poster's interoccasion variability of the absorption rate is 143%, and its residual error for this product, 96%, is the largest of the four. The curve is the typical patient's: a given tablet may peak much earlier or later, and higher or lower, and some profiles have several peaks. The source is a poster with 12 subjects in this arm, not a peer-reviewed paper, and it did no covariate analysis. Body size follows the library's [fat-free-mass scaling](help:models/fat-free-mass); with the switch off, the 70 kg values are used unscaled. There is no effect site and no shaded band, and no CYP2C9 or CYP2D6 term.
