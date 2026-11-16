### The model

Kerbusch and colleagues (*Br J Clin Pharmacol* 2000;49:555-561) studied 15 adults (23 to 72 years, 49 to 82 kg) with soft-tissue sarcoma, each given 9 or 12 g/m² as a 72-hour continuous infusion. One compartment, volume 43.5 L, with a clearance that the drug itself increases.

### Autoinduction

Ifosfamide induces its own metabolism. The model carries a relative amount of enzyme that starts at 1 and whose breakdown ifosfamide inhibits (half the maximum inhibition at 30.7 µM, 8.0 mg/L). While drug is present the enzyme accumulates, about doubling in the first day of an infusion, and the clearance (2.94 L/h before any induction) rises with it. So during a constant infusion the concentration peaks after about 20 hours and then falls, and a second course given soon after the first is cleared faster. After the infusion the enzyme returns to its baseline with a half-life of 12.7 hours. The enzyme is carried through the whole dose table, so the curve depends on the dose history, not just the current dose.

Run with the paper's numbers, the model gives peaks of 108 and 204 µM after 10 and 20 g over 72 hours, as the paper's Figure 6 shows. The paper's Table 2 prints its simulated areas in "µM h", but they are g·h/L (1.65 for 10 g, printed as 1.7), and its 20-g value does not reproduce.

### What is plotted

Plasma ifosfamide, in mcg/mL. Ifosfamide has no salt; doses are entered in mg of ifosfamide. The active and toxic species (4-hydroxyifosfamide, the mustard, chloroacetaldehyde) are not modelled, so there is no therapeutic range.

### Where to be careful

- **Fitted to 72-hour infusions.** Shorter infusions (1 to 3 hours daily, the common regimen) run the same equations, which is an extrapolation.
- **No covariates.** Size is scaled by the library's factors; renal function, age and sex were not studied.
- **Fifteen patients.**
- **Not in pregnancy:** one patient's maternal and neonatal levels have been reported (Mir 2011), no comparison; nothing is adjusted.
