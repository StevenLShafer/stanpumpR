### A derived model

No population model of oral thioguanine (Tabloid) has been published. stanpumpR builds one, on Steven Shafer's decision of 16 November 2026, from two studies by the same pediatric group at the National Cancer Institute:

- **How the body handles it:** Kitchen 1997's intravenous model ([thioguanineIV](help:drugs/thioguanineIV)), two compartments with elimination that saturates, scaled by body-surface area.
- **How it is absorbed:** Lowe 2001's measurements in 41 children with leukaemia, 1 to 9 years, after their first 60 mg/m² oral dose.

Three numbers come from Lowe:

- **Bioavailability, 23%:** his mean oral exposure times his own intravenous clearance in the same children, divided by the dose.
- **Absorption rate, 0.62 per hour:** chosen so that the peak matches his median peak, 0.29 µmol/L.
- **Lag, 1 hour:** chosen so that the peak falls at his median time, 2 hours.

The model then gives an exposure of 1.03 µmol/L·h, Lowe's median, and a half-life of 1.75 hours between 4 and 8 hours after the dose, near his 1.6. Neither was used to build it.

### What is plotted

Plasma thioguanine, in mcg/mL (1 µmol/L is 0.167 mcg/mL). The active species are the thioguanine nucleotides inside red cells and leukaemic cells, which are not modelled, and which Lowe found unrelated to plasma levels. There is no therapeutic range, so none is drawn.

### Where to be careful

- **Absorption is erratic.** Lowe's exposures spanned 50-fold (0.18 to 9.5 µmol/L·h) at the same dose per m². The curve is a median child's.
- **Derived, not fitted.** The absorption was set to Lowe's medians, not estimated from his concentrations.
- **Genotype.** TPMT and NUDT15 govern the nucleotides and the dose, not plasma thioguanine; neither is modelled.
- **Pregnancy:** no pharmacokinetic data were found; nothing is changed.
