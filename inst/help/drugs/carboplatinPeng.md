### The model

Carboplatin in children from Peng and colleagues (*Eur J Cancer* 1995;31A:1804-1810), who built a Bayesian method of estimating a child's carboplatin exposure from one or two samples. Its prior, the population model, is two compartments per m² of body-surface area, fitted in 22 children with cancer given 60- to 90-minute infusions:

- central volume 5.09 L/m², clearance 6.31 L/h/m² (105 mL/min/m²);
- a peripheral compartment of 6.95 L/m², exchanging at 7.7 L/h/m².

The method was tested in 23 more children aged 1 month to 18.4 years (5 to 63 kg). The paper does not print the ages of the 22 children the prior came from; the selector uses the tested children's ranges. Body-surface area is Mosteller's.

stanpumpR chooses it for infants and for children from 8.3 to 18.4 years, whom neither [carboplatinPediatric](help:drugs/carboplatinPediatric) (Duong 2019, 1.7-8.3 years) nor [carboplatinDuffull](help:drugs/carboplatinDuffull) (adults) contains (Steven Shafer's decision, 17 November 2026). See [carboplatin](help:drugs/carboplatin) for how the model is chosen.

### What is plotted

Ultrafilterable (unbound) plasma carboplatin, measured as platinum by atomic absorption, in mcg/mL (mg/L). Dosing aims at an exposure (an AUC), which is not drawn; there is no band.

### Where to be careful

- **Its clearance is higher than Duong 2019's.** For the same child the clearance is 40 to 50% above Duong's (2.24 against 1.51 L/h at 6 months, 9.40 against 6.72 L/h at 14 years), and 19% above the median the paper measured in its own tested children (88 mL/min/m², range 51 to 129). The curve for a child chosen this model is therefore lower than the one Duong's model would draw.
- **Renal function is not a covariate.** The paper's renal dosing equation, clearance = GFR + 0.36 × weight, is a separate method; a child with poor renal function will clear carboplatin more slowly than drawn.
- **Not a monitoring tool.** The paper's purpose was to estimate a child's own exposure from measured levels.

Every parameter was read in the paper's PDF.
