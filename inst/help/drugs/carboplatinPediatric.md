### The model

Carboplatin in children comes from Duong and colleagues (*Br J Clin Pharmacol* 2019;85:136-146): 51 children aged 1.7 to 8.3 years (8.6 to 31 kg) with high-risk neuroblastoma, given high-dose carboplatin, etoposide and melphalan before autologous transplantation; carboplatin as a 96-hour infusion. Two compartments describe it, scaled by weight alone:

- at 15.5 kg, clearance 2.7 L/h (45 mL/min), central volume 8.3 L, and a peripheral compartment of 0.74 L exchanging at 0.23 L/h;
- the clearances scale with weight to the ¾ power, the volumes in proportion.

The paper names its reference weight only as the population's median weight. 15.5 kg is used because the paper's own carboplatin exposures reproduce with it; the printed median, 14.5 kg, puts them 2 to 7% low. The same paper is the library's [melphalanPediatric](help:drugs/melphalanPediatric) and [etoposideDuong](help:drugs/etoposideDuong); the adults' carboplatin is [carboplatin](help:drugs/carboplatin).

### What is plotted

Unbound plasma carboplatin in mcg/mL (mg/L). Doses are entered in mg or as an infusion rate; a dose prescribed per kg or per m² must be converted first. Dosing aims at an exposure (an AUC), which is not drawn; there is no band.

### Where to be careful

- **Renal function is not a covariate.** Measured GFR was tested and did not survive once weight was in the model, but only one child had a GFR below 76 mL/min/1.73 m², and children with a GFR below 60 were excluded. In a child with poor renal function the curve will overestimate clearance.
- **The early curve is an extrapolation.** No sample was taken before 6 hours into the 96-hour infusion, so the peripheral compartment is essentially unidentified.
- **Children 1.7 to 8.3 years.** Not a monitoring tool.

Every parameter was read in the paper's PDF.
