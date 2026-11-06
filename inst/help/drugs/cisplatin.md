### The model

Cisplatin comes from Urien and Lokiec (*Br J Clin Pharmacol* 2004;57:756-763): 43 adults in two phase I studies, given 15 to 40 mg/m² over about 30 minutes, daily for five days or twice a month. Platinum was measured, both unbound (in an ultrafiltrate of plasma, the active form) and in total. Two compartments describe the unbound platinum, from which platinum binds irreversibly to plasma proteins; the bound platinum, inactive and slowly cleared, is plotted in its own panel, [cisplatinBound](help:drugs/cisplatinBound):

- unbound: central volume 23.4 L × (body surface area / 1.7)^1.60, clearance 35.5 L/h × (body surface area / 1.7)^0.83 × (creatinine clearance / 81)^0.36, and a peripheral compartment of 12.0 L exchanging at 8.64 L/h;
- half-lives 20 minutes and 1.3 hours; the bound platinum's 50 hours.

Total plasma platinum, which most older studies report, is the sum of the two panels.

### What is plotted

**Unbound platinum** in ng/mL (µg/L of platinum, as measured). Doses are entered in mg of cisplatin as labelled; a dose prescribed per m² must be multiplied by the body surface area first. There is no effect site and no band.

### Covariates

- **Body surface area** (Mosteller's formula; the paper names none).
- **Serum creatinine**, from the Patient Profile, in Cockcroft-Gault. Left blank, the library's assumed normal value is used.
- **Held at the reference**: serum protein at 70 g/L, and the dose at 25 mg/m² (a larger dose per m² binds proportionally less, which the curve cannot show). Both act only on the bound platinum.

### Where to be careful

- **The text's equation for the central volume** prints an exponent of 2.3, which belongs to the paper's separate total-platinum model; Table 4's 1.60 is used.
- **Nephrotoxicity** lowers the creatinine clearance during a course; the curve holds the value entered. Not a monitoring tool.

Every parameter was read in the paper's PDF.
