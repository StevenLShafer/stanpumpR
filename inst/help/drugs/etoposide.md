### The model

Etoposide in adults comes from Krogh-Madsen and colleagues (*Cancer Chemother Pharmacol* 2012;69:1155-1163): 23 Danish adults with acute myeloid leukaemia given 100 mg/m² over one hour on five days, with cytarabine and daunorubicin. Two compartments describe it:

- clearance 0.99 L/h + 0.00996 × creatinine clearance (Cockcroft-Gault, mL/min), so about half renal;
- central volume 9.13 L in men and 6.58 L in women; a peripheral compartment of 4.75 L exchanging at 1.14 L/h.

For a man with a creatinine clearance of 116 mL/min, clearance is 2.1 L/h and the half-lives are 1.4 and 5.9 hours. The children's model is [etoposidePediatric](help:drugs/etoposidePediatric).

### What is plotted

Plasma etoposide in mcg/mL (mg/L). Doses are entered in mg or as an infusion rate; a dose prescribed per m² must be multiplied by the body surface area first. There is no effect site and no band.

### Covariates

- **Serum creatinine**, from the Patient Profile, in Cockcroft-Gault. Left blank, the library's assumed normal value is used.
- **Sex**, on the central volume.
- **Held at the reference**: the white cell count at diagnosis, which raised clearance slightly in the study and has no Patient Profile field.

### Where to be careful

- **The paper's text and figure imply a steeper renal effect** than its table: a 30% fall in clearance at a creatinine clearance of 60 mL/min, where the table's slope gives 26%. The table is used.
- **23 patients**, sampled over the first day. Not a monitoring tool.

Every parameter was read in the paper's PDF.
