### The model

Capecitabine, an oral prodrug of 5-FU, comes from Gieschke and colleagues (*Br J Clin Pharmacol* 2003;55:252-263; the same model with its equations in Blesch and colleagues, *Invest New Drugs* 2003;21:195-223): 505 patients with colorectal cancer given 1250 mg/m² twice daily. The model is a chain:

- capecitabine is absorbed (rate 1.09 per hour) and converted to **5′-DFUR**, which is plotted: volume 90.6 L, clearance 75.8 L/h, half-life 50 minutes;
- 5′-DFUR is converted, all of it, to **5-FU**, plotted in its own panel, [capecitabine5FU](help:drugs/capecitabine5FU): volume 17.8 L, clearance 1190 L/h, half-life 37 seconds, so 5-FU follows the rate at which it is formed;
- 5-FU is converted to FBAL, which the library does not plot (a third step in a chain needs an engine the library does not yet have).

Every volume and clearance is apparent. The model works in moles: a mg of capecitabine yields 0.685 mg of 5′-DFUR and, from it, 0.362 mg of 5-FU. On that reading the model reproduces the paper's exposures (5′-DFUR AUC 20.3 against the median 20.1 µg·h/mL after 2250 mg; 5-FU 0.68 against 0.71).

### What is plotted

Plasma 5′-DFUR in mcg/mL (mg/L). Doses are entered in mg of capecitabine by mouth (`mg PO`, or `mg PO bid`); a dose prescribed per m² must be multiplied by the body surface area first. This 5-FU is not the library's intravenous [fluorouracil](help:drugs/fluorouracil). There is no effect site and no band.

### Covariates

None act on 5′-DFUR or 5-FU apart from alkaline phosphatase, held at 140 U/L (5-FU's clearance is 11% lower at twice that). Creatinine clearance and body surface area act only on FBAL.

### Where to be careful

- **Plasma 5-FU after capecitabine** predicts tissue exposure, safety and efficacy poorly, as the authors say; capecitabine is converted to 5-FU preferentially in tumour.
- **Renal impairment** raises FBAL (and toxicity), which is not plotted.
- **DPD deficiency** is not represented. Not a monitoring tool.

Every parameter was read in the papers' PDFs.
