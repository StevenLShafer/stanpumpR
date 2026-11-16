### The model

Colistin is formed from [colistimethate](help:drugs/colistimethate) (CMS) and is not dosed. It comes from the same paper, Garonzik and colleagues (*Antimicrob Agents Chemother* 2011;55:3284-3294), in one compartment:

- **Volume:** 45.1 L.
- **Clearance:** 0.0147 × CrCL + 2.19 L/h, with the creatinine clearance in mL/min/1.73 m².

All of CMS's nonrenal clearance forms colistin. The fraction that actually becomes colistin is not identifiable, so the volume and clearance are each divided by it. Both are divided again by 1.78, the factor that puts the model in milligrams of colistin base activity (see [colistimethate](help:drugs/colistimethate)).

Colistin itself is hardly excreted by the kidney. Its clearance carries the creatinine clearance because, as renal function falls, more of each CMS dose is converted to colistin rather than excreted, and the fit carries that effect here.

### What is plotted

Total plasma colistin in mg/L. No line and no band are drawn.

The international consensus guideline (Tsuji and colleagues, *Pharmacotherapy* 2019;39:10-39, doi 10.1002/phar.2209) sets the target as an exposure: an AUC of about 50 mg·h/L, "a target average steady-state plasma concentration ... of ~2 mg/L for total drug". It adds that this "should be considered as a maximum tolerable exposure", because higher concentrations increase both the incidence and the severity of acute kidney injury. Nation and colleagues (2017) put the rise in nephrotoxicity above about 2.5 mg/L. An average over the day is not a band on the curve, so none is drawn.

The library's antibiotics carry a line at free drug at the MIC. For colistin that would be the EUCAST breakpoint of 2 mg/L for Enterobacterales and *Acinetobacter* (v16.1, 2026). With about half of colistin unbound in critically ill patients (0.49, Nation 2017), the line would sit at 4.1 mg/L of total colistin, above the exposure the guideline calls tolerable. Whether to draw it is still to be decided, so for now it is not drawn.
