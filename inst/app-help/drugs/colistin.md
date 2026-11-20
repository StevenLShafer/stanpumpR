### The model

Colistin is formed from [colistimethate](help:drugs/colistimethate) (CMS) and is not dosed. It comes from the same paper, Garonzik and colleagues (*Antimicrob Agents Chemother* 2011;55:3284-3294), in one compartment:

- **Volume:** 45.1 L.
- **Clearance:** 0.0147 × CrCL + 2.19 L/h, with the creatinine clearance in mL/min/1.73 m².

The source found no trend in clearance against body size. With *Adjust weight to fat-free mass* ticked, the volume is scaled in proportion to fat-free mass and the nonrenal 2.19 L/h to its 0.75 power, anchored at Garonzik's typical patient (median 59.1 kg, 162.0 cm, 71 years), as colistimethate's are, so the colistin formed and colistin's own clearance move together. The renal term carries weight inside the creatinine clearance and is not scaled. Unticked, both are as published.

All of CMS's nonrenal clearance forms colistin. The fraction that actually becomes colistin is not identifiable, so the volume and clearance are each divided by it. Both are divided again by 1.78, the factor that puts the model in milligrams of colistin base activity (see [colistimethate](help:drugs/colistimethate)).

Colistin itself is hardly excreted by the kidney. Its clearance carries the creatinine clearance because, as renal function falls, more of each CMS dose is converted to colistin rather than excreted, and the fit carries that effect here.

### What is plotted

Total plasma colistin in mg/L, with a line at **4.1 mg/L** and no band.

### The MIC line

The line follows the library's rule for every antibiotic: it marks where free drug equals the MIC (see [Antibiotics](help:models/antibiotics)). The MIC is the EUCAST breakpoint of 2 mg/L for Enterobacterales and *Acinetobacter* (v16.1, 2026). About half of colistin is bound in critically ill patients (unbound fraction 0.49; Nation and colleagues, *Clin Infect Dis* 2017;64:565). Free colistin is therefore 2 mg/L when total colistin is 2 / 0.49 = 4.1 mg/L. *Time until threshold* gives the time until total colistin falls below 4.1 mg/L, which is when free colistin falls below the MIC.

**The line sits above the exposure the consensus guideline calls tolerable.** The international guideline (Tsuji and colleagues, *Pharmacotherapy* 2019;39:10-39, doi 10.1002/phar.2209) sets the target as an exposure: an AUC of about 50 mg·h/L, "a target average steady-state plasma concentration ... of ~2 mg/L for total drug". It adds that this "should be considered as a maximum tolerable exposure", because higher concentrations increase both the incidence and the severity of acute kidney injury. Nation and colleagues put the rise in nephrotoxicity above about 2.5 mg/L.

The two numbers measure different things. The line is an instant, when free colistin equals the MIC. The guideline's target is an average of total colistin over the day, set by kidney toxicity. The guideline expects that average to kill an isolate at the 2 mg/L breakpoint in the mouse thigh model. It adds that the average is "very likely to be suboptimal for the systemic treatment of a lung infection" unless the MIC is well below the breakpoint.

So, read the line this way:

- **A regimen that meets the guideline's target stays below the line.** Colistin's long half-life keeps its curve flat. In this model a regimen averaging 2 mg/L peaks at only about 2.2 mg/L, so free colistin stays near 1 mg/L, half the breakpoint MIC.
- **Keeping the curve above the line means more than twice the tolerable exposure.** An average at or above 4.1 mg/L is more than twice the guideline's maximum tolerable exposure.

This is the reason for the source's own advice that "colistin may best be used as part of a highly active combination, especially for patients with moderate to good renal function and/or for organisms with MICs of ≥1.0 mg/liter" (Garonzik 2011).

The line was drawn on Steven L. Shafer's decision of 2026-11-16, with this explanation.

### No band

Most antibiotics' band marks the expected MBC, the concentration of free drug that kills. Colistin has none: no MBC source has been read, and the guideline's target is an average over the day, not a range on the curve.
