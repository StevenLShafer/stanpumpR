### The model

Gentamicin's parameters are from Smit and colleagues (*J Antimicrob Chemother* 2020;75:3286-3292), a two-compartment model of total serum gentamicin fitted in 542 individuals, mostly overweight or obese hospital patients with 28 healthy participants of a prospective study, and validated in 208 more: clearance 3.53 L/h at a de-indexed eGFR of 74 mL/min, scaling linearly with eGFR; central volume 16.6 L at 70 kg, scaling linearly with weight; intercompartmental clearance 1.48 L/h and peripheral volume 13.4 L, fixed. The source's 25 per cent reduction in clearance for intensive-care admission is not an input and is left off.

### Covariates

Clearance follows the CKD-EPI 2009 eGFR de-indexed by Du Bois body surface area, from the **Serum creatinine** in the Patient Profile. Left blank, the creatinine is **assumed normal** for the patient's age and sex (in a child, read on the adult scale; see [Renal function](help:models/covariates)): renal decline with age is then represented but renal impairment is not, and gentamicin is the drug for which that matters most. Enter it. Weight enters the central volume and the body surface area: the pharmacokinetic weight under the default [fat-free-mass scaling](help:models/fat-free-mass), total weight with the switch off, when the size-free Q and Vp are also fixed as published.

### Effect site

None; only the plasma concentration is plotted. *Time until threshold* is therefore timed on the plasma curve. That curve is **total** gentamicin (bound plus free), but it is free drug that acts on the organism, so the threshold is the total concentration at which the **free** concentration equals the MIC. The line shows how long, with no further dose, until free gentamicin falls below the MIC. The MIC is **2 mg/L**, the susceptible breakpoint for *E. coli* and the other Enterobacterales that CLSI (since 2023) and EUCAST share. Gentamicin is essentially unbound in serum, so the threshold is the MIC itself. See *Time until threshold: free drug at the MIC* above.

### Typical concentrations

The shaded band runs from a trough ceiling of about 1 mcg/mL to the historical peak benchmark of 8 to 10 mcg/mL. That benchmark (Kashuba and colleagues, *Antimicrob Agents Chemother* 1999;43:623-629) was a peak extrapolated to 30 minutes after a 30-minute infusion, not the end-of-infusion maximum this two-compartment model shows, which is higher.

### Where to be careful

The training range of eGFR was about 6 to 216 mL/min and renal replacement was excluded. Nothing here predicts nephrotoxicity or ototoxicity; a trough is a surrogate, not a probability.
