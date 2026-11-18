### A toxic metabolite

PPX (2',6'-pipecoloxylidide) is formed from [ropivacaine](help:drugs/ropivacaine) in the liver by CYP3A4. It is a convulsant, roughly a twelfth as potent as unbound ropivacaine (an unpublished manufacturer's figure in rats, quoted by every paper that uses it), and it contributes nothing to the block. It is drawn with its **label in red**. Its half-life is several times ropivacaine's, so during a perineural or epidural infusion lasting days it keeps rising after ropivacaine has levelled off. That is when it matters.

### The model

The parameters are from Sunder and colleagues (*Anesthesiology* 2026;144:90-102), who measured total and unbound ropivacaine and PPX in venous plasma for 26 hours in 20 children and adolescents, aged 4 to 17 and weighing 17 to 95 kg, given a lumbar plexus block of 2 mg/kg and an infusion of 0.4 mg/kg/h. PPX is **one compartment**, for its **total** plasma concentration (the library's ropivacaine is total too), scaled by total body mass:

| Parameter | At 40 kg | At 70 kg |
|---|---|---|
| Clearance | 0.29 L/min | 0.44 L/min (26.5 L/h) |
| Volume | 351 L | 614 L |
| Half-life | 14.0 h | 16.1 h |

The clearance scales with weight to the 0.75 power and the volume in proportion. PPX reaches the plasma after a short metabolic delay, which Sunder modelled as three compartments in series with a mean of 4.9 minutes. The library uses one compartment with the same mean; against a half-life of 16 hours the difference is under 0.1% of the peak.

### Formed only, from all of ropivacaine's clearance

PPX was never given directly, so the fraction of ropivacaine converted and PPX's volume cannot be told apart. Sunder took all of ropivacaine's clearance to form PPX, milligram for milligram, so the clearance and volume above are apparent: the true values are smaller in proportion to the fraction actually converted, while the concentrations formed from ropivacaine are the same either way. PPX therefore has **no dosing unit**: it appears only when ropivacaine is given, by any route (an injection, a nerve block in **mg RA**, a perineural or epidural infusion in **mg/hr RA**). At steady state its concentration is the ropivacaine infusion rate over PPX's clearance: 20 mg/h in a 70 kg patient approaches 0.75 mg/L, over about two days.

### Checked against adults

Sunder studied children and adolescents. Its model was checked against two adult studies it was not fitted to, both of which measured total PPX in venous plasma at the end of an infusion. The doses were entered as tissue doses:

| Study | Regimen | Measured | This model |
|---|---|---|---|
| Burm 2000 (*Anesthesiology* 93:395-403) | 50 mg, then 20 mg/h epidural for 72 h, 71 kg | 1.0 ± 0.4 mg/L | 0.71 |
| Burm 2000 | 50 mg, then 30 mg/h for 72 h, 82 kg | 1.2 ± 0.4 | 0.95 |
| Ekatodramis 2003 (*Anesthesiology* 98:143-150) | 225 mg interscalene, then 12 mg/h from 6 to 54 h, 83 kg | 0.35 ± 0.18 | 0.37 |
| Ekatodramis 2003 | 225 mg, then 18 mg/h from 6 to 54 h, 75 kg | 0.70 ± 0.38 | 0.58 |

All four are within one standard deviation. Against Burm's 72-hour infusions the model is 20 to 30% low. Burm's patients' alpha-1-acid glycoprotein doubled over the three days, and that binds PPX and raises its total, not its unbound, concentration; Sunder's 26 hours could not see it.

Two other models were read and not used. Aarons and colleagues (*Br J Anaesth* 2011;107:409-424) fitted children from birth to 12 years and warn against extrapolating to adults: their PPX volume grows with weight to the power 1.67, which gives a two-day half-life at 70 kg. Yoo and colleagues (*Eur J Anaesthesiol* 2025;42:751-753), the one adult model, studied five patients with chronic kidney disease for 24 hours, and their letter prints no PPX volume.

### No band

No equilibration delay has been published for PPX, so the row plots plasma and has no effect site. No threshold has been established for PPX alone. The convention in these papers adds a twelfth of PPX to **unbound** ropivacaine and compares the sum with 0.34 to 0.38 mg/L. The library plots total ropivacaine and does not model its binding, so that sum cannot be drawn, and **no band or threshold is drawn**.

### Where to be careful

PPX is cleared partly by the kidney, and its renal clearance follows creatinine clearance (Pere and colleagues, *Br J Anaesth* 2011). The model has **no renal term**, so in renal impairment it may underpredict. Sunder found PPX clearance varying more than twofold between patients (a coefficient of variation of 155%), so an individual's PPX may be well above this typical curve. Over infusions longer than a day, the rise in alpha-1-acid glycoprotein raises total PPX above what this model shows.
