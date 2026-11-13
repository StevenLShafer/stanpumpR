### A toxic metabolite

MEGX (monoethylglycinexylidide) is formed from [lidocaine](help:drugs/lidocaine) in the liver, by N-deethylation (CYP1A2 and CYP3A4). It is nearly as potent as lidocaine in causing central nervous system and cardiac toxicity, and it contributes little to the analgesia lidocaine is given for. It is therefore drawn with its **label in red**: its curve adds to the risk of local anesthetic systemic toxicity, not to the effect. MEGX is cleared more slowly than lidocaine, so during a long infusion it keeps rising after lidocaine has levelled off, and it is what accumulates over days of a perineural catheter or a postoperative infusion.

### The model

The parameters are from Foong and colleagues (*Pharm Res* 2025;42:451-473), a joint population model of lidocaine, MEGX and glycinexylidide (GX) in 98 adults given lidocaine during laparoscopic donor nephrectomy or cholecystectomy, with plasma sampled for 24 hours. Lidocaine's elimination enters **one transit compartment**, which empties into MEGX's central compartment with a mean transit time of 0.513 hours. MEGX has **two compartments**:

| Parameter | Value at 70 kg |
|---|---|
| Transit time, lidocaine to MEGX | 0.513 h |
| Central volume | 30.7 L |
| Clearance | 52.8 L/h |
| Intercompartmental clearance | 442 L/h |
| Peripheral volume | 480 L |

These give half-lives of 2.5 minutes and 7.4 hours. The peripheral volume was 480 L after cholecystectomy and 302 L after donor nephrectomy (a terminal half-life of 4.8 hours); the library uses the larger group's value. Neither changes the steady state.

### Formed only, from all of lidocaine's clearance

MEGX was never given on its own, so the fraction of lidocaine converted to MEGX and MEGX's volumes cannot both be estimated. When Foong fixed the central volume to a published value and estimated the fraction, it came out at one; the final model fixes it to one. So the library forms MEGX from **all** of lidocaine's clearance, mole for mole (MEGX 206.28 g/mol, lidocaine 234.34 g/mol), and the volumes and clearances above are on that scale: the true values are these multiplied by the fraction actually converted, while the concentrations formed from lidocaine are the same either way. For that reason MEGX has **no dosing unit**: it appears only when lidocaine is given. See [Active metabolites](help:models/metabolites).

Because all of lidocaine's clearance forms MEGX, every milligram of lidocaine given becomes 0.88 mg of MEGX whatever lidocaine's own clearance is, and at steady state the MEGX concentration is the lidocaine infusion rate times 0.88 over MEGX's clearance. At 1.5 mg/kg/h in a 70 kg patient (105 mg/h), MEGX approaches 105 x 0.88 / 52.8 = **1.75 mcg/mL**, more than the lidocaine itself (1.16 mcg/mL with the library's lidocaine), but it takes about a day to get most of the way there. The library's lidocaine is Schnider's 1996 model, not Foong's, whose clearance is about half as large; Schnider's faster clearance forms MEGX somewhat sooner after a bolus, but the amount formed and the steady state are the same.

MEGX is formed whatever the route of lidocaine: intravenous doses and infusions, nerve blocks (**mg RA**) and perineural infusions (**mg/hr RA**) all form it.

### Covariates

Size only. Foong found that all of MEGX's volumes and clearances scaled best with total body mass, volumes in proportion and clearances to the 0.75 power, and that is applied as fitted, so *Adjust weight to fat-free mass* does not change MEGX.

### No band

No human equilibration delay has been published for MEGX, so the row plots plasma and has no effect site. No toxic or therapeutic concentration has been established for MEGX alone, so **no band is drawn**.

### Lidocaine + MEGX against 5 mcg/mL

The clinical convention (Crouch and colleagues, *Anesthesiology* 2023;138:71-81) adds MEGX to lidocaine, and a quarter of glycinexylidide (GX), and compares the sum with lidocaine's toxic threshold of 5 mcg/mL. The MEGX panel draws that sum as a second, red line, **lidocaine + MEGX** (the plasma concentrations added), with a **dashed red line at 5 mcg/mL**. GX is not modelled, so the line drawn leaves out its quarter-weighted share and is slightly low. The sum is drawn only on the concentration axis, not when the plot is normalized.

### Where to be careful

The published estimates of MEGX clearance differ more than threefold, and this model uses the lowest, so it predicts the highest MEGX:

| Source | Patients | MEGX clearance |
|---|---|---|
| Foong 2025 (used) | 98 surgical, sampled to 24 h | 52.8 L/h at 70 kg |
| Crouch 2023 | 15 living liver donors, before resection | 101 L/h (one compartment, volume fixed to lidocaine's) |
| Tognolini 2025 (*Anaesthesia* 80:511-521) | 30 with obesity, median 107 kg | about 186 L/h total (4.19 L/min unbound, 74% unbound), sampled only 2 h after the infusion |

Foong's is the only one sampled long enough after the infusion to see MEGX fall, which is what a clearance estimate needs. MEGX's elimination depends on the liver, as lidocaine's does, so it is slower in hepatic disease, heart failure and after liver resection (a hepatectomy model, *Drug Des Devel Ther* 2025;19:6255-6268, estimated MEGX accumulating to toxic levels within a day at ordinary infusion rates in that population). Foong also found lidocaine's own clearance falling to 40% after surgery, which the library's lidocaine does not include.
