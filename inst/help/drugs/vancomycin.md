### The model

Vancomycin's parameters are from Thomson and colleagues (*J Antimicrob Chemother* 2009;63:1050-1057), a two-compartment model of total serum vancomycin fitted in 398 hospitalised patients and evaluated in 100 more: clearance 2.99 L/h at a creatinine clearance of 66 mL/min, rising by 1.54 per cent per mL/min; central and peripheral volumes 0.675 and 0.732 L/kg; intercompartmental clearance 2.28 L/h. The source table labels that last figure in units of a rate constant but identifies it as a clearance, and the code reads it as 2.28 L/h.

### Covariates

Clearance follows Cockcroft-Gault creatinine clearance, from the **Serum creatinine** in the Patient Profile (floored at 0.68 mg/dL, 60 µmol/L, as in the source). Left blank, the creatinine is **assumed normal** for the patient's age and sex (in a child, read on the adult scale; see [Renal function](help:models/covariates)): renal decline with age is then represented but renal impairment is not, and for accumulation in a patient with a raised creatinine, which is the vancomycin question that matters most, the model would be optimistic. Enter it. Both volumes scale linearly with weight, which is the pharmacokinetic weight under the default [fat-free-mass scaling](help:models/fat-free-mass) and total weight with the switch off; the same weight enters Cockcroft-Gault. The intercompartmental clearance, fixed in the source, scales with the fat-free-mass ratio to the 0.75 power when the switch is on and is fixed when it is off.

### Effect site

None; only the plasma concentration is plotted. *Time until threshold* is therefore timed on the plasma curve. That curve is **total** vancomycin (bound plus free), but it is free drug that acts on the organism, so the threshold is the total concentration at which the **free** concentration equals the MIC. The line shows how long, with no further dose, until free vancomycin falls below the MIC. Vancomycin's free fraction is taken as **0.70**: Dejaco and colleagues (*Antimicrob Agents Chemother* 2026;70:e01593-25) measured 0.72 in 706 samples from 228 adult in-patients at body temperature and pH 7.4, unaffected by concentration or albumin, and recommend 0.70; Stove and colleagues (*Ther Drug Monit* 2015;37:180-187) found 0.725 by equilibrium dialysis. The label's "about 55 per cent bound" comes from ultrafiltration at room temperature or high centrifugal force, which overstates binding. With an MIC of 1 mg/L the threshold is therefore 1.4 mcg/mL total. See *Time until threshold: free drug at the MIC* above.

### Typical concentrations

The shaded band (2.9 to 11 mcg/mL total) marks free vancomycin at the measured **MBC** of *Staphylococcus aureus*, from its MBC50 of 2 mg/L to its MBC90 of 8 mg/L (Honda and colleagues, *BMC Infect Dis* 2011;11:335), divided by the free fraction. Enterococci, most viridans streptococci and tolerant staphylococci are not killed by vancomycin at any level the curve reaches and are not in the band. The old trough of 10 to 20 mcg/mL total starts near the top of the band. The 2020 consensus guideline (Rybak and colleagues, *Am J Health Syst Pharm* 2020;77:835-864) recommends instead a total AUC over 24 hours of 400 to 600 mg·h/L at an MIC of 1 mg/L for serious MRSA infection; at steady state that AUC is the daily dose divided by clearance, and can be read off the curve.

### Where to be careful

The clearance equation is linear in creatinine clearance and reaches zero at 1.06 mL/min; it is not a model of anuria or dialysis, and the code refuses rather than clamps if driven there. Loading doses by weight, nephrotoxicity and the vancomycin infusion reaction are outside the model.

### Pregnancy

With **Pregnant** ticked, the creatinine clearance Thomson's clearance term reads is multiplied by 1.5, the rise in glomerular filtration the review the library follows (Ansari, Carvalho, Shafer and Flood, *Anesth Analg* 2016;122:786-804) gives for pregnancy; the term is linear with a small negative intercept, so the clearance rises by slightly more than 1.5. The review reports a larger volume of distribution and a higher clearance in pregnancy, with the change in half-life unknown; it prints no factor for vancomycin itself. The larger volume is not applied, so the simulated peak in a parturient is too high. See [Pregnancy](help:models/pregnancy).
