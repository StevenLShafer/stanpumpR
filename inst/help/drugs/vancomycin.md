### The model

Vancomycin's parameters are from Thomson and colleagues (*J Antimicrob Chemother* 2009;63:1050-1057), a two-compartment model of total serum vancomycin fitted in 398 hospitalised patients and evaluated in 100 more: clearance 2.99 L/h at a creatinine clearance of 66 mL/min, rising by 1.54 per cent per mL/min; central and peripheral volumes 0.675 and 0.732 L/kg; intercompartmental clearance 2.28 L/h. The source table labels that last figure in units of a rate constant but identifies it as a clearance, and the code reads it as 2.28 L/h.

### Covariates

Clearance follows Cockcroft-Gault creatinine clearance, estimated from age, sex and body size at an **assumed normal creatinine** because stanpumpR collects none. Renal decline with age is represented; **renal impairment is not**, and that is the vancomycin question that matters most: the model is optimistic about accumulation in a patient with a raised creatinine. Both volumes scale linearly with weight, which is the pharmacokinetic weight under the default [fat-free-mass scaling](help:models/fat-free-mass) and total weight with the switch off.

### Effect site

None; only the plasma concentration is plotted.

### Typical concentrations

The shaded band is the traditional trough range, 10 to 20 mcg/mL. The 2020 consensus guideline (Rybak and colleagues, *Am J Health Syst Pharm* 2020;77:835-864) recommends instead a total AUC over 24 hours of 400 to 600 mg·h/L at an MIC of 1 mg/L for serious MRSA infection; at steady state that AUC is the daily dose divided by clearance, and can be read off the curve.

### Where to be careful

The clearance equation is linear in creatinine clearance and reaches zero at 1.06 mL/min; it is not a model of anuria or dialysis, and the code refuses rather than clamps if driven there. Loading doses by weight, nephrotoxicity and the vancomycin infusion reaction are outside the model.
