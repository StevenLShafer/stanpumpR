### The model

Gabapentin's parameters are from Tran and colleagues (*J Pharmacokinet Pharmacodyn* 2017;44:567-579), who fitted 173 healthy young men given single oral doses of 300, 400 or 800 mg, with rich sampling to 24 hours: one compartment, first-order absorption (0.86/h) after a lag of 0.31 h, clearance on Cockcroft-Gault creatinine clearance, and a bioavailability that falls with the size of the dose. Gabapentin is not metabolised and is less than 3 per cent bound; the kidney clears it unchanged.

Three things differ from the published model, and each is recorded in the drug file:

- **Clearance is proportional to creatinine clearance.** Tran estimated an exponent of 0.33 over creatinine clearances of 66 to 170 mL/min in young men with normal kidneys, where Cockcroft-Gault mostly reflects muscle mass. Gabapentin's clearance is proportional to creatinine clearance across renal impairment (Blum and colleagues, *Clin Pharmacol Ther* 1994;56:154-159; the Neurontin label), and Tran's exponent would predict a 9-hour half-life at a creatinine clearance of 20 mL/min where the label reports about 52 hours below 30. The exponent is 1, anchored at Tran's median of 106.3 mL/min.
- **The disposition is re-anchored to the intravenous volume.** Without intravenous data, Tran's model assumes a very small dose is absorbed completely. Tran's subjects had lower exposure than Western ones, and the published model predicts about 30 per cent below Western single-dose studies. The volume is therefore set to the 58 L measured after intravenous gabapentin (Neurontin label), and clearance is scaled by the same 58/81, which keeps Tran's half-life, absorption and dose-bioavailability curve. The model then gives AUCs of 26, 31 and 40 mcg·h/mL after 300, 400 and 600 mg (observed 25 to 28, 34 and 44) and a peak of 3.9 mcg/mL after 600 mg (observed 3.9 to 4.2), and a clearance close to gabapentin's measured renal clearance.
- **Saturable absorption is applied dose by dose.** Each oral dose is scaled by its own fraction absorbed, 1 &minus; 0.906 &times; D / (571 + D): 0.69 at 300 mg, 0.54 at 600 mg, 0.39 at 1200 mg. So 1200 mg gives about 2.2 times the exposure of 300 mg, not 4 times. Two doses entered as separate rows at the same time are each scaled by their own size, so enter a single dose as one row. See [Oral, intramuscular and intranasal doses](help:models/absorption).

### Oral only

There is no intravenous gabapentin product, and the absorption model describes immediate-release capsules and tablets. The gastroretentive tablet (Gralise) and the prodrug gabapentin enacarbil (Horizant) are different inputs and are not represented.

### Covariates

Clearance follows Cockcroft-Gault creatinine clearance, from the **Serum creatinine** in the Patient Profile. Left blank, the creatinine is **assumed normal** for the patient's sex: the fall in renal function with age is then represented but renal impairment is not. Gabapentin accumulates in renal impairment, and this is the drug in the library for which entering the creatinine matters most. Tran found no effect of weight on the volume, so the volume follows the [fat-free-mass scaling](help:models/fat-free-mass) with the switch on and is 58 L for everyone with it off; Cockcroft-Gault uses the pharmacokinetic weight with the switch on and total weight with it off. Dialysis is not modelled.

### Effect site

None; only the plasma concentration is plotted. No equilibration delay between plasma and the site of gabapentin's effect has been estimated in humans. Zhou and colleagues (*Front Pharmacol* 2026) linked pain scores to an effect compartment but fixed its rate rather than estimating it, and cerebrospinal fluid concentrations rise slowly and reach only about a tenth of plasma at 6 hours (Ben-Menachem and colleagues, *Epilepsy Res* 1992;11:45-49), which an effect site that equilibrates with plasma does not describe. Gabapentin is not an opioid and is not on the MEAC panel.

### Typical concentrations

The shaded band, 4.1 to 9.4 mcg/mL, is the steady-state average concentration corresponding to the 1200 to 3600 mg/day doses effective in postherpetic neuralgia, from the exposure-response analysis of 690 patients in the Neurontin new drug application (an AUC over 8 hours of 32.8 to 75.1 mg·h/L; Healy and colleagues, *Pharmacol Res Perspect* 2023;11:e01138). It is for orientation: it describes chronic neuropathic pain, not a perioperative target, and a single preoperative dose of 600 mg peaks below it.

### Where to be careful

In the GAP trial (Baos and colleagues, *Anesthesiology* 2025;143:851-861), 1196 patients having cardiac, thoracic or abdominal surgery received 600 mg before surgery and 300 mg twice daily for two days, or placebo. Gabapentin did not shorten hospital stay or produce a clinically important reduction in acute pain or opioid use, and more patients who received it reported pain at four months. A concentration curve says nothing about whether a perioperative dose helps. Food (a high-protein meal raises the peak by about a third), genetic variation in absorption (ABCB1) and renal secretion (OCTN1), and saturation shared between doses taken close together are not represented.
