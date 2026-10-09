### The model

Pregabalin's parameters are from Chan and colleagues (*Clin Pharmacol Ther* 2021;110:132-140), who pooled ten Pfizer studies of 724 adults and 255 children, healthy volunteers and patients with focal seizures, with creatinine clearances from about 10 to 260 mL/min: one compartment, first-order absorption (10/h) after a lag of 0.32 h, clearance on creatinine clearance, and clearance and volume on weight and sex. Pregabalin is not metabolised and not bound to plasma proteins; the kidney clears it unchanged.

The covariate equations are in the paper's supplement, which could not be read; the model here is read from its parameter table. Chan's absorption rate is described as a multiple of the elimination rate but tabulated as 10/h. Read as a multiple, the plasma would peak about 2.4 hours after a fasted dose; read as published, it peaks at 0.76 hours, as the fasted volunteer studies do (0.7 to 1.3 hours; Bockbrader and colleagues, *J Clin Pharmacol* 2010;50:941-950). The published reading is used.

The typical patient's half-life is 5.6 hours. After 150 mg the model gives a peak of 3.6 mcg/mL and an AUC of 30 mcg·h/mL (observed 3.9 to 4.7 mcg/mL and 24 to 30 mcg·h/mL), and 7.8 mcg/mL two hours after 300 mg in a 65 kg woman, against 7.67 &plusmn; 3.00 mcg/mL measured at incision in women given 300 mg two hours before induction (Müller and colleagues, *Anesth Analg* 2026;143:373-382). Absorption is linear: unlike [gabapentin](help:drugs/gabapentin), twice the dose gives twice the concentration.

### Oral only

There is no intravenous pregabalin product, and every model is apparent: clearance and volume are divided by a bioavailability of 90 per cent or more, which is independent of dose. The model describes immediate-release capsules taken fasting. Food lowers the peak by a quarter to a third and delays it to about 3 hours without changing the amount absorbed; that is not represented. Neither is the extended-release tablet.

### Covariates

Clearance is proportional to Cockcroft-Gault creatinine clearance, normalised to 1.73 m&sup2; of body surface area, up to 96.4 mL/min/1.73 m&sup2;, and constant above it, so a creatinine clearance above normal does not speed elimination. The creatinine is the **Serum creatinine** in the Patient Profile; left blank, it is **assumed normal** for the patient's sex, which captures the fall in renal function with age but not renal impairment. Pregabalin accumulates in renal impairment, and the recommended dose halves for each halving of creatinine clearance below 60 mL/min (Randinitis and colleagues, *J Clin Pharmacol* 2003;43:277-283). Clearance and volume also scale with weight, and are 8 and 17 per cent lower in women. With the [fat-free-mass switch](help:models/fat-free-mass) on, the weight terms, Cockcroft-Gault and the body surface area all use the pharmacokinetic weight; with it off, total body weight, as published. Haemodialysis, which removes pregabalin efficiently, is not modelled.

### Effect site

The effect site peaks 4.7 hours after an oral dose. The delay is from van Esdonk and colleagues (*CPT Pharmacometrics Syst Pharmacol* 2018;7:573-580), who gave 300 mg to 16 volunteers and found that the cold pressor pain tolerance threshold followed the plasma concentration through a turnover compartment with a rate of 0.39/h, a half-time of 1.8 hours. With a linear drug effect on the production of the response, that is exactly an effect site with k<sub>e0</sub> 0.39/h; whether it is here depends on equations in the paper's supplement, and the delay is the same order either way. It is the only human estimate of the delay to pregabalin's analgesic effect. It is not the entry into cerebrospinal fluid, which is slower and partial: a tenth to a fifth of the plasma concentration, peaking about 8 hours after a dose (Buvanendran and colleagues, *Reg Anesth Pain Med* 2010;35:535-538). Pregabalin is not an opioid and is not on the MEAC panel.

### Typical concentrations

The shaded band, 1.3 to 5.4 mcg/mL, is the median steady-state average concentration in adults taking 150 to 600 mg a day, the labelled range for neuropathic pain and focal seizures (Chan and colleagues, 2021). It is for orientation: it describes chronic treatment, not a perioperative target, and a single dose of 150 or 300 mg peaks above it.

### Where to be careful

A meta-analysis of 281 trials of perioperative gabapentin and pregabalin (Verret and colleagues, *Anesthesiology* 2020;133:265-279) found reductions in acute postoperative pain below the minimal clinically important difference, no effect on chronic postsurgical pain, and more dizziness and visual disturbance. Pregabalin adds to the sedation and respiratory depression of opioids, particularly in the elderly and in patients with lung disease (US Food and Drug Administration safety communication, December 2019). A concentration curve says nothing about whether a perioperative dose helps.
