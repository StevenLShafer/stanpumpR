### The model

This is the extended-release **tablet** (Osmolex ER), from the FDA reviewers' own population model in its clinical pharmacology review (NDA 209410): 24 adults with normal, moderately impaired and severely impaired renal function given one 160 mg tablet. One compartment, apparent volume 437 L, with an elimination rate constant proportional to the MDRD eGFR (0.02 per hour at 42 mL/min/1.73 m²), from the Patient Profile's **Serum creatinine** (an assumed normal creatinine for age and sex when blank). Each dose enters two ways at once: 79.9% is absorbed first order at 0.344 per hour, and 20.1% enters at a constant rate over 8.8 hours. The review prints those numbers without drawing the structure, and this reading reproduces the study's normal and severe groups; it under-predicts the moderate group. The half-life is about 16 hours at an eGFR of 90 and 73 hours at 20.

### Doses as labelled

Osmolex ER tablets are labelled in mg of amantadine base (129 mg is 160 mg of the hydrochloride); the model was fitted to the hydrochloride and converts. Immediate-release amantadine is labelled as the hydrochloride, and Gocovri, the bedtime capsule, as the base; neither is modelled, and their absorption differs, so do not enter them here.

### The band

300 to 600 ng/mL at trough, the AGNP consensus guideline's reference range. Confusion, hallucinations and falls rise with the concentration, especially in the elderly and in renal impairment, where amantadine accumulates over weeks.

### What is not in the model

No weight or sex term (24 subjects); [the scaling switch](help:models/fat-free-mass) scales the volume and clearance to fat-free mass. Quinidine and quinine lower amantadine's renal clearance about 30%, and acid urine speeds it; neither is applied.
