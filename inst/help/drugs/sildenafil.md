### The model

Sildenafil's parameters come from Pfizer's population model as printed and reproduced in the FDA's Clinical Pharmacology Review of Revatio injection (NDA 22-473, 2009). The model was fitted to 45 healthy men who received sildenafil intravenously (20 to 80 mg) and orally, 1479 concentrations in all. It has two compartments with first-order absorption after a lag:

- clearance 29.5 L/h;
- central volume 73.2 L;
- intercompartmental clearance 5.32 L/h;
- peripheral volume 33.3 L;
- absorption rate constant 25.6 /h, after a lag of 0.485 h.

Oral bioavailability depends on the dose, as 0.355 × (dose/20 mg)<sup>0.289</sup>: 0.355 at 20 mg, 0.46 at 50 mg (the label's measured value is 41%), and 0.57 at 100 mg. The intravenous dose is fully bioavailable.

### Checks

The label gives a steady-state volume of 105 L (this model: 106.5 L) and a terminal half-life of "about 4 hours" (this model: 5.5 h).

The Viagra label puts the peak after 100 mg orally at "approximately 440 ng/mL". This model gives 716 ng/mL at 39 minutes, the over-prediction the review warned of: the model "cannot predict the AUC after PO administration for doses over 50 mg".

In REVAKI-1 (Ring and colleagues, *Br J Clin Pharmacol* 2017;83:709-720, doi 10.1111/bcp.13162), patients of about 70 years were given 10 mg intravenously at cardiac surgery. Their geometric mean clearance was 17.6 L/h and their AUC 568 ng·h/mL. This model gives 29.5 L/h and 339 ng·h/mL. Expect higher concentrations than plotted in older, sicker or perioperative patients.

### Pulmonary hypertension

The Patient Profile's **Pulmonary hypertension** field switches between two populations. It is shown while sildenafil is in the dose table.

- **No** (the default): the healthy volunteers' model above, which suits a man taking Viagra for erectile dysfunction.
- **Yes**: clearance is 30% lower, 20.7 L/h, the adjustment the FDA review itself validated. The review found that the healthy model under-predicts patients with pulmonary arterial hypertension (PAH), who often have severe heart disease. It concluded that "the population model that accounts for 30% difference in clearance between healthy volunteer and PAH patients predicts exposures in PAH patients". Its own estimates of the patients' clearance were 24.7 L/h intravenously (19% lower) and 22.1 L/h orally (33% lower).

The volumes, absorption and bioavailability stay the healthy volunteers'; the review measured none of them in patients. On 20 mg three times a day, the lower clearance more than doubles the trough (6.4 to 14.5 ng/mL) but raises the peak only about 10% (96 to 105 ng/mL). In study A1481140, patients with PAH on that dose had a mean peak of 81.4 ng/mL from sparse samples, and 439.5 ng/mL on 80 mg, where this model gives 629. The healthy model's very fast absorption puts its peaks high, which the review warned of above 50 mg. The Revatio label reports steady-state concentrations 20 to 50% higher in pulmonary hypertension than in volunteers.

Not modelled in either population:

- Healthy volunteers aged 65 or more had concentrations about 84% higher than younger ones.
- Severe renal impairment (creatinine clearance below 30 mL/min) and cirrhosis roughly double exposure.

### Metabolite

N-desmethylsildenafil has about half the parent's potency at PDE5. After an oral dose its concentration is about 40% of the parent's; after an intravenous dose it is 14%, because most of it is formed on the first pass. It is not modelled, as no adult model of parent and metabolite together was found.

### Dose and band

Revatio and Viagra are sildenafil citrate, labelled as the equivalent mass of sildenafil ("sildenafil citrate equivalent to 20 mg of sildenafil"), so doses are milligrams of sildenafil. Revatio is 20 mg three times a day orally or 10 mg three times a day intravenously. No therapeutic range has been established.
