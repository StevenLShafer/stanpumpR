### The model

Colistimethate (colistin methanesulfonate, CMS) is the inactive prodrug of [colistin](help:drugs/colistin). The model is Garonzik and colleagues' population model in critically ill adults (*Antimicrob Agents Chemother* 2011;55:3284-3294, doi 10.1128/AAC.01733-10). It was fitted to 105 patients in Thailand and the United States, median age 71 (19-92) and weight 59 kg (30-106), with a creatinine clearance of 0 to 169 mL/min/1.73 m². Each was on a regimen their physician chose, from 75 to 410 mg of colistin base activity a day.

CMS has two compartments:

- **Central volume:** 11.9 L × (weight/60).
- **Peripheral volume:** 18.7 L, with an intercompartmental clearance of 7.98 L/h.
- **Clearance:** 0.0613 × CrCL + 1.90 L/h. The renal part rises with creatinine clearance; the nonrenal 1.90 L/h is what forms colistin.

As renal function falls, more of each dose is converted to colistin rather than excreted unchanged, which is why the colistin curve depends so strongly on creatinine clearance. Garonzik's model is the interim analysis of the study that Nation and colleagues completed in 2017 (*Clin Infect Dis* 64:565, 214 patients). The final model's parameters are in that paper's supplement, which has not been read.

### Creatinine clearance

The source used the Jelliffe equation, in mL/min/1.73 m². Its dosing table says Cockcroft-Gault may be used instead, normalised to 1.73 m² of body surface area, and that is what is done here. The serum creatinine comes from the Patient Profile; a blank field means an assumed normal creatinine for age and sex. The creatinine clearance is held within the studied 0-169 mL/min/1.73 m².

Dialysis and continuous renal replacement are not modelled: the source estimated their clearances, but the Patient Profile has no field for them, so both are held at zero.

### Dose

Doses are milligrams of **colistin base activity (CBA)**, as the US product is labelled ("colistimethate sodium equivalent to 150 mg colistin base activity per vial") and as the source prescribed it. One million IU is about 30-33 mg CBA.

The source does not say in what unit its model's dose entered. Its own numbers settle it: its dosing algorithm needs 30 mg CBA a day for each 1 mg/L of colistin when the creatinine clearance is zero, where the model needs 52.6 of its own units. The source's conversion, 150 mg CBA ≈ 400 mg of CMS sodium, times the colistin each CMS molecule releases (1169.5 of 1749.8 g/mol, from the label's formula), gives the same ratio, 1.78. So every volume and clearance is divided by 1.78, and a dose in mg CBA gives the concentrations the model gives. The final model's papers use 1 million IU ≈ 33 mg CBA, which would make the factor 1.60.

Intravenous only. The label also gives it intramuscularly, which the source did not study. The model was fitted in adults and is offered at every age.

### Checks

Garonzik reports the median terminal half-lives of the individual fits, so these are not independent of the model:

| Creatinine clearance (mL/min/1.73 m²) | CMS, Garonzik | CMS, this model | Colistin, Garonzik | Colistin, this model |
|---|---|---|---|---|
| Below 10 (model at 5) | 11 h | 10.7 h | 13 h | 13.8 h |
| 11-69 (model at 40) | 5.6 h | 6.0 h | 13 h | 11.3 h |
| Above 70 (model at 100) | 4.6 h | 3.8 h | 9.1 h | 8.5 h |

Nation 2017's look-up table, from the final model on 214 patients (110 not in Garonzik's fit), gives the doses its intercept-adjusted regression expects to bring about 80% of patients to an average colistin concentration of at least 2 mg/L, which puts the median near 2.9 mg/L. At those doses this model gives 4.4, 2.5, 2.0 and 1.7 mg/L at a creatinine clearance of 0, 35, 75 and 100. The interim model's renal effect is steeper than the final model's, which used Cockcroft-Gault in mL/min.

The label's "half-life of 2–3 hours" was measured with a microbiological assay that cannot tell CMS from colistin, so it is not a check.

### What is plotted

Plasma CMS in mg/L. CMS is inactive, so it has no line or band.

### Pregnancy

Pregnancy was an exclusion criterion in the source, and no pregnancy pharmacokinetics were found.
