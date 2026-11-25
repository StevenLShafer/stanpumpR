### The model

Tang and colleagues' population analysis (*J Clin Pharmacol* 2023) of 103 patients with acute myocardial infarction in the TIMI 10B trial, given 30, 40 or 50 mg as one intravenous bolus and sampled to 6 hours (785 concentrations). Two compartments, with every volume and clearance scaled to weight (exponents fixed at 1 for the volumes and 0.75 for the clearances, about 70 kg):

- clearance (91.3 + 0.322 × (CrCl − 82.6)) mL/min × (weight/70)^0.75, where CrCl is the Cockcroft-Gault creatinine clearance normalised to 70 kg, in which the weight cancels;
- central volume 3.61 L and peripheral volume 0.914 L × weight/70;
- intercompartmental clearance 10.0 mL/min × (weight/70)^0.75.

For the paper's median patient (81.8 kg, CrCl 82.6 mL/min/70 kg) clearance is 103 mL/min, 29% of it through the renal term, the central volume 4.2 L and the terminal half-life 77 minutes. The dose divided by that clearance reproduces the paper's simulated exposures: 79.7, 398 and 487 mcg·min/mL after 0.1 mg/kg, 0.5 mg/kg and 50 mg. The label gives clearance of 99 to 119 mL/min, which the model matches, a central volume of 4.22 to 5.43 L, and a steady-state volume of 6.12 to 8.01 L and terminal half-life of 90 to 130 minutes, both larger than the model's (5.3 L and 77 minutes at 81.8 kg). Clearance is the parameter to compare across studies; a terminal phase depends on how long and how sensitively a study sampled, and this one sampled for 6 hours.

The assay (ELISA) also reads the patient's own tissue plasminogen activator, which the paper carried as a baseline signal of 18.6 ng/mL. The curve is the tenecteplase given, without that baseline.

### Covariates

- **Weight**, on every parameter, as fitted. With the fat-free-mass switch on, the weight in these terms is the pharmacokinetic weight.
- **Serum creatinine**, from the Patient Profile, through the renal term. Blank means an assumed normal creatinine for age and sex (0.9 mg/dL in men and 0.8 in women from 18 years); a child's creatinine is put on the adult scale. Even with no renal function, clearance is 64.7 mL/min at 70 kg. The label says liver metabolism is the major route; this later analysis found the renal share, and the model applies it.
- **Pregnancy**: with the creatinine blank, the creatinine clearance of the renal term is raised 1.5-fold, the rise in filtration of pregnancy. No tenecteplase concentrations have been measured in pregnancy.

### Product and doses

TNKase, `mg` (and `mg/kg`) as one intravenous bolus over 5 seconds. The label doses by weight band: for ST-elevation myocardial infarction 30 to 50 mg, and for acute ischaemic stroke 15 to 25 mg, about 0.25 mg/kg, with a maximum of 25 mg. The model was fitted in myocardial infarction. In the paper's external check in acute ischaemic stroke (study N1811s, one sample an hour after the dose), the model over-predicted the observed mean concentrations by 39% on average, and the authors found that neither renal function nor weight explained the difference, so the curve may run high for a patient with stroke.

### What is not plotted

Concentration only, in mcg/mL. Tenecteplase converts plasminogen to plasmin on fibrin; its effect outlasts its concentration, and no fibrinogen, plasminogen or bleeding endpoint is drawn. No therapeutic range is established.
