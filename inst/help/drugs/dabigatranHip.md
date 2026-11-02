### Research model — not for clinical dosing

This is a research and teaching model of plasma concentration. It is **not** validated clinical decision support: a simulated concentration is not a dilute thrombin time, an ecarin clotting time, a bleeding or clotting risk, or a safe time for surgery or a neuraxial block.

### A second dabigatran model, for the post-operative patient

The library already has a dabigatran model from the RE-LY atrial-fibrillation trial ([dabigatran](help:drugs/dabigatran), Liesenfeld 2011). **This** model is the separate post-operative one of Trocóniz and colleagues (*J Clin Pharmacol* 2007;47:371-382), the first population analysis of dabigatran, fitted to 4604 concentrations from 287 patients given 12.5–300 mg once or twice daily for up to 10 days after **primary elective total hip replacement**. Its defining feature is that the pharmacokinetics **change over the first days after surgery**.

### The model

An **apparent** (/F) two-compartment model with first-order oral input and a 0.4-hour lag, fitted to total BIBR 953 ZW (free dabigatran plus its active acylglucuronides). Doses are entered as milligrams of the prodrug **dabigatran etexilate**, as in the trial. The central volume is 30.8 L, the peripheral volume 136 L, and the inter-compartmental clearance 13.6 L/h.

Absorption and apparent clearance are **lower on the day of surgery and the next day** (what the paper labels the "< 24 h" phase) than on days 2 to 10 (the "> 24 h" phase), which the authors attribute to the reduced gastrointestinal motility and raised gastric pH that follow surgery:

- **Days 0–1:** apparent clearance 70.9 L/h, absorption rate constant 0.022 /h (a very slow, flip-flop absorption whose 31.5-hour half-life limits the decline).
- **Days 2–10:** apparent clearance 106.2 L/h at the reference patient, absorption rate constant about 0.093 /h — roughly four times faster.

### Which phase a dose uses

The paper's breakpoint is defined relative to **surgery**, which the simulator has no input for. This model takes the first simulated time as the time of surgery (the trial gave the first dose 4–8 hours later) and **splits the doses at 24 hours**: a dose given before 24 hours is absorbed and cleared at the day-0–1 rates for its whole life, a dose given at or after 24 hours at the day-2–10 rates. The source's NONMEM convention — whether absorption still under way at 24 hours instead speeds up — is not published, so this dose-time reading was chosen; it keeps the two phases independent and avoids an artificial jump in the plasma level at 24 hours. The time from surgery to the first dose, and its variability, are not represented.

### Covariates

- **Serum gastrin** raises apparent clearance in both phases. There is no gastrin field in the [Patient Profile](help:patient-profile), so it is fixed at the study's mean (gastrin is right-skewed, so the mean is not a median), and the gastrin effect is not shown; the clearances above are at that mean.
- **Creatinine clearance** (Cockcroft–Gault, from the serum creatinine) raises the day-2-to-10 clearance; **serum creatinine** and **age** both lower the day-2-to-10 absorption rate. The serum creatinine comes from the Patient Profile, or an assumed normal value when blank. These were fitted over serum creatinine 0.66–1.81 mg/dL, creatinine clearance 29–161 mL/min and age 35–88 years; outside that range the model extrapolates and the absorption factors are held positive.
- **Body size**: the source found no effect of weight, sex, smoking or alcohol, so every apparent parameter is size-free and scales with the library's [fat-free-mass switch](help:models/fat-free-mass); weight also enters through the Cockcroft–Gault creatinine clearance.

### Where to be careful

This is a single-dose-to-10-day hip-replacement model in otherwise healthy surgical patients; it is not the chronic atrial-fibrillation model, and it does not represent the time from surgery to the first dose, co-medication (H₂-blockers, proton-pump inhibitors and prokinetics were exclusion or covariate factors), or severe renal impairment (only nine patients had a creatinine clearance below 40 mL/min). A simulated concentration is not an anticoagulant assay or a procedure-safe time.
