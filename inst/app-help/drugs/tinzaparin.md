### Research model — not for clinical dosing

This is a research and teaching model of anti-Xa activity. It is **not** validated clinical decision support: a simulated anti-Xa activity is not a bleeding or clotting risk, a dose recommendation, or a safe time for surgery or a neuraxial block.

### The model

Tinzaparin kinetics are from Barrett and colleagues (*Int J Clin Pharmacol Ther* 2001;39:431-446, PMID 11680668), a two-compartment model with first-order subcutaneous absorption and a constant endogenous anti-Xa activity, fitted to 2,631 anti-Xa observations from 425 patients in two Phase III trials. The library uses the final model from the DVT-**treatment** data set (175 anti-Xa IU/kg once daily), which the authors extended across dose and indication. Doses are entered in anti-Xa **international units (IU)**, given **subcutaneously**, and the curve is anti-Xa activity in IU/mL.

Every disposition parameter is **per kilogram**: apparent clearance 0.0176 L/h/kg, central volume 0.0981 L/kg, peripheral volume 0.864 L/kg and inter-compartmental clearance 0.007 L/h/kg; the absorption rate constant is 0.206 /h. The parameters are apparent (CL/F, V/F), so tinzaparin's absorbed fraction (about 87% by anti-Xa activity) is already inside them and is not applied again. The model's endogenous baseline of 0.098 IU/mL is **not** added: the plotted curve is the drug-induced anti-Xa activity, as for enoxaparin and fondaparinux.

### Covariates

Apparent clearance carries two covariates, as the paper fitted them:

- **Plasma creatinine** — clearance falls as a power of the *raw* serum creatinine in mg/dL (not a creatinine clearance, unlike enoxaparin's and fondaparinux's MDRD term), centred on the population median of 1.0 mg/dL. Clearance is about 77% of typical at a creatinine of 2.2 mg/dL (the 95th percentile). The value is read from the serum creatinine in the [Patient Profile](help:patient-profile) on the adult-equivalent scale, or an assumed normal value for age and sex when blank.
- **Obesity** — clearance falls with percent above ideal body weight, centred on the median 14%, reaching about 81% of typical at the 95th-percentile PIBW of 50%.

The authors judged both effects "probably not clinically significant" over the range studied, and weight-based dosing (IU/kg) handles most of the variability; the covariates are kept because they do shift clearance at the extremes. Weight is evaluated at the pharmacokinetic weight under the [fat-free-mass switch](help:models/fat-free-mass).

### The patient this represents

The fit spans a broad adult range (age 21-92, weight 37-151 kg, plasma creatinine 0.29-5.5 mg/dL). The predicted steady-state anti-Xa peak on 175 IU/kg once daily is about 0.68 IU/mL in a patient of median renal function, rising toward 0.8 IU/mL as clearance falls with renal impairment — matching the paper's Figure 5. Anti-Xa activity is plotted as the model's output, not a clinical threshold; routine anti-Xa monitoring is not advised in typical adults.

**Innohep** (tinzaparin) was withdrawn from the US market in 2011 and has no current US prescribing information; it remains marketed in Europe and Canada.
