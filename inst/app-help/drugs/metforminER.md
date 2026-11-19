### The model

Duong and colleagues' population analysis of metformin (*Clin Pharmacokinet* 2013) had 305 subjects in three studies:

- 120 patients with type 2 diabetes, with creatinine clearances of 15 to 127 mL/min;
- 16 healthy volunteers given immediate- and extended-release tablets over 35 days;
- 169 healthy volunteers given single immediate-release doses.

This drug is the **extended-release** tablet (Glumetza, Glucophage XR, Fortamet and their generics). The immediate-release tablet is [metformin](help:drugs/metformin), from a different study.

- **Disposition:** two compartments. Apparent clearance 1212 mL/min at a creatinine clearance of 100 mL/min, central volume 149 L at 70 kg, intercompartmental clearance 203 mL/min, peripheral volume 182 L. The last two have no size covariate in the paper: with **Adjust weight to fat-free mass** ticked they take the library's fat-free-mass scaling, unticked they are the published values.
- **The tablet:** drug enters the circulation at a constant rate for 9.55 hours, with no lag. The relative bioavailability falls with the size of the dose: 1.26 at 500 mg, 1.16 at 1000 mg, 1.06 at 1500 mg and 1 at 2000 mg. It is interpolated between those sizes and held at the end values outside them.

### Covariates

- **Renal function:** clearance in proportion to creatinine clearance, with no non-renal part. The creatinine clearance is Cockcroft-Gault calculated on the Janmahasatian lean body weight, as in the paper. It uses the entered serum creatinine or an [assumed normal](help:models/covariates) one. The data reached 15 mL/min. The patient with end-stage kidney disease taking 2000 mg a day had the highest average concentration, 7.7 mg/L; the model gives 7.6.
- **Weight:** the central volume in proportion to body weight. It uses the pharmacokinetic weight with **Adjust weight to fat-free mass** ticked and total weight with it unticked.

Age and the 57 transporter variants tested did not change the clearance.

### Doses

`mg PO` and `mg PO qd`, one extended-release dose a day, as labelled. The immediate-release tablet's own absorption is not this drug's.

### What is not plotted

Concentration only, in mcg/mL. Glucose, HbA1c and lactate are not drawn. See [Antidiabetic models and their limits](help:models/antidiabetics).

The pregnancy factors are metformin's own, measured with the immediate-release tablet. The clearance factor is applied only when **Serum creatinine** is blank: a measured creatinine in pregnancy already raises the creatinine clearance, and the two together would count the rise twice. The volume factors apply either way.
