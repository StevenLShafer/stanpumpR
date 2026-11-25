### The model

ELOCTATE, recombinant factor VIII fused to the Fc domain of immunoglobulin G1, an extended-half-life concentrate for haemophilia A. Its model is Allard and colleagues' (*Pharmaceutics* 2020), the same one as the library's [factor VIII](help:drugs/factorVIII), fitted to 258 patients with haemophilia A aged 3 to 77 years, of whom 136 were given Elocta, as it is named outside the US. The only difference the source found between the products was clearance: ELOCTATE's is 0.674 times the standard products' (exp(−0.394)), so its half-life is about 1.5 times as long.

- clearance 204 mL/h × (age/30)^−0.214 × (weight/64)^0.75 × 0.674;
- central volume 2,640 mL × (weight/64)^0.827;
- intercompartmental clearance 135 mL/h, peripheral volume 339 mL.

At 64 kg and 30 years the clearance is 2.15 mL/kg/h and the terminal half-life 15.2 hours; the label's adults: 2.06 mL/h/kg and 19.7 hours. The clearance agrees; the half-life is shorter, as it is for the standard products against ADVATE's label.

### What is plotted

Factor VIII activity in **IU/dL** (% of normal): the Patient Profile's **Factor VIII baseline** plus the activity infused. Left blank, the baseline is the source population's, severe haemophilia A, 0.5 IU/dL. The shaded band is the label's target for major surgery, 80 to 120 IU/dL before and after the operation (ELOCTATE prescribing information, Table 2); for minor surgery the label's target is 50 to 80.

### Covariates, doses and what is not modelled

As for [factor VIII](help:drugs/factorVIII): weight on clearance and the central volume as fitted, age on clearance held at 3 years, fat-free-mass scaling of the rest; `IU/kg` or `IU` as a bolus and `IU/kg/hr` or `IU/hr` as an infusion; the patient's own factor VIII held constant, no inhibitor. No pharmacokinetics in pregnancy have been reported, and nothing is applied.
