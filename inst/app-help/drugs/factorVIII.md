### The model

Allard and colleagues' real-life population analysis (*Pharmaceutics* 2020) of 258 patients with haemophilia A, none with an inhibitor, from 13 French haemophilia centres: 87 children and adolescents and 171 adults, aged 3 to 77 years (median 30) and weighing 15 to 130 kg (median 64). Eight concentrates were pooled; the brand and the structure of the molecule (full-length, B-domain deleted, plasma-derived) did not change the kinetics, and only the extended-half-life Elocta (Eloctate) cleared more slowly. This drug is the seven standard-half-life concentrates (Advate, Kogenate, Kovaltry, Novoeight, Refacto, Afstyla, Factane). Two compartments:

- clearance 204 mL/h × (age/30)^−0.214 × (weight/64)^0.75;
- central volume 2,640 mL × (weight/64)^0.827;
- intercompartmental clearance 135 mL/h, peripheral volume 339 mL.

At the medians (64 kg, 30 years) the clearance is 3.2 mL/kg/h, the volume at steady state 47 mL/kg, and the half-lives 1.5 and 10.4 hours. The ADVATE label's adults: clearance 3 ± 1 mL/kg/h, volume 0.4 dL/kg, half-life 12.0 ± 4.2 hours. A bolus raises the activity by 2.4 IU/dL per IU/kg; the labels' dosing rule is 2.

### What is plotted

Factor VIII activity in **IU/dL**, which is the same number as % of normal: the patient's own activity plus the activity infused. The patient's own is the Patient Profile's **Factor VIII baseline**, shown while factor VIII is in the dose table. Left blank, it is the source population's: severe haemophilia A, below 1 IU/dL, taken as 0.5 IU/dL. A carrier, a patient with mild or moderate haemophilia, or a patient with von Willebrand disease has a higher baseline, which should be entered, or the curve is drawn too low.

The shaded band is the label's target for major surgery, 80 to 120 IU/dL before and after the operation (ADVATE prescribing information, Table 2); for minor surgery the label's target is 60 to 100 IU/dL, and for bleeding episodes 20 to 100 by severity (Table 1). The targets are total activity, as the curve is.

### Covariates

- **Weight**, on clearance and the central volume, as fitted; with the fat-free-mass switch on, the weight in these terms is the pharmacokinetic weight. The intercompartmental clearance and peripheral volume have no weight term in the source and are scaled by fat-free mass.
- **Age**, on clearance: children clear factor VIII faster per kilogram, as the label also reports. The term is held at 3 years, the youngest patient studied.
- **Pregnancy**: no pharmacokinetics of a factor VIII concentrate in pregnancy have been reported, and nothing is applied. The patient's own activity rises in late pregnancy, which the baseline field carries.

### Doses

`IU/kg` or `IU` as a bolus, and `IU/kg/hr` or `IU/hr` as a continuous infusion, which is used after major surgery.

### Not modelled

The patient's own factor VIII is held constant: it is not produced or cleared in the model, so a perioperative rise in endogenous factor VIII (an acute-phase response) is not drawn. An inhibitor, which shortens the half-life and lowers the recovery, is not represented: the source excluded patients with one. Von Willebrand factor, which carries factor VIII and whose level sets its clearance, was tested in the source and not kept for lack of data.
