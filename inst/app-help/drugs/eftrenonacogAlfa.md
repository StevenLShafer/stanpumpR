### The model

ALPROLIX, recombinant factor IX fused to the Fc domain of immunoglobulin G1, an extended-half-life concentrate for haemophilia B. Koopman and colleagues' population analysis (*Br J Clin Pharmacol* 2024) of 37 patients with haemophilia B (35 severe) from Dutch and British real-world practice, aged 2 to 71 years (14 under 12) and weighing 12 to 103 kg, built because the earlier model (Diao 2014, patients 12 years and older) underpredicted children's levels. Two compartments, every parameter scaled to weight about 73 kg:

- clearance 1.41 dL/h × (weight/73)^0.75 × (1 − 0.0047 × (age − 15.8));
- central volume 73.1 dL × weight/73;
- intercompartmental clearance 2.77 dL/h × (weight/73)^0.75, peripheral volume 80.1 dL × weight/73.

At 73 kg and 16 years the terminal half-life is 87 hours and a bolus raises the activity by 1.0 IU/dL per IU/kg; the label's adults: 86 to 97 hours and 1.02 to 1.12. The clearance, 1.9 mL/h/kg, is about a third below the label's 2.6 to 3.3; the authors found the same against the earlier model, and attribute it to real-world sampling.

A standard factor IX (BeneFIX) has a half-life of about a day, a quarter of this product's, so this curve is not for it.

### What is plotted

Factor IX activity in **IU/dL**, the same number as % of normal: the patient's own activity plus the activity infused. The patient's own is the Patient Profile's **Factor IX baseline**, shown while ALPROLIX is in the dose table. Left blank, it is the source population's: severe haemophilia B, below 1 IU/dL, taken as 0.5 IU/dL. A carrier, or a patient with mild or moderate haemophilia B, has a higher baseline, which should be entered.

The shaded band is the label's initial target for major surgery, 60 to 100 IU/dL (ALPROLIX prescribing information, Table 2); for minor surgery the label's target is 50 to 80 IU/dL. The targets are total activity, as the curve is.

### Covariates

- **Weight**, on every parameter, as fitted; with the fat-free-mass switch on, the pharmacokinetic weight.
- **Age**, on clearance, linear: 6% higher at 2 years than at 16, a third lower at 90.
- **Pregnancy**: no pharmacokinetics in pregnancy have been reported, and nothing is applied.

### Doses

`IU/kg` or `IU` as a bolus, and `IU/kg/hr` or `IU/hr` as an infusion.

### Not modelled

The patient's own factor IX is held constant. An inhibitor is not represented. Factor IX also distributes into the extravascular space (it binds collagen IV), which the model's peripheral compartment absorbs; the extravascular factor IX is not drawn.
