### The model

Moffett and colleagues' population analysis (*Br J Clin Pharmacol* 2017) of 184 inpatients under 19 years old given Thrombate III (plasma-derived antithrombin, Grifols), most of them infants (median age 0.35 years, median weight 5.2 kg) receiving antithrombin for a low activity during a heparin infusion. It was validated in 30 more children. One compartment:

- clearance 0.917 dL/h × (weight/70)^0.75 + 0.129 dL/h × (heparin units/h ÷ 173);
- volume 67.9 dL × weight/70 × (baseline/60)^−0.389, where the baseline is the antithrombin activity before the dose.

**This is the only population model of a plasma-derived antithrombin concentrate in the library**, and it is used for adults as well (Steven L. Shafer, 2026-11-26). At 70 kg and a baseline of 60 IU/dL it gives a half-life of 51 hours (2.1 days) and a rise of 1.03 IU/dL for each IU/kg. The Thrombate III label, from 8 asymptomatic adults with hereditary deficiency, gives a rise of 1.4 IU/dL per IU/kg and a half-life of 3.8 days (functional assay). So for an adult with hereditary deficiency the curve probably rises less and falls faster than the activity will. The source's children were critically ill and consuming antithrombin; the authors suggest this explains the larger volume at a lower baseline. The label's own dosing rule assumes the 1.4.

### What is plotted

Antithrombin activity in **IU/dL** (% of normal): the patient's own plus the activity infused. The patient's own is the Patient Profile's **Antithrombin baseline**, shown while antithrombin is in the dose table. Left blank, it is 60 IU/dL, the source's reference. The baseline also sets the model's volume, so a lower baseline gives a smaller rise for the same dose. For the volume it is held between 25 and 93 IU/dL (the source cohort's mean ± 2 standard deviations, 59 ± 2 × 17), because the paper prints no range.

The band is the label's target, 80 to 120% of normal (IU/dL).

### Covariates

- **Weight**, on both parameters, as fitted; with the fat-free-mass switch on, the pharmacokinetic weight.
- **Antithrombin baseline**, on the volume (above).
- **Heparin is held at none.** In the source, a heparin infusion speeds clearance in proportion to its rate: the median child, on 34 units/kg/h, cleared antithrombin twice as fast as without heparin (half-life 13 against 27 hours). stanpumpR does not read one drug's doses into another's model, so the curve is that of a patient given no heparin. **With heparin running, the activity falls faster than drawn.** The label also warns that the half-life is shortened after surgery, haemorrhage or acute thrombosis.
- **Pregnancy**: no change in the plasma-derived concentrate's clearance or volume has been measured against non-pregnant women. Weiner 1990 measured a half-life of 29 hours in five healthy pregnant women, with no non-pregnant comparison. For the recombinant product, [antithrombin alfa](help:drugs/antithrombinAlfa), DeJongh 2014 found clearance and volume about twice as large at delivery. Nothing is applied.

### Doses

`IU/kg` or `IU` as a dose (the label gives it over 10 to 20 minutes), and `IU/kg/hr` or `IU/hr` as an infusion. The label's loading dose is (120 − baseline) × weight ÷ 1.4, then about 60% of it every 24 hours, adjusted to keep the activity between 80 and 120%.

### Not modelled

The patient's own antithrombin is held constant; its production, and its consumption in disease or by heparin, are not drawn.
