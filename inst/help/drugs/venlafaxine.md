### The model

Wang and colleagues (*Front Pharmacol* 2022;13:978202, [doi:10.3389/fphar.2022.978202](https://doi.org/10.3389/fphar.2022.978202)) fitted venlafaxine and its active metabolite O-desmethylvenlafaxine ([desvenlafaxine](help:drugs/desvenlafaxine), ODV) together. They pooled two sources: a bioequivalence study in 24 healthy Chinese men, each with 15 samples over 36 hours after 50 mg of immediate-release venlafaxine, and drug-monitoring troughs from 127 psychiatric inpatients aged 14 to 86 on 25 to 300 mg a day. Each drug has one compartment, with apparent oral parameters (Table 2):

| Parameter | Value |
|---|---|
| Venlafaxine CL/F | 80.9 L/h in healthy volunteers; 61.7% lower, **31.0 L/h, in patients** |
| Venlafaxine V/F | 628 L |
| ODV CL/F | 22.1 L/h |
| ODV V/F | 238 L |
| ka | 0.63 /h, fixed |
| First-pass fraction to ODV | 0.048 |

**The patients' clearance is used**, since they are who take the drug. Venlafaxine's half-life is then 14.0 hours; in the healthy volunteers it was 5.4 hours. ODV's half-life is 7.5 hours in both groups.

### How venlafaxine becomes ODV

The paper's diagram (Figure 1) gives venlafaxine a single exit: a first-order conversion, K23, into ODV. ODV leaves by its own clearance. So venlafaxine's clearance in Table 2 *is* its conversion to ODV, and K23 = CL/F ÷ V/F, although the table does not list it separately. The paper's own figure for the healthy half-life, 5.4 hours, is what that gives. In addition, 4.8% of each absorbed dose becomes ODV during first pass, before reaching the circulation, and goes straight to ODV's compartment. The concentrations were modelled in µmol/L, so ODV is formed mole for mole: a milligram of venlafaxine yields 0.95 mg of ODV.

### Dosing and the range

Venlafaxine is offered as **mg PO** and once-, twice- or three-times-daily schedules. Enter the dose as labelled. The absorption rate is the immediate-release tablet's; most of the patients took sustained-release tablets, which absorb more slowly, but the formulation did not change the fitted clearance, so averages on a steady regimen are reliable and peaks and troughs on an extended-release product are not.

The AGNP consensus range (Hiemke and colleagues, 2018), 100 to 400 ng/mL, is for **venlafaxine plus ODV**, so no band is drawn on either row. On 150 mg a day the model's patient averages 192 ng/mL of venlafaxine and 269 of ODV, a sum of 460, above the range. That follows from the patients' low clearance: the same dose in the healthy volunteers' model averages about 300.

### Not modelled

The amisulpride interaction the source found (venlafaxine clearance 39% lower, ODV 59% higher) needs a co-medication field the app lacks. CYP2D6 and CYP2C19 genotype were not available to the authors, so the **CYP 2D6** field does not change this model, although CYP2D6 forms ODV; the patients' low clearance is close to that of CYP2D6 poor metabolisers. Age, sex and weight were tested and not retained. The minor metabolites are not modelled. The cohort was adolescents and adults; the infant and child in the table above are extrapolation. See [Antidepressant models and their limits](help:models/antidepressants).
