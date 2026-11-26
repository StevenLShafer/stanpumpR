### The model

Desmopressin (DDAVP) is a synthetic analogue of vasopressin. Given intravenously at 0.3 mcg/kg over 15 to 30 minutes, it releases von Willebrand factor and factor VIII from the endothelium. It is used to treat bleeding in haemophilia A and von Willebrand disease, and to reverse the platelet dysfunction of aspirin in the oozing surgical patient.

The disposition is from Agersø and colleagues (*Br J Clin Pharmacol* 2004;58:352-358, [doi:10.1111/j.1365-2125.2004.02175.x](https://doi.org/10.1111/j.1365-2125.2004.02175.x)). They gave 2 mcg of desmopressin acetate intravenously to 24 adults aged 49 to 68: 6 with normal renal function and 18 with renal impairment, none on dialysis. Plasma was sampled from a vein, and plasma and urine were fitted together. The model has three compartments:

- volumes 13.9, 12.5 and 11.7 L;
- intercompartmental clearances 4.43 and 27.5 L/h;
- a renal clearance of 2.41 L/h and a non-renal clearance of 3.91 L/h at a creatinine clearance of 60 mL/min/1.73 m².

### Renal function

Both the renal and the non-renal clearance fall in a straight line with creatinine clearance:

- each mL/min below 60 lowers the renal clearance by 1.74% of its value at 60;
- each mL/min below 60 lowers the non-renal clearance by 0.933% of its value at 60.

The study's four groups had a total clearance of 10, 6.9, 4.9 and 2.9 L/h at creatinine clearances of 103, 72, 37 and 16 mL/min; the model gives 9.7, 7.3, 4.5 and 2.9 L/h. Why the non-renal clearance also falls is not known.

The creatinine clearance is estimated from the **Serum creatinine** field by Cockcroft-Gault, indexed to 1.73 m² as the study's measured clearances were. It is held within the 10 to 144 mL/min/1.73 m² the study spanned. A blank field means the normal creatinine for the patient's age and sex, so an older patient with a blank field is given an older patient's clearance, but not renal impairment. Enter the creatinine to see the effect of impaired kidneys.

### Body size

The study tested weight, height and body mass index, and none improved the fit. As for every drug in the library, the volumes and clearances are still scaled by fat-free mass from the 70 kg reference man. With *Adjust weight to fat-free mass* unticked, the published values are used unscaled. The label doses by actual body weight (0.3 mcg/kg, to a maximum of 20 mcg), which the dose table does not cap.

Doses are entered as desmopressin acetate, as every US product is labelled and as both studies dosed it.

### Effect site

The effect site is the release of factor VIII. Its rate constant, ke0 2.59 /h (a half-time of 16 minutes), is from Argenti and colleagues (*Am J Ther* 1997;4:3-8, [doi:10.1097/00045391-199701000-00002](https://doi.org/10.1097/00045391-199701000-00002)). They gave 0.3 mcg/kg over 15 minutes to 28 healthy men and measured von Willebrand factor (ristocetin cofactor) and factor VIII coagulant activity for 24 hours. Factor VIII's rate constant was chosen over von Willebrand factor's (2.16 /h) because it was the better estimated: its coefficient of variation between subjects was 43%, against 106%. On this model the two put the peak effect 30 and 34 minutes after a bolus.

After 0.3 mcg/kg over 15 minutes, the plasma concentration peaks at about 1100 pg/mL at the end of the infusion, as Argenti measured, and the effect site about half an hour later.

### % effect

The Graph Option **% effect** draws two panels from Argenti's sigmoid Emax models:

| End point | EC50 | Emax (% of predose) | Hill coefficient | ke0 |
|---|---|---|---|---|
| von Willebrand factor | 237 pg/mL | 367% | 0.94 | 2.16 /h (its own effect site) |
| Factor VIII:C | 314 pg/mL | 388% | 0.89 | 2.59 /h (this drug's effect site) |

100% on these panels is the largest possible rise: to 3.7 times baseline for von Willebrand factor and 3.9 times for factor VIII. After 0.3 mcg/kg the von Willebrand panel reaches about 70%, a rise to about 2.9 times baseline. Argenti measured about 2.6 to 2.9 times.

The variability between subjects was large: the coefficients of variation were about 30 to 45% for Emax and EC50, 43% for the factor VIII ke0 and 106% for the von Willebrand factor ke0.

### Where to be careful

- **Two studies.** Argenti's mean clearance, 7.0 L/h in young healthy men, is about 30% below what Agersø's model gives for such men (about 10 L/h, which its healthy group measured). Argenti's EC50s were estimated on his own, higher concentrations, so on this model the effect is probably understated somewhat.
- **Venous sampling.** stanpumpR prefers arterial pharmacokinetics, and no arterial study of desmopressin exists. Over a 15 to 30 minute infusion the difference is small.
- **Tachyphylaxis.** The label warns that the response lessens when doses are repeated more often than every 48 hours. That is not modelled.
- **Not modelled.** The antidiuretic effect, and with it fluid retention and hyponatraemia; the subcutaneous and intranasal routes.
- **Pregnancy.** Davison and colleagues (1993) measured desmopressin's clearance in the same six women in late pregnancy and 10 to 12 weeks after delivery: 0.18 against 0.15 L/min, not significantly different. Vasopressin's clearance rose fivefold, because the placenta's vasopressinase does not break down desmopressin. No pregnancy factor is applied.
