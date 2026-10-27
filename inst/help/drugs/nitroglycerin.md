### The infusion set changes the dose

**What reaches the patient depends on the infusion set, and the difference is large.** Nitroglycerin is taken up by the plastic of many intravenous administration sets, so part of what the pump delivers never reaches the vein. **This curve assumes the whole dose arrives**, as it does through a set that does not take up the drug. Through one that does, the patient receives, and the plasma holds, much less than drawn.

How much less was measured by Kim and colleagues (*Medicine* 2018;97:e9829). They gave nine healthy men 20 mcg/min for 30 minutes through 100 cm sets of three materials, each man through all three:

| Set material | Peak plasma concentration, against polyolefin | Area under the curve, against polyolefin |
|---|---|---|
| Polyolefin | 1 | 1 |
| PVC | 0.33 | 0.31 |
| Polyurethane | 0.21 | 0.14 |

The fall in arterial pressure was smaller to match: one man fainted, and one had a headache, both with the polyolefin set only. The authors cite studies in which longer sets and slower flows took up proportionately more. So through a PVC or polyurethane set, expect a third to a seventh of the exposure drawn here, and less still at low rates through long tubing. When a dose seems to do too little, or a change of set changes the response, look at the tubing.

The delivery path can also slow the drug's arrival even when the plastic takes none up. Armstrong's patients were infused through Teflon tubing, which does not take up nitroglycerin, yet their plasma rose far more slowly than the drug's own 2-minute half-life allows (see below).

### The model

Intravenous nitroglycerin's clearance comes from Armstrong, Watts and Moffat (*Br J Clin Pharmacol* 1983;16:385-390), who infused 21 mcg/min into ten patients with congestive cardiac failure: **6.2 L/min**, in arterial plasma. The plateau it predicts at the study's rate, 3.4 ng/mL, matches the one observed.

The volume comes from how fast the drug leaves. After an infusion stops, Armstrong's group measured half-lives of 1.1 to 2.8 minutes, consistent with the 1.9 minutes of their earlier study (*Circulation* 1980;62:160-166). One compartment with that half-life has a volume of 6.2 × 1.9 / ln 2 = **17.0 L**: it reaches 90% of its plateau in about 6 minutes, and 5 minutes after a stop 16% of the plateau remains.

The same 1983 paper also reported a slow rise, a time constant of 8.1 minutes, and the model used it until 2026. In any linear model the fall after a long infusion mirrors the rise, so a slow rise and a fast fall cannot both belong to the drug. Two later studies with specific assays side with the fall. Sun and colleagues (*J Clin Pharmacol* 1995;35:390-397) infused isotope-labelled nitroglycerin into 18 healthy men: the plasma was at its plateau at the first sample, 15 minutes, and fell to about a fifth of it in the 5 minutes after the infusion stopped. Kim and colleagues (*Medicine* 2018;97:e9829) infused 20 mcg/min into 9 healthy men: the plasma reached steady state in about 5 minutes, and most samples 3 to 10 minutes after the stop were below the assay's limit. The slow rise belongs to the delivery, not the drug.

### Covariates

None in the source; the parameters are those of the study's adults, scaled to fat-free mass when [the scaling switch](help:models/fat-free-mass) is on.

### Where to be careful

Nitroglycerin's plasma kinetics are notoriously erratic. In eight healthy volunteers McNiff and colleagues (*J Pharm Sci* 1981) found an apparent clearance of 0.3 to 1 L/min/kg, **three to ten times** the one drawn here, a volume of about 3 L/kg and a half-life of about 3 minutes, with two subjects' concentrations oscillating too much to analyse. Clearance follows cardiac output, which is low in heart failure, and may change with the dose and the duration of the infusion (Fung, *Am J Cardiol* 1987;60:4H-9H). Above all it depends on **where the blood is drawn**. The curve is **arterial** plasma. In 20 patients with heart failure on an infusion, Armstrong and colleagues (*Circulation* 1982;66:1273-1276) measured 25.1 ng/mL in the radial artery, 29.8 in the pulmonary artery and only 7.3 in a peripheral vein: the arm removes about 60% of the drug in one pass. **A venous sample therefore reads about a third of the curve.** Sun's and Kim's venous clearances in healthy men, about 10 to 15 L/min, roughly agree with Armstrong's arterial 6.2 once that difference is allowed for (6.2 / 0.4 = 15.5). So this is a consistent set of numbers in the patients most likely to receive the drug, not a precise one.

**A bolus is beyond the model, so none is offered**: nitroglycerin is dosed here only as an infusion, in mcg/min or mcg/kg/min. After 0.25 or 0.5 mg intravenously for uterine relaxation at caesarean delivery, David and colleagues (*Obstet Gynecol* 1998;91:119-124; *Am J Obstet Gynecol* 2000;182:955-961) measured median venous concentrations of 38 and 80 ng/mL at 1 minute, about four times what this model gives. Those samples came from the arm of the injection, and the two papers disagree on how fast the concentration fell, so they give no model, but the drug's early volume after a bolus is smaller than one compartment can show. Curry and colleagues (*Biopharm Drug Dispos* 1993;14:107-118) gave 0.1 mg intravenously to six patients at cardiac catheterisation and found a two-compartment decline in femoral venous plasma, with half-lives of 0.4 and 7.4 minutes. Their mean rate constants describe an infusion that reaches 84% of its plateau in 2 minutes and falls to 10% of it 5 minutes after a stop, as fast as this model or faster. But their printed clearance (2.1 L/min) disagrees with their own dose over area (0.45 L/min), so their numbers are not used.

The pulmonary pressures followed the measured concentration with the same time constant as its rise, so on the same delivery there is no delay to model, but no coefficients are given and no effect is drawn. Tolerance develops within a day of continuous infusion. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
