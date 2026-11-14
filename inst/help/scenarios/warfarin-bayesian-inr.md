## What to look for

A 62-year-old, 82 kg, 176 cm man is started on **warfarin 5 mg once a day**. His genotype has not been tested, so the Patient Profile keeps the defaults: CYP2C9 \*1/\*1 and **VKORC1 AA**, the most warfarin-sensitive genotype. stanpumpR assumes the most sensitive genotype on purpose, so that an untested patient's predicted INR errs high (see [Patient profile](help:patient-profile)). His INR is measured on the mornings of days 3, 5, 7 and 10, before that day's dose: **1.3, 1.6, 1.9 and 2.2**. The scenario puts those four values into the **Bayesian Updating** panel in the left sidebar. Open the panel to see them.

- The measured INRs are the **points** on the *warfarin (INR)* panel. The **lighter line**, named *warfarin (population)* in the legend, is the model's own prediction for this patient: it passes well above the points and heads for an INR near 4.
- The **darker line** is warfarin after the [Bayesian update](help:models/bayesian): the model's clearance and volume adjusted to the four measurements. Under the table the panel says by how much (*clearance x 1.45 (+45%), volume x 1.3 (+30%)*). The updated line passes close to every point and settles near **2.4**, inside the usual 2-3 target.
- The small table under that gives each measured INR beside the model's prediction and the updated one.

## Try next

- Move **Residual variability** from 5% up to 20%. Each measurement now counts for less against the model, and the updated line moves back toward the population line. Only the ratio of the two variabilities changes the estimate.
- Delete the day-10 INR from the table (or **Clear measurements** and type them in again one by one). Each row changes the estimate as soon as it is complete, and the fewer the measurements, the more the estimate keeps to the model.
- Change **VKORC1** from AA to GG in the Patient Profile. The model's own prediction falls to near the measurements, and the update has much less to correct. The update attributes the whole difference to clearance and volume, because it has no way to tell a less sensitive patient from one who clears warfarin faster.
- Change the dose in the dose table (to 4 mg a day from day 10, say) and press **Apply Changes**. The updated model, not the population one, predicts the INR on the new dose.

## Background

[Bayesian updating from measured concentrations](help:models/bayesian) explains what is estimated, the two variabilities, and the cautions. The INR model is the library's [warfarin](help:drugs/warfarin) (Xue 2017). It is a research and teaching model, and so is its update: a simulated INR is **not** a dose recommendation, a bleeding risk or a procedure-safe time.
