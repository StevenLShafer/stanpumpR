### The model

Liu and colleagues pooled twelve trials of ciprofol (*CPT Pharmacometrics Syst Pharmacol* 2024;13:823-836, [doi:10.1002/psp4.13121](https://doi.org/10.1002/psp4.13121)). They studied 462 adults aged 18 to 77 and weighing 41.6 to 97.9 kg: 125 healthy volunteers, 225 patients having elective surgery and 112 ventilated patients in intensive care. The model has three compartments:

- clearance 80.9 L/h at 62 kg, times (weight/62)^0.578, 9% higher in women;
- central volume 3.53 L at 33 years, times (age/33)^0.496;
- intercompartmental clearances 73.9 and 28.6 L/h, and peripheral volumes 51.9 and 419 L.

These are the paper's values for an intensive-care patient. stanpumpR uses its values for the patients outside intensive care (clearance 21% lower), after long infusions, and in arterial samples. Venous samples gave a clearance 18% lower.

### Why the long-infusion values

The single-bolus studies gave much smaller peripheral volumes, a fifth of the long infusions' deep volume. Their slow phase probably fell below the assay's limit of 5 ng/mL before it could be seen. A model cannot change its volumes with the length of an infusion, so the long-infusion values, which saw the deep compartment, are used for every dose.

### Body size

Clearance carries the paper's own weight term, evaluated at the fat-free-mass-adjusted weight. The volumes and intercompartmental clearances have no weight term, so they are scaled by fat-free mass, anchored at the study's typical patient (61.7 kg, 165 cm, 38 years, about half women). With *Adjust weight to fat-free mass* unticked the published values are used. The age term is held at 18 years for younger patients.

### When stanpumpR uses it

This is one of [two ciprofol models](help:drugs/ciprofol), the one for adults.

### Where to be careful

- The intensive-care clearance, 27% higher than the one used here, is not offered.
- 85% of the patients were Asian.
