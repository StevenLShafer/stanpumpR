### The model

Wang and colleagues gave a single bolus of 0.6 mg/kg ciprofol over 30 seconds to 25 Chinese children aged 1 to 9 and weighing 9.8 to 37 kg (*BMC Anesthesiol* 2026;26:141, [doi:10.1186/s12871-026-03647-9](https://doi.org/10.1186/s12871-026-03647-9)). Arterial samples were taken for 3 hours. The model has three compartments, every parameter per kilogram:

- clearance 31.2 mL/min/kg;
- central volume 506 mL/kg;
- intercompartmental clearances 28.2 and 19.8 mL/min/kg;
- peripheral volumes 231 and 1360 mL/kg.

The central volume also fell with blood urea nitrogen. The Patient Profile has no BUN, so it is held at the study's reference, 5 mmol/L.

### When stanpumpR uses it

This is one of [two ciprofol models](help:drugs/ciprofol), the one for children.

### Where to be careful

- A single small bolus study, sampled for 3 hours: the deep compartment and the behaviour after a long infusion are uncertain.
- The time to peak effect is assumed to be the adults'.
- With *Adjust weight to fat-free mass* ticked the per-kilogram values are applied to the fat-free-mass-adjusted weight.
