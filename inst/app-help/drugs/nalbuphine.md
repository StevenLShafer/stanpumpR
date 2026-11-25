### The model

Nie and colleagues' population analysis (*Front Pharmacol* 2023) of 27 adults, 21 to 76 years, 48 to 82 kg, having elective surgery under general anesthesia at the Fourth Hospital of Hebei Medical University. Each received nalbuphine about 0.24 mg/kg intravenously (353 concentrations); 20 further patients served as the external check. Two compartments:

- clearance 32.9 L/h, central volume 32.5 L, peripheral volume 83.5 L;
- intercompartmental clearance 245 L/h × (HNF / 617.96)^−0.58, where HNF is the hourly net fluid volume infused during the operation (fluids and blood in, urine out). It was the only covariate the authors kept. It is not a Patient Profile field, so it is held at the study mean of 618 mL/h, where the clearance is 245 L/h. The authors' simulations found no need to change the dose for it.

For the typical patient the terminal half-life is about 2.6 hours; the label gives 5 hours, and Wilson (1986) measured 2.4 hours in six women in labor. Studies differ in assay and in how long they sampled, so clearance is the number to compare: Nie's 32.9 L/h is about 0.5 L/h/kg, lower than the 1.8 L/h/kg reported in conscious volunteers, which the authors attribute to anesthesia and surgery lowering hepatic blood flow. The curve therefore describes a patient under anesthesia; a conscious patient may clear the drug faster.

### Covariates

The paper retained no size covariate, so every parameter takes the library's fat-free-mass scaling, set so that a patient like the study's cohort (about 63 kg, 164 cm, 54 years, 15 women and 12 men) receives the published values. With the switch off the published values are used unchanged. Age, sex and renal function were not found to matter in this small sample, which is not evidence that they do not.

- **Pregnancy**: not applied. The only measurements are six women in labor (Wilson 1986) and a series of 28 parturients (Nicolle 1996), neither with a comparison group.

### Product and doses

Nalbuphine hydrochloride injection, `mg` or `mg/kg` as a bolus and `mg/hr` as an infusion, intravenous. The label also allows subcutaneous and intramuscular injection, which the library does not model. The paper does not say whether its doses were of the base or the hydrochloride; they are taken as the injection as labelled, with no conversion.

### What is not plotted

Concentration only, in ng/mL. There is no effect site, no MEAC and no therapeutic range: the label says the minimum effective analgesic concentration varies widely among patients and can rise with tolerance, and the AGNP consensus (Hart 2026) does not list nalbuphine.
