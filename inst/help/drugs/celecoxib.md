### The model

**This is not a published population model.** No two-compartment population model of celecoxib could be reproduced: the FDA review of NDA 211759 (VYSCOXA oral suspension, 2025) states that the applicant's models were two-compartment but publishes no coefficients. The parameters here were fitted by stanpumpR's maintainers to the **mean** plasma curve of a single 200 mg Celebrex capsule taken fasting by 51 healthy adults (Study 915/22, Figure 1 of the FDA Clinical Pharmacology Review). The 19 mean points were digitized from the figure. Their AUC(0–72 h), 5424 ng·h/mL, agrees with the review's mean of 5379. The fit is two-compartment with first-order absorption and a lag: CL/F 36.9 L/h, V1/F 237 L, Q/F 72.0 L/h, V2/F 413 L, absorption rate 0.985 per hour, lag 0.85 h. It follows the mean curve within 8% from 1 to 48 hours. Its terminal half-time is 15.0 h, against 14.4 h in the review.

A fit to mean data has no variability between patients. A mean curve is also smoother than any one patient's, so absorption here is slower, and the peak lower, than in a typical individual: 445 ng/mL for 200 mg, where the mean of individual peaks is about 480–690 ng/mL across studies. The parameters are **apparent**: no absolute bioavailability study exists. Celecoxib is therefore offered by mouth only, with a bioavailability of 1.

### Independent checks

Two studies not used in the fit:

- NCT04526197 (35 healthy adults, 200 mg fasting) reports AUC(0–∞) 6743 ng·h/mL. That implies a CL/F of 29.7 L/h, 20% below this fit and within that study's 38% variability.
- Itthipanichpong and colleagues (*J Med Assoc Thai* 2005;88:632-638; 18 Thai men averaging 63 kg) report CL/F 35.9 L/h, which is what this model gives at 63 kg.
- Werner and colleagues (*Biomed Chromatogr* 2002;16:56-60; 12 healthy adults, 200 mg) report a mean AUC(0–∞) of 6246 ng·h/mL in the 11 with normal CYP2C9 function, a CL/F of 32.0 L/h. Their one CYP2C9 poor metabolizer had twice that AUC.
- Brenner and colleagues (*Clin Pharmacokinet* 2003;42:283-292) report steady-state AUCs on 200 mg twice daily implying CL/F of 34.5 L/h in young adults (71 kg) and 35.7 L/h in older adults (82 kg). They found no effect of age once weight was accounted for.

The label gives about 30 L/h.

### Body size

The weight effect is from Krishnaswami and colleagues (*J Clin Pharmacol* 2012;52:1134-1149). Their one-compartment NONMEM model combined 152 children aged 2 to 17 with juvenile rheumatoid arthritis and 36 adults with rheumatoid arthritis. It found CL/F proportional to weight^0.265 and V/F to weight^0.499. The label's statement that 10 kg and 25 kg patients have 40% and 24% lower clearance than a 70 kg adult follows from that exponent. Here clearances scale by (W/70)^0.265 and volumes by (W/70)^0.499. W is the pharmacokinetic weight with the [fat-free-mass switch](help:models/fat-free-mass) on and total weight with it off. Stempak and colleagues (*Clin Pharmacol Ther* 2002;72:490-497) measured a median CL/F of about 1.1 L/h/kg in 11 children with cancer, higher than this model's 0.8 at 40 kg. They sampled for only 12 hours, and their patients were receiving chemotherapy.

### Effect site

ke0 is supplied directly from Hannam and colleagues (*Paediatr Anaesth* 2023;33:291-302). They re-analysed mean pain relief after celecoxib 200 and 400 mg for dental surgery in adults and found an equilibration half-time of 1.12 hours, an effect-site Ce50 of 242 ng/mL and an Emax of 3.25 on a 0–4 pain-relief scale. They estimated it on their own one-compartment kinetics (CL/F 49 L/h, V/F 346 L), so pairing it with this fit is an assumption, as it is for ibuprofen. The *time until threshold* level is the Ce50, 242 ng/mL in the effect site. Celecoxib is not an opioid and is not on the MEAC panel.

### Where to be careful

Not modelled:

- **Food.** A high-fat meal raises the suspension's peak by 144% and its AUC by 35–50%.
- **The oral suspension,** which peaks 22% lower than capsules at the same AUC.
- **Doses above 200 mg,** whose exposure rises less than proportionally because the drug dissolves poorly.
- **CYP2C9 genotype.** Exposure is 3–7 times higher in \*3/\*3, from 8 subjects, with no per-allele model.
- **Age, race and sex.** AUC is about 50% higher in the elderly and about 40% higher in Black subjects.
- **Hepatic impairment.**

There is no CYP2D6 term.
