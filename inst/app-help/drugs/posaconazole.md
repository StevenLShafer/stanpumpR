### The model

No published population model of intravenous posaconazole can be built from what is printed, so this one is **derived** from the US label rather than fitted. The NOXAFIL label (revised 2/2026, section 12.3) gives, for the injection, a total body clearance of 7.3 L/h, a terminal half-life of 27 hours and a volume of distribution of 261 L. In its Table 19, healthy volunteers given 300 mg over 30 minutes reached a mean peak of 2840 ng/mL. A two-compartment model has four parameters, and those four numbers determine it exactly. At 70 kg the result is a central volume of 95.1 L, a peripheral volume of 166 L, a clearance of 7.3 L/h and an intercompartmental clearance of 34.5 L/h. Its half-lives are 1.1 hours and 27 hours.

This is a stand-in, put in on Steven L. Shafer's decision of 2026-11-26. The one population model of the delayed-release tablet (van Iersel 2018) lists its covariate effects without the equations or the reference weight needed to use them. It will be replaced when a published model can be built.

### The checks

None of these entered the derivation.

- **Healthy volunteers, 300 mg over 30 minutes:** the label's area under the curve to 12 hours is 13 000 ng·h/mL; the model gives 13 300 (+2%).
- **Patients at steady state, 300 mg over 90 minutes once daily** (label Table 20, 49 patients): peak 3280 and trough 1090 ng/mL; the model gives 3480 and 1155 (+6% each). The average concentration is 14% above the label's 1500 ng/mL, the patients' clearance being 8.3 L/h.
- **Patients on the first day** (label Table 19, 22 patients): the model overstates the first dose by about half (peak 2370 against 1590 ng/mL). The label does not say why patients' first-day values are so low, and they vary widely (CV 62%).

### What is plotted

Total plasma posaconazole in mcg/mL. Doses are posaconazole as labelled. The injection is given only as an infusion, usually over 90 minutes; the label forbids a bolus. Children from 2 years take 6 mg/kg up to 300 mg. The label reports doses from 200 to 300 mg as proportional; the European product information reports single infusions below 200 mg as rising more than in proportion, so the model fits the clinical doses only.

The oral forms (delayed-release tablet, suspension, powder for suspension) are not offered: each absorbs differently, and their absorption depends on food and on the tablet strength in ways the tablet's population model describes but does not print.

### Covariates

None beyond body size. The label prints no weight for the subjects behind its values, so 70 kg is assumed. With [the scaling switch](help:models/fat-free-mass) on, the 70-kg values are scaled to fat-free mass; with it off, linearly in weight. The label found no clinically significant effect of age, sex, renal impairment or indication. The kidneys excrete almost no unchanged posaconazole, so the Serum creatinine field does not affect it. In renal impairment the injection's vehicle (betadex sulfobutyl ether sodium) accumulates; that concerns the vehicle, not the drug.

### Target concentrations

No band is drawn. The antimicrobial drug-monitoring position paper of Abdul-Aziz and colleagues (*Intensive Care Med* 2020;46:1127-1153, [doi:10.1007/s00134-020-06050-1](https://doi.org/10.1007/s00134-020-06050-1)) gives trough thresholds rather than a range: above 0.5-0.7 mg/L for prophylaxis and above 1 mg/L for treatment. It describes no exposure-related toxicity and calls the European Medicines Agency's trough limit of 3.75-4 mg/L "yet to be validated clinically".
