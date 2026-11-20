### What is plotted

The curve is the plasma concentration of rosuvastatin, in ng/mL. Rosuvastatin lowers LDL cholesterol by inhibiting HMG-CoA reductase in the liver; the cholesterol response, which takes about four weeks to reach its maximum, is not plotted.

### The model

The model is Perrine Courlet and colleagues' population model (*Clin Pharmacokinet* 2021;60:379-390, doi [10.1007/s40262-020-00946-3](https://doi.org/10.1007/s40262-020-00946-3)), from the Swiss HIV Cohort Study. They had 154 concentrations from 65 adults taking rosuvastatin 5 to 20 mg once daily for high cholesterol: six people sampled richly over a day and 62 sampled at their routine visits. The median age was 55 years, the median weight 75 kg, and 8 of the 65 were women.

It has two compartments. Each tablet is released into the gut at a constant rate over 0.461 hours and then absorbed with a rate constant of 0.306 /h. The apparent clearance (CL/F) is 122 L/h, the central volume 144 L, the peripheral volume 1610 L and the intercompartmental clearance 69 L/h. These are apparent oral parameters, divided by a bioavailability the study could not measure (the label gives about 20%), so only oral doses are offered.

No covariate was retained. Sex, age, weight, creatinine clearance, liver enzymes and boosted HIV protease inhibitors were all tested on clearance.

### Why this model

It is the only adult population model of rosuvastatin whose full text could be read. Its patients took the drug at home as prescribed, which is the population stanpumpR prefers for a drug taken that way.

The models fitted in healthy volunteers describe a different population. Macpherson and colleagues' analysis in children (*Eur J Clin Pharmacol* 2016;72:19-27) compares adults: the median CL/F of adult patients with dyslipidaemia was 78.8 L/h, against 302 L/h in healthy adult volunteers. Courlet's 122 L/h lies between them. The AstraZeneca adult model (Tzeng 2008, *Curr Med Res Opin* 24:2575-2585) could not be read.

### What to keep in mind

- **The patients were living with HIV**, and nearly half of the samples were taken on ritonavir-boosted darunavir. The study ruled out a fall in clearance of more than 22% with boosted protease inhibitors, but these drugs raise the peak.
- **The peak comes early.** After a single 20 mg dose this model peaks at 1.7 hours with 20.7 ng/mL. The label says 3 to 5 hours. The absorption was fitted to six people, and the central volume is the least certain of the parameters. Macpherson's model, evaluated at a 75 kg man, peaks at 2.1 hours with 8.1 ng/mL. The daily exposure (AUC) is the most reliable output: 164 ng·h/mL at steady state on 20 mg a day, as Courlet reported.
- **The terminal half-life is 25.6 hours**, against the label's 19 hours.
- **Asian ancestry** roughly doubles exposure (label); the Crestor label starts Asian patients at 5 mg. The model does not represent this.
- **Severe renal impairment** (creatinine clearance below 30 mL/min/1.73 m²) raises concentrations about threefold (label); mild to moderate impairment does not. No renal term is applied.
- **Transporter genotypes** (SLCO1B1, ABCG2) and interacting drugs (cyclosporine, gemfibrozil) raise exposure and are not represented.

### Body size

The published parameters have no size covariate. With [the scaling switch](help:models/fat-free-mass) on they scale by fat-free mass from the 70 kg reference man; with it off they are the published values for everyone. They should be anchored at Courlet's cohort (75 kg, 55 years, mostly men), but the paper prints no height, and the population height the rule needs could not be obtained when the model was built. Anchoring would lower this model's values by 1% to 5%.

### Dose

Crestor tablets are labelled as rosuvastatin (5 mg contains 5.2 mg of rosuvastatin calcium). Doses are entered as labelled, and the curve is rosuvastatin.

### The band

No band is drawn. Statins are dosed to the cholesterol response, not to a concentration, and no therapeutic range for rosuvastatin is established; the Crestor label states none.

### Pregnancy

Two paired studies from Ribeirão Preto measured rosuvastatin in the third trimester. Pego and colleagues (2024, healthy women) found the 24-hour area twice that 8 to 12 weeks after delivery (18.7 against 9.5 ng·h/mL); Moreira and colleagues (2023, women living with HIV) found 19.5 against 13.3 postpartum and 13.8 in nonpregnant women. The half-life did not change, so the rise is in bioavailability. Because the postpartum values were unusually low, the rise against a woman who is not pregnant lies somewhere between 1.4- and 2-fold. Only the abstracts of these studies were read, and nothing is applied. The label advises stopping rosuvastatin when pregnancy is recognised.
