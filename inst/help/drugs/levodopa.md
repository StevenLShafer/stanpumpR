### The model

Levodopa's plasma model comes from Othman and Dutta at AbbVie (*Br J Clin Pharmacol* 2014;78:94-105), confirmed in the FDA's clinical pharmacology review of the intestinal gel: 68 patients with advanced Parkinson's disease, 23 on carbidopa-levodopa 25/100 tablets and 45 on the gel, fitted in NONMEM. Two compartments: apparent clearance 24.8 L/h, central volume 58.5 L at 70 kg (proportional to weight), intercompartmental clearance 6.8 L/h and peripheral volume 72.9 L. Each tablet passes from the gut through **one transit compartment**, both steps at 2.4 per hour, so the absorption starts slowly and the plasma peaks about 80 minutes after a 200 mg tablet (the paper's own simulation: 75 minutes). See [absorption](help:models/absorption).

### The effect site

The effect site is the motor response, from Mao and colleagues (*J Clin Pharmacol* 2013;53:523-531): in 27 patients with advanced disease the improvement in UPDRS Part III followed an effect site with an equilibration half-life of 0.39 hours (ke0 1.80 per hour), half-maximal at 0.81 mcg/mL, with a Hill coefficient of 2.5 and a maximal improvement of 63%. The effect site peaks about 40 minutes after the plasma, about 2 hours after a tablet. Mao fitted the effect to each patient's measured plasma levodopa, so it can be placed on any plasma model. This is the response of advanced, fluctuating disease; in early disease the effect of each dose is slower, smaller and smoothed by a long-duration response that lasts days (see [Parkinson disease drugs](help:models/parkinsons)).

### Threshold and band

**Time until threshold** is measured to 0.81 mcg/mL in the effect site: when the motor effect falls below half its maximum, the model's estimate of when the dose wears off. The band, 0.9 to 2.0 mcg/mL, is the AGNP consensus guideline's reference range one hour after 250/25 mg.

### Covariates

Weight on the central volume (with [the scaling switch](help:models/fat-free-mass) on, the fat-free-mass weight). Clearance had no weight or sex term; age nearly reached significance and was not included. Women's exposure is 27 to 40% higher in non-model studies, mostly through weight. The COMT Val158Met genotype does not change levodopa's kinetics.

### What is not in the model

The model is valid for carbidopa 4:1 only (carbidopa reduces levodopa's clearance and saturates the peripheral decarboxylase at 70 to 100 mg a day), not for benserazide. COMT inhibitors (entacapone, tolcapone, opicapone) raise levodopa's exposure 35 to 80% and are not applied. Protein meals and slow gastric emptying make absorption erratic in advanced disease (the variability of the transit rate between patients was 88%). The extended-release capsules (Rytary, Crexont) and controlled-release tablets are not modelled; dosing them as tablets would draw the wrong shape.
