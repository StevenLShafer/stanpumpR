### The model

Maraviroc (Selzentry) is an oral HIV-1 entry inhibitor that blocks the CCR5 co-receptor. Chan and colleagues pooled 17 Pfizer studies (*Br J Clin Pharmacol* 2008;65(Suppl 1):76-85): 413 subjects, 365 of them healthy volunteers and 48 with asymptomatic HIV infection. They were 18 to 54 years old (median 30) and weighed 46 to 109 kg (median 71); 317 were men and 95 were Asian. They took single or repeated doses of 100 to 1200 mg, mostly fasted.

The model has two compartments, with first-order absorption after a lag of 0.198 h. Its disposition is systemic, not apparent:

- clearance 51.45 L/h: a hepatic extraction ratio of 0.662 of a hepatic plasma flow fixed at 59.59 L/h, plus a renal clearance fixed at 12 L/h;
- central volume 132 L, peripheral volume 277 L;
- intercompartmental clearance 16.4 L/h at age 30, rising with age as (age/30)^0.349, 27% higher at 60.

What reaches the circulation depends on the size of each dose. The fraction absorbed rises with dose, Dose^1.39/(51.2^1.39 + Dose^1.39), and the third of it that escapes the liver's first pass gives a bioavailability of 24% at 100 mg, 31% at 300 mg and 33% at 600 mg. The absorption rate also rises with dose, 0.277 × Dose^0.173 per hour: 0.61 /h at 100 mg and 0.74 /h at 300 mg. So stanpumpR runs each dose size as its own linear system, with its own bioavailability and absorption rate, and adds them. The dose in both terms is held to the 100 to 1200 mg the studies gave.

### Covariates

Weight, sex and HIV status were tested and none was kept. stanpumpR scales every parameter by body size all the same, as it does for every model: with [the scaling switch](help:models/fat-free-mass) on, the volumes follow fat-free mass and the clearances fat-free mass to the power 0.75, anchored at the 70 kg reference man. The cohort's median, 71 kg and 30 years, three quarters men, is close to him. The paper prints no height, so the anchor could not be set at the cohort itself. The age term is the paper's, held at 18 years below that age.

Three things in the paper are **not applied**:

- **Race.** Asian subjects had a lower hepatic extraction, so a higher bioavailability and a 26.5% higher AUC. The Patient Profile has no race field, so the curve is a non-Asian's.
- **Food.** A high-fat meal slows absorption and lowers the AUC by 26% to 43%, more at low doses. The curve is fasted, as most of the data were; the label lets maraviroc be taken with or without food.
- **Drug interactions.** The label's dose depends on the other drugs: 300 mg twice daily with non-interacting drugs, 150 mg with a strong CYP3A inhibitor, 600 mg with a CYP3A inducer. The model has no interactions: a dose you enter is simulated as if maraviroc were given alone, so 150 mg entered for a patient on a boosted protease inhibitor will be drawn far too low.

### How well it agrees

The model reproduces the paper's own derived values: the single-dose AUCs of its Table 5 (471, 805, 1815 and 3817 ng·h/mL at 100, 150, 300 and 600 mg) and its terminal half-life of 15.9 h. Against the label's Table 11 at 300 mg twice daily, which was not used to build it:

| | AUC12 (ng·h/mL) | Peak (ng/mL) | Trough (ng/mL) |
|---|---|---|---|
| Healthy volunteers, phase 1 (64) | 2908 | 888 | 43.1 |
| Asymptomatic HIV, phase 2a (8) | 2550 | 618 | 33.6 |
| Phase 2b/3 trials (label, sparse sampling) | 1513-1865 | 266-287 | 37-60 |
| stanpumpR (70 kg man, 30 years) | 1815 | 345 | 39 |

The trough agrees with every study. The AUC agrees with the patients' trials and is 38% below the healthy volunteers'. The peak is well below the richly sampled volunteers'. The paper's residual error was largest during absorption (74% about an hour after a dose), so the peak is the least certain part of the curve.

### Pregnancy

Colbers and colleagues (*Clin Infect Dis* 2015) studied 18 women with HIV in the third trimester and again after delivery. In pregnancy the AUC over a dosing interval was 0.72 times the postpartum value and the peak 0.70 times; the trough fell 15%. Because the AUC and the peak fell alike, stanpumpR applies the factor to bioavailability: with the Patient Profile's Pregnant field on, the bioavailability is multiplied by 0.72. Most of these women took maraviroc with a protease inhibitor, in both periods.

### No band

The label quotes no therapeutic range. Its clinical trials related virological success to the modelled trough, and a trough target of 50 ng/mL is used in the HIV literature, but that is a guide to dosing in combination therapy, not an established therapeutic range, so no band is drawn.

### What is not plotted

Concentration only, in ng/mL. CCR5 occupancy and viral load are not drawn.
