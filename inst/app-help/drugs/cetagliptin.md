### The model

Lu and colleagues' population analysis of cetagliptin, an investigational DPP-4 inhibitor (*Clin Pharmacokinet* 2024). The data were 108 subjects given cetagliptin in four phase 1 studies in China: single doses of 12.5 to 400 mg and a food-effect study in healthy volunteers, 50, 100 and 200 mg daily for 7 to 10 days in healthy volunteers, and 50 and 100 mg daily for 14 days in patients with type 2 diabetes.

- **Disposition:** two compartments, an apparent clearance of 30.55 L/h, a central volume of 204 L, an intercompartmental clearance of 133 L/h and a peripheral volume of 160 L.
- **Absorption:** first order, 1.56/h, with no bioavailability term: the parameters are apparent ones, from oral data only.
- **Efflux:** a saturable transport from the central compartment back into the gut, with Vmax 41,731 µg/h and Kt 11.63 ng/mL. The drug sent back is absorbed again.

Exposure rises faster than the dose because the efflux saturates, not because elimination does: the clearance is linear. The curve comes from numerical integration over the whole dose table, the engine that runs [phenytoin](help:drugs/phenytoin).

### Why this model and not Zhou 2024

Until November 2026 the library used Zhou and colleagues' model of 20 patients given 50 or 100 mg (*Front Endocrinol* 2024), which fitted the same over-proportional exposure with saturable *elimination*. Tested on a study Zhou never saw (Lu 2022, healthy volunteers on 50, 100 and 200 mg daily for a week), it doubles the steady-state exposure at 200 mg: an AUC over a day of 14,900 against the measured 7,790 ng·h/mL. Lu 2024 was fitted to that study, so the study cannot test Lu's model; Zhou's patients can, and Lu's model predicts their AUC within 5 to 22% and their peak concentrations within about 10%, better than Zhou's own model predicts its peak concentrations.

| | Measured | Lu 2024 model |
|---|---|---|
| Healthy, 50 mg, day 1 / day 7 | 857 / 1440 | 875 / 1637 |
| Healthy, 100 mg, day 1 / day 7 | 2280 / 3120 | 2170 / 3273 |
| Healthy, 200 mg, day 1 / day 7 | 5500 / 7790 | 4904 / 6547 |
| Type 2 diabetes, 50 mg, day 1 / day 14 | 717 / 1530 | 875 / 1637 |
| Type 2 diabetes, 100 mg, day 1 / day 14 | 1830 / 3120 | 2170 / 3273 |

AUC over a day, ng·h/mL, for a 70 kg man with **Adjust weight to fat-free mass** unticked.

### Covariates

None: the model has none. The volumes, the clearances and the efflux Vmax are scaled by the library's fat-free-mass factors about the reference patient; with **Adjust weight to fat-free mass** unticked they are the published values. Lu prints no demographics; the three studies it pooled do, and one of them is not yet in the archive, so the model is not yet anchored at the cohort's typical subject.

### Doses

`mg PO` and `mg PO qd`, as the studies give them; no salt is named. 12.5 to 400 mg were studied.

### What is not plotted

Concentration only, in ng/mL. The DPP-4 inhibition the paper fitted alongside (a sigmoid Emax model, maximum 91.3% and half-maximal concentration 5.69 ng/mL) is not drawn. See [Antidiabetic models and their limits](help:models/antidiabetics).
