### The model

Tecovirimat (TPOXX) is an antiviral against orthopoxviruses, approved in the United States to treat smallpox. Its effectiveness cannot be tested in people, so the label supports the dose by showing that it gives higher exposures than those that protected infected monkeys and rabbits.

The model is the human half of Leeds and colleagues' analysis (*Antimicrob Agents Chemother* 2013;57:1136-1143). It was fitted to one placebo-controlled trial of 107 healthy volunteers aged 19 to 74, about half of them women, with a mean weight of 78.4 kg. They took 400 or 600 mg once a day for 14 days with food. The model has two compartments, with first-order absorption after a lag:

- absorption rate 1.06 /h after a lag of 1.46 h;
- clearance 41.15 L/h and intercompartmental clearance 36.79 L/h, each × (weight/78.4)^0.75;
- central volume 281.51 L in women and 217.44 L in men, and peripheral volume 413.53 L, each × weight/78.4.

The parameters are apparent oral ones, so only the oral route is offered. An intravenous product (TPOXX injection) exists, but no intravenous model is in the archive. The paper also fits monkeys, and that part is not used.

### Covariates

The model's own weight terms and its sex term on the central volume are applied. With [the scaling switch](help:models/fat-free-mass) on, the weight in those terms is the pharmacokinetic weight, as for every model with its own weight covariate. The label finds no clinically significant effect of age, sex, ethnicity, renal impairment or hepatic impairment on the capsule's pharmacokinetics.

### Doses

The adult dose is 600 mg every 12 hours for 14 days, taken within 30 minutes after a full meal of moderate or high fat; 600 mg every 8 hours from 120 kg. Doses are in mg of tecovirimat, as the label gives them (`mg PO bid`, or `mg PO tid` from 120 kg).

### How well it agrees

The label's steady state at 600 mg every 12 hours was not used to build the model:

| | Label | stanpumpR |
|---|---|---|
| AUC(0-24), under 120 kg | 29,816 ng·h/mL | 29,170 (78.4 kg) |
| Peak, under 120 kg | 2159 ng/mL | 2155 |
| Trough, under 120 kg | 845 ng/mL | 610 |
| AUC(0-24), over 120 kg | 19,500 ng·h/mL | 19,960 (130 kg) |
| Peak, over 120 kg | 1300 ng/mL | 1403 |
| Trough, over 120 kg | 585 ng/mL | 438 |
| Terminal half-life | 19 h | 16.7 h (men), 17.4 h (women) |

The exposure and the peak agree closely at both weights. The troughs are about a quarter low at both, so the model's concentrations at the end of a dosing interval are a little lower than measured. The model peaks about 3 hours after a dose; the label gives a median of 6 hours (range 2-24) with food.

### No band

No therapeutic range is established. The label justifies the dose by exposure in animals, not by a human concentration target.

### What is not plotted

Concentration only, in ng/mL. No antiviral effect is drawn.
