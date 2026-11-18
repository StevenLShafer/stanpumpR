### The model

Macitentan's parameters come from Bartolucci and colleagues (*Clin Pharmacokinet* 2021;60:1605-1619, doi 10.1007/s40262-021-01049-3). They fitted 452 healthy volunteers and patients with pulmonary arterial hypertension in nine studies, at doses of 0.2 to 600 mg.

Macitentan and its active metabolite, [aprocitentan](help:drugs/aprocitentan), each have one compartment:

- macitentan: volume 34.4 L and apparent clearance 1.39 L/h, of which 62% forms aprocitentan;
- aprocitentan: volume 23.5 L and clearance 0.321 L/h.

A dose is absorbed by three parallel paths:

1. Most of the dose is released into the gut over 2.77 hours and absorbed at 0.312 /h.
2. The rest of the macitentan is absorbed slowly, at 0.0502 /h, after a lag of 11.6 hours. This gives the second peak seen after larger doses.
3. 6.3% is released over 3.46 hours and absorbed at 0.381 /h directly as aprocitentan, formed before it reaches the circulation.

The share taken by the first path falls with the dose (84% at 10 mg). All values are apparent, because the absolute bioavailability is unknown.

### Checks

These check the implementation against the paper's own description of its typical woman with pulmonary hypertension on 10 mg daily, and against the Opsumit label.

| | Source | This model |
|---|---|---|
| Macitentan peak after the first dose | 9 h (abstract) | 9.1 h |
| Macitentan accumulation | twofold (abstract) | 2.13 |
| Aprocitentan peak after the first dose | 51 h (abstract) | 47.7 h |
| Aprocitentan accumulation | 12.5-fold (abstract) | 12.4 |
| Half-lives, macitentan and aprocitentan | 16 and 48 h (label) | 17.2 and 50.7 h |
| Aprocitentan's exposure relative to macitentan's, steady state | 3 times (label) | 2.98 |

### Covariates

- **Weight** acts on both volumes and both clearances. With [the scaling switch](help:models/fat-free-mass) on it is the pharmacokinetic weight; with it off, total weight.
- **Sex:** aprocitentan's clearance is 15.7% higher in men.
- **Renal function:** moderate or severe renal impairment (an MDRD eGFR below 60 mL/min/1.73 m², from the Patient Profile's **Serum creatinine**) lowers aprocitentan's clearance by 13.1%, a step as published.

The reference is the clinical case: a patient with pulmonary hypertension taking the tablet. Healthy volunteers cleared macitentan 27% faster. Asian race, which the Patient Profile does not record, changed the volumes and aprocitentan's clearance.

### Readings the paper leaves open

- Its text gives the second path a zero-order duration, but no table prints one, and Figure 1 shows only its lag. The path is lagged and first order here.
- Its footnote writes the dose effect on bioavailability as exp(α (dose − 10)), with α positive, and calls it a *decrease*; its text says exposure above 25 mg was lower than expected. The decrease is applied. It is 1 at the labelled 10 mg.
- The footnote's "0.81 for a dose of 10 mg" for the first path's share is the base model's; the final model gives 0.84.

### Dose and band

Opsumit tablets contain 10 mg of macitentan (no salt), taken once daily. Macitentan is contraindicated in pregnancy. No therapeutic range has been established.
