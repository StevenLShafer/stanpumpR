### The model

Desethylamiodarone is the active metabolite of [amiodarone](help:drugs/amiodarone). Its kinetics come from the same study as the parent's (Pollak, Bouillon and Shafer, *Clin Pharmacol Ther* 2000;67:642-652), fitted to the same 605 trough samples: two compartments, with these typical values and between-patient variability:

| Parameter | Typical value | Variability (CV) |
|---|---|---|
| V1 | 2790 L | 21% |
| V2 | 7830 L | 109% |
| CL1 | 254 L/day | 30% |
| CL2 | 151 L/day | 44% |

These give half-lives of 4.5 days and 60.4 days. The paper reports a terminal half-life of 62 days, which its own table of parameters does not reproduce; the computed value is shown above and the parameters are used as published.

### Formed only

The metabolite's model was fitted after the parent's: its input was taken to be all the amiodarone each patient's own fitted parameters said had been permanently cleared from the serum. That input is itself apparent (it carries amiodarone's unknown bioavailability), and it assumes that every milligram of amiodarone cleared becomes a milligram of desethylamiodarone. The parameters above are therefore on that assumed scale. Scaling the metabolite's volumes, its clearances and the amount formed by any common factor leaves the *formed* concentrations unchanged, so the concentrations plotted after an amiodarone dose are identified, but the scale for a direct dose is not. Desethylamiodarone is therefore offered with **no dosing unit**: it appears only when amiodarone is given. See [Active metabolites](help:models/metabolites).

### Effect and concentrations

Desethylamiodarone appears to be as potent as amiodarone and as toxic (Pollak, citing Nattel and Talajic, *Drugs* 1988;36:121-131, for potency). As for the parent, no human equilibration rate for its effect has been published, so the row plots serum concentrations and has no effect site.

The 1.0 to 2.5 mg/L therapeutic window is for amiodarone. No range has been established for the metabolite, so **no band is drawn**, and there is no default recovery threshold. Because all of amiodarone's clearance forms the metabolite, at steady state its concentration is the daily dose over its own clearance: 343 mg/day of amiodarone gives 1.35 mg/L, 0.90 of the parent's 1.50. It gets there slowly. On Pollak's regimen this model gives 0.56 mg/L at a week, 0.95 at four weeks and 1.34 at a year, while the parent is already near its target within days.

### Covariates

None was significant. The parameters take the same [fat-free-mass scaling](help:models/fat-free-mass) as amiodarone's, identically, because the metabolite's apparent scale is consistent with the parent's only if the two scale together.

### Where to be careful

The parameters are apparent, and depend on the assumption that all of the amiodarone cleared becomes desethylamiodarone. The true metabolite volumes and clearances are these multiplied by amiodarone's bioavailability and by the fraction actually converted, so smaller, although the formed concentrations shown are unaffected. The peripheral volume varies very widely between patients (CV 109%), so an individual's metabolite curve can differ greatly from this typical one, particularly in how slowly it rises.
