### The model

Ketamine's parameters are from Domino and colleagues (*Clin Pharmacol Ther* 1984;36:645-653), who gave racemic ketamine intravenously to volunteers. The model is a central volume of 0.063 L/kg with fixed rate constants; the elimination rate constant of 0.44/min from a small central volume gives a high clearance, about 1.9 L/min at 70 kg.

### Covariates

Weight only, scaling the volumes and clearances linearly.

### Effect site

The time to peak effect is 3 minutes and is described in the code as a guess.

### Typical concentrations

The shaded band, 0.1 to 0.16 mcg/mL, is an **analgesic** range for sub-anesthetic ketamine, from a 2013 review cited in the code. Anesthetic concentrations are an order of magnitude higher, around 1 to 2 mcg/mL. The default recovery threshold is 0.1 mcg/mL. See [the ketamine infusion scenario](scenario:ketamine-infusion).

### Where to be careful

Norketamine, an active metabolite with about a third of ketamine's potency that accumulates during infusions, is not modelled. The S-enantiomer, used alone in many countries, has different kinetics. Domino's volunteers were young and healthy.
