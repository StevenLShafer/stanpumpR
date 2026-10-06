### The model

Desmetramadol (O-desmethyltramadol, or M1) is the active metabolite that carries [tramadol](help:drugs/tramadol)'s μ-opioid effect. Its disposition is from the same joint model as the parent (Holford and colleagues, *J Pharmacol Clin Toxicol* 2014;2:1023): two compartments, a clearance of 84.2 L/h, a central volume of 78.9 L and a peripheral volume of 131 L at 70 kg, with clearances scaling to the 0.75 power of size and volumes linearly.

### Not given directly

Desmetramadol is a real drug and has been given to humans, but this parameter set cannot predict what a direct dose does. Holford had no human intravenous metabolite data and fixed the metabolite's central volume from measurements in three dogs. Multiplying the metabolite's volumes, clearances and the formation clearance feeding it by any common factor leaves the *formed* concentrations unchanged, so the concentrations plotted after a tramadol dose are identified, but the scale for a direct dose is not. Desmetramadol is therefore offered with **no dosing unit at all**: it appears only when tramadol is given. See [Active metabolites](help:models/metabolites).

### Effect site

ke0 is **supplied directly** rather than solved from a tPeak. The engine solves ke0 so that the effect site peaks at the observed tPeak against whichever plasma curve the observation followed, but neither the intravenous nor the oral curve applies here: desmetramadol has no curve of its own, and its peak effect is observed after an oral dose of the *parent*, whose profile the engine cannot build at the point it resolves this drug. So the value, ke0 = 0.0288/min (an equilibration half-time of about 24 minutes), was solved once against the metabolite profile formed from 100 mg of oral tramadol in a normal metaboliser, giving an effect-site peak at 150 minutes, and recorded. It is **provisional**: the 2.5-hour peak analgesia it was solved from carries no citation yet, and it is valid only for tramadol's current absorption, formation and first-pass parameters.

### MEAC

The MEAC, 84 ng/mL, is cited but needs primary verification: Lee 2019's introduction gives it, citing earlier work that was not retrieved. It is the right quantity in the right units, a minimum effective concentration of total M1, which is a firmer basis than most of the provisional potencies in the library, but the analyte (total racemic M1 against the active enantiomer) should be checked.

### Covariates

Weight, by Holford's allometry; under the default [fat-free-mass scaling](help:models/fat-free-mass) the same exponents apply to the fat-free-mass ratio. Age, height and sex do not enter.

### Where to be careful

The absolute concentration scale is not identified, only the formed concentrations relative to the tramadol dose. The two enantiomers of M1 are formed and eliminated stereoselectively and the model carries total racemic M1, which matters for anyone comparing with an enantiomer-specific analgesia benchmark.
