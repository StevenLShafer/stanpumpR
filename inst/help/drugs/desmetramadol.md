### The model

Desmetramadol (O-desmethyltramadol, or M1) is the active metabolite that carries [tramadol](help:drugs/tramadol)'s μ-opioid effect. Its disposition is from the same joint model as the parent (Allegaert and colleagues, *Clin Pharmacokinet* 2015;54:167-178): two compartments, a clearance of 71.4 L/h, a central volume of 78.9 L, a peripheral volume of 133 L and an intercompartmental clearance of 562 L/h at 70 kg, with clearances scaling to the 0.75 power of size and volumes linearly. The clearance matures as glomerular filtration does, 0.36 of the adult value at term and 0.90 at one year; the authors found the metabolite's elimination indistinguishable from it. Until October 2026 these were Holford and colleagues' adult values (*J Pharmacol Clin Toxicol* 2014;2:1023): 84.2 L/h, 131 L and 274 L/h, with no maturation.

### Not given directly

Desmetramadol is a real drug and has been given to humans, but this parameter set cannot predict what a direct dose does. Neither Allegaert's group nor Holford's before it had human intravenous metabolite data; both fixed the metabolite's central volume from measurements in dogs given the metabolite intravenously. Multiplying the metabolite's volumes, clearances and the formation clearance feeding it by any common factor leaves the *formed* concentrations unchanged, so the concentrations plotted after a tramadol dose are identified, but the scale for a direct dose is not. Desmetramadol is therefore offered with **no dosing unit at all**: it appears only when tramadol is given. See [Active metabolites](help:models/metabolites).

### Effect site

ke0 is **supplied directly** rather than solved from a tPeak. The engine solves ke0 so that the effect site peaks at the observed tPeak against whichever plasma curve the observation followed, but neither the intravenous nor the oral curve applies here: desmetramadol has no curve of its own, and its peak effect is observed after an oral dose of the *parent*, whose profile the engine cannot build at the point it resolves this drug. So the value, ke0 = 0.0930/min (an equilibration half-time of about 7.5 minutes), was solved once against the metabolite profile formed from 100 mg of oral tramadol in a normal metaboliser, giving an effect-site peak at 150 minutes, and recorded. On Allegaert's disposition the formed metabolite already peaks at about 138 minutes, so the equilibration needed is short; on Holford's, where it peaked at 93 minutes, it was 24 minutes. It is **provisional**: the 2.5-hour peak analgesia it was solved from carries no citation yet, and it is valid only for tramadol's current absorption, formation and first-pass parameters.

### MEAC

The MEAC, 84 ng/mL, is a **working value**, not a measured minimum effective concentration of M1. Lee 2019's introduction calls it one, citing earlier work, and the number matches Grond and colleagues (*Br J Clin Pharmacol* 1999;48:254-257): in patients using racemic tramadol for patient-controlled analgesia after surgery, the mean serum concentration of total M1 sampled just before each demand was 84 ± 34 ng/mL. At those moments tramadol itself was at 590 ng/mL, and part of the analgesia was the parent's, so the figure describes a mixture, not M1 alone. It is also total racemic M1, whereas the μ activity belongs to the (+) enantiomer. No study has measured analgesia against M1 alone, so the value is kept as the divisor for the MEAC panel; read percentages of it as rough. The shaded band, 20 to 80 ng/mL, is a range of formed concentrations, not an analgesic range.

### Covariates

Weight, by Allegaert's allometry, and age, through the maturation of the clearance; under the default [fat-free-mass scaling](help:models/fat-free-mass) the same exponents apply to the fat-free-mass ratio, and with the switch off to weight/70, as published. Age, height and sex do not enter except through fat-free mass.

### Where to be careful

The absolute concentration scale is not identified, only the formed concentrations relative to the tramadol dose. The two enantiomers of M1 are formed and eliminated stereoselectively and the model carries total racemic M1, which matters for anyone comparing with an enantiomer-specific analgesia benchmark.
