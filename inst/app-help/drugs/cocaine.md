### A research model, behind an opt-in

Cocaine is one of stanpumpR's **illicit-drug research models**. These are hidden until you turn on **Show illicit drugs** above the dose table (off by default), and their names appear in red in the dose table. The curve is a *modelled plasma concentration* offered for research and teaching. It is not dosing advice, a safety threshold, an impairment or intoxication prediction, or an individual clinical prediction. It describes pharmaceutical cocaine hydrochloride insufflated under study conditions, not street cocaine, whose purity, adulterants, route and co-exposures commonly dominate what happens to a person.

### The model, and the reduction inside it

The disposition is Chow and colleagues' (Chow MJ, Ambre JJ, Ruo TI, et al. *Clin Pharmacol Ther* 1985;38(3):318-324), who fitted a two-compartment model in five adults and published its summaries: a steady-state volume of distribution of 131.8 L (1.96 L/kg), an elimination clearance of 2.10 L/min and a terminal half-life of 48 minutes. The micro-constants are not in the accessible report, so the two-compartment model cannot be rebuilt.

What stanpumpR carries is therefore a **one-compartment reduction** that uses Chow's steady-state volume as a stand-in for the central volume, giving a half-life of 43.5 minutes. That sits between Javaid's independent one-compartment intravenous fit (41.4 minutes) and Chow's own 48 minutes.

### Why only the intranasal route is offered

The reduction's weakness is the early distribution phase. Jeffcoat and colleagues measured a distinct distribution half-life of 11 minutes after an intravenous dose, with an elimination half-life of 78 minutes. A one-compartment model whose volume is the *steady-state* volume gets the first minutes after an intravenous bolus badly wrong, because the true central volume is much smaller. **Intravenous cocaine is therefore not offered.** Only the intranasal route is, where the concentrations are governed by absorption rather than by distribution.

Absorption and bioavailability are Jeffcoat's, as published: an absorption half-time of 11.7 minutes with a bioavailability of about 80% (Jeffcoat AR, Perez-Reyes M, Hill JM, et al. *Drug Metab Dispos* 1989;17(2):153-159).

### One nasal path, which is the simplification

A nasal dose does not take one route into the blood. Fattinger and colleagues deconvolved the systemic input and found **two peaks**, near 10 and 45 minutes: part of the dose crosses the nasal mucosa and the rest is swallowed and absorbed from the gut. The mucosal path accounted for 19% of the nasal dose (95% CI 11-26%) and contributed 31% of systemic exposure (95% CI 23-37%), while swallowed cocaine has a median oral bioavailability of 33%. Those three percentages have three different denominators and must never be multiplied together as if they were one mass balance.

stanpumpR carries Jeffcoat's **single** absorption path. It reproduces the overall intranasal exposure, not the two-peaked input shape.

One consequence is worth stating plainly: the exposure plotted here is Jeffcoat's. His bioavailability of about 80% is higher than Fattinger's protocol implies, so a nasal dose simulated here reaches a higher concentration than Fattinger's subjects would suggest. That difference belongs to the two protocols, not to an error in either.

### What is not modelled

- **No effect site.** Chow did estimate a biophase, putting the peak chronotropic effect 7.3 minutes after an intravenous bolus, with an initial slope of 0.3 beats per minute per ng/mL. But that fit also needed a *first-order attenuation* term, because the chronotropic effect declines faster than either the plasma or the biophase concentration. stanpumpR's single equilibration constant cannot carry the attenuation, and a ke0 taken without it would over-predict sustained tachycardia. So there is no effect site, no MEAC and no concentration band. No source establishes a universal euphoria EC50 or a toxicity threshold.
- **No metabolites.** Benzoylecgonine, ecgonine methyl ester, norcocaine and, with alcohol, cocaethylene are separate analytes with their own formation and elimination. Their formation fractions are not identified by the sources used here.
- **No oral or smoked route.** Oral bioavailability is dose-dependent (0.32 at 100 mg, 0.45 at 200 mg) and the systemic input peaks near 45 minutes, which no single first-order depot reproduces. Smoked cocaine is absorbed in about a minute, but how much survives heating depends on the device, so a bioavailability of the nominal load does not exist. stanpumpR has no pulmonary route. See [Research models of drugs of abuse](help:models/illicit-drugs) for the route-by-route record.

### Covariates

Neither source published a size covariate. With *Adjust weight to fat-free mass* on, the volume scales with the Al-Sallami fat-free mass ratio and clearance with its 0.75 power; with it off, both scale linearly with weight ÷ 70. That scaling is a stanpumpR convention, **not** a published population fit. Chow's subjects averaged about 67 kg, so reading his absolute parameters as a 70 kg adult's shifts them by about 4%.

### Where to be careful

Read this as an intranasal plasma-exposure estimate for pharmaceutical cocaine within the studied dose range, and nothing more: not an effect, not a potency, not a threshold, and not a prediction for any individual.
