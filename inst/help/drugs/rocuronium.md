### The model

Rocuronium's parameters are from Plaud and colleagues (*Clin Pharmacol Ther* 1995;58:185-191), who modelled rocuronium's kinetics and its effect at the vocal cords and the adductor pollicis. It is a **two-compartment** model: a central volume of 0.056 L/kg with elimination and one peripheral compartment.

### Covariates

Weight only, scaling the volumes and clearances linearly.

### Effect site

The time to peak effect is 2.2 minutes, from Cortínez and colleagues (*Br J Anaesth* 2007;99:679-685). Plaud's own ke0 at the adductor pollicis (0.168/min, present in the file as `k41` but unused) would give a much later peak; the two sites of effect differ, the vocal cords equilibrating faster, and the library uses the faster value.

### Typical concentrations and recovery

The shaded band is 1 to 2.2 mcg/mL and the default recovery threshold 1 mcg/mL. Here "effect" is a concentration, not a twitch height: the model does not include the sigmoid relationship between concentration and block, so the plot cannot show train-of-four counts. The threshold of 1 mcg/mL is roughly the concentration at which recovery of neuromuscular function begins, and *Time until threshold* is then the time to the start of recovery, not to full reversal. See [the rocuronium scenario](scenario:rocuronium-recovery).

### Where to be careful

Rocuronium's duration is prolonged by hepatic and renal dysfunction, by the inhaled agents (a pharmacodynamic potentiation) and by hypothermia, and shortened by sugammadex, none of which is modelled. Plaud's patients were adults under propofol-opioid anesthesia.
