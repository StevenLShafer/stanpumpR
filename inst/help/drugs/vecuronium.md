### The model

Vecuronium's kinetics are from Vega-Villa and colleagues (*Br J Anaesth* 2014;112:304-310), who compared 20 adults with major burns with 20 age- and sex-matched surgical controls after 0.12 mg/kg, sampling for 12 hours. The library uses their **non-burn** reference values: a **three-compartment** model with V1 10.25 L, V2 9.7 L, V3 43.79 L, clearance 0.51 L/min and distribution clearances 0.66 and 0.095 L/min. The long sampling found a terminal half-life of about 6.6 hours, at concentrations far below any neuromuscular effect; older, shorter studies reported 1 to 2 hours.

### Covariates

Size only. The published parameters are absolute, not per kilogram, and burn status was the only covariate the source retained. With *Adjust weight to fat-free mass* ticked, the volumes scale with the patient's fat-free mass relative to the 70 kg, 170 cm reference man and the clearances with that ratio to the 0.75 power; unticked, the parameters are used as published.

### Effect site

The effect-site rate constant, 0.26/min, is from Heier and colleagues (*Anesth Analg* 1994;78:973-977), measured at the adductor pollicis in normothermic adults under isoflurane, nitrous oxide and fentanyl. Following Minto and colleagues (*Anesthesiology* 2003;99:324-333), a ke0 should travel to another kinetic model as the time to peak effect it gives with its own kinetics. Heier's kinetics have not yet been checked, so the time to peak effect, 5.9 minutes, is provisional: it is the peak that 0.26/min gives against the Vega-Villa model.

### Typical concentrations and recovery

Heier's steady-state concentration for 50% twitch depression is 79 ng/mL, with a slope of 4.4. From those, the shaded band runs from 100 ng/mL (the first twitch recovered to 25% of baseline) through 130 ng/mL (90% block) to 155 ng/mL (95% block), and the default recovery threshold is 100 ng/mL, so *Time until threshold* is the time to 25% recovery. The plot shows concentration, not twitch height or train-of-four counts. As a check, 0.1 mg/kg in a 70 kg adult gives complete block and 25% recovery at about 36 minutes, against a labelled clinical duration of 25 to 40 minutes.

### Where to be careful

Vecuronium's 3-desacetyl metabolite is active and accumulates in renal failure; it is not modelled. Duration is prolonged by hepatic and renal dysfunction, by the inhaled agents and by hypothermia, and burns cause resistance; none of these is modelled. Reversal by sugammadex or neostigmine does not change the vecuronium row.
