The inhaled anesthetics are simulated by a separate engine whose structure and parameters follow **Gas Man®**, the model of inhaled anesthetic uptake and distribution developed by James H. Philip at Brigham and Women's Hospital and Harvard Medical School. Gas Man is closed source; the engine was written from the published description of its model and then validated against Gas Man's own output. This page gives the model; the deliberate departures from Gas Man are on [their own page](help:models/gas-differences).

## Compartments

For each soluble gas (nitrous oxide, sevoflurane, isoflurane, desflurane, and nitrogen, which is always carried) the state is five gas tensions in per cent of one atmosphere: the breathing circuit, the alveolar gas, and three tissue groups.

| Compartment | Volume at 70 kg | Share of cardiac output |
|---|---|---|
| Alveolar gas (FRC) | 2.5 L | |
| Vessel-rich group (brain) | 6 L | 0.76 |
| Muscle group | 33 L | 0.18 |
| Fat group | 14.5 L | 0.06 |

Volumes scale linearly with weight. Cardiac output is 5 L/min at 70 kg, scaled by (weight/70)^0.75. The blood-flow fractions are Gas Man's `[Ratio]` values, carried unchanged because they are part of the validation chain; the code notes that no source for them has been established. Oxygen is modelled in the gas phase only, with a metabolic sink of 3.5 mL/kg/min: it binds haemoglobin non-linearly and has no meaningful partition coefficient.

## Partition coefficients

The capacity of a tissue for a gas is its volume times its tissue:gas partition coefficient. The table stores tissue:gas coefficients directly, as Gas Man's configuration does:

| Gas | Blood:gas | Brain:gas | Muscle:gas | Fat:gas | MAC at 40 (%) |
|---|---|---|---|---|---|
| nitrous oxide | 0.47 | 0.42 | 0.54 | 1.08 | 110 |
| sevoflurane | 0.65 | 1.1 | 2.4 | 34 | 2.1 |
| isoflurane | 1.3 | 2.1 | 4.5 | 70 | 1.1 |
| desflurane | 0.42 | 0.54 | 0.97 | 13 | 6.0 |
| nitrogen | 0.014 | 0.010 | 0.014 | 0.070 | (200, flagged) |

Nitrogen's MAC of 200 per cent is Gas Man's own figure, carried as it stands and flagged: Eger's estimate is about 55 times higher, and nitrogen is not summed into MAC, so the value is inert. Each gas's page under [Drug library](help:drugs/index) shows these values.

## Equations

Fresh gas composition follows from the flowmeter and vaporizer settings; the vapour dilutes the carrier gases. The circuit is the ideal circle system: with fresh gas flow Q and minute ventilation MV (alveolar ventilation VA = 0.7 MV),

```
Q >= MV:  F_circuit = F_fresh                              no rebreathing
Q <  MV:  F_circuit = f F_fresh + (1 - f) F_alveolar,   f = Q / (VA + 0.3 Q)
```

The alveolar tension of gas i changes with ventilation and with uptake into the blood,

```
V_alv dF_alv/dt = VA (F_circuit - F_alv) - uptake_i + F_alv × (total uptake + VO2 correction)
```

where the last term is the **concentration and second gas effect**: gas taken up in bulk (and oxygen consumed) shrinks the alveolar volume, concentrating what remains. Uptake of each gas is cardiac output times the blood:gas coefficient times the alveolar-to-mixed-venous tension difference; each tissue takes up at its share of cardiac output times the arterial-to-tissue difference divided by its capacity. Mixed venous tension is the flow-weighted mean of the tissue tensions.

## Integration

Within each segment between dose-table changes the system is linear with constant coefficients once the total-uptake coupling is linearised at the segment's start, and is advanced exactly by matrix exponential (a Padé approximation with scaling and squaring). Gas Man splits each time step into sequential sub-updates; the two agree as the step shrinks, and the repository's convergence test checks that they do.

## MAC

MAC is age-adjusted by the Mapleson relation, MAC(age) = MAC40 × 10^(−0.00269 (age − 40)), about 6 per cent per decade. The MAC-equivalents series is the sum over potent agents of alveolar tension divided by the agent's age-adjusted MAC. Nitrous oxide is included.

## Validation

Richard H. Epstein ran Gas Man through its API for the same scenarios and the outputs were compared column by column. In the first concordance run the two agreed to one part in a million on the alveolar tension, with the residual traced to Gas Man's single-precision arithmetic. Five further scenarios covering low flows, other agents, other weights and changing settings followed. The record is `inst/validation/VALIDATION.md`; see [Testing and validation](help:validation).

## References

Philip JH. Gas Man: an example of goal oriented computer-assisted teaching which results in learning. *Int J Clin Monit Comput* 1986;3:165-173.

Weber J, Schmidt J, Wirth S, Schumann S, Philip JH, Eberhart LHJ. Context-sensitive decrement times for inhaled anesthetics in obese patients explored with Gas Man®. *J Clin Monit Comput* 2021 (PMC7943506).

Hendrickx JFA, Lemmens HJM, Shafer SL. Do distribution volumes and clearances relate to tissue volumes and blood flows? A computer simulation. *BMC Anesthesiol* 2006;6:7.

Mapleson WW. Effect of age on MAC in humans: a meta-analysis. *Br J Anaesth* 1996;76:179-185.
