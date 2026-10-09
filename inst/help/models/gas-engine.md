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

Fresh gas composition follows from the flowmeter and vaporizer settings; the vapour dilutes the carrier gases. Air is split into 20.93 per cent oxygen and 78.07 per cent nitrogen; its remaining 1 per cent, mostly argon, is not carried.

The circuit is the ideal circle system. With fresh gas flow Q, minute ventilation MV (alveolar ventilation VA = 0.7 MV) and U the total uptake when it is positive (zero otherwise), the patient inspires MV + U and the circuit supplies it:

```
Q >= MV + U:  F_circuit = F_fresh                          no rebreathing
Q <  MV + U:  F_circuit = f F_fresh + g F_alveolar
              k = (MV + U - Q) / (MV (1 - c_E))            exhaled gas rebreathed, per unit exhaled
              D = MV + U - k (MV - VA)
              f = Q / D,   g = k VA / D
```

Here c_E is the carbon dioxide fraction of exhaled gas (carbon dioxide production over minute ventilation), which the absorber removes from whatever is rebreathed. With no uptake and no carbon dioxide this reduces to f = Q / (VA + 0.3 Q), g = 1 − f, and the threshold is the familiar Q = MV; the uptake raises it slightly, by about 0.05 L/min on oxygen and sevoflurane and by up to about 0.9 L/min early in an induction with 4 L/min of nitrous oxide in 6 L/min. See [Carbon dioxide](help:models/gas-differences) on the comparison page.

The alveolar tension of gas i changes with ventilation and with uptake into the blood,

```
V_alv dF_alv/dt = VA (F_circuit - F_alv) - uptake_i + total uptake × F_circuit    (total uptake > 0)
                                                    + total uptake × F_alv        (total uptake < 0)
```

where the last term is the **concentration and second gas effect**: gas taken up in bulk shrinks the alveolar volume, so make-up gas is drawn in from the circuit, concentrating what remains; when gas comes back out of the blood, as on emergence, alveolar gas is pushed out instead. The total uptake is the sum of every soluble gas's uptake (nitrogen included) plus the oxygen consumed less the carbon dioxide that replaces it, VO2 × (1 − 0.8). Uptake of each gas is cardiac output times the blood:gas coefficient times the alveolar-to-mixed-venous tension difference; each tissue takes up at its share of cardiac output times the arterial-to-tissue difference divided by its capacity. Mixed venous tension is the flow-weighted mean of the tissue tensions.

## Integration

Between dose-table changes the settings are constant, and every equation above would be linear with constant coefficients but for one term: the total uptake that couples the gases depends on the state. The engine therefore advances in short steps. Within each step the total uptake (and with it the circuit blend) is held at its value at the start of the step, which makes the step linear, and the step is then solved exactly by matrix exponential (a Padé approximation with scaling and squaring). The uptake is recomputed at the start of the next step. So the propagation within a step is exact, but the coupling between steps is an approximation of first order in the step size: the answer depends slightly on the step and converges as it shrinks. Gas Man holds its total uptake per step in the same way and in addition splits each step into sequential sub-updates; the two converge to the same answer as the step shrinks, and the repository's convergence test checks that they do.

The step is about the plot length divided by 600: 0.1 minute on a one-hour plot, 0.4 minute on a four-hour plot, 2.4 minutes on a day. Measured against the same engine at a 40-times finer step:

- **Without nitrous oxide** the coupling is weak. In the teaching scenarios and the Gas Man validation scenarios that use no nitrous oxide, on their own plot lengths of half an hour to four hours, the alveolar tensions agree to within 0.003 percentage points, under 0.1 per cent of the agent's peak.
- **During a nitrous oxide wash-in** the coupling is strong. In [the second gas effect scenario](scenario:second-gas-effect) (4 L/min nitrous oxide in 6 L/min, with 2 per cent sevoflurane) the alveolar nitrous oxide and sevoflurane run low at the first plotted points, by about 1.5 per cent of their value on a one-hour plot, 6 per cent on a four-hour plot and 13 per cent on a 24-hour plot. The error fades as uptake slows, to under 1 per cent after about 1, 2 and 12 minutes respectively, and roughly halves when the step is halved.

To read the first minutes of a nitrous oxide induction closely, use a short plot.

## MAC

MAC is age-adjusted by the Mapleson relation, MAC(age) = MAC40 × 10^(−0.00269 (age − 40)), about 6 per cent per decade. The MAC-equivalents series is the sum over potent agents of alveolar tension divided by the agent's age-adjusted MAC. Nitrous oxide is included.

## Validation

Richard H. Epstein ran Gas Man through its API for the same scenarios and the outputs were compared column by column. In the first concordance run the two agreed to one part in a million on the alveolar tension, with the residual traced to Gas Man's single-precision arithmetic. Five further scenarios covering low flows, other agents, other weights and changing settings followed. The record is `inst/validation/VALIDATION.md`; see [Testing and validation](help:validation).

## References

Philip JH. Gas Man: an example of goal oriented computer-assisted teaching which results in learning. *Int J Clin Monit Comput* 1986;3:165-173.

Weber J, Schmidt J, Wirth S, Schumann S, Philip JH, Eberhart LHJ. Context-sensitive decrement times for inhaled anesthetics in obese patients explored with Gas Man®. *J Clin Monit Comput* 2021 (PMC7943506).

Hendrickx JFA, Lemmens HJM, Shafer SL. Do distribution volumes and clearances relate to tissue volumes and blood flows? A computer simulation. *BMC Anesthesiol* 2006;6:7.

Mapleson WW. Effect of age on MAC in humans: a meta-analysis. *Br J Anaesth* 1996;76:179-185.
