The inhaled anesthetics are simulated by a separate engine whose structure and parameters follow **GasMan®**, the model of inhaled anesthetic uptake and distribution developed by James H. Philip at Brigham and Women's Hospital and Harvard Medical School. GasMan's source code is published under the GPL-3.0 licence ([gasmanonline](https://github.com/rasman/gasmanonline)); the engine was written from the published description of its model, with a few defaults read from that source, and then validated against GasMan's own output. This page gives the model; the deliberate departures from GasMan are on [their own page](help:models/gas-differences).

## Compartments

For each soluble gas (nitrous oxide, sevoflurane, isoflurane, desflurane, and nitrogen, which is always carried) the state is five gas tensions in per cent of one atmosphere: the breathing circuit, the alveolar gas, and three tissue groups.

| Compartment | Volume at 70 kg | Share of cardiac output |
|---|---|---|
| Alveolar gas (FRC) | 2.5 L | |
| Vessel-rich group (brain) | 6 L | 0.76 |
| Muscle group | 33 L | 0.18 |
| Fat group | 14.5 L | 0.06 |

Volumes scale linearly with weight. Cardiac output is 5 L/min at 70 kg, scaled by (weight/70)^0.75. The blood-flow fractions are GasMan's `[Ratio]` values, carried unchanged because they are part of the validation chain; the code notes that no source for them has been established. Oxygen is modelled in the gas phase only, with a metabolic sink of 3.5 mL/kg/min: it binds haemoglobin non-linearly and has no meaningful partition coefficient.

## Partition coefficients

The capacity of a tissue for a gas is its volume times its tissue:gas partition coefficient. The table stores tissue:gas coefficients directly, as GasMan's configuration does:

| Gas | Blood:gas | Brain:gas | Muscle:gas | Fat:gas | MAC at 40 (%) |
|---|---|---|---|---|---|
| nitrous oxide | 0.47 | 0.42 | 0.54 | 1.08 | 104 |
| sevoflurane | 0.65 | 1.1 | 2.4 | 34 | 1.80 |
| isoflurane | 1.3 | 2.1 | 4.5 | 70 | 1.17 |
| desflurane | 0.42 | 0.54 | 0.97 | 13 | 6.6 |
| nitrogen | 0.014 | 0.010 | 0.014 | 0.070 | (200, flagged) |

The partition coefficients are GasMan's. The MACs at 40 are Mapleson's (*Br J Anaesth* 1996;76:179-185), the meta-analysis whose age relation the engine applies, adopted on 23 November 2026: the program was validated against GasMan first and has since moved to more recent research. GasMan's own, which the US labels share, were 110, 2.1, 1.1 and 6.0 per cent; MAC enters no uptake, so that validation is unchanged. Nitrogen's MAC of 200 per cent is GasMan's own figure, carried as it stands and flagged: the estimate of Koblin, Eger and colleagues, 110 atmospheres in rats (*Anesth Analg* 1998;87:419-424), is about 55 times higher, and nitrogen is not summed into MAC, so the value is inert. Each gas's page under [Drug library](help:drugs/index) shows these values.

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

k is the share of the exhaled gas that is rebreathed, so it cannot exceed one. It would when the fresh gas flow is less than the uptake plus the carbon dioxide absorbed (Q < U + VCO2; with no agent being taken up, that is the oxygen consumed, 0.25 L/min at 70 kg). The circuit is then closed: everything exhaled is rebreathed. On emergence, when the total uptake is negative, the alveoli push gas out (P, minus the total uptake; zero otherwise), and that gas is in the circuit too: it is rebreathed as far as the shortfall needs (R of it) and only the rest is vented. Unless it covers the shortfall, nothing is vented and still the circuit falls short, so a real reservoir bag empties. The engine rebreathes all the exhaled gas, and the pushed-out gas the shortfall needs, and no more, and the alveoli receive only what the circuit holds:

```
Q < U + VCO2:  k = 1,  R = min(U + VCO2 - Q, P)           pushed-out gas rebreathed
               D = Q + VA - VCO2 + R                       what reaches the alveoli
               F_circuit = (Q F_fresh + (VA + R) F_alveolar) / D
               s = D / (VA + U)                            the share of their draw the circuit meets
```

Each gas then gains only what the fresh gas brings, and loses only what the blood takes up and what is vented of the pushed-out gas, P − R, which is the circuit's surplus, max(0, Q + P − VCO2), and none while the circuit is short; no gas is created, and none disappears. The alveolar volume is fixed in the model, so the volume the circuit cannot replace appears as gas tensions summing to less than an atmosphere rather than as an emptying bag; with no fresh gas at all the oxygen is used up, as it would be in life. Likewise with no ventilation, the alveoli draw what is taken up from the fresh gas, and no more than the fresh gas supplies.

The alveolar tension of gas i, written here as a fraction, changes with ventilation and with uptake into the blood (each term in L/min),

```
V_alv dF_alv/dt = s (VA + U) F_circuit - VA F_alv - uptake_i - P F_alv
```

with s = 1 except in the closed circuit above. U F_circuit and P F_alv are the **concentration and second gas effect**: gas taken up in bulk shrinks the alveolar volume, so make-up gas is drawn in from the circuit, concentrating what remains; when gas comes back out of the blood, as on emergence, alveolar gas is pushed out instead. In the closed circuit the first term is Q F_fresh + (VA + R) F_alv, so the equation becomes V_alv dF_alv/dt = Q F_fresh − uptake_i − (P − R) F_alv. The total uptake is the sum of every soluble gas's uptake (nitrogen included) plus the oxygen consumed less the carbon dioxide that replaces it, VO2 × (1 − 0.8). Uptake of each gas is cardiac output times the blood:gas coefficient times the alveolar-to-mixed-venous tension difference; each tissue takes up at its share of cardiac output times the arterial-to-tissue difference divided by its capacity. Mixed venous tension is the flow-weighted mean of the tissue tensions.

## Integration

Between dose-table changes the settings are constant, and every equation above would be linear with constant coefficients but for one term: the total uptake that couples the gases depends on the state. The engine therefore advances in short steps. Within each step the total uptake (and with it the circuit blend) is held at its value at the start of the step, which makes the step linear, and the step is then solved exactly by matrix exponential (a Padé approximation with scaling and squaring). The uptake is recomputed at the start of the next step. So the propagation within a step is exact, but the coupling between steps is an approximation of first order in the step size: the answer depends slightly on the step and converges as it shrinks. GasMan holds its total uptake per step in the same way and in addition splits each step into sequential sub-updates; the two converge to the same answer as the step shrinks, and the repository's convergence test checks that they do.

The steps are short just after each change of setting, when the uptake changes fastest, and grow from there: the first is a hundredth of a minute, and each is a tenth longer than the one before, until they reach the plot length divided by 600 (0.1 minute on a one-hour plot, 2.4 minutes on a day). So each step is about a tenth of the time since the last change, and the error is about the same on any plot length. Measured against the same engine with much shorter steps, in a nitrous oxide induction (4 L/min nitrous oxide in 6 L/min, with 2 per cent sevoflurane, as in [the second gas effect scenario](scenario:second-gas-effect)), in its emergence, and in the same induction in a 4 kg infant, on plots of an hour, a day and a week, the alveolar tensions were never more than 0.6 per cent of their value from the reference. Without nitrous oxide the coupling is weak and the error smaller still: under 0.1 per cent of the agent's peak in the teaching and validation scenarios even with the earlier, longer steps.

Until 23 November 2026 every step was the plot length divided by 600, so a day's plot stepped 2.4 minutes from the start of a nitrous oxide induction, and its first points read 13 per cent low (ChatGPT's audit of the gas engine found it; the page then advised a short plot).

## MAC

MAC is age-adjusted by the Mapleson relation, MAC(age) = MAC40 × 10^(−0.00269 (age − 40)), about 6 per cent per decade, with MAC40 Mapleson's own values from the same meta-analysis (the table above). The MAC-equivalents series is the sum over potent agents of alveolar tension divided by the agent's age-adjusted MAC. Nitrous oxide is included.

## Validation

Richard H. Epstein ran GasMan through its API for the same scenarios and the outputs were compared column by column. In the first concordance run the two agreed to one part in a million on the alveolar tension, with the residual traced to GasMan's single-precision arithmetic. Five further scenarios covering low flows, other agents, a 100 kg patient and changing settings followed, run with GasMan's own program built from its source; their 520 values are frozen in the repository and checked on every build, and on 24 November 2026 a sixth, a 5 kg patient, was added. The record is `inst/validation/VALIDATION.md`; see [Testing and validation](help:validation).

## References

Philip JH. GasMan: an example of goal oriented computer-assisted teaching which results in learning. *Int J Clin Monit Comput* 1986;3:165-173.

Weber J, Schmidt J, Wirth S, Schumann S, Philip JH, Eberhart LHJ. Context-sensitive decrement times for inhaled anesthetics in obese patients explored with GasMan®. *J Clin Monit Comput* 2021 (PMC7943506).

Hendrickx JFA, Lemmens HJM, Shafer SL. Do distribution volumes and clearances relate to tissue volumes and blood flows? A computer simulation. *BMC Anesthesiol* 2006;6:7.

Mapleson WW. Effect of age on MAC in humans: a meta-analysis. *Br J Anaesth* 1996;76:179-185.
