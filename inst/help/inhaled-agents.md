The inhaled anesthetics are entered in the dose table like any other drug. Seven entries appear in the drug list:

| Entry | Units | Meaning |
|---|---|---|
| air, oxygen, nitrousOxide | L/min | Fresh gas flows at the flowmeters |
| sevoflurane, isoflurane, desflurane | % | Vaporizer settings |
| ventilation | L/min | Minute ventilation |

Set the flows, the vaporizer and the ventilation, and the program simulates alveolar and brain tensions for each agent, the inspired and alveolar oxygen, and a **MAC equivalents** panel. Nitrogen is carried implicitly and washes out; you do not enter it.

The model is Gas Man®'s, the model of inhaled anesthetic uptake and distribution developed by James H. Philip, with a handful of deliberate differences listed under [Where the gas engine differs from Gas Man](help:models/gas-differences). The equations are under [The inhaled-gas engine](help:models/gas-engine).

## What is plotted

For each agent two lines, as for an intravenous drug:

- The "plasma" line is the **alveolar (end-tidal) tension**, the quantity the monitor measures.
- The "effect site" line is the **vessel-rich group (brain) tension**.

Both are in per cent of one atmosphere. An **oxygen** panel shows the inspired and alveolar oxygen whenever any gas flow is running, because a hypoxic mixture should be visible whatever else is given.

## MAC and MAC equivalents

Two meanings of "MAC" are kept apart.

**MAC** is a property of an agent: the alveolar concentration at which half of patients do not move to a surgical incision. For sevoflurane it is 2.1 per cent at age 40. It falls with age, by about 6 per cent per decade (the Mapleson relation), so it is 2.5 per cent at age 10 and 1.6 per cent at age 80. It does not change during an anesthetic.

The **MAC equivalents** panel shows the patient's alveolar concentration *as a multiple of that MAC*, summed over the potent agents present, including nitrous oxide. "1 MAC of sevoflurane" means an alveolar concentration of one MAC equivalent. Agents given together are treated as additive, and one number is what is titrated to.

## Rules the dose table enforces

- **Ventilation must be greater than zero** whenever a gas is being given; without it nothing carries gas to the alveoli. If you enter a gas and there is no ventilation row, one is added: 5.7 L/min at 70 kg, scaled to the patient by (weight/70)^0.75. That is the minute ventilation whose alveolar part is Gas Man's default of 4 L/min.
- **Entering nitrous oxide adds an oxygen row** at 21 per cent of the fresh gas if there is none.
- Gas flows and ventilation are **rounded to 0.1 L/min**.

Ventilation is **minute ventilation**. Thirty per cent of it is taken to be dead space, so the alveolar ventilation, which exchanges gas, is 70 per cent of what you enter.

Cardiac output is fixed at Gas Man's default of 5 L/min at 70 kg, scaled by (weight/70)^0.75, and is not currently a user input.

## Fresh gas flow and rebreathing

The breathing circuit is the "ideal" circle system: once the fresh gas flow reaches the minute ventilation there is no rebreathing and the patient inspires fresh gas and nothing else. Below that, the shortfall is made up with exhaled gas. There is no circuit volume, so a change at the vaporizer reaches the patient at once. At low flows the alveolar concentration lags the vaporizer setting and the oxygen consumed (3.5 mL/kg/min) is no longer there to dilute what remains, so concentrations run higher than the fresh gas would suggest. [The sevoflurane wash-in scenario](scenario:sevoflurane-washin) shows this.

## The concentration and second gas effects

Nitrous oxide taken up in bulk concentrates whatever else is in the alveolus, so sevoflurane rises faster in its presence. The engine reproduces this coupling, as Gas Man does. [The second gas effect scenario](scenario:second-gas-effect) shows it.

## Opioids and MAC

Opioids lower MAC. Under **Graph Options**, ticking *Include opioid - MAC interaction* reports MAC equivalents relative to the opioid-reduced MAC, so the same end-tidal concentration reads as more MAC equivalents when an opioid is on board. The gas concentrations themselves do not change. This is an approximate model and is expected to be replaced; see [Opioid reduction of MAC](help:models/opioid-mac).

## Time until threshold for the gases

*Time until threshold* works for the inhaled agents and for MAC equivalents as for the intravenous drugs: at each moment, how long until the concentration would fall to the threshold if the agent were turned off then. For a gas "turned off" means the vaporizer or the nitrous oxide is turned off, the fresh gas flow is turned up so that there is no rebreathing (which is what is done to wake a patient), and ventilation stays as it is. Each agent is its own decision, so each panel shows the time for that agent alone.

| Panel | What is timed | Default threshold |
|---|---|---|
| sevoflurane, isoflurane, desflurane | Brain (vessel-rich group) tension | 0.1 × the age-adjusted MAC of that agent |
| nitrous oxide | Brain tension | 10% |
| MAC equivalents | The summed alveolar series, with every agent turned off | 0.1 MAC equivalents |
| oxygen | Not timed | |

The thresholds can be changed in the Drug Thresholds dialog, where the volatile agents are shown at the patient's age. See [Time until threshold](help:models/recovery) and the [emergence scenarios](help:scenarios/emergence-sevoflurane).

## Validation

The engine has been checked against Gas Man itself across five scenarios, with each disagreement traced to its cause. Richard H. Epstein ran the Gas Man side of the comparison. The record is in the repository at `inst/validation/VALIDATION.md`; see [Testing and validation](help:validation).
