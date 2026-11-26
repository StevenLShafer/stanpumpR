The inhaled anesthetics are entered in the dose table like any other drug. Seven entries appear in the drug list:

| Entry | Units | Meaning |
|---|---|---|
| air, oxygen, nitrousOxide | L/min | Fresh gas flows at the flowmeters |
| sevoflurane, isoflurane, desflurane | % | Vaporizer settings |
| ventilation | L/min | Minute ventilation |

Set the flows, the vaporizer and the ventilation, and the program simulates alveolar and brain tensions for each agent, the inspired and alveolar oxygen, and a **MAC equivalents** panel. Nitrogen is carried implicitly and washes out; you do not enter it.

The model is GasMan®'s, the model of inhaled anesthetic uptake and distribution developed by James H. Philip, with a handful of deliberate differences listed under [Where the gas engine differs from GasMan](help:models/gas-differences). The equations are under [The inhaled-gas engine](help:models/gas-engine).

## What is plotted

For each agent two lines, as for an intravenous drug:

- The "plasma" line is the **alveolar (end-tidal) tension**, the quantity the monitor measures.
- The "effect site" line is the **vessel-rich group (brain) tension**.

Both are in per cent of one atmosphere. An **oxygen** panel shows the alveolar oxygen whenever any gas flow is running, because a hypoxic mixture should be visible whatever else is given. Its second line is the **arterial** oxygen tension, the same as end-tidal unless an endobronchial tube shunts blood past the ventilated lung (see [Physiology](help:models/physiology)); the hover gives the PaO2 in mmHg and the SaO2.

## MAC and MAC equivalents

Two meanings of "MAC" are kept apart.

**MAC** is a property of an agent: the alveolar concentration at which half of patients do not move to a surgical incision. For sevoflurane it is 1.8 per cent at age 40, the value of Mapleson's 1996 meta-analysis of MAC and age (GasMan and the US label give 2.1 per cent; see [How stanpumpR differs from GasMan](help:models/gas-differences)). It falls with age, by about 6 per cent per decade (the same relation), so it is 2.2 per cent at age 10 and 1.4 per cent at age 80, at 37 °C. Its only change during an anesthetic is with body temperature, which lowers it (below and in [Physiology](help:models/physiology)).

The **MAC equivalents** panel shows the patient's alveolar concentration *as a multiple of that MAC*, summed over the potent agents present, including nitrous oxide. "1 MAC of sevoflurane" means an alveolar concentration of one MAC equivalent. Agents given together are treated as additive, and one number is what is titrated to.

The panel has two lines, as each agent's panel does. Its **plasma** line is the end-tidal MAC equivalents just described, and its *time until threshold* is read from that line. Its **effect site** line is the brain's: each agent's tension in the vessel-rich group, which lags the end-tidal tension, as a multiple of its MAC, summed over the agents (Steven Shafer, 25 November 2026). With the default line types (Graph Options: plasma line blank, effect-site line solid) the panel shows the brain's line. Turn on the plasma line to see the end-tidal line beside it: on induction the brain trails the end-tidal line, and on emergence it stays above it.

## Rules the dose table enforces

- **Ventilation must be greater than zero** whenever a gas is being given; without it nothing carries gas to the alveoli. If you enter a gas and there is no ventilation row, one is added: 5.7 L/min at 70 kg, scaled to the patient by (weight/70)^0.75. That is the minute ventilation whose alveolar part is GasMan's default of 4 L/min.
- **Entering nitrous oxide adds an oxygen row** at 21 per cent of the fresh gas if there is none.
- Gas flows and ventilation are **rounded to 0.1 L/min**.
- Inhaled agents are simulated only on plots of **7 days or less**. On a longer plot nothing is simulated, and a message asks you to remove the gas rows or choose a shorter Max time; see [Time display](help:time-display).

Ventilation is **minute ventilation**. Thirty per cent of it is taken to be dead space, so the alveolar ventilation, which exchanges gas, is 70 per cent of what you enter.

Cardiac output is GasMan's default of 5 L/min at 70 kg. For any other patient it follows body composition (see [The gas engine](help:models/gas-engine)): a 150 kg man of 170 cm has 7.1 L/min. With **Adjust weight to fat-free mass** unticked, or without height, age or sex, it is scaled by (weight/70)^0.75, which gives that man 8.9 L/min. The **Cardiac output** slider of the [Physiology sliders](help:models/physiology) changes it. The Visualizer's readout gives the cardiac output and the alveolar ventilation the simulation ran on.

## Fresh gas flow and rebreathing

The breathing circuit is the "ideal" circle system: once the fresh gas flow reaches the minute ventilation plus the gas being taken up from the alveoli, there is no rebreathing and the patient inspires fresh gas and nothing else. Below that, the shortfall is made up with exhaled gas. Below the uptake itself (the oxygen consumed, 0.25 L/min at 70 kg, plus the agents being absorbed) not even all of the exhaled gas can make it up: the circuit is closed, nothing is vented, and no gas is created to fill the gap (see [The inhaled-gas engine](help:models/gas-engine)). The uptake term is small, about 0.05 L/min on oxygen and a volatile agent, so the familiar rule of thumb, no rebreathing at a fresh gas flow equal to the minute ventilation, is nearly right; early in a nitrous oxide induction it can be close to 1 L/min. See [Where the gas engine differs from GasMan](help:models/gas-differences) for the exact rule. There is no circuit volume, so a change at the vaporizer reaches the patient at once. At low flows the alveolar concentration lags the vaporizer setting and the oxygen consumed (3.5 mL/kg/min) is no longer there to dilute what remains, so concentrations run higher than the fresh gas would suggest. [The sevoflurane wash-in scenario](scenario:sevoflurane-washin) shows this.

## The concentration and second gas effects

Nitrous oxide taken up in bulk concentrates whatever else is in the alveolus, so sevoflurane rises faster in its presence. The engine reproduces this coupling, as GasMan does. [The second gas effect scenario](scenario:second-gas-effect) shows it.

## Opioids and MAC

Opioids lower MAC. Under **Graph Options**, ticking *Include opioid - MAC interaction* reports MAC equivalents relative to the opioid-reduced MAC, so the same end-tidal concentration reads as more MAC equivalents when an opioid is on board. The gas concentrations themselves do not change. This is an approximate model and is expected to be replaced; see [Opioid reduction of MAC](help:models/opioid-mac).

## Time until threshold for the gases

*Time until threshold* works for the inhaled agents and for MAC equivalents as for the intravenous drugs: at each moment, how long until the concentration would fall to the threshold if the agent were turned off then. For a gas "turned off" means the vaporizer or the nitrous oxide is turned off, the fresh gas flow is turned up so that there is no rebreathing (which is what is done to wake a patient), and ventilation stays as it is. Each agent is its own decision, so each panel shows the time for that agent alone.

| Panel | What is timed | Default threshold |
|---|---|---|
| sevoflurane, isoflurane, desflurane | Brain (vessel-rich group) tension | 0.1 × the MAC of that agent at the patient's age and temperature |
| nitrous oxide | Brain tension | 10% at 37 °C |
| MAC equivalents | The summed alveolar series, with every agent turned off | 0.1 MAC equivalents |
| oxygen | Not timed | |

The thresholds can be changed in the Drug Thresholds dialog, where the volatile agents are shown at the patient's age.

**A gas's threshold is its value at 37 °C**, the one clinicians know, whether it is the default or one you type. Body temperature lowers MAC (by 4.6 per cent per degree for the volatile agents, 2.1 per cent for nitrous oxide; see [Physiology](help:models/physiology)), and the threshold moves with it exactly as it moves with age, so a threshold of a tenth of MAC stays a tenth of MAC in a cold patient. Type the number you would use at 37 °C; stanpumpR works out what it is at 32 °C. Sevoflurane's default, 0.18 per cent at age 40 and 37 °C, is used as 0.138 per cent at 32 °C, and the time until threshold is longer for it: 12.8 minutes rather than 11.2 after half an hour at 2 per cent in 6 L/min of oxygen (70 kg, age 40). With a temperature event, the threshold is the one at the temperature in force when the agent is turned off, and the threshold line steps with the event, so it is drawn at the threshold the time is counted to (0.18 per cent before an event that cools the patient to 28 °C, 0.105 per cent after it). While the physiology is off normal the shaded band is replaced by a grey ribbon from the curve at normal physiology to the curve as simulated (see [Physiology](help:models/physiology)). This replaced, on 24 November 2026, a rule that held a typed threshold fixed whatever the temperature. See [Time until threshold](help:models/recovery) and the [emergence scenarios](help:scenarios/emergence-sevoflurane).

## The greenhouse effect

The volatile agents and nitrous oxide are greenhouse gases. Nothing in the simulation removes agent from the body except breathing, so all the agent delivered reaches the atmosphere: through the pop-off valve during the case, or breathed out after it, unless it is captured. The Visualizer's amounts card gives the **CO<sub>2</sub> equivalent of the agent delivered**, and the fresh gas label gives the rate at the present setting, in kg of CO<sub>2</sub> equivalent an hour and as the number of typical US cars that emit as much.

The CO<sub>2</sub> equivalent is the mass of agent times its 100-year global warming potential, the horizon used by the US Environmental Protection Agency, the European Environment Agency and the Kyoto Protocol (Sulbaek Andersen and colleagues 2012). The mass comes from the vapour delivered, taken as an ideal gas at 20 °C, where the flowmeters read it. A typical US passenger car emits 5.1 tonnes of CO<sub>2</sub> a year (the same paper), 0.58 kg an hour.

<!-- generated: gas-greenhouse-table -->

The arithmetic carries two teaching points. First, the agent the patient does not take up is thrown away at once, so the fresh gas flow, not the patient, decides most of the emission: at 6 L/min, most of the sevoflurane delivered goes straight out through the pop-off valve (the [wash-in scenario](scenario:visualizer-sevoflurane-washin)), and an overpressure induction followed by 1 L/min emits a third as much in the hour, for a store in the patient of the same size (the [overpressure and low flow scenario](scenario:visualizer-sevoflurane-overpressure)). Second, the agents differ by far more than their potency: an hour of desflurane at about 1 MAC emits fifty times the CO<sub>2</sub> equivalent of an hour of sevoflurane at the same flow, and nitrous oxide, whose lifetime in the atmosphere is over a century, is given in such volumes that its emission is large too.

## Validation

The engine has been checked against GasMan itself across five scenarios, with each disagreement traced to its cause. Richard H. Epstein ran the GasMan side of the comparison. The record is in the repository at `inst/validation/VALIDATION.md`; see [Testing and validation](help:validation).
