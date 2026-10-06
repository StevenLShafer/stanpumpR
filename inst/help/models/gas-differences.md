The parameters and defaults of the inhaled-gas engine are Gas Man®'s, and the intent for now is to give the same answers Gas Man gives. Nine differences are deliberate, confirmed by Dr Shafer in October 2026, and will remain. Anyone comparing the two side by side should know them.

| | Gas Man | stanpumpR | Why |
|---|---|---|---|
| Breathing circuit | Defaults to "Semi-closed": the whole circuit is one well-mixed 8 L volume, so some exhaled gas is rebreathed at any fresh gas flow, however high | The "Ideal" circuit, which Gas Man also offers: no rebreathing once fresh gas flow reaches minute ventilation; below that, the shortfall is made up with exhaled gas. No circuit volume, so no lag | It is how a circle system behaves. The mixing box has no threshold at fresh gas flow = ventilation and understates the inspired concentration at moderate and high flows |
| Ventilation and dead space | The ventilation setting is alveolar ventilation; there is no dead space | The ventilation setting is minute ventilation, 30 per cent of it dead space. Rebreathing stops when fresh gas flow reaches the minute ventilation | Minute ventilation is what is set on a ventilator and read from a monitor |
| Oxygen consumption and gas volume | No oxygen, so no volume is lost to it | Oxygen consumed (3.5 mL/kg/min) shrinks the gas volume, as uptake of an anesthetic does. Carbon dioxide replaces most of it in the alveoli and is then removed by the absorber from whatever exhaled gas is rebreathed | Without it the gas fractions do not add up at low flows. With 0.3 L/min of oxygen and 1 L/min of nitrous oxide, what leaves the circuit is the 1.3 L/min delivered less the 0.21 L/min consumed: 92 per cent nitrous oxide and 8 per cent oxygen, not the 77 and 23 delivered |
| MAC and age | One MAC per agent, no age term | MAC adjusted for the patient's age by the Mapleson relation | MAC falls about 6 per cent per decade, and the patient's age is already an input |
| MAC across agents | Each agent reported separately | A single MAC-equivalents series, the sum of each potent agent's alveolar concentration as a fraction of its own MAC | Agents given together are additive, and one number is what is titrated to |
| Oxygen | Not modelled | Modelled in the circuit and alveoli, with metabolic consumption; cannot go below zero | The inspired and alveolar oxygen matter whatever else is given, and a hypoxic mixture should be visible |
| Nitrogen | Carried only if nitrogen is added to the run as an agent | Always carried; its washout from the body is part of the summed uptake that couples the gases | The patient starts full of nitrogen whether or not anyone enters it, and it leaves through the same alveoli |
| Starting nitrogen | 80 per cent | 78.07 per cent, with oxygen at 20.93 | Room air, so that the gas fractions sum correctly once oxygen is modelled |
| Integration | Each time step is split into sequential sub-updates | Each step is advanced exactly, by matrix exponential | Accuracy does not then depend on the step size |

## Consequences when comparing side by side

- Enter in Gas Man the **alveolar** ventilation, 70 per cent of the minute ventilation used here, and expect a small difference whenever fresh gas flow is below the minute ventilation.
- At low fresh gas flows expect the concentrations here to run higher than Gas Man's, because the oxygen consumed is no longer there to dilute them.
- Set Gas Man's circuit to **Ideal**. This is the largest of the differences. With Gas Man left on Semi-closed, 2 per cent sevoflurane at 8 L/min gives an alveolar concentration of 0.47 per cent at one minute and 1.59 at thirty; with the ideal circuit, here and in Gas Man, it is 1.09 and 1.71.
- To reproduce a Gas Man MAC value, set the age to 40, where the age adjustment is exactly 1, and compare one agent at a time.
- Add nitrogen as an agent in Gas Man, delivered at 0 per cent (or at 78 per cent of any air flow), before comparing. Without it Gas Man leaves nitrogen washout out of the uptake coupling, which by itself moves alveolar sevoflurane by about 0.3 to 0.5 per cent during a wash-in.
- The two integrations do not agree digit for digit at any fixed step size. They converge to a common answer as the step shrinks.

## Carbon dioxide

Carbon dioxide is not shown as a gas, but it is accounted for. Alveolar gas holds about 5 per cent of it (carbon dioxide production over alveolar ventilation, with production at 0.8 of oxygen consumption), so the alveolar concentrations shown add up to about 95 per cent; inspired gas, which has been through the absorber, adds up to 100. Because the patient breathes in slightly more than they breathe out, the fresh gas flow that stops rebreathing is the minute ventilation plus what is being taken up, a little above the minute ventilation itself.

## The rebreathing rule of thumb

The circuit follows the rule that rebreathing stops once fresh gas flow reaches minute ventilation (Feldman JM, Lampotang S, Hendrickx J. Is rebreathing prevented when FGF equals MV? APSF Newsletter, 20 October 2022). The model has no circuit volume, so a change at the vaporizer reaches the patient at once; the gas already in a real circuit takes a little time to mix out, which is not clinically important.
