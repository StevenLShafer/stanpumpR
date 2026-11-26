# Expanding the gas engine: end-tidal carbon dioxide, oxygen saturation and the circle system

Written on 2026-11-26 at the end of the session that ran both rounds of the external gas-engine audit (`docs/gas-engine-audit/`), so that the next session starts from the discussion as well as from the code. Nothing here has been built. Every item marked **[decision]** is Steven Shafer's to make. Record each answer here, dated and in his words, before the work it governs begins.

The session that wrote this is `session_0169csHpVDvu9xMPZSSHrUa1`. A cloud session can ask it a question with `send_message` while it is not archived. Its memory has been condensed once, though, so this page and the repository are the better source.

## What Dr Shafer asked for, in his words (2026-11-26)

- "I want to look at %oxygen saturation and at end tidal carbon dioxide. These will be somewhat consequential."
- "The end-tidal carbon dioxide is important because there is a 'gotcha' in the closed system that we should capture. Because 100% of the fresh gas flow goes to the patient during the inspiratory cycle, you can drive the carbon dioxide quite low if you turn the fresh gas flow very high. There is no clinical reason to raise fresh gas flow above the ventilatory rate, because when fresh gas flow = minute ventilation no gas is rebreathed. The design of the circle valves and escape valves is such that during inspiration the ventilator pumps fresh gas into the circuit, and during exhalation the fresh gas pushes the exhaled gas into the pop-off valve, where the exhaled gas exits the circuit."
- "Circle systems are the ONLY systems used in anesthesia." "stanpumpR should match clinical practice as well as possible." Recorded in `AGENTS.md`, *Circle systems only, under controlled ventilation*.
- "We always assume controlled ventilation in these models. When patients breath spontaneously we need to account for ventilatory depression. However, the feedback between anesthetic gas concentration and ventilatory depression is complex, and changes the engine to ODE." Spontaneous ventilation is **not** part of this expansion. The help pages say it is not modelled.

## What the engine does today

- **Circuit.** The ideal circle (`gasCircuitBlend()` in `R/advanceClosedFormGas.R`). The patient inspires minute ventilation plus the net gas taken up. When the fresh gas covers that, the patient breathes fresh gas only. Below it, exhaled gas makes up the shortfall, its CO₂ removed by the absorber. Below the uptake itself the circuit is closed. The circuit holds no gas: a real circle's 3 to 5 L is not modelled, and a pending-work row proposes it (`papers/pending-work.csv`).
- **Ventilation.** An input: the minute ventilation set on the ventilator. 30% of it is dead space (`GAS_DEAD_SPACE_FRACTION`), so alveolar ventilation is 0.7 × minute ventilation. Nothing the agents do changes it. The fresh gas flow does not add to it either.
- **Oxygen.** Carried in the circuit and the alveoli. Consumption is `VO2 = 0.0035 × weight` L/min (`getGasBody()`), the same at any body temperature.
- **Carbon dioxide.** Not a state. Production is `VCO2 = GAS_RESPIRATORY_QUOTIENT × VO2`, with the quotient 0.8 marked "NOT YET CONFIRMED BY SHAFER" in `R/gasProperties.R`. The alveolar fraction is taken at its steady value, 100 VCO2 / VA, so it follows any change in ventilation instantly. Inspired CO₂ is zero, the absorber taking it out. CO₂ enters only the gas-volume coupling and the circuit blend (the long header of `R/advanceClosedFormGas.R`, "THE CLOSED CIRCUIT" and the lines on c_E). It is never shown.
- **Arterial oxygen.** `R/arterialOxygen.R`: Severinghaus 1979 from the alveolar oxygen, through the endobronchial tube's shunt only. Without a tube, PaO₂ equals the alveolar PO₂ (713 mmHg per 100% at sea level, `GAS_ALVEOLAR_TORR`), so the saturation reads near 100% whenever the alveolar oxygen is adequate. It is shown in the oxygen panel's hover and, with a tube, in the Visualizer.

## The fresh-gas gotcha, in numbers

Delivered minute ventilation = set minute ventilation + inspiratory fraction × fresh gas flow. At I:E 1:2 the inspiratory fraction is one third. Steady-state alveolar PCO₂ = (VCO₂ / VA) × 713 mmHg. A 70 kg patient, VO₂ 0.245 L/min, VCO₂ 0.196 L/min, set to 6 L/min:

| Fresh gas flow | Delivered ventilation | Alveolar ventilation | End-tidal CO₂ |
|---|---|---|---|
| no coupling (today) | 6.0 L/min | 4.2 L/min | 33 mmHg |
| 2 L/min | 6.7 | 4.7 | 30 |
| 6 L/min (= minute ventilation) | 8.0 | 5.6 | 25 |
| 10 L/min | 9.3 | 6.5 | 21 |

Two consequences beyond the CO₂:
- The added ventilation speeds every agent's uptake.
- It changes every run at the default settings, because the fresh gas flow is never zero. Coupling would therefore be a new row of `inst/app-help/models/gas-differences.md`. The GasMan comparisons would need it off: the semi-closed circuit already takes no physiology GasMan lacks (`gasNotGasMansBody()`), and the ideal-circuit comparisons run with `deadSpace = 0` and `oxygenUptake = FALSE`.

**Whether the machine adds the fresh gas depends on its design.** This is the writing session's understanding, to be confirmed in sources before anything is built:
- **Dräger piston machines** store the fresh gas in the bag during inspiration. The circle-systems module below says so for the Primus.
- **Many current GE machines** measure and compensate the delivered volume.
- **In pressure control,** the pressure sets the volume.

So the gotcha belongs to volume control on a machine that neither decouples nor compensates.

## Decisions for Dr Shafer

1. **[decision] Fresh gas coupling.** Should delivered ventilation be set ventilation + inspiratory fraction × fresh gas flow? If so, is the I:E ratio an input, defaulting to 1:2, or a fixed fraction? Alternatively, should the model assume a compensated or decoupled machine, or should the user say which kind of machine is in use? (That would be a fact about the patient's setup, not a choice between published models.)
2. **[decision] End-tidal CO₂ dynamics.** One option is the steady state, VCO₂ / VA, which moves the instant the ventilation does. The other is a model of the body's CO₂ stores, in which a real PaCO₂ moves over minutes. Under controlled ventilation the stores are still linear, so the matrix-exponential engine can carry them. A source must be read before building the stores.
3. **[decision] Display.** Should end-tidal CO₂ and SpO₂ be lines, and on their own panel? When are they drawn: whenever a gas or oxygen is in the dose table, or only on request?
4. **[decision] A baseline shunt under anaesthesia.** Atelectasis gives a venous admixture, a figure to be found in a source, and with it a realistic A–a gradient. The engine sends the agents through the same shunt (`gasArterial()`), so this would also lower every agent's arterial tension a little. Apply it to oxygen only, to everything, or not at all?
5. **[decision] Oxygen consumption and temperature.** Oxygen consumption and CO₂ production fall in hypothermia. The size of the fall, per °C, is to be read in a source, not taken from memory. Today the temperature slider does not change them, so the end-tidal CO₂ of a cold patient would read high. Should they follow the slider?
6. **[decision] The respiratory quotient**, 0.8, which `R/gasProperties.R` says he has not yet confirmed. End-tidal CO₂ is proportional to it.

## Not in this expansion

- The circle's own volume. A pending-work row proposes it, for his decision, with Herbert and Magee 2017 and Lerou and Booij's system model as its sources.
- Spontaneous ventilation and its depression by the agents (Munson, Eger and Bowers 1973). It would make the engine a nonlinear ODE: hold the ventilation fixed per step, as the uptake is held, or use an adaptive solver such as `mmDormandPrince()`.
- The lung's ventilation/perfusion scatter (round 1, decision 9: a single mixed lung until Peyton's mathematics is checked).

## Sources Dr Shafer supplied (2026-11-26)

- **OpenAnesthesia.** Malark C, Tomlinson H, McCoy N. *Anesthesia Breathing Systems*. Last updated 5 February 2024. https://www.openanesthesia.org/keywords/anesthesia-breathing-systems/. Read in full. It classifies systems as open, semi-open, semi-closed and closed. A semi-closed circle (APL valve open, partial rebreathing) is "the most common breathing systems in the United States". It also says a high fresh gas flow is needed for the first 5 to 10 minutes "to equilibrate the breathing system".
- **StatPearls.** Yao PY, Hendrix JM, Stahl DL. *Anesthesia Breathing Systems*. StatPearls, updated 13 December 2025 (NBK574503; PMID 34662017 by Europe PMC). Only the abstract was read: NCBI answers automated readers with a reCAPTCHA. It uses the same four-way classification.
- **perioperativeCPD team.** *Understanding the circle system* (2022). CC BY-NC-SA 4.0, for private use, so it is cited here and not copied; it was uploaded to the session and is not in the archive. Points that bear on the model:
  - A circle holds 3 to 5 L, and 22 mm tubing holds 400 to 500 mL per metre.
  - Above 1 L/min the inspired concentrations are close to the fresh gas's. Below it they must be monitored.
  - A table of the oxygen fraction needed at low flows, against 250 mL/min of consumption.
  - A chart of rebreathed and fresh shares against fresh gas flow. It gives no source and no minute ventilation, so it should not be calibrated to.
  - GE bellows machines cut the bag and APL valve out of the circuit during ventilation. Dräger's Primus stores the fresh gas in the bag.
  - It cites Bokoch and Weston, *Inhaled Anesthetics: Delivery Systems*, in Miller's Anesthesia, 9th edition (2020), and Herbert and Magee, *BJA Education* 2017. Either may settle the machine question in decision 1.

## How the next session should proceed

1. Read this page and `AGENTS.md`'s gas-engine sections: *Circle systems only*, *Physiology sliders* and *Rules from Carbon's third UI audit*.
2. Read the header of `R/advanceClosedFormGas.R` (the closed circuit and the CO₂ lines), `R/arterialOxygen.R`, and the help pages `inst/app-help/models/gas-engine.md`, `gas-differences.md` and `inst/app-help/drugs/ventilation.md`.
3. Put the decisions to Dr Shafer and record each answer here.
4. Find and read the sources the numbers above still need, on the PDF. Ask for any paper not in stanpumpRPapers, and write it in `papers/pending-work.csv` meanwhile.
5. Build each piece as its own pull request with its tests. Keep the GasMan comparisons where they are, and add a row to `gas-differences.md` for any new default departure from GasMan.
