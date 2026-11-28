# Gas engine expansion: the mathematics, for review

Reviewed and accepted by Steven L. Shafer on 2026-11-28 (section 12). A draft for his review, written before any code (the order of work in `docs/gas-engine-expansion.md`, which holds the decisions this draft applies). Drafted by Claude Code, 2026-11-28. Nothing here is built. Where the draft had to choose something no decision covers, the choice is marked **[for decision]** and collected in section 12.

The notation is the engine's own (`R/advanceClosedFormGas.R`, *The equations*): tensions `F` in % of a sea-level atmosphere, flows in L/min, `MV` the ventilation, `VA` the alveolar ventilation, `Qco` the cardiac output, `d` the dead-space fraction, `u` the summed uptake.

## 1. Units, and one correction to the engine as it stands

The engine's unit of tension is fixed: 1% is 7.13 mmHg (`GAS_ALVEOLAR_TORR` / 100), the dry share of a sea-level atmosphere at 37 °C. A gas amount is therefore counted in litres at body temperature and 713 mmHg.

Oxygen consumption and CO₂ production are STPD volumes by convention (0 °C, 760 mmHg, dry). One litre STPD is, in the engine's unit,

    kappa(T) = (760 / 713) (273.15 + T) / 273.15          1.2103 at 37 °C

The engine today subtracts `VO2 = 0.0035 × weight` L/min from the alveolar gas with no conversion (kappa = 1). On room air at 70 kg and 4 L/min of alveolar ventilation it gives an alveolar oxygen of 107 mmHg where the alveolar gas equation gives 99 (measured in code, 2026-11-28; this draft first said 106 and 100). With CO₂ as a state the same omission would give an alveolar CO₂ of 35 mmHg where the classical `PACO2 = 0.863 VCO2 / VA` gives 42.3. **[for decision 1]** Recommended: 3.5 mL/kg/min is STPD, and the sink becomes `kappa(T) VO2`. GasMan has no oxygen, so its validation does not move; the oxygen panel and the arterial oxygen under a shunt do, by this amount.

For CO₂, written in mmHg and mL STPD, the same conversion is

    gamma(T) = 1000 × 273.15 / ((273.15 + T) × 760)       1.1588 mL STPD per L per mmHg at 37 °C

and 1 / gamma(37) is the 0.863 of the textbooks.

### 1a. The fresh gas too (decided 2026-11-28)

Converting the oxygen consumption alone breaks the closed circuit: the fresh gas is a volume at the flowmeters, dry and at room temperature, and it too was taken litre for litre. On its way to the alveoli it warms and is humidified:

    e(T, P_B) = (P_B / (P_B - PH2O)) (273.15 + T) / (273.15 + 20)      1.1277 at 37 °C and sea level

and every fresh gas flow `Q` in the equations below is `e` times the flowmeters' reading. "Convert both, assuming 20 degrees C at the flowmeter." Rebreathing then stops at a flowmeter reading of `(MV + u+) / e`, and oxygen at 0.25 L/min on the flowmeter is 0.233 L/min STPD.

## 2. Metabolism

    VO2(37)  = m × 3.5 mL/kg/min × weight                 m: the metabolism slider, 0.5 to 2, default 1
    VO2(T)   = VO2(37) × 2.0^((T - 37) / 10)              Q10 2.0 (Tveita and Sieck 2022)
    VCO2(T)  = RQ × VO2(T)                                RQ: the slider, 0.7 to 1.0, default 0.8

`VCO2` is what the tissues produce. What the lungs excrete is a result (section 6) and equals it only in the steady state. At 32 °C `VO2` is 0.707 of normal; at 28 °C, 0.536.

## 3. Ventilation

    phi     = I / (I + E)                                 1/3 at the default 1:2
    MV_del  = MV_set + phi Q                              delivered = the ventilator's setting + the fresh gas's inspiratory share
    VA      = (1 - d) MV_del                              d = 0.3 (Nunn and Hill 1960)

`MV_del` replaces `MV` everywhere in the engine's circuit equations. The fresh gas flow that stops rebreathing, `Q >= MV_del + u+`, becomes

    Q* = (MV_set + u+) / (1 - phi)

and the time until threshold runs at `Q*`, where `MV_del = (MV_set + phi u+) / (1 - phi)`, 1.5 times the set ventilation at 1:2.

At 70 kg the default ventilation is 5.71 L/min. With 2 L/min of fresh gas the delivered ventilation is 6.38 L/min and `VA` 4.47, so the default patient's steady PaCO₂ is 37.8 mmHg, against 42.3 with no fresh gas. **[for decision 2]** The default ventilation was set from GasMan's alveolar 4 L/min before the fresh gas added to it. Recommended: leave it, and let the help page show the arithmetic.

## 4. Water vapour

    PH2O(T) = (47 / 47.103) × 6.1121 exp((18.678 - T / 234.5) (T / (257.14 + T))) × 0.750062   mmHg

Buck's 1981 equation, scaled by 0.9978 so that 37 °C gives exactly 47 (28.3 at 28 °C, 35.6 at 32 °C, 49.6 at 38 °C). The paper is not yet read (`papers/pending-work.csv`); the constants above are from memory and must be confirmed on it.

The unit of tension stays 7.13 mmHg at every temperature. The vapour pressure enters only where the physics has it, the humidification of the inspired gas:

    h(T, P_B) = ((P_B - PH2O(T)) / P_B) / (713 / 760)     gasHumidityRatio(), exactly 1 at 37 °C and sea level

and in `kappa(T)` and `gamma(T)` above through the temperature. **[for decision 3]** This differs in form from the entry recorded on 2026-11-27, which had the vapour pressure enter `GAS_ALVEOLAR_TORR` itself. Keeping the unit fixed means no state has to be rescaled when the temperature steps, and the numbers reported are the same.

## 5. Shunt, for every gas

With `s` the shunt (0.10 under anesthesia, the slider; 0.50 with an endobronchial tube), the pulmonary capillary flow is `(1 - s) Qco`:

    V_alv dF_A/dt = VA (F_I - F_A) - lambda_b (1 - s) Qco (F_A - F_v)
    F_a           = (1 - s) F_A + s F_v                   arterial; the tissues see F_a
    V_t lambda_t dF_t/dt = Q_t lambda_b (F_a - F_t)

These are the engine's endobronchial equations (`gasArterial()`), now in force at 10% by default. The tube differs only in also halving the ventilated lung's gas volume. The GasMan validation runs at `s = 0`.

`lambda_b` is Esper 2015's (isoflurane 1.45, sevoflurane 0.74, desflurane 0.57), times `exp(c (Hct - 45))` (Zhou 2002). The tissue:gas coefficients are unchanged, so tissue:blood falls. **[for decision 4]**

## 6. Carbon dioxide

### 6.1 The stores (Farhi and Rahn 1960, Table 2)

Blood and tissue CO₂ content is linear in tension between 30 and 80 mmHg, blood at `beta = 4.5` mL/L/mmHg. Each compartment `j` is a capacitance `C_j` (mL/mmHg: its tissue's slope times its mass, plus `beta` times the venous blood equilibrated with it) fed by its blood flow and its own production `M_j`. Farhi and Rahn's compartments map onto the engine's groups closely:

| Engine group | Farhi and Rahn | Their flow, L/min | Engine's flow at 70 kg | C, mL/mmHg | Production, mL/min | Time constant, min |
|---|---|---|---|---|---|---|
| Lungs with arterial blood | Alveoli | — | — | 14.8 (gas 4.0 + blood 10.8) | — | 2.47 |
| Vessel-rich group | Heart + brain + other | 4.04 | 3.8 | 39.6 | 164 | 2.3 |
| — brain and heart, when split | Heart + brain | 0.94 | (the protected share) | 8.2 | 94 | 1.9 |
| — the rest of the group | Other | 3.10 | | 31.4 | 70 | 2.0 |
| Muscle group | Muscle | 0.85 | 0.9 | 120.2 | 40 | 30 |
| Fat group | Bone, fat | — | 0.3 | none | none | — |

Proposed **[for decision 5]**:

- `C_j` scales with the engine's own compartment volume: `C_VRG = 39.6 × V_brain / 6`, `C_muscle = 120.2 × V_muscle / 33`, so body composition applies to CO₂ as to the agents.
- Production is shared in the proportions of the table, vessel-rich 164/204 and muscle 40/204 (brain and heart 94/204 when split), each times `VCO2(T)`. Farhi and Rahn's four compartments account for 204 of their 240 mL/min; the rest is not assigned by them.
- Fat and bone hold no CO₂ store and produce none, as in the source: fat's venous blood returns at the arterial tension. The source leaves bone (113 L of CO₂) and fat out as too slow to matter over hours. A run of days would see them; the help page will say so.
- The blood's CO₂ curve is the straight line. Dash's nonlinear total-CO₂ expression is not used: a linear system is what the exact solver carries, and Farhi and Rahn's line holds over 30 to 80 mmHg. Outside that range the help page says the model is extrapolated. This reads "dissociation curves for all, unless trivial" as satisfied for CO₂ by the linear curve.

### 6.2 Temperature and alpha-stat

At constant content, blood PCO₂ falls as blood cools:

    k(T) = 10^(0.019 (37 - T))                            Kelman and Nunn 1966; Ashwood 1983, eq 22

So every blood and tissue CO₂ tension is carried as `P*`, the tension that blood would read at 37 °C, which is what a blood gas machine reports and what alpha-stat calls normal at 40. The stores' equations are then the same at every temperature. Only the lung, where blood meets gas at body temperature, converts: capillary blood leaving alveoli at `P_A` has `P* = k(T) P_A`. Assumed: the tissue slopes follow the same `k(T)` as blood.

### 6.3 The equations

State: `P_A` (alveolar, mmHg, in vivo) and `P*_j` for each tissue group.

    P*_c = k P_A                                          end-capillary
    P*_a = (1 - s) k P_A + s P*_v                         arterial
    P*_v = sum_j f_j P*_j                                 mixed venous; f_fat P*_a for fat

    C_j dP*_j/dt = beta Q_j (P*_a - P*_j) + M_j

    C_L dP_A/dt  = beta (1 - s) Qco (P*_v - k P_A) - gamma(T) VA (P_A - P_I)
    C_L          = gamma(T) V_alv + k beta V_art          V_art = 2.4 L × weight / 70

`P_I = 0`: the absorber removes all CO₂ and none is in the fresh gas (decided). With flows fixed the system is linear, `dy/dt = A y + b`, and is advanced by the same matrix exponential as each gas, on the same sub-steps.

In the steady state `P_A = VCO2 / (gamma VA)`, the textbook equation, whatever the shunt.

### 6.4 What couples back into the other gases

Today the engine takes CO₂'s alveolar fraction at its steady value and the exhaled CO₂ as `c_E = VCO2 / MV`. Both become the state's values, held over each sub-step as the uptake is:

    excretion  E = gamma VA P_A × kappa / 1000            L/min in the engine's unit
    c_E        = E / MV_del
    alveolar gas shrinks by  kappa VO2 - E                in place of VO2 (1 - RQ)

so during hyperventilation, when excretion exceeds production, the alveolar gas shrinks less, and the circuit's closed-circuit test `Q < u+ + VCO2` uses `E`.

### 6.5 What is reported

    End-tidal CO2          P_ET   = (1 - a) P_A           in vivo, as a capnograph reads it
    Arterial, alpha-stat   PaCO2  = P*_a                  as read at 37 °C
    Arterial, pH-stat box  PaCO2  = P*_a / k              in vivo

With an ideal lung the arterial to end-tidal difference is only the shunt's, `s (P*_v - k P_A)`, about 0.6 mmHg at 10%. Nunn and Hill measured 4.6 (SD 2.5) mmHg, which is alveolar dead space: gas from unperfused alveoli, at inspired composition, diluting the end-tidal sample. They found it "roughly proportional to the tidal volume". **[for decision 6]** Proposed: of the 30% physiological dead space, a share `a` of the alveolar tidal volume is alveolar dead space, with `a = 0.10`, which gives 4.6 mmHg at a PaCO₂ of 40 with the shunt's 0.6 (anatomical 22% and alveolar 8% of the tidal volume). It changes only what is displayed as end-tidal CO₂. The same dilution applies in principle to the end-tidal oxygen and agent; recommended that those stay the alveolar values in this expansion, and the question be recorded.

## 7. pH

    pH(37) = 6.1 + log10( HCO3 / (0.0307 × PaCO2) )       PaCO2 = P*_a; HCO3 the slider, 20 to 32, default 24 mEq/L
    pH(T)  = pH(37) + 0.0147 (37 - T)                     Rosenthal 1948; shown only with the pH-stat box

Bicarbonate 24 at 40 mmHg gives 7.39; 20 gives 7.31 and 32 gives 7.52. The slider holds the bicarbonate fixed, so this is not a buffer line: an acute rise in PaCO₂ in a patient raises bicarbonate a little, and the model's pH falls slightly more than the patient's. The constants are read in Dash's papers: pK 6.1 at plasma pH 7.4 (Dash 2016, Table 1, on the page image) and the solubility of CO₂ in plasma 3.07e-5 M/mmHg at 37 °C, 0.0307 mmol/L/mmHg (Dash and Bassingthwaighte 2010, equation 10b, PubMed Central text; from Austin 1963).

## 8. Oxygen

### 8.1 Alveolar

Unchanged in form, a gas-phase state with a sink, now `kappa(T) VO2(T)`, the inspired gas humidified at `PH2O(T)`. The alveolar gas equation of Fenn, Rahn and Otis falls out of the mass balance in the steady state; a test will hold the engine to it.

### 8.2 The dissociation curve (Dash 2016)

    S    = (PO2 / P50)^n / (1 + (PO2 / P50)^n)            eq 6
    n    = 2.82 - 1.20 × 10^(-PO2 / 29.25)                eq 11
    P50  = 26.8 × (P50_pH / 26.8) (P50_CO2 / 26.8) (P50_T / 26.8)       eq 10, DPG at its standard value
    P50_pH  = 26.8 - 25.535 x + 10.646 x^2 - 1.764 x^3,   x = pH_rbc - 7.24 = 0.795 (pH - 7.4)   eq 9a
    P50_CO2 = 26.8 + 0.1273 (PCO2 - 40) + 1.083e-4 (PCO2 - 40)^2        eq 9b
    P50_T   = 26.8 + 1.435 (T - 37) + 0.04163 (T - 37)^2 + 6.86e-4 (T - 37)^3   eq 9d

Equations 9a to 9d, 10 and 11 were confirmed on the page image (printed pages 101 and 102). P50 is 26.8 mmHg at 37 °C, 20.6 at 32 °C and 16.8 at 28 °C. The curve is evaluated in vivo: at body temperature, with the in-vivo `pH(T)` and in-vivo PCO₂. **Item 7, settled on the sources (2026-11-28)**: the pH in equation 9a is the red cell's, `pH_rbc = 0.795 pH + 1.357`, standard 7.24. Dash 2016 fits and plots P50 against `pH_rbc` throughout (Figures 2, 3 and 6, the last over 5.8 to 8.2), and states that its pH term is the 2010 paper's times 1/0.833; Dash and Bassingthwaighte 2010 (PMID 20162361, equation 11, read in the PubMed Central text) writes that term as -21.279 (pH_rbc - 7.24), and 21.279 / 25.535 is 0.833. In plasma terms the fixed-acid Bohr coefficient is then -0.33, and with CO₂ as the acid, the CO₂ term added, about -0.5, beside Severinghaus's -0.40 and -0.48.

### 8.3 Content

    C_O2 = 1.34 × Hb × S + 0.00265 × PO2                  mL/dL; Hb in g/dL, Hct = 3 Hb

The dissolved coefficient is Dash's, `W_bl × alpha_O2` = 0.81 × 1.46e-6 M/mmHg, at hematocrit 45% (it is `(1 - Hct) 0.94 + Hct 0.65` times `alpha_O2`, so it moves a little with the slider). **[for decision 8]** Dash's own capacity is 4 O₂ per haemoglobin, 1.39 mL/g. Recommended: keep 1.34, the clinical value, which allows for the haemoglobin that carries no oxygen.

### 8.4 Arterial

As now, with the new curve and `s` in force by default:

    C_a = C_c' - (s / (1 - s)) VO2(T) / Qco               C_c' the content at the alveolar PO2

inverted to PaO₂ in vivo and SaO₂. Reported alpha-stat PaO₂ is the tension the same blood reads at 37 °C: the PO₂ at which the 37 °C curve, at `pH(37)` and `P*_a`, holds the same content. Saturation is the same either way. With the default 10% shunt the default patient on 50% oxygen reads a PaO₂ well below the alveolar value; the help page will show the numbers once computed in code.

Oxygen stores are not a state: the lungs are taken to load `VO2` at every moment, as today. So a step in ventilation moves CO₂ through its stores over minutes to an hour, and oxygen at once apart from the alveolar gas's own time constant.

## 9. Temperature: everything that moves, in one place

| Quantity | Relation | At 32 °C |
|---|---|---|
| Oxygen consumption, CO₂ production | × 2.0^((T - 37)/10) | × 0.707 |
| Drug clearance (unchanged, separate) | × exp(0.0637 (T - 37)) | × 0.727 |
| Blood:gas of the volatile agents | Zhou 2002's slopes | as now |
| Tissue:gas | Zhou and Liu 2001's tissue slopes | new |
| MAC | Liu 2001; Antognini 1994 | as now |
| Water vapour | section 4 | 35.6 mmHg |
| Blood PCO₂ at constant content | × 10^(-0.019 (37 - T)) | × 0.803 |
| P50 | Dash eq 9d | 20.6 mmHg |
| pH, displayed in vivo | + 0.0147 (37 - T) | + 0.074 |
| Gas volume per mole | kappa(T), gamma(T) | 1.6% |

At 32 °C and unchanged ventilation the in-vivo alveolar CO₂ falls to 0.707 of normal and the reported (37 °C) PaCO₂ to 0.707 × 1.245 = 0.88 of normal: 37 mmHg where it was 42.

## 10. Method of solution

Nothing new. Per sub-step, with the uptake, the CO₂ excretion and the circuit blend held at their values at its start: one matrix exponential per gas, one for CO₂ (three to five states), oxygen as now. Arterial oxygen, pH and the displayed values are algebraic and computed afterwards from the states. At a step in physiology the CO₂ states carry over unchanged (they are 37 °C-equivalent tensions of contents that do not jump), except `P_A`, which is continuous as a partial pressure.

The starting state is the steady state of the settings at time 0 with the patient breathing air at the default ventilation: `P_A = VCO2 / (gamma VA)`, every `P*_j` at its steady value.

## 11. What will be tested

- The steady states against the alveolar ventilation equation and the alveolar gas equation, by hand.
- The CO₂ system against an independent Runge-Kutta integration, and its time constants against Farhi and Rahn's Table 2.
- Conservation: CO₂ produced equals CO₂ excreted plus the change in stores, to rounding.
- Dash's curve against the paper's own printed points and against Severinghaus's table.
- Every GasMan comparison unchanged at `s = 0`, GasMan's blood:gas, no fresh-gas coupling, no CO₂ state.
- The semi-closed circuit refuses all of it (`gasNotGasMansBody()`).

## 12. For decision

**Decided, 2026-11-28**: "Yes to all eight, and yes to the pH-stat default not appearing unless temperature changes from 37 degrees celsius." Each recommendation below is therefore the decision; item 7 remains a reading task, with no decision needed unless the sources disagree.

1. Oxygen consumption is STPD and is converted (× 1.21 at 37 °C) before it leaves the alveolar gas. This corrects the engine as it stands. Recommended: yes.
2. The default ventilation stays where it is, so the default patient with 2 L/min of fresh gas sits at a PaCO₂ of about 38. Recommended: yes.
3. The unit of tension stays 7.13 mmHg at every temperature, the vapour pressure entering through the humidified inspired gas. Recommended: yes.
4. With Esper's blood:gas coefficients the tissue:gas coefficients stay as they are. Recommended: yes, as was decided for hematocrit.
5. CO₂ stores: Farhi and Rahn's compartments on the engine's groups, scaled by the engine's volumes; production shared 164:40 between vessel-rich and muscle; no store and no production in fat and bone; the blood curve linear. Recommended: yes.
6. End-tidal CO₂ is the alveolar value less an alveolar dead space of 10% of the alveolar tidal volume, to reproduce Nunn and Hill's 4.6 mmHg; end-tidal oxygen and agent stay alveolar for now. Recommended: yes, and the alternative is an ideal lung with a 0.6 mmHg difference.
7. Dash's pH term: settled on the sources, the red cell's pH (section 8.2).
8. Oxygen capacity stays 1.34 mL/g with Dash's saturation and Dash's dissolved coefficient. Recommended: yes.

Still to read before coding: Buck 1981 (or another vapour-pressure source); Fenn, Rahn and Otis's equations 2 and 17. Dash and Bassingthwaighte 2010 was read in the PubMed Central text only, which lacks its appendices and Table 1; its PDF is not in hand.
