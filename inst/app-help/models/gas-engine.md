The inhaled anesthetics are simulated by a separate engine whose structure and parameters follow **GasMan®**, the model of inhaled anesthetic uptake and distribution developed by James H. Philip at Brigham and Women's Hospital and Harvard Medical School. GasMan's source code is published in its authors' repository ([gasmanonline](https://github.com/rasman/gasmanonline)), whose licence is GPL-3.0; the engine was written from the published description of its model, with a few defaults read from that source, and then validated against GasMan's own output. This page gives the model; the deliberate departures from GasMan are on [their own page](help:models/gas-differences).

**A circle system, under controlled ventilation.** Circle systems are the only breathing systems used in anesthesia (Steven Shafer, 26 November 2026), and the engine models one: the ideal circle below, carbon dioxide absorbed and any shortfall of fresh gas made up with exhaled gas. Its one known simplification is the circle's own volume. A real circle holds 3 to 5 litres in its bag or bellows, absorber and tubing (perioperativeCPD, *Understanding the circle system*, 2022), and a change at the vaporizer must turn that gas over before the patient breathes it: in seconds at a high fresh gas flow, in minutes at a low one, the time constant being about the circle's volume over the flow (4 minutes for 4 litres at 1 L/min, 8 at 0.5 L/min). The model holds no gas in the circuit, so the fresh gas's part of what the patient inspires changes at once. The ventilation is an input, the minute ventilation set on the ventilator: the model assumes controlled ventilation. In a patient breathing spontaneously the agents depress ventilation, which slows their own uptake (Munson, Eger and Bowers 1973); that feedback is not modelled, and modelling it would make the engine's equations nonlinear. GasMan's semi-closed circuit, one well-mixed 8 litre box, is kept only for the comparison with GasMan ([Where the gas engine differs from GasMan](help:models/gas-differences)).

## Compartments

For each soluble gas (nitrous oxide, sevoflurane, isoflurane, desflurane, and nitrogen, which is always carried) the state is five gas tensions in per cent of one atmosphere: the breathing circuit, the alveolar gas, and three tissue groups (six when the cardiac-output slider is off normal and the brain and heart become a compartment of their own; see [Physiology](help:models/physiology)).

| Compartment | Volume at 70 kg | Share of cardiac output |
|---|---|---|
| Alveolar gas (FRC) | 2.5 L | |
| Vessel-rich group (brain) | 6 L | 0.76 |
| Muscle group | 33 L | 0.18 |
| Fat group | 14.5 L | 0.06 |

The volumes and flows above are GasMan's 70 kg standard, which is stanpumpR's reference man (70 kg, 170 cm, 35 years). For any other patient they scale by **body composition**: the vessel-rich group, the muscle group and the lungs with the patient's fat-free mass (the Al-Sallami equation, the library's standard; see [Fat-free mass](help:models/fat-free-mass)), the fat group with the fat mass, the vessel-rich and muscle groups' blood flows with fat-free mass to the 0.75, the fat group's flow with its volume to the 0.75 (Weber and colleagues' scaling for obese patients, 2021), and cardiac output is their sum (Steven Shafer, 25 November 2026). A 150 kg man of 170 cm then has 67.5 L of fat group where linear scaling gave 31 L, 47 L of muscle group where it gave 71, and a cardiac output of 7.1 L/min where it gave 8.9. With **Adjust weight to fat-free mass** unticked in the Patient Profile the volumes scale linearly with weight and cardiac output with (weight/70)^0.75, as GasMan does. Oxygen consumption stays 3.5 mL/kg/min of total weight, and the default ventilation scales with total weight, as clinicians set it (Steven Shafer, 25 November 2026). The blood-flow fractions in the table are GasMan's `[Ratio]` values for the reference man, which linear scaling keeps for every patient; they are kept as the reference because they are part of the validation chain, and the code notes that no source for them has been established. With body-composition scaling each patient's fractions follow from the scaled flows: the 150 kg man's vessel-rich group takes 70% of his output, his muscle group 17% and his fat group 13%. Oxygen is modelled in the gas phase only, with a metabolic sink of 3.5 mL/kg/min: it binds haemoglobin non-linearly and has no meaningful partition coefficient.

## Partition coefficients

The capacity of a tissue for a gas is its volume times its tissue:gas partition coefficient. The table stores tissue:gas coefficients directly, as GasMan's configuration does:

| Gas | Blood:gas | Brain:gas | Muscle:gas | Fat:gas | MAC at 40 (%) |
|---|---|---|---|---|---|
| nitrous oxide | 0.47 | 0.42 | 0.54 | 1.08 | 104 |
| sevoflurane | 0.74 | 1.1 | 2.4 | 34 | 1.80 |
| isoflurane | 1.45 | 2.1 | 4.5 | 70 | 1.17 |
| desflurane | 0.57 | 0.54 | 0.97 | 13 | 6.6 |
| nitrogen | 0.014 | 0.010 | 0.014 | 0.070 | (200, flagged) |

The tissue:gas coefficients, and the blood:gas coefficients of nitrous oxide and nitrogen, are GasMan's. The blood:gas coefficients of the three volatile agents are Esper's, measured in 120 adult patients (*Anesth Analg* 2015;120:45-50), adopted on 28 November 2026 in place of GasMan's 0.65, 1.3 and 0.42; the tissue:gas coefficients, which are properties of the tissues, did not move, so each tissue:blood ratio fell (isoflurane's brain:blood from 1.62 to 1.45). They are the values at 37 °C and a haematocrit of 45 per cent, and move with body temperature and with the Haemoglobin slider (see [Physiology](help:models/physiology)). The MACs at 40 are Mapleson's (*Br J Anaesth* 1996;76:179-185), the meta-analysis whose age relation the engine applies, adopted on 23 November 2026: the program was validated against GasMan first and has since moved to more recent research. GasMan's own, which the US labels share, were 110, 2.1, 1.1 and 6.0 per cent; MAC enters no uptake, so that validation is unchanged. Nitrogen's MAC of 200 per cent is GasMan's own figure, carried as it stands and flagged: the estimate of Koblin, Eger and colleagues, 110 atmospheres in rats (*Anesth Analg* 1998;87:419-424), is about 55 times higher, and nitrogen is not summed into MAC, so the value is inert. Each gas's page under [Drug library](help:drugs/index) shows these values.

## Equations

Fresh gas composition follows from the flowmeter and vaporizer settings; the vapour dilutes the carrier gases. Air is split into 20.93 per cent oxygen and 78.07 per cent nitrogen; its remaining 1 per cent, mostly argon, is not carried.

### Three volumes, three conditions

The engine counts gas in the lungs, at body temperature and saturated with water vapour. Its three volumes are stated in three different conditions, and each is converted before it is used (Steven Shafer, 28 November 2026: "Convert both, assuming 20 degrees C at the flowmeter"):

| Volume | Stated at | Litres in the lungs per litre stated |
|---|---|---|
| Ventilation | body temperature, saturated | 1.000 |
| Fresh gas, at the flowmeters | 20 °C, dry | 1.128 |
| Oxygen consumed, carbon dioxide produced | 0 °C, dry, 760 mmHg (STPD) | 1.210 |

A litre of dry gas leaving a flowmeter at 20 °C warms to 37 °C, which expands it by 310/293, 5.8 per cent, and takes up water vapour at 47 mmHg, which expands it by 760/713, 6.6 per cent: 12.8 per cent together. So rebreathing stops when the flowmeters read about 89 per cent of the delivered minute ventilation, not 100, because that much gas fills the breath once it is warm and wet (89 per cent of the ventilation plus whatever is being taken up, as the equations below have it). Oxygen consumption, 3.5 mL/kg/min, is an STPD volume by convention, and is 21 per cent larger in the lungs. In a closed circuit, 0.25 L/min of oxygen on the flowmeter is 0.233 L/min STPD, 5 per cent short of the 0.245 L/min a 70 kg patient consumes.

Until that date all three were taken litre for litre. The closed circuit's arithmetic came out even, and the alveolar oxygen read about 9 mmHg high on room air (107 mmHg where the alveolar gas equation gives 99, at an alveolar ventilation of 4 L/min), because the oxygen consumed was subtracted as too small a volume. In the equations below, Q is the fresh gas flow in the lungs' litres, 1.128 times the flowmeters' reading at 37 °C and sea level (more at altitude, where the same water vapour is a larger share of the pressure; less in a cold patient, whose lungs warm it less).

None of this changes what you enter or read. The flowmeter and vaporizer settings are shown as you set them, and the Visualizer counts the vapour delivered as flow times setting, in millilitres at the machine: 2 L/min of 2 per cent for an hour is 2.4 L. The conversion happens inside the engine, where a flowmeter's litre meets a litre of ventilation.

The fresh gas that enters the circuit during inspiration goes to the patient with the ventilator's breath, so the delivered minute ventilation is the ventilation set plus the inspiratory share of the fresh gas (since 28 November 2026; Steven Shafer, 27 November 2026, after Cicman 1993):

```
MV = MV_set + phi Q,   phi = I / (I + E)
```

Nothing is added while the ventilation set is 0. phi is a third at the default I:E ratio of 1:2 (the Physiology panel offers 1:1 to 1:4), and Q is in the lungs' litres. Because the fresh gas enlarges the breath it must fill, rebreathing stops (Q >= MV + U, below) only when Q >= (MV_set + U) / (1 - phi): a flowmeter reading of 1.33 times the set ventilation at 1:2. The time until threshold runs at that flow, so at MV = MV_set / (1 - phi), 1.5 times the setting at 1:2 (no uptake is added, the tissues giving agent back during a washout). In GasMan's mode phi is 0. See *Fresh gas and the delivered ventilation* on the [physiology](help:models/physiology) page.

The circuit is the ideal circle system. With fresh gas flow Q, delivered minute ventilation MV (alveolar ventilation VA = 0.7 MV) and U the total uptake when it is positive (zero otherwise), the patient inspires MV + U and the circuit supplies it:

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

with s = 1 except in the closed circuit above. U F_circuit and P F_alv are the **concentration and second gas effect**: gas taken up in bulk shrinks the alveolar volume, so make-up gas is drawn in from the circuit, concentrating what remains; when gas comes back out of the blood, as on emergence, alveolar gas is pushed out instead. In the closed circuit the first term is Q F_fresh + (VA + R) F_alv, so the equation becomes V_alv dF_alv/dt = Q F_fresh − uptake_i − (P − R) F_alv. The total uptake is the sum of every soluble gas's uptake (nitrogen included) plus the oxygen consumed less the carbon dioxide that replaces it, VO2 × (1 − 0.8). Uptake of each gas is the unshunted cardiac output times the blood:gas coefficient times the alveolar-to-mixed-venous tension difference; each tissue takes up at its share of cardiac output times the arterial-to-tissue difference divided by its capacity. A share s of the cardiac output, the **shunt**, passes no ventilated alveoli and reaches the arteries as mixed venous blood, unchanged, so for every gas

```
uptake_i  = lambda_blood (1 - s) Qco (F_alv - F_venous)
F_arterial = (1 - s) F_alv + s F_venous
```

(this s is the shunt, not the circuit's share above). It is 10 per cent under anesthesia and 50 per cent with an endobronchial tube, set by the Shunt fraction slider (see [Physiology](help:models/physiology), *Shunt under anesthesia*); GasMan has none, and the comparisons with GasMan run at s = 0. Mixed venous tension is the flow-weighted mean of the tissue tensions.

## Integration

Between dose-table changes the settings are constant, and every equation above would be linear with constant coefficients but for one term: the total uptake that couples the gases depends on the state. The engine therefore advances in short steps. Within each step the total uptake (and with it the circuit blend) is held at its value at the start of the step, which makes the step linear, and the step is then solved exactly by matrix exponential (a Padé approximation with scaling and squaring). The uptake is recomputed at the start of the next step. So the propagation within a step is exact, but the coupling between steps is an approximation of first order in the step size: the answer depends slightly on the step and converges as it shrinks. GasMan holds its total uptake per step in the same way and in addition splits each step into sequential sub-updates; the two converge to the same answer as the step shrinks, and the repository's convergence test checks that they do.

The steps are short just after each change of setting, when the uptake changes fastest, and grow from there: the first is a hundredth of a minute, and each is a tenth longer than the one before, until they reach the plot length divided by 600 (0.1 minute on a one-hour plot, 2.4 minutes on a day). So each step is about a tenth of the time since the last change, and the error is about the same on any plot length. Measured against the same engine with much shorter steps, in a nitrous oxide induction (4 L/min nitrous oxide in 6 L/min, with 2 per cent sevoflurane, as in [the second gas effect scenario](scenario:second-gas-effect)), in its emergence, and in the same induction in a 4 kg infant, on plots of an hour, a day and a week, the alveolar tensions were within 0.6 per cent of their value from the reference. A finer reference, in a 4 kg infant three months old at its default ventilation (0.67 L/min), found 0.67 per cent at eight seconds, so the error is under 0.7 per cent in the cases measured. Without nitrous oxide the coupling is weak and the error smaller still: under 0.1 per cent of the agent's peak in the teaching and validation scenarios even with the earlier, longer steps.

Until 23 November 2026 every step was the plot length divided by 600, so a day's plot stepped 2.4 minutes from the start of a nitrous oxide induction, and its first points read 13 per cent low (ChatGPT's audit of the gas engine found it; the page then advised a short plot).

## MAC

MAC is age-adjusted by the Mapleson relation, MAC(age) = MAC40 × 10^(−0.00269 (age − 40)), about 6 per cent per decade, with MAC40 Mapleson's own values from the same meta-analysis (the table above). The MAC-equivalents series is the sum over potent agents of alveolar tension divided by the agent's age-adjusted MAC. Nitrous oxide is included. The panel's effect-site line is the same sum over the brain (vessel-rich group) tensions.

Mapleson fitted ages from 1 year; below that the relation is extrapolated, and moves MAC by under 1 per cent from its value at 1 year. MAC measured in term neonates and infants is higher than that, and for isoflurane and desflurane it peaks within the first year (Cameron 1984; Taylor and Lerman 1991; Lerman 1994); in preterm neonates isoflurane MAC is lower (1.28 per cent under 32 weeks' gestation, 1.41 per cent at 32 to 37 weeks, against about 1.50 here at birth; LeDez and Lerman 1987). Whether those measurements should replace the relation in infants will be decided once the papers have been read.

**Pregnancy** does not change MAC here. Lower MAC has been reported in pregnancy: isoflurane MAC 28 per cent lower at 8 to 12 weeks (Gin and Chan 1994), halothane and enflurane MAC lower in early pregnancy (Chan 1996), isoflurane MAC lower in the first day or so after delivery (Zhou 1995; Chan and Gin 1995), and 25 to 40 per cent lower in ewes near term (Palahniuk 1974). The evidence is thin. No human study measured MAC at term. The human studies compared pregnant with nonpregnant women, a comparison that cannot be randomised, and the up-and-down end point rests on the observer's judgement of movement at each step (Gin and Chan report that their single observer was blinded). Progesterone, the mechanism the authors proposed, did not correlate with MAC in Gin and Chan's own data, and at term pregnancy did not change sevoflurane's effect on the EEG (Ueyama 2010). Steven Shafer decided on 25 November 2026 that no pregnancy factor is applied: he cannot envision a mechanism by which pregnancy would alter MAC, and the studies do not separate the investigators' expectation from what they found.

## Validation

Richard H. Epstein ran GasMan through its API for the same scenarios and the outputs were compared column by column. In the first concordance run the two agreed to one part in a million on the alveolar tension, with the residual traced to GasMan's single-precision arithmetic. Five further scenarios covering low flows, other agents, a 100 kg patient and changing settings followed, run with GasMan's own program built from its source; their 520 values are frozen in the repository and checked on every build, and on 24 November 2026 a sixth, a 5 kg patient, was added. The record is `inst/validation/VALIDATION.md`; see [Testing and validation](help:validation).

## References

Philip JH. GasMan: an example of goal oriented computer-assisted teaching which results in learning. *Int J Clin Monit Comput* 1986;3:165-173.

Weber J, Schmidt J, Wirth S, Schumann S, Philip JH, Eberhart LHJ. Context-sensitive decrement times for inhaled anesthetics in obese patients explored with GasMan®. *J Clin Monit Comput* 2021 (PMC7943506).

Hendrickx JFA, Lemmens HJM, Shafer SL. Do distribution volumes and clearances relate to tissue volumes and blood flows? A computer simulation. *BMC Anesthesiol* 2006;6:7.

Mapleson WW. Effect of age on MAC in humans: a meta-analysis. *Br J Anaesth* 1996;76:179-185.

Cicman JH, Skibo VF, Yoder JM. Anesthesia systems. Part II: operating principles of fundamental components. *J Clin Monit* 1993;9:104-111.

Esper T, Wehner M, Meinecke CD, Rueffert H. Blood/gas partition coefficients for isoflurane, sevoflurane, and desflurane in a clinically relevant patient population. *Anesth Analg* 2015;120:45-50. doi:10.1213/ANE.0000000000000516

Cameron CB, Robinson S, Gregory GA. The minimum anesthetic concentration of isoflurane in children. *Anesth Analg* 1984;63:418-420.

Taylor RH, Lerman J. Minimum alveolar concentration of desflurane and hemodynamic responses in neonates, infants, and children. *Anesthesiology* 1991;75:975-979.

Lerman J, Sikich N, Kleinman S, Yentis S. The pharmacology of sevoflurane in infants and children. *Anesthesiology* 1994;80:814-824.

LeDez KM, Lerman J. The minimum alveolar concentration (MAC) of isoflurane in preterm neonates. *Anesthesiology* 1987;67:301-307.

Palahniuk RJ, Shnider SM, Eger EI 2nd. Pregnancy decreases the requirement for inhaled anesthetic agents. *Anesthesiology* 1974;41:82-83.

Gin T, Chan MT. Decreased minimum alveolar concentration of isoflurane in pregnant humans. *Anesthesiology* 1994;81:829-832.

Chan MT, Gin T. Postpartum changes in the minimum alveolar concentration of isoflurane. *Anesthesiology* 1995;82:1360-1363.

Zhou HH, Norman P, DeLima LG, Mehta M, Bass D. The minimum alveolar concentration of isoflurane in patients undergoing bilateral tubal ligation in the postpartum period. *Anesthesiology* 1995;82:1364-1368.

Chan MT, Mainland P, Gin T. Minimum alveolar concentration of halothane and enflurane are decreased in early pregnancy. *Anesthesiology* 1996;85:782-786.

Ueyama H, Hagihira S, Takashina M, Nakae A, Mashimo T. Pregnancy does not enhance volatile anesthetic sensitivity on the brain: an electroencephalographic analysis study. *Anesthesiology* 2010;113:577-584.
