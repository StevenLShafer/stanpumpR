**This model is provisional.** It is the best published irbesartan model this library can run, but the library can only approximate its absorption, and the approximation misses the published curve by up to 4.7% of the peak. The details are below.

### The model

Irbesartan's parameters come from Karatza and Karalis (*Eur J Pharm Sci* 2020;153:105498), a population analysis of 1664 concentrations from 32 healthy Asian men, aged 18 to 45, each given two 300 mg tablets (Aprovel and a generic) in a crossover. Two compartments with first-order elimination: apparent clearance 13.5 L/h, apparent central volume 13.8 L, apparent intercompartmental clearance 17.7 L/h and apparent peripheral volume 85.8 L.

Every parameter is **apparent**, divided by the oral bioavailability, because the study gave oral doses only. So no intravenous unit is offered.

### The absorption is a delay equation, approximated here

Many of the individual profiles had two peaks. To describe them the authors used a **delay differential equation**: the rate at which the gut absorbs now depends on how much was in the gut 1.68 hours ago, with a rate constant of 0.304 per hour. At the typical values the curve has a single peak, at 1.9 hours.

This library cannot run a delay equation. Each oral dose is therefore given as twelve small doses, spread over the first four hours, each absorbed in the ordinary first-order way. The twelve were fitted to the exact published curve. What that costs:

- **Worst point on the curve:** 4.7% of the peak.
- **The peak:** 2.3% low, and about 0.1 hour late (2.0 hours against 1.9).
- **With *Adjust weight to fat-free mass* on:** up to 5.2% for a 45 kg woman, because the fit was made at the published values.

The library's usual standard for such an approximation is 0.53%, so this is about nine times worse. More pulses would not close the gap: no set of non-negative doses can get below about 3.5%, because the exact equation briefly absorbs more than the whole dose and then gives some back.

### Compared with the label

The model was not fitted to the Avapro label, so the label is a real check.

- **Clearance:** 13.5 L/h, inside the label's apparent range of about 12 to 18 L/h.
- **Time to peak:** 1.9 hours, inside the label's 1.5 to 2 hours.
- **Half-life:** 8.2 hours, **shorter** than the label's 11 to 15 hours. The source sampled for 48 hours.

On the other hand, the label says irbesartan accumulates by less than 20% on once-daily dosing. This curve accumulates by 6% at the peak over a week; a single 13 hour half-life would predict about 39%. So which half-life is right is not settled here. If the label's is, the trough on daily dosing drawn here is too low.

### Body size, kidneys, sex and age

The authors tested weight, height, age and body-mass index on every parameter, and kept none. The study's average patient is not printed, so the published values are used as the 70 kg reference patient's. With *Adjust weight to fat-free mass* off, the published disposition values are used unchanged; with it on, the library's fat-free-mass scaling applies. Either way, the absorption is the twelve-pulse stand-in described above, with its error of up to 4.7% of the peak.

**There is no renal term.** Irbesartan is cleared mainly by liver oxidation (CYP2C9) and glucuronidation, its renal clearance is only 3 to 3.5 mL/min, and the label reports no change in renal impairment or on haemodialysis. A creatinine entered in the [Patient Profile](help:patient-profile) does not reach this drug.

The label reports concentrations 11 to 44% higher in women, 20 to 50% higher in the elderly and about 25% higher in Black subjects. **None of this is in the model**, which comes from young Asian men only.

### Effect site and blood pressure

This curve is the **plasma concentration** only. [This library does not plot a blood pressure](help:models/antihypertensives). Two blood-pressure models have been published for irbesartan, Hedaya 2015 and Huang 2006, each in normotensive men after a single 300 mg dose and each fitted on its own pharmacokinetics. Their concentrations for half the maximum effect differ sevenfold, so neither gives a pressure.

Tick **Graph Options → [% effect](help:models/percent-effect)** to draw the fall in systolic pressure as a percentage of its maximum, 0 to 100%, with no baseline. That panel uses Huang 2006 (EC50 0.29 mcg/mL, Hill 0.75, k<sub>e0</sub> 0.62 /h), **provisionally**: of the two, only Huang leaves an effect at the 24-hour trough, as a once-daily drug must. Huang's EC50 was fitted on concentrations about half of Karatza's, so the panel probably overstates the effect, and Huang's own abstract found blood pressure almost unchanged after 150 mg. Treat the panel as the time course of the effect, not its size. No band is drawn: there is no established therapeutic plasma range for irbesartan, which is dosed to blood pressure.

### Pregnancy

Irbesartan, like every angiotensin receptor blocker, is contraindicated in pregnancy (a boxed warning on the label). The pregnancy setting changes nothing here.
