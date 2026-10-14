### The model

Morphine's parameters are from Lötsch and colleagues (*Clin Pharmacol Ther* 2002;72:151-162), a population study of morphine and morphine-6-glucuronide after an intravenous bolus in eight young volunteers (four men and four women, 23 to 30 years). The library uses their Model A, a standard three-compartment model whose parameters are **absolute**, not per kilogram: volumes of 17.5, 86 and 196 L, an elimination clearance of 74 L/h (1.23 L/min), a rapid intercompartmental clearance of 134 L/h and a slow one of 19.5 L/h at a body surface area of 1.83 m². The slow clearance carries the paper's one covariate, the fourth power of body surface area: Q3 = 19.5 × (BSA/1.83)⁴ L/h, so at 2.2 m² it is 2.1 times its value at 1.83 m². Until October 2026 the library held the median rate constants and could not follow that covariate.

### Oral: tablet and liquid

Oral morphine is entered as **mg PO tablet** (immediate-release tablet), **mg PO liquid** (oral solution) or, for paediatric dosing, **mg/kg PO liquid**, each also available qd, bid, tid or qid. Both are calibrated to the studies of Atrux-Tallau and colleagues (*Clin Drug Investig* 2022;42:1101-1112), of morphine sulfate in fasted volunteers. Doses are labelled mg of morphine sulfate, as prescribed. The tablet comes from the pivotal study (39 subjects, 30 mg Sevredol tablets): peak 28.5 ng/mL at a median 45 minutes, AUC 117.4 ng·h/mL. The liquid comes from the smaller pilot crossover (17 subjects), where Oramorph solution peaked about 30% higher than the tablets (37.9 ng/mL) at the same median time, and every formulation's AUC agreed to within 10%.

The two forms share Lötsch's disposition and effect site and differ only in how fast they are absorbed. Both have a bioavailability of 0.29, set from the pivotal tablet AUC, because the pilot found the same exposure from each. Both start to be absorbed 17.6 minutes after the dose, after which the liquid is absorbed faster (half-time 37 minutes against the tablet's 53). The plasma peak is about 35 minutes after liquid and 45 minutes after a tablet, and the liquid's peak is a third higher. The liquid's faster absorption rests on the pilot study's 17 subjects. Because morphine's effect site is so slow, the difference in peak effect is much smaller than the difference in plasma peak.

Modified-release morphine is not modelled, nor is food, which slows absorption. Oral morphine forms much more morphine-6-glucuronide than intravenous morphine, because of first-pass glucuronidation, and that active metabolite is not in the model (below), so the effect of an oral dose is understated.

### Covariates

Body surface area on the slow intercompartmental clearance, as above, in both positions of the switch, as the library does for every model with a size covariate of its own. The other five parameters are the published values with the switch off, and under the default [fat-free-mass scaling](help:models/fat-free-mass) the volumes scale with the patient's fat-free mass and the clearances with that ratio to the 0.75 power. **Be careful outside roughly 1.5 to 2.2 m²**: a fourth power fitted to eight young adults is an extrapolation there. In a small child the slow clearance becomes tiny (about 0.6 L/h at 0.8 m²) and the deep compartment hardly fills, which raises the predicted plasma concentration a little during an infusion; the paper's subjects were adults.

### Effect site

The time to peak effect is 93.8 minutes, the slowest in the library by far. It is the library's own value, carried from the program's opioid tables, not from Lötsch 2002, which measured no effect. The same group's pupil measurements (Lötsch and colleagues, *Anesthesiology* 2001;95:1329-1338) gave morphine a plasma-to-effect-site transfer half-life of 2.8 hours (range 1.8 to 4.4), and Skarke and colleagues (*Clin Pharmacol Ther* 2003;73:107-121) 2.6 hours for pain tolerance, which with these kinetics would put the peak later still. Morphine is relatively hydrophilic and crosses the blood-brain barrier slowly, so its effect-site concentration lags the plasma by well over an hour. A bolus of morphine therefore never achieves an effect-site concentration near its plasma peak, and the clinical corollary is that titrating morphine to effect every five minutes overshoots: the effect of the previous dose has barely begun.

### MEAC and typical concentrations

MEAC is 8 ng/mL (0.008 mcg/mL in the plotted units) and the shaded band 6.4 to 16 ng/mL. These are the library's values, not Lötsch's. For comparison, Dahlström and colleagues (*Clin Pharmacokinet* 1982;7:266-279) measured a mean minimum effective concentration of 16 ± 9 ng/mL in patients using morphine patient-controlled analgesia after abdominal surgery, which is the top of the band.

### Active metabolite

Morphine-6-glucuronide is an active metabolite that accumulates in renal failure. It is not in the current model. Morphine is itself now the modelled metabolite of [codeine](help:drugs/codeine): giving codeine adds a morphine row, which is where codeine's analgesia appears. A second-generation link to morphine-6-glucuronide would need a two-stage cascade, which the engine does not yet do. See [Active metabolites](help:models/metabolites).

### Where to be careful

Renal function, which governs the metabolite, is not a covariate. The model describes young healthy volunteers; the elderly and the renally impaired lie outside it. The naloxone scenario shows how morphine's slow kinetics outlast its antagonist: [Naloxone after morphine](scenario:naloxone-morphine).

### Pregnancy

With **Pregnant** ticked, the elimination clearance is multiplied by 1.7 and nothing else moves. The review the library follows (Ansari, Carvalho, Shafer and Flood, *Anesth Analg* 2016;122:786-804) reports morphine's volume of distribution unchanged in pregnancy and its clearance increased by more than 70 per cent, by glucuronidation, so the half-life falls. The data are from late pregnancy and labour. Morphine-6-glucuronide, whose formation rises with the clearance, is not modelled. See [Pregnancy](help:models/pregnancy).
