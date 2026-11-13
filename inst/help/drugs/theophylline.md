### The model

No population model of theophylline with intravenous data and a complete printed description could be read in full, so the disposition is the FDA label's: **Theophylline in 5% Dextrose Injection** (B. Braun, NDA 019826, 2012). One compartment, as every population model of the drug is, with the label's volume of **0.45 L/kg** and a clearance taken from its **Table V**, the initial infusion rates that give a steady-state serum theophylline of 10 mcg/mL in an otherwise healthy **nonsmoker**. A rate that holds 10 mg/L is the clearance times 10 mg/L, so the adult rate of 0.4 mg/kg/h is a clearance of 0.04 L/kg/h (0.67 mL/kg/min, half-life 7.8 h). Table V covers every age, which the label's measured means do not:

| Age | Table V rate | Clearance |
|---|---|---|
| up to 24 days | 1 mg/kg every 12 h | 0.0083 L/kg/h |
| 24 days to 6 weeks | 1.5 mg/kg every 12 h | 0.0125 L/kg/h |
| 6 to 52 weeks | 0.008 × age in weeks + 0.21 mg/kg/h | rate ÷ 10 mg/L |
| 1 to 9 years | 0.8 mg/kg/h | 0.08 L/kg/h |
| 9 to 12 years | 0.7 mg/kg/h | 0.07 L/kg/h |
| 12 to 16 years (nonsmokers) | 0.5 mg/kg/h | 0.05 L/kg/h |
| 16 to 60 years | 0.4 mg/kg/h | 0.04 L/kg/h |
| over 60 years | 0.3 mg/kg/h | 0.03 L/kg/h |

Clearance steps at each boundary, as the label's dosing does. The label's measured means (its Table I) agree: 0.65 mL/kg/min in nonsmoking adult asthmatics (8.7 h), 0.41 over 60, 1.7 at 1-4 years and 1.4 at 6-17 years.

### Body size

The label works per kilogram of **ideal** body weight, because theophylline "distributes poorly into body fat". With [the scaling switch](help:models/fat-free-mass) on, both parameters are per kilogram of the pharmacokinetic weight, which follows lean rather than total mass; with it off, per kilogram of total weight.

### Aminophylline

Intravenous theophylline is usually given as **aminophylline**, the ethylenediamine dihydrate salt, which is 78.9% theophylline by mass (the label rounds it to "theophylline/0.8"). Doses entered as **mg aminophylline** (or mg/kg, mg/hr, mg/kg/hr aminophylline) are converted to theophylline; plain mg are theophylline itself. The curve is serum theophylline either way. Aslaksen and colleagues (*Br J Clin Pharmacol* 1981;11:269-273) found intravenous theophylline and an equivalent dose of aminophylline indistinguishable.

### Oral

Immediate-release theophylline is completely absorbed: Taylor and colleagues (*Br J Clin Pharmacol* 1983;16:413-416) measured a bioavailability of 93-100% in eight healthy nonsmokers and an absorption rate constant of 2.31 /h on average; a 5 mg/kg dose then peaks at 9.7 mcg/mL near 1.5 hours, as they measured. Sustained-release products differ from one to the next and are not offered.

### The band

The band is the label's **10 to 15 mcg/mL**: "maintaining average serum theophylline concentrations between 10 and 15 mcg/mL will achieve most of the drug's potential therapeutic benefit while minimizing the risk of serious adverse events." Bronchodilation occurs from 5 to 20 mcg/mL, and above **20 mcg/mL** "both the frequency and severity of adverse reactions increase", which is the threshold the time until threshold reads. The US asthma guideline (NAEPP, 2007) recommends 5 to 15 mcg/mL.

### Where to be careful

- **Smoking** raises clearance by about 50% in young adults and 80% in the elderly (label); the model is a nonsmoker. Heart failure, cor pulmonale, liver disease and sepsis with multi-organ failure halve it (Table V's 0.2 mg/kg/h). Many drugs interact (cimetidine, ciprofloxacin, erythromycin, fluvoxamine; rifampin, phenytoin).
- **Elimination saturates.** The label's clearances were measured below 20 mcg/mL, and "non-linearity of elimination may begin in some patients at serum theophylline concentrations less than 10 mcg/mL". The model is linear, so at high concentrations, and in an overdose, it underestimates both the concentration and how long it takes to fall.
- **Distribution.** One compartment cannot show the first half hour after a bolus, when the concentration is higher than this curve: give a loading dose over 30 minutes, as the label does.
- **Pregnancy**: clearance falls late in pregnancy (the label's third-trimester half-life is 13 h), not yet applied; see below.
