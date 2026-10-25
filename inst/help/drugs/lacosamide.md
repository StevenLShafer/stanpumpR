### The model

From a 2026 analysis of 180 Chinese adults (*BMC Pharmacol Toxicol*, doi:10.1186/s40360-026-01114-2): one compartment, with an apparent clearance of 1.86 L/h, 0.875 times that in women, times Cockcroft-Gault creatinine clearance over 119 to the power 0.311; the volume (0.6 L/kg) and absorption (6.47/h) were fixed from the literature because only troughs were sampled.

### Routes and renal function

Oral (`mg PO`, with schedules) and intravenous (`mg`, `mg/hr`): bioavailability is complete and the injection is interchangeable mg for mg, so the intravenous route uses the same parameters with bioavailability 1. Renal function comes from the entered serum creatinine (a child's read on the adult scale) or an [assumed normal](help:models/covariates) one; about 40 per cent of a dose is renally excreted. The half-life is about 16 hours.

### What is not plotted

Carbamazepine raises clearance about 1.5-fold (not applied: no co-medication field). The FDA's 12-hour-exposure response model for focal seizures was not retrievable with its coefficients, so no seizure prediction is drawn. There is no effect site.

### Pregnancy

With **Pregnant** ticked, the whole apparent clearance is multiplied by 1.5, directly and not through the creatinine-clearance term; the volume and absorption are unchanged. In MONEAD the dose-normalized concentration fell by up to 39.9 per cent (Pennell and colleagues, *JAMA Neurol* 2022;79:370-379; 1.66-fold), and a pregnancy PBPK model evaluated against those data predicts clearance 48 per cent higher at 40 weeks (Barry and colleagues, *Epilepsia* 2025;66:346-355); 1.5 lies between them. Carried through the renal term, the rise in filtration would give only 1.13-fold, since only about 40 per cent of lacosamide is excreted unchanged. See [Pregnancy](help:models/pregnancy).

### Band

1 to 12 mg/L, 12 hours after a dose, with 6 as the typical value (its midpoint, not a target): the AGNP consensus guidelines' 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5, https://doi.org/10.1055/a-2860-7861; laboratory alert level 24 mg/L). There is no clear concentration-efficacy relation; the upper limit comes from an observational study of adverse reactions (Otero Torres and colleagues, 2023), and enzyme-inducing co-medication lowers concentrations. It replaces the 10 to 20 mg/L of Patsalos and colleagues (*Ther Drug Monit* 2018), which was drawn from samples 0 to 3 hours after a dose and so describes peaks.
