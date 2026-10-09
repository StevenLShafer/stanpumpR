### The model

Clonazepam's parameters are from dos Santos and colleagues (*Ther Drug Monit* 2009;31:566-574), who gave 23 healthy men a single 4 mg dose as two 2 mg tablets and sampled plasma for 72 hours. The model has two compartments, with first-order absorption (2.21/h) after a lag of 22 minutes. The apparent clearance is 2.92 L/h, the central and peripheral volumes are 141 and 34.8 L, and the intercompartmental clearance is 10.2 L/h. The distribution half-life is 1.9 hours and the terminal half-life 42 hours.

In the reference man, 2 mg peaks at 12.5 ng/mL at 2.0 hours. Volunteers given 2 mg tablets peak at 13 to 17 ng/mL at 1 to 4 hours, with a half-life of 30 to 43 hours (Crevoisier and colleagues, *Eur Neurol* 2003;49:173-177; Berlin and Dahlström, *Eur J Clin Pharmacol* 1975;9:155-159; Genis-Najera and Sañudo-Maury, *Neurol Ther* 2024;13:141-152). The model's exposure is about a fifth higher than the first two found.

The specification this model was built from proposed Kruizinga and colleagues' model (*Br J Clin Pharmacol* 2022;88:2236-2245), fitted to an oral solution in 20 young adults sampled for 48 hours. Its parameters were checked against the paper and are correct, but it describes tablets less well: a peak of 9.9 ng/mL after 2 mg, and a 57-hour half-life, because sampling stopped at 48 hours.

### Oral only

Both studies had oral data only, so the clearance and volumes are divided by an unknown bioavailability, which is about 90 per cent (Crevoisier and colleagues). An oral study cannot show how fast clonazepam distributes after an injection, and the intravenous studies did not capture it either: Berlin and Dahlström sampled from 10 minutes, too late for the first phase. Two minutes after 0.5 mg intravenously, 27 ng/mL has been measured (Schols-Hendriks and colleagues, *Br J Clin Pharmacol* 1995;39:449-451); no published model describes that first half hour. Clonazepam is therefore offered only by mouth (`mg PO`, with the daily schedules). The lag is part of the tablet model; during it, time until threshold reads blank.

### Covariates

None in the source, which enrolled men within 15 per cent of ideal body weight. With the [fat-free-mass switch](help:models/fat-free-mass) on, the volumes scale with fat-free mass and the clearances with its 0.75 power; with it off, everyone receives the published values. Other antiepileptic drugs that induce its metabolism raise clearance by 22 to 75 per cent (Yukawa and colleagues, *J Clin Pharmacol* 2002;42:81-88); that is not modelled.

### Effect site

None; only the plasma concentration is plotted, and it is the concentration that drives the effect. dos Santos found that the site of action "is not kinetically distinguishable from the plasma compartment": the peak concentration and the peak impairment of the Digit Symbol Substitution Test came at the same time. Their final model related the test to plasma concentration directly (half-maximal impairment at 9.3 ng/mL), with an acute tolerance that raises that concentration over the following day, so the effect wanes faster than the concentration falls. That tolerance is not modelled.

### Typical concentrations

The shaded band, 20 to 70 ng/mL, is the reference range for epilepsy (Patsalos and colleagues, *Epilepsia* 2008;49:1239-1276); the typical value, 40 ng/mL, is its midpoint, not a target. Lower concentrations suffice for panic disorder and anxiety: 0.5 mg twice daily averages about 14 ng/mL at steady state, below the band but above the concentration at which dos Santos's volunteers were half-maximally impaired. No time-until-threshold level is set.

### Where to be careful

Clonazepam's long half-life means it takes a week or more to reach steady state, and as long to wash out. It adds to the sedation and respiratory depression of opioids and other sedatives. The model describes young healthy men taking tablets; women, the elderly and hepatic disease are not represented.
