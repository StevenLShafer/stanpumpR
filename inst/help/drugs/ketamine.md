### The model

Ketamine's parameters are from Domino and colleagues (*Clin Pharmacol Ther* 1984;36:645-653), who gave 2.2 mg/kg of racemic ketamine intravenously to eight healthy men, with and without diazepam. The paper reports means of individually fitted quantities: a central volume of 0.063 L/kg, a clearance of 0.848 L/kg/h (0.99 L/min at 70 kg), a deep volume of about 1.51 L/kg and three half-lives of 24 seconds, 4.7 minutes and 2.2 hours. Means of individually fitted parameters need not describe the data, and these do not: the mean rate constants give twice the measured clearance, and a model built to reproduce all the summary values at once runs nearly twice too high at 4 to 5 minutes and far too low after 12 hours. The library's model is instead fitted to Domino's measured mean concentrations, all 16 from 1 minute to 24 hours (placebo arm). It has a central volume of 0.058 L/kg with fixed rate constants, a clearance of 0.79 L/kg/h (0.92 L/min at 70 kg), a steady-state volume of 3.1 L/kg and half-lives of 0.6, 25 and 288 minutes, and is within about 30% of every measured mean except at 8 hours (+49%). The terminal half-life is longer than Domino's 2.25 hours because his 12 and 24 hour concentrations require it. Diazepam premedication slowed redistribution and is not modelled.

### Covariates

Size only. By default, with *Adjust weight to fat-free mass* ticked, the volumes scale with the patient's fat-free mass relative to the 70 kg, 170 cm reference man and the clearances with that ratio to the 0.75 power, so height, age and sex enter through fat-free mass ([fat-free-mass scaling](help:models/fat-free-mass)). Domino's model is per kilogram of total body weight, with fixed rate constants: unticking the box restores that, scaling the volumes and clearances linearly with weight.

### Effect site

The time to peak effect is 3 minutes. It is an assumption: Domino measured no effect, and no separately sourced equilibration has been adopted.

### Typical concentrations

The shaded band, 0.1 to 0.16 mcg/mL, is an **analgesic** threshold for sub-anesthetic ketamine: "the ketamine analgesic effect persists for steady-state plasma concentrations superior to 100–160 ng/ml" (Mion and Villevieille, *CNS Neurosci Ther* 2013;19:370-380, citing Clements and colleagues, *J Pharm Sci* 1982;71:539-542). In Clements' volunteers the pain threshold in an ischaemic exercise test rose consistently above 160 ng/mL and not below 80, and the mean concentration was 186 ng/mL when pain was none or mild against 101 ng/mL when it was moderate or severe, so the band spans the transition into analgesia. The same review puts impaired memory at 70 ng/mL, nystagmus at about 200 and psychedelic effects from as low as 50. Anesthetic concentrations are an order of magnitude higher, around 1 to 2 mcg/mL. The default recovery threshold is 0.1 mcg/mL. See [the ketamine infusion scenario](scenario:ketamine-infusion). The AGNP consensus guidelines' 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5) gives 0.06 to 0.41 mcg/mL immediately after an infusion of 0.5 mg/kg over 40 minutes, for depression; it is not drawn, since this band is for analgesia.

### Where to be careful

Norketamine, an active metabolite with about a third of ketamine's potency that accumulates during infusions, is not modelled. The S-enantiomer, used alone in many countries, has different kinetics. Domino's volunteers were young and healthy.
