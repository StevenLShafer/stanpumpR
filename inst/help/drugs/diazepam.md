### The model

Diazepam's parameters are from Hung, Dyck, Varvel, Shafer and Stanski (*Can J Anaesth* 1996;43:450-455). They gave 4 healthy men (80 kg on average) 30 mg intravenously over 5 minutes, sampled arterial blood for 2 hours and venous blood for 10 days, and fitted three compartments to each subject. The means are a central volume of 3.43 L, peripheral volumes of 8.47 and 87.5 L, and clearances of 0.027, 1.10 and 0.335 L/min. The half-lives are 1.3 minutes, 24 minutes and 45 hours.

Four subjects is a small study, and the parameters are their means. The model reproduces the larger studies:

- **Clearance**, 27 mL/min: "between 20 and 32 ml/min" in 33 volunteers (Klotz and colleagues, *J Clin Invest* 1975;55:347-359), and 26.6 mL/min in 48 men (Greenblatt and colleagues, *Ther Drug Monit* 1989;11:652-657).
- **Early concentrations**: within 10 to 15 per cent of those Mould and colleagues measured after 0.1 and 0.2 mg/kg (*Clin Pharmacol Ther* 1995;58:35-43).

The model this was built from proposed a paediatric model (McCann and colleagues, 2025). At 70 kg that model predicts an oral peak about a third of what adults reach, so it was not used. Mould and colleagues' paper, the natural adult source, has diazepam's effect site but no disposition model: three hours of sampling could not define its elimination.

### Routes

- **Intravenous**: bolus or infusion.
- **Oral**: bioavailability 0.94 (Divoll and colleagues, *Anesth Analg* 1983;62:1-8). The absorption half-life, 20 minutes, was chosen so that 10 mg peaks at 302 ng/mL. The geometric mean peaks of 46 fasting adults were 286 and 338 ng/mL (Hogan and colleagues, *Epilepsia* 2020;61:455-464). The model's peak comes at 27 minutes, earlier than their median of 45 to 60 minutes.
- **Intramuscular**: bioavailability 1.0 (Hung and colleagues). The absorption half-life, 50 minutes, gives a peak of 200 ng/mL after 10 mg, as Hung measured, at 53 minutes rather than the observed 34. Diazepam precipitates in muscle and is absorbed erratically; Hung found absorption still at 20 to 50 per cent of its peak rate an hour after the injection.

Rectal gel and nasal spray are not offered.

### Covariates

None in the source. With the [fat-free-mass switch](help:models/fat-free-mass) on, the volumes scale with fat-free mass and the clearances with its 0.75 power; with it off, everyone receives the published values.

Diazepam's half-life rises steeply with age, from about 20 hours at 20 years to about 90 hours at 80, because its volume grows while its clearance does not (Klotz and colleagues). It is more than doubled in cirrhosis. Neither is modelled.

### Effect site

The time to peak effect is 2.6 minutes. Bührer and colleagues (*Clin Pharmacol Ther* 1990;48:555-567) related arterial concentrations to the EEG in volunteers and found an equilibration half-time of 1.6 minutes, against 4.8 minutes for midazolam. On this model's bolus curve, that puts the effect-site peak at 2.6 minutes. Mould and colleagues found 1.2 minutes for the Digit Symbol Substitution Test. Diazepam reaches its peak effect faster than midazolam, and much faster than [lorazepam](help:drugs/lorazepam).

### Typical concentrations

The shaded band runs from 150 to 600 ng/mL, with a typical value of 300:

- The bottom is a little above the effect-site concentration that halved DSST performance, 116 to 132 ng/mL (Mould and colleagues).
- 300 ng/mL has been cited as the effective therapeutic concentration (Chevassus and colleagues, *BMC Clin Pharmacol* 2004;4:3).
- 200 to 600 ng/mL is the target in status epilepticus (Ku and colleagues, *CPT Pharmacometrics Syst Pharmacol* 2018;7:718-727).

The default time-until-threshold level is 150 ng/mL. Diazepam is about a sixth as potent as midazolam (Mould and colleagues).

### Where to be careful

Diazepam's active metabolite, nordiazepam, is not modelled. About half of each dose reaches the circulation as nordiazepam (Greenblatt and colleagues, *J Clin Pharmacol* 1988;28:853-859), which has a half-life of days. After repeated doses, sedation therefore outlasts what this curve shows.

Because diazepam's elimination is so slow, its offset after a single dose comes from redistribution. After repeated doses, offset comes from elimination instead, and takes days.
