### The model

Midazolam's parameters were fitted to the data of Bührer and colleagues (*Clin Pharmacol Ther* 1990;48:544-554), who gave men aged 33 to 43 doses of 3.75 to 25 mg at 5 mg/min. Bührer's papers do not print the parameters; Zomorodi and colleagues do, in Table 3 of their study of sedation after coronary bypass surgery (*Anesthesiology* 1998;89:1418-1429). This was the set STANPUMP used for midazolam target-controlled infusion, and it drove the infusions in Zomorodi's study; later versions of STANPUMP carried the kinetics Zomorodi fitted in those patients instead. The model is a fixed three-compartment model with volumes of 3.3, 17.6 and 96.8 L and clearances of 0.535, 2.01 and 0.832 L/min. Earlier versions of this page credited Mould and colleagues (1995), whose paper reports only noncompartmental kinetics.

### Covariates

None in the source. The parameters are for a typical adult; they take the default [fat-free-mass scaling](help:models/fat-free-mass) and are used as published with the switch off. Midazolam's clearance is known to fall with age and with CYP3A4 inhibition, and its volume to rise with obesity; none of this is in the model.

### Effect site

The time to peak effect is 3.0 minutes, from the same volunteers as the kinetics. Bührer's EEG equilibration half-time, fitted parametrically against his three-compartment kinetics, was 5.6 minutes (*Clin Pharmacol Ther* 1990;48:555-567); with this model it puts the peak at 3.0 minutes. His nonparametric estimate, 4.8 minutes, would put it at 2.7. In his volunteers, the EEG effect of 3.75 mg given over 45 seconds peaked 2.2 and 2.9 minutes after the infusion started. Earlier versions of the library used 4 minutes, a value with no recorded source.

### Typical concentrations

The shaded band, 0.04 to 0.12 mcg/mL, is a sedation range. Concentrations needed for hypnosis as a sole agent are several times higher. The default recovery threshold is 0.04 mcg/mL.

### Where to be careful

The active metabolite α-hydroxymidazolam, which matters mainly in renal failure and during prolonged infusion, is not modelled. Midazolam's context-sensitive half-time grows markedly with long infusions because of its large slow compartment; the model will show this, but the model's typical adult is not the intensive care patient in whom it is most often seen.

### Pregnancy

With **Pregnant** ticked, the elimination clearance is multiplied by 1.45; the volumes and the distribution clearances are unchanged. The only paired data are oral: Hebert and colleagues (*Clin Pharmacol Ther* 2008;84:248-253) gave 13 women 2 mg of oral midazolam at 28 to 32 weeks and again after delivery, and the apparent oral clearance was 1.7 to 1.8 times higher in pregnancy. An oral ratio overstates the change after an intravenous dose, so it is carried to this intravenous model through the mechanism Quinney and colleagues (*CPT Pharmacometrics Syst Pharmacol* 2012;1:e2) fitted to the same data, hepatic CYP3A activity 1.6 times higher and an unbound fraction 1.2 times higher, in a well-stirred liver with a blood flow of 1.5 L/min: the clearance of 0.535 L/min becomes 0.774 L/min, 1.45 times higher (1.54 with a blood flow of 2 L/min). The older intravenous studies disagree: Wilson and colleagues (1987) found the lowest clearance in women in active labour, so labour itself may slow it. The time to peak effect is held, so the effect site equilibrates a little more slowly. See [Pregnancy](help:models/pregnancy).
