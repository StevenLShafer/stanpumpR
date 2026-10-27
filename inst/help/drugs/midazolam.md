### The model

Midazolam's parameters were fitted to the data of Bührer and colleagues (*Clin Pharmacol Ther* 1990;48:544-554), who gave men aged 33 to 43 doses of 3.75 to 25 mg at 5 mg/min. Bührer's papers do not print the parameters; Zomorodi and colleagues do, in Table 3 of their study of sedation after coronary bypass surgery (*Anesthesiology* 1998;89:1418-1429). This was the set STANPUMP used for midazolam target-controlled infusion, and it drove the infusions in Zomorodi's study; later versions of STANPUMP carried the kinetics Zomorodi fitted in those patients instead. The model is a fixed three-compartment model with volumes of 3.3, 17.6 and 96.8 L and clearances of 0.535, 2.01 and 0.832 L/min. Earlier versions of this page credited Mould and colleagues (1995), whose paper reports only noncompartmental kinetics.

### Covariates

None in the source. The parameters are for a typical adult; they take the default [fat-free-mass scaling](help:models/fat-free-mass) and are used as published with the switch off. Midazolam's clearance is known to fall with age and with CYP3A4 inhibition, and its volume to rise with obesity; none of this is in the model.

### Effect site

The time to peak effect is 3.0 minutes, from the same volunteers as the kinetics. Bührer's EEG equilibration half-time, fitted parametrically against his three-compartment kinetics, was 5.6 minutes (*Clin Pharmacol Ther* 1990;48:555-567); with this model it puts the peak at 3.0 minutes. His nonparametric estimate, 4.8 minutes, would put it at 2.7. In his volunteers, the EEG effect of 3.75 mg given over 45 seconds peaked 2.2 and 2.9 minutes after the infusion started. Earlier versions of the library used 4 minutes, a value with no recorded source.

### Typical concentrations

The shaded band, 0.05 to 0.15 mcg/mL (50 to 150 ng/mL), is the sedation range of the AGNP consensus guidelines' 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5, doi [10.1055/a-2860-7861](https://doi.org/10.1055/a-2860-7861)), which 10 mg by mouth reaches after 1 to 2 hours; the laboratory alert level is 1 mcg/mL, and the guideline's level of recommendation is its lowest, 4. It attributes the range to Matsuda and colleagues (*Acta Anaesthesiol Scand* 2022;66:818-822), but in that study of oral premedication in children the serum concentration was the same, about 48 ng/mL, whether or not the child was sedated, so the study does not itself establish the range. It replaced 0.04 to 0.12 mcg/mL, whose source the library did not record. Reaction time slows above 0.02 mcg/mL. Concentrations needed for hypnosis as a sole agent are several times higher. The default recovery threshold is 0.04 mcg/mL.

### Where to be careful

The active metabolite α-hydroxymidazolam, which matters mainly in renal failure and during prolonged infusion, is not modelled. Midazolam's context-sensitive half-time grows markedly with long infusions because of its large slow compartment; the model will show this, but the model's typical adult is not the intensive care patient in whom it is most often seen.

### Pregnancy

With **Pregnant** ticked, the elimination clearance is multiplied by 1.28; the volumes and the distribution clearances are unchanged. The factor is from the one intravenous comparison, Wilson and colleagues (*Anaesthesia* 1987;42:1057-1062): 5 mg intravenously, sampled for 2 hours, in women in active labour, women awaiting caesarean section, women at the start of caesarean section, and two groups of nonpregnant gynaecological women. The women awaiting caesarean section, pregnant but neither labouring nor anaesthetised, cleared midazolam 1.28 times as fast as the gynaecological women once each group is set against this model at its own age and weight. The paired oral data of Hebert and colleagues (*Clin Pharmacol Ther* 2008;84:248-253; apparent oral clearance 1.7 to 1.8 times higher at 28 to 32 weeks), carried to an intravenous dose through the mechanism Quinney and colleagues fitted (hepatic CYP3A 1.6 times, unbound fraction 1.2 times, a well-stirred liver at 1.5 L/min), give 1.45, the same direction. In **active labour** Wilson found the clearance lower (0.91 times) and the volume smaller; the field does not represent labour. The time to peak effect is held, so the effect site equilibrates a little more slowly. See [Pregnancy](help:models/pregnancy).
