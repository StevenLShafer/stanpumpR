### The model

Ciprofol (HSK3486) is an intravenous hypnotic, a close analogue of propofol and about four to five times as potent. It is approved in China for sedation, for general anaesthesia and for sedation in intensive care. It is not approved in the United States.

stanpumpR has two ciprofol models and chooses between them by the patient's age and weight:

- [Liu 2024](help:drugs/ciprofolLiu), 462 adults aged 18 to 77, for adults;
- [Wang 2026](help:drugs/ciprofolWang), 25 children aged 1 to 9, for children.

A child older than 9 gets the children's model up to about 13 and a half years, and the adults' model after that, the one whose ages are nearer.

Both models use arterial samples. stanpumpR prefers arterial pharmacokinetics, because the brain sees the arterial concentration.

### Effect site

The time to peak effect after a bolus is 2.94 minutes. It comes from the bispectral index (BIS) model of Liu and colleagues (*J Clin Anesth* 2024;92:111317, [doi:10.1016/j.jclinane.2023.111317](https://doi.org/10.1016/j.jclinane.2023.111317)), who fitted ke0 7.04 /h in 115 adults having elective surgery. On their pharmacokinetic model, and on the adult model here, that ke0 puts the peak at 2.94 minutes. stanpumpR solves ke0 from that time on each patient's own model. The children's time to peak is assumed to be the adults'; no study has measured it in children.

### % effect

The Graph Option **% effect** draws a BIS panel from the same paper: an inhibitory sigmoid Emax model on the effect site, with EC50 582 ng/mL and Hill coefficient 2.14. Emax was fixed at 1, so the panel is the fall in BIS as a percentage of the patient's own baseline. With a baseline of 95, a BIS of 60 is a fall of 37% and a BIS of 40 a fall of 58%.

### Where to be careful

- **No band.** Neither paper gives a therapeutic range, and ciprofol is titrated to effect. No time until threshold is drawn.
- **No target-controlled infusion.** It is not offered.
- **Mostly Chinese patients.** 85% of Liu's adults and all of Wang's children were Asian.
- **Pregnancy.** No pharmacokinetic study in pregnancy has been published.
