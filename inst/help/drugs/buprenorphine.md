### The model

Buprenorphine's disposition is from Björnsson and colleagues (*Clin Pharmacokinet* 2023;62:1427-1443), a population model fitted to 10,658 concentrations from 252 people in four studies of intravenous, sublingual and subcutaneous depot (CAM2038) buprenorphine. It is three-compartment: clearance 52.1 L/h, central volume 64.3 L, peripheral volumes 130 and 1580 L with intercompartmental clearances 186 and 60.3 L/h. The half-lives are about 7 minutes, 1.5 hours and 41 hours.

This model was chosen over the larger Jones 2021 SUBLOCADE analysis because Björnsson's included intravenous data, so its parameters are on the absolute scale. Jones's are relative to the unknown bioavailability of the depot and cannot be used for an intravenous dose.

### Covariates

Clearance falls with age and rises with weight: CL = 52.1 × (age/35)<sup>−0.233</sup> × (weight/72.4)<sup>0.413</sup> L/h, which is 60.8 L/h at 18 years and 45.1 L/h at 65. The source studied adults, and the age term grows without limit in early childhood, so it is evaluated at no younger than 18 years: a child gets the clearance of an 18-year-old of the same size. The model is not validated in children. With the [fat-free-mass switch](help:models/fat-free-mass) on, clearance sees the pharmacokinetic weight and the volumes and intercompartmental clearances take the library factors; with it off, clearance sees total weight and the rest are the published values.

The central volume is the healthy-volunteer value. The paper estimated 237 L in participants with opioid use disorder, but only the healthy volunteers received intravenous buprenorphine, and the authors attribute the difference to that.

### Sublingual

The **mg SL** unit is a sublingual tablet or film. The source model has two parallel absorption pathways, a fast burst and a slow mucosal tail. stanpumpR carries every route as a single first-order input, so one absorption constant (1.03/h) and a lag (17.6 minutes, the tablet dissolving) were fitted together to the published input at 16 mg. The peak comes within about 5% of the published one (5.8 against 6.1 ng/mL, at 56 against 52 minutes). The slow tail is not reproduced, so a daily trough is about 15% low. At steady state, 16 mg daily gives about 0.8 to 6.6 ng/mL, against the paper's 0.85 to 6.1.

Because of the lag, the *time until threshold* reads "not yet absorbed" for the first 17.6 minutes after each sublingual dose, scheduled repeats included. The engine has no state for a dose until its lag has passed. Without the lag the peak was about 20% low; the lag was preferred because the peak matters more for this drug.

Sublingual bioavailability falls with dose in the source: 18% at 8 mg, 14% at 16 mg, 12% at 24 mg (0.14 × (dose/16)<sup>−0.371</sup>). Like gabapentin's oral absorption, each sublingual dose is scaled by its own fraction absorbed, 0.423 × (1 − 0.817 × D/(3.43 + D)) with D in mg. This form was fitted to the power law over 2 to 32 mg, the range of the marketed tablets and films, and agrees with it within 2.5% there (see the table under Extravascular routes). Below 2 mg the power law climbs without limit (71% at 0.2 mg); this form levels off at 42%, nearer the 51% Kuhlman 1996 measured at 4 mg. Analgesic sublingual doses (0.2 to 0.4 mg, about 40% here) are still outside the range the source was fitted on. Two doses entered as separate rows at the same time are scaled separately, not by their sum.

### Intranasal: withdrawn

Buprenorphine is no longer offered intranasally. Until October 2026 an **mg IN** research route followed Eriksen and colleagues (*J Pharm Pharmacol* 1989;41:803-805): 0.3 mg by nasal spray in nine volunteers, bioavailability 48%, peak at 31 minutes. On this disposition it predicted a peak of 0.45 ng/mL after 0.3 mg, a quarter of the 1.77 ng/mL Eriksen measured by radioimmunoassay, and no bioavailability can close the gap: it would have to be 191%. The discrepancy lies in the disposition or the assay, not the absorption; intranasal data were not part of Björnsson's fit, and no intranasal product is approved. The route would need a fit of a full intranasal concentration-time profile to come back.

### Effect site

The effect site comes from Yassen and colleagues' model of buprenorphine's antinociceptive effect in healthy volunteers (*Anesthesiology* 2006;104:1232-1242): ke0 0.00447/min, an equilibration half-time of 155 minutes. Their model also had slow receptor binding. They found that equilibration with the biophase, not receptor kinetics, limits the rate of onset and offset, so a single effect compartment can stand in for it.

That ke0 was estimated with Yassen's own disposition, not Björnsson's. So it is carried as the time to peak effect it gives there, 61 minutes after an intravenous bolus, and the ke0 is solved again for Björnsson's disposition (the method of Minto and colleagues, *Anesthesiology* 2003;99:324-333). Put directly on Björnsson's disposition, the same ke0 would delay the peak to about 2¼ hours. Yassen observed peak effect at 70 to 100 minutes; the receptor-binding step accounts for the small extra delay. Escher 2007's maximum at 2 hours is for the RIII reflex and electrical thresholds; cold-pressor tolerance peaked at 30 minutes.

### Typical concentrations

There is no established MEAC for buprenorphine. As a high-affinity partial agonist it does not add to full agonists the way the MEAC panel assumes, so it is not on that panel.

The shaded band, **0.2 to 1 ng/mL** with 0.4 as the typical value, is the library's own analgesic band, read on the effect site. It comes from simulating, with this model, the doses of the Oxford postoperative studies:

- **0.2 ng/mL, the lower edge and the time-until-threshold level.** McQuay and colleagues (*Br J Anaesth* 1980;52:1013-1019) gave 20 patients 0.3 mg intravenously at induction for hip replacement and 0.3 mg again 3 hours later, then let them call for diamorphine on demand. Analgesia lasted a median of 480 minutes after the second dose. At that time this model gives an effect-site concentration of 0.20 ng/mL (a 65-year-old, 66 kg man, the study's average patient; plasma 0.15). That is an estimate of the concentration at which those patients asked for more pain relief, a minimum effective concentration by the demand method, but an illustrative one: it is simulated for one reference patient at the group's median time (the time to the fifth demand; the second to fourth gave similar medians), not measured at any patient's demand, and each earlier demand had already delivered 0.25 mg of diamorphine. It is not a validated threshold.
- **1 ng/mL, the upper edge.** The simulated peak of the same two doses. It is also about the top of 0.3 mg intravenously every 6 hours, and the same group reported excellent analgesia from sublingual buprenorphine at plasma levels that never exceeded 1 ng/mL.
- **0.4 ng/mL, the typical value.** The steady-state peak of 0.4 mg sublingually every 8 hours (Bullingham and colleagues, *Br J Clin Pharmacol* 1981;12:117-122, about 9 hours of analgesia), and of a 20 mcg/h patch. The patches' measured peaks are 0.18 to 0.47 ng/mL at 5 to 20 mcg/h (Reidenberg 2001, quoted by Johnson and colleagues 2005).

The band is read on the effect site because buprenorphine's effect far outlasts its plasma level: the Oxford group found no correlation between peak plasma concentration and the duration of analgesia (Bullingham and colleagues, *Clin Pharmacol Ther* 1980;28:667-672), and in chronic pain no correlation between level and analgesia was found either (Adriaensen 1985, quoted by Johnson and colleagues, *J Pain Symptom Manage* 2005;29:297-326). Yassen's slow equilibration carries most of that lag.

The AGNP consensus guidelines' 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5, doi [10.1055/a-2860-7861](https://doi.org/10.1055/a-2860-7861)) gives 0.5 to 2.5 ng/mL, sampled 24 hours after a dose, with a laboratory alert level of 4 ng/mL, but for substitution treatment of **opioid use disorder**, not for analgesia, so it is not drawn, as methadone's and morphine's ranges for that use are not. It was drawn until October 2026. It is a trough: sublingual doses of 8 to 24 mg a day give troughs of 0.7 to 1.4 ng/mL, averages of 1.4 to 2.8 and peaks of 4.3 to 8.9 (Laffont and colleagues, *Front Pharmacol* 2022;13:1052113). Laffont found opioid use and craving at their lowest when the concentration was held at 2 to 3 ng/mL (68 to 75% μ-receptor occupancy), withdrawal controlled from about 1 ng/mL, and withdrawal scores at their lowest near 4 ng/mL. By Nasser 2014's model (occupancy = 91.4% × C/(0.67 + C)), 70% μ-receptor occupancy is 2.2 ng/mL.

### Not offered

Intramuscular and swallowed oral doses are not offered: no usable human study was found for either, and they are not borrowed from another route. Long-acting products are also not offered: the weekly and monthly depots (Brixadi/Buvidal, Sublocade), the patches (Butrans, Transtec) and the implant. Each releases drug through parallel pathways, a zero-order phase or a removal event, which a single first-order input cannot represent.

### Where to be careful

This is a research simulation. Its parameters are typical values, and the spread between patients is wide: the central volume alone varies by about 80%. Norbuprenorphine is not modelled. The respiratory "ceiling" seen in volunteer studies does not mean an overdose is safe, especially with benzodiazepines or other sedatives.

### Pregnancy

With **Pregnant** ticked, the elimination clearance is multiplied by 1.83; the volumes, the distribution clearances and the sublingual absorption are unchanged. Caritis's group studied women on maintenance buprenorphine in pregnancy and again after delivery: Bastian and colleagues (*Am J Obstet Gynecol* 2017;216:64.e1-64.e7) found the dose-normalised exposure about 50% lower in pregnancy, with the time to peak unchanged, and Zhu and colleagues (*J Addict Med* 2025;19:129-134; 22 women) found it 119 against 218 in the second half of pregnancy, a ratio of 1.83. The cause is faster metabolism by CYP3A and glucuronidation, not a change in absorption, so the factor is applied to the clearance of the intravenous disposition and every route is changed alike. The time to peak effect is held, so the effect site equilibrates a little more slowly. See [Pregnancy](help:models/pregnancy).
