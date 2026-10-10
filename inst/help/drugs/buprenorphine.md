### The model

Buprenorphine's disposition is from Björnsson and colleagues (*Clin Pharmacokinet* 2023;62:1427-1443), a population model fitted to 10,658 concentrations from 252 people in four studies of intravenous, sublingual and subcutaneous depot (CAM2038) buprenorphine. It is three-compartment: clearance 52.1 L/h, central volume 64.3 L, peripheral volumes 130 and 1580 L with intercompartmental clearances 186 and 60.3 L/h. The half-lives are about 7 minutes, 1.5 hours and 41 hours.

This model was chosen over the larger Jones 2021 SUBLOCADE analysis because Björnsson's included intravenous data, so its parameters are on the absolute scale. Jones's are relative to the unknown bioavailability of the depot and cannot be used for an intravenous dose.

### Covariates

Clearance falls with age and rises with weight: CL = 52.1 × (age/35)<sup>−0.233</sup> × (weight/72.4)<sup>0.413</sup> L/h, which is 60.8 L/h at 18 years and 45.1 L/h at 65. With the [fat-free-mass switch](help:models/fat-free-mass) on, clearance sees the pharmacokinetic weight and the volumes and intercompartmental clearances take the library factors; with it off, clearance sees total weight and the rest are the published values.

The central volume is the healthy-volunteer value. The paper estimated 237 L in participants with opioid use disorder, but only the healthy volunteers received intravenous buprenorphine, and the authors attribute the difference to that.

### Sublingual

The **mg SL** unit is a sublingual tablet or film. The source model has two parallel absorption pathways, a fast burst and a slow mucosal tail. stanpumpR carries every route as a single first-order input, so one absorption constant (0.78/h, no lag) was fitted to the published input at 16 mg. It reproduces the time to peak (50 against 52 minutes) and the area under the curve. The peak is about 20% low and a daily trough about 15% low. At steady state, 16 mg daily gives about 0.8 to 5.7 ng/mL, against the paper's 0.85 to 6.1.

Sublingual bioavailability falls with dose in the source: 18% at 8 mg, 14% at 16 mg, 12% at 24 mg (0.14 × (dose/16)<sup>−0.371</sup>). The simulator uses **14%** for every sublingual dose. That suits doses used in treating opioid use disorder. It probably **under-predicts analgesic sublingual doses** (0.2 to 0.4 mg): the dose equation, taken far outside the range it was fitted on, gives about 60%, and Kuhlman 1996 measured 51% at 4 mg.

### Intranasal (research)

The **mg IN** unit follows Eriksen 1989: 0.3 mg by nasal spray in nine volunteers, bioavailability 48%, peak at 31 minutes. No intranasal product is approved. The predicted peak after 0.3 mg is about 0.45 ng/mL, well below the 1.8 ng/mL Eriksen reported by radioimmunoassay. That gap is unresolved.

### Effect site

ke0 is 0.00447/min (equilibration half-time 155 minutes), from Yassen and colleagues' model of buprenorphine's antinociceptive effect in healthy volunteers (*Anesthesiology* 2006;104:1232-1242). Their model also had slow receptor binding. They found that equilibration with the biophase, not receptor kinetics, limits the rate of onset and offset, so a single effect compartment can stand in for it. The effect site peaks about 2¼ hours after an intravenous bolus. Escher 2007 observed maximum antinociception at 2 hours after 0.15 mg.

### Typical concentrations

There is no established MEAC for buprenorphine. As a high-affinity partial agonist it does not add to full agonists the way the MEAC panel assumes, so it is not on that panel. The shaded band is the **opioid use disorder** range:

- 1.25 ng/mL: withdrawal suppression. This is also the recovery threshold.
- 2.2 ng/mL: 70% μ-receptor occupancy, by Nasser 2014's model (occupancy = 91.4% × C/(0.67 + C)).
- 3 ng/mL: the upper end of the 2 to 3 ng/mL quoted for blocking opioid reinforcement.

These are plasma concentrations from maintenance treatment. They are not analgesic targets.

### Not offered

Intramuscular and swallowed oral doses are not offered: no usable human study was found for either, and they are not borrowed from another route. Long-acting products are also not offered: the weekly and monthly depots (Brixadi/Buvidal, Sublocade), the patches (Butrans, Transtec) and the implant. Each releases drug through parallel pathways, a zero-order phase or a removal event, which a single first-order input cannot represent.

### Where to be careful

This is a research simulation. Its parameters are typical values, and the spread between patients is wide: the central volume alone varies by about 80%. Norbuprenorphine is not modelled. The respiratory "ceiling" seen in volunteer studies does not mean an overdose is safe, especially with benzodiazepines or other sedatives.
