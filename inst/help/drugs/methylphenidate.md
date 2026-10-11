### What is plotted

The plasma concentration of **d-methylphenidate** (d-MPH), the active enantiomer, in ng/mL, after an oral dose of racemic **immediate-release** methylphenidate (Ritalin) entered as the tablet strength. The inactive l-enantiomer is not modelled.

### The model

The parameters are from Lyauk and colleagues (*Clin Transl Sci* 2016;9:337-345), who fitted 503 d-MPH concentrations from 122 healthy Danish adults after a single 10 mg Ritalin tablet: three transit compartments into an absorption depot (mean transit time 0.505 h, absorption 0.418/h), then two compartments, with apparent clearance 233 L/h, central volume 97.6 L, intercompartmental clearance 70.1 L/h and peripheral volume 252 L at 70 kg. Women's mean transit time was 1.925 times men's.

### Dose basis

The parameters are **apparent** d-MPH parameters, and the paper does not say whether its dose record was the 10 mg tablet or its 5 mg of d-MPH, a factor of two in every concentration. In the same subjects, Stage and colleagues (*Br J Clin Pharmacol* 2017;83:1506-1514) measured a median d-MPH AUC of 21.4 ng·h/mL after 10 mg, which with Lyauk's clearance is exactly a 5 mg input. So **half the tablet mass is converted to d-MPH**, once; that 0.5 is shown as the bioavailability but it is a dose basis, not an oral bioavailability. A 10 mg dose here gives an AUC of 21.4 ng·h/mL and a peak of about 5 ng/mL at 1.1 to 1.5 hours.

### Oral only

Apparent parameters predict oral concentrations but not intravenous ones, so only oral units are offered. **mg PO** is the immediate-release tablet (Ritalin), and the frequencies (bid, tid) repeat it. **mg PO XR** is Concerta (below). Ritalin LA, Aptensio XR, Metadate CD and the other extended-release products are different inputs and are not represented: **mg PO XR here means Concerta only**.

### Concerta

No published model gives Concerta's input in numbers, so it was **fitted for stanpumpR** and attached to the disposition above. The fit used the shape of the mean curve that Childress and colleagues measured after Concerta 54 mg and 2 × 36 mg in fasted healthy adults (*Clin Pharmacol Drug Dev* 2025;14:829-835). Each Concerta dose is given as:

- **22 per cent at once**, the drug in the tablet's overcoat (the label's figure), absorbed like the immediate-release tablet;
- **78 per cent from 2 to 15 hours**, the osmotic core, delivered at a rate falling steadily to zero and given as small doses every 15 minutes.

The 2 and 15 hours were chosen so that the curve's shape matches Childress's. They do not depend on how high the curve is.

| Share of the area under the curve | 0–3 h | 3–7 h | 7–12 h | after 12 h | Peak time |
|---|---|---|---|---|---|
| Model | 0.11 | 0.27 | 0.35 | 0.27 | 6.8 h |
| Concerta 54 mg | 0.10 | 0.26 | 0.33 | 0.31 | 7.0 h |
| Concerta 2 × 36 mg | 0.11 | 0.30 | 0.34 | 0.25 | 6.5 h |

The falling rate describes delivery into the blood, not release from the tablet. The pump releases steadily, but drug released late, in the colon, is absorbed less well.

**The curve is lower than Childress measured.** The disposition is unchanged, so the amount absorbed is fixed by the dose basis and clearance above. The model gives an area under the curve of 116 ng·h/mL after 54 mg, against Childress's 174 (33 per cent low), and a peak of 8.9 against 14.6 ng/mL (39 per cent low). This is a difference between studies rather than between formulations: the same clearance reproduces Stage's measured d-methylphenidate exposure after the immediate-release tablet. Childress measured total methylphenidate, l-isomer included, in a different population and laboratory. The shape is the reliable part. Treat the height as uncertain by about a third, in the direction of being too low.

### How the absorption is reduced

The engine absorbs first-order after a lag; it cannot carry a chain of transit compartments. The lag (21 min in men, 38 min in women) and absorption rate were fitted to the published model's own typical curve, with its disposition unchanged. The area, clearance and terminal decline are exact, and the peak height is within 1 per cent; the peak comes about 7 minutes early in men and 18 minutes early in women, and the curve is up to 20 to 30 per cent low in the first half hour after the lag. From 4 hours on the two agree within 3 per cent of the peak.

### Covariates

Weight scales the model to fat-free mass with the [fat-free-mass switch](help:models/fat-free-mass) on; with it off the published allometry on total weight is used (clearances by weight to the 0.75, volumes by weight). Sex lengthens absorption as Lyauk found.

Lyauk also estimated large effects of **CES1 genotype** on clearance (the G143E variant, rs115629050 and CES1A2 copies, which raise exposure by 22 to 143 per cent). The Patient Profile has no CES1 field, so every patient here is the wild-type reference.

### Population and variability

Healthy adults after one 10 mg dose. **Children are an extrapolation**: for children, use the separate entry [methylphenidate in children](help:drugs/methylphenidatePediatric), Shader and colleagues' model of 273 children. That entry plots total methylphenidate rather than d-methylphenidate, so its numbers are not on the same scale as this page's.

The variability between people is large and is not shown: Lyauk estimated 62 per cent on the transit time, 22 per cent on clearance and 90 per cent on the central volume, so an individual's peak can sit far from this curve.

### Effect site and typical range

None. No calibrated concentration-effect model exists for a classroom measure (SKAMP, PERMP) or for weekly symptom scores (ADHD-RS-IV) after Ritalin, and no concentration is an established therapeutic threshold, so no band is drawn. The Emax relationship between peak concentration and ADHD-RS-IV of Teuscher and colleagues (2015) was fitted to Aptensio XR and is not used. Nothing on this plot is a dose recommendation.
