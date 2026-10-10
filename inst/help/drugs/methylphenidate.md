### What is plotted

The plasma concentration of **d-methylphenidate** (d-MPH), the active enantiomer, in ng/mL, after an oral dose of racemic **immediate-release** methylphenidate (Ritalin) entered as the tablet strength. The inactive l-enantiomer is not modelled.

### The model

The parameters are from Lyauk and colleagues (*Clin Transl Sci* 2016;9:337-345), who fitted 503 d-MPH concentrations from 122 healthy Danish adults after a single 10 mg Ritalin tablet: three transit compartments into an absorption depot (mean transit time 0.505 h, absorption 0.418/h), then two compartments, with apparent clearance 233 L/h, central volume 97.6 L, intercompartmental clearance 70.1 L/h and peripheral volume 252 L at 70 kg. Women's mean transit time was 1.925 times men's.

### Dose basis

The parameters are **apparent** d-MPH parameters, and the paper does not say whether its dose record was the 10 mg tablet or its 5 mg of d-MPH, a factor of two in every concentration. In the same subjects, Stage and colleagues (*Br J Clin Pharmacol* 2017;83:1506-1514) measured a median d-MPH AUC of 21.4 ng·h/mL after 10 mg, which with Lyauk's clearance is exactly a 5 mg input. So **half the tablet mass is converted to d-MPH**, once; that 0.5 is shown as the bioavailability but it is a dose basis, not an oral bioavailability. A 10 mg dose here gives an AUC of 21.4 ng·h/mL and a peak of about 5 ng/mL at 1.1 to 1.5 hours.

### Oral only, immediate release only

Apparent parameters predict oral concentrations but not intravenous ones, so only oral units are offered. The model describes the immediate-release tablet. **Concerta, Ritalin LA, Aptensio XR and the other extended-release products are not represented**: each has its own product-specific input, and none has a published numerical input model this engine could carry. Entering a Concerta strength here simulates an immediate-release tablet of that size, which is wrong. The frequencies (bid, tid) repeat the immediate-release dose.

### How the absorption is reduced

The engine absorbs first-order after a lag; it cannot carry a chain of transit compartments. The lag (21 min in men, 38 min in women) and absorption rate were fitted to the published model's own typical curve, with its disposition unchanged. The area, clearance and terminal decline are exact, and the peak height is within 1 per cent; the peak comes about 7 minutes early in men and 18 minutes early in women, and the curve is up to 20 to 30 per cent low in the first half hour after the lag. From 4 hours on the two agree within 3 per cent of the peak.

### Covariates

Weight scales the model to fat-free mass with the [fat-free-mass switch](help:models/fat-free-mass) on; with it off the published allometry on total weight is used (clearances by weight to the 0.75, volumes by weight). Sex lengthens absorption as Lyauk found.

Lyauk also estimated large effects of **CES1 genotype** on clearance (the G143E variant, rs115629050 and CES1A2 copies, which raise exposure by 22 to 143 per cent). The Patient Profile has no CES1 field, so every patient here is the wild-type reference.

### Population and variability

Healthy adults after one 10 mg dose. **Children are an extrapolation.** The pediatric population model (Shader and colleagues, *J Clin Pharmacol* 1999;39:775-785; 273 children, clearance 90.7 mL/min/kg, half-life 4.5 hours) is published without an absorption rate, so it cannot produce a curve without an invented one and is not offered.

The variability between people is large and is not shown: Lyauk estimated 62 per cent on the transit time, 22 per cent on clearance and 90 per cent on the central volume, so an individual's peak can sit far from this curve.

### Effect site and typical range

None. No calibrated concentration-effect model exists for a classroom measure (SKAMP, PERMP) or for weekly symptom scores (ADHD-RS-IV) after Ritalin, and no concentration is an established therapeutic threshold, so no band is drawn. The Emax relationship between peak concentration and ADHD-RS-IV of Teuscher and colleagues (2015) was fitted to Aptensio XR and is not used. Nothing on this plot is a dose recommendation.
