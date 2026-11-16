### The model

Irinotecan comes from Klein and colleagues (*Clin Pharmacol Ther* 2002;72:638-647): 78 patients in two dose-escalation studies, given 100 to 340 mg/m² over 90 minutes. Irinotecan, a prodrug, is described by three compartments, and all of its elimination forms the active metabolite [SN-38](help:drugs/irinotecanSN38), which is plotted in its own panel:

- central volume 5.52 L, clearance 25.2 L/h, steady-state volume 262 L;
- half-lives of under a minute, 2.5 hours and 13.5 hours.

SN-38 in turn forms SN-38 glucuronide, which the paper also modelled and the library does not carry.

### What is plotted

Plasma irinotecan in ng/mL. Doses are entered in mg as labelled (Camptosar is labelled as the hydrochloride trihydrate; the paper does not say which it recorded). A dose prescribed per m² must be multiplied by the body surface area first. There is no effect site and no band.

### Where to be careful

- **No covariates.** The paper reports its covariate effects only as percentages that cannot be rebuilt into equations, so the curve is the same for every adult apart from the library's own scaling for body size.
- **UGT1A1.** Patients with the *28 genotype form SN-38 glucuronide more slowly and are exposed to more SN-38; this was not studied and is not represented.
- **Not a monitoring tool.**

Every parameter was read in the paper's PDF.
