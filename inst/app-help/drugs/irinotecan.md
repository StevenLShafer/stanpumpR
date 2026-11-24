### The model

Irinotecan comes from Klein and colleagues (*Clin Pharmacol Ther* 2002;72:638-647): 78 patients in two dose-escalation studies, given 100 to 340 mg/m² over 90 minutes. Irinotecan, a prodrug, is described by three compartments, and all of its elimination forms the active metabolite [SN-38](help:drugs/irinotecanSN38), which is plotted in its own panel:

- central volume 5.52 L, clearance 25.2 L/h, steady-state volume 262 L;
- half-lives of under a minute, 2.5 hours and 13.5 hours.

SN-38 in turn forms SN-38 glucuronide, which the paper also modelled and the library does not carry.

### What is plotted

Plasma irinotecan in ng/mL. Doses are entered in mg as labelled (Camptosar is labelled as the hydrochloride trihydrate; the paper does not say which it recorded). A dose prescribed per m² or per kg can be entered as such (mg/m², mg/kg; body surface area by Mosteller, from the Patient Profile's weight and height). There is no effect site and no band.

### Where to be careful

- **Interactions are not modelled.** Strong CYP3A4 or UGT1A1 inhibitors (ketoconazole) raise irinotecan and SN-38 exposure, and strong CYP3A4 inducers lower it; the Camptosar label says not to give strong CYP3A4 inhibitors with irinotecan (7.2, 7.3). The enzyme-inducing antiseizure drugs (carbamazepine, phenytoin, phenobarbital) are such inducers, and the library's antiseizure interactions do not reach irinotecan.
- **No covariates.** The paper reports its covariate effects only as percentages that cannot be rebuilt into equations, so the curve is the same for every adult apart from the library's own scaling for body size.
- **UGT1A1.** Patients with the *28 genotype form SN-38 glucuronide more slowly and are exposed to more SN-38; this was not studied and is not represented.
- **Not a monitoring tool.**

Every parameter was read in the paper's PDF.
