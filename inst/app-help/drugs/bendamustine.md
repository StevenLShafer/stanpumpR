### The model

Bendamustine comes from Owen and colleagues (*Cancer Chemother Pharmacol* 2010;66:1039-1049): 78 adults with rituximab-refractory indolent non-Hodgkin lymphoma, aged 31 to 84 years, given 120 mg/m² over 60 minutes on days 1 and 2 of 21-day cycles. Three compartments describe it, with no covariates (sex, age, weight, body surface area, creatinine clearance and liver tests were all tested and none was kept):

- clearance 31.7 L/h;
- central volume 14.1 L, peripheral volumes 0.92 L and 25.2 L;
- intercompartmental clearances 0.989 L/h and 0.159 L/h.

The half-lives are 0.29, 0.7 and 110 hours, as the paper reports; the last phase holds less than 1% of the drug's area, so in practice bendamustine is gone within a few hours of an infusion. For 120 mg/m² at the cohort's median 2.0 m² the model peaks at about 6000 ng/mL at the end of the hour, against the paper's median of 5839, and the two daily doses give an area of 13,750 ng·h/mL against its median 13,635.

### Dose: the hydrochloride

Enter doses as **bendamustine hydrochloride**, the labelled amount (TREANDA and its generics; 394.7 g/mol). The paper does not say how it handled the salt, but its peaks and areas are reproduced only when the dose is converted to bendamustine itself (358.3 g/mol), so the model converts it: each mg of the hydrochloride is 0.908 mg of bendamustine. The curve is plasma bendamustine in ng/mL.

### What is not plotted

The two active metabolites, M3 and M4, were each fitted on their own rather than as products of the parent, so they cannot be drawn from this model; the label puts their concentrations at about a tenth and a hundredth of the parent's.

### Body size

With no size covariate, the parameters are taken as the reference adult's and scaled by the library's factors when the fat-free-mass switch is on (fixed when it is off). The study's patients were larger (median 85 kg).

### Where to be careful

- **The label differs:** the TREANDA label gives a clearance of about 700 mL/min (42 L/h) and a steady-state volume of about 25 L, earlier summaries that this model does not use.
- **No therapeutic range:** the paper relates exposure (area and peak) to response, not a concentration band.
- **Not in pregnancy:** no pregnancy pharmacokinetics are known (the label warns of fetal harm), and nothing is adjusted.
- **Not a monitoring tool:** the curve is a population prediction.

The parameters were read in the paper's PubMed Central text; the PDF is still to confirm them. See [chemotherapy](help:models/chemotherapy).
