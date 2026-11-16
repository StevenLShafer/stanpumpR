### The model

Cladribine's parameters come from Lindemalm and colleagues (*BMC Pharmacol* 2005;5:4), a population analysis of 1102 plasma concentrations from 161 patients with lymphoid malignancies, mostly hairy cell and chronic lymphocytic leukaemia, treated in Sweden and the UK between 1990 and 1996. They were 22 to 89 years old (mean 60) and weighed 48 to 118 kg; four in five were men. Each had a 2-hour intravenous infusion, a continuous infusion, an oral solution or a subcutaneous injection, and all three routes were fitted together. Three compartments describe the disposition: a clearance of 39.3 L/h, a central volume of 71.7 L, peripheral volumes of 475 and 73.6 L, and intercompartmental clearances of 51.1 and 105 L/h. The half-lives are 0.2, 1.3 and 16 hours. An oral dose is absorbed at 1.31 per hour with a bioavailability of 35%, and a subcutaneous one at 2.48 per hour, completely.

### What is plotted

Plasma cladribine in ng/mL. Cladribine acts as its triphosphate inside lymphocytes, which is not plotted, and neither is its metabolite 2-chloroadenine. There is no effect site and no band: no therapeutic concentration range is established. Doses are entered in mg; a dose prescribed per kilogram or per m² has to be converted first.

### Covariates

None was significant. Age, weight, height, sex, body surface area, diagnosis, creatinine clearance, liver tests and lymphocyte count were all tested, and the authors conclude that a fixed dose does as well as dosing by weight or surface area. With [the scaling switch](help:models/fat-free-mass) on, the parameters are scaled to fat-free mass from the 70-kg reference man; with it off they are the published values for everyone.

### Where to be careful

- **Renal impairment.** About half of cladribine's clearance is renal in children (Kearns 1994), but no patient in this study had severe renal impairment, so the model has no renal term and the curve is that of a patient with near-normal renal function, whatever creatinine is entered.
- **The oral route is the solution.** The oral data are a solution in saline taken after an overnight fast. The multiple-sclerosis tablet (Mavenclad) is a different product with its own model (Savic 2017), which this is not.
- **Adults only.** Children get the adult model scaled by size, an extrapolation.

Every parameter was confirmed against the paper's PDF (its Table 3). See [chemotherapy](help:models/chemotherapy) for the other models considered.
