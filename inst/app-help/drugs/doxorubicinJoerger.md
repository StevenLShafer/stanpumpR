### The model

Doxorubicin comes from Joerger and colleagues (*Clin Pharmacokinet* 2007;46:1051-1068): 59 women with breast cancer given 60 mg/m² over 15 minutes, before cyclophosphamide, in their first cycle. Two compartments describe doxorubicin, and all of its clearance forms the metabolite [doxorubicinol](help:drugs/doxorubicinol), which is plotted in its own panel:

- clearance 47.6 L/h × (body surface area / 1.8 m²)^1.4 × (age / 56)^−0.54 × (AST / 21 U/L)^−0.24, the AST from the Patient Profile's **Serum AST** (21 U/L when blank);
- central volume 12.3 L, and a peripheral compartment of 421 L exchanging at 60.3 L/h.

The half-lives are 5 minutes and 11 hours. The terminal half-life usually reported is 20 to 48 hours; the samples here stopped at 24 hours, so the curve's late tail falls too fast.

### What is plotted

Plasma doxorubicin in mcg/mL (mg/L). Doses are entered **as labelled, in mg of doxorubicin hydrochloride**, and the curve is the base: every volume and clearance in the tables below is the paper's divided by 0.937, the base's share of the hydrochloride (so V1 shows as 13.1 L). A dose prescribed per m² or per kg can be entered as such (mg/m², mg/kg; body surface area by Mosteller, from the Patient Profile's weight and height). There is no effect site and no band.

### Covariates

- **Body surface area** (Mosteller's formula; the paper names none) and **age**, on clearance; age is held at 18 for younger patients.
- **AST** is read from the Patient Profile's Serum AST field (21 U/L, the equation's reference, when blank): clearance is 15% lower at an AST of 41 U/L. The study printed the AST only as a median of 24 and an interquartile range of 15 to 38 U/L, so an AST far outside that range is an extrapolation. Do not also lower the Liver function slider for the same injury. A raised bilirubin, the usual basis for reducing the dose, was not a covariate.
- **Serum creatinine** acts only on doxorubicinol.

### Where to be careful

- **The terminal phase is underestimated** (sampling to 24 hours), so late concentrations are too low.
- **Liver impairment** is not represented. Not a monitoring tool.
- The same paper is the library's [cyclophosphamide](help:drugs/cyclophosphamide) model.

Every parameter was read in the paper's PDF.

### Pregnancy

With Pregnant ticked, Janssen 2021's factors are applied (clearance ×1.14, central volume ×1.08), from 22 pregnant women; exposure falls about a tenth, and doxorubicinol is formed faster with the parent's clearance.
