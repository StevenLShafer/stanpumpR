### The model

Cyclophosphamide comes from Joerger and colleagues (*Clin Pharmacokinet* 2007;46:1051-1068): 59 women with breast cancer given 600 mg/m² over 15 minutes after doxorubicin, in their first cycle. One compartment describes it:

- clearance 4.23 L/h; volume 34.6 L; half-life 5.7 hours.

No body-size covariate was found. The same paper is the library's [doxorubicin](help:drugs/doxorubicin) model.

### What is plotted

Plasma cyclophosphamide in mcg/mL (mg/L). Cyclophosphamide is a **prodrug**: its active metabolite, 4-hydroxycyclophosphamide, was not measured and is not plotted. Doses are entered in mg or as an infusion rate; a dose prescribed per m² must be multiplied by the body surface area first. There is no effect site and no band.

### Where to be careful

- **Autoinduction.** Cyclophosphamide induces its own metabolism over days of repeated or high-dose treatment; each patient here gave one profile in the first cycle, and the curve does not speed up with repeated doses.
- **Enzyme-inducing antiepileptics** nearly double the clearance (×1.89 with carbamazepine in this study); the curve assumes none.
- **Women with breast cancer**, at conventional doses. Not a monitoring tool.

Every parameter was read in the paper's PDF.
