### The model

Docetaxel comes from Bruno and colleagues (*J Pharmacokinet Biopharm* 1996;24:153-172), the model behind the drug's labelling: 547 adults at their first cycle, 26 sampled richly in phase 1 and 521 sparsely in phase 2, given 75 or 100 mg/m² over one hour. Three compartments describe the disposition:

- clearance = body surface area × (22.1 − 3.55 × α1-acid glycoprotein − 0.095 × age + 0.225 × albumin) L/h, the proteins in g/L, reduced by a third in patients with both raised transaminases and raised alkaline phosphatase;
- central volume 8.31 L; peripheral compartments of 5.1 L and 135 L.

At the study's mean covariates clearance is 36.7 L/h, and the half-lives are 6 minutes, 30 minutes and 11.4 hours, as the paper reports.

### What is plotted

Plasma docetaxel in mcg/mL (mg/L). Doses are entered in mg or as an infusion rate; a dose prescribed per m² must be multiplied by the body surface area first. There is no effect site and no band.

### Covariates

- **Body surface area** (Mosteller's formula; the paper names none) and **age**, on clearance. Age is held at 18 for younger patients.
- **Albumin**, from the Patient Profile; left blank, the study's mean, 4.01 g/dL.
- **Held at the reference**, because the Patient Profile has no field for them: α1-acid glycoprotein at 1.46 g/L, the study's mean, and normal liver tests. Clearance is about 19% lower at a glycoprotein of 2.6 g/L, and a third lower in a patient with transaminases above 60 IU/L and alkaline phosphatase above 300 IU/L, the group in whom docetaxel is most toxic.

### Where to be careful

- **Liver impairment** lowers clearance far more than the curve shows unless the liver tests are normal.
- **CYP3A4 inhibitors** (ketoconazole and others) were not studied and raise exposure.
- **Not a monitoring tool:** the curve is a population prediction.

Every parameter was read in the paper's PDF.

### Pregnancy

With Pregnant ticked, Janssen 2021's factors are applied (clearance ×1.08, central volume ×1.19, the smaller peripheral volume ×1.12), from 9 pregnant women; exposure falls about 7%.
