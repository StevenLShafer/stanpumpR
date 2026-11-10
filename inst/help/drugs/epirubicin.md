### The model

Ralph and colleagues (*Cancer Chemother Pharmacol* 2003;52:34-40) studied 105 women with advanced breast cancer, 35 to 79 years, most with liver metastases, given epirubicin alone (12.5 to 120 mg/m²) as a rapid injection. Three compartments: central volume 10.3 L, intercompartmental clearances 30.2 and 61.5 L/h, peripheral volumes 35.7 and 772 L.

### The AST sets the clearance

Clearance is 72.9 × (1 − 0.135 × ln AST) L/h, with the AST in U/L, so a damaged liver clears less: 43 L/h at a normal AST of 20 U/L, 29 L/h at 90 (the study's median), 7 L/h at 815. Enter the **Serum AST** in the Patient Profile (it appears while epirubicin is in the dose table). Left blank, a normal AST of 20 U/L is assumed.

The equation reaches zero clearance at an AST of about 1650 U/L, which the authors did not address. stanpumpR holds the AST within the 7 to 815 U/L the study covered, so an AST above 815 is treated as 815.

No other covariate survived: body-surface area, weight, bilirubin and liver metastases each explained under 2% of the variability.

### What is plotted

Plasma epirubicin, in ng/mL. Doses are entered as epirubicin hydrochloride, the approved product; the paper does not say whether its doses were the salt or the base, and stanpumpR's rule is then to read them as the approved product. The metabolite epirubicinol is not modelled. Ralph used an AUC of 4000 ng·h/mL as a dosing target; there is no therapeutic range, so none is drawn.

### Where to be careful

- **An external test failed.** In 79 patients from another centre given FEC, the model over-predicted concentrations by 81% (Ralph 2006).
- **Women only, first cycle, samples to 48 hours.** The 772-L terminal volume rests on little data.
- **Pregnancy:** with Pregnant ticked, van Hasselt 2014's factors are applied (clearance ×1.10, central volume ×1.55, the smaller peripheral volume ×2.08), from 10 pregnant women; the typical exposure falls about 8%.
