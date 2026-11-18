### The model

Methylprednisolone's parameters are from Hong and colleagues (*Pharm Res* 2007;24:1088-1097), a population model of intravenous methylprednisolone sodium succinate in five healthy men: clearance 22.8 L/h, volume 78.4 L, one compartment. Half-time 2.4 hours. No covariates were fitted.

### Oral route

Bioavailability, 0.82, is from Al-Habet and Rogers (*Br J Clin Pharmacol* 1989;27:285-290), who compared 20 mg tablets with intravenous succinate in five subjects. Neither study yields an absorption constant, so the one in the code (1.28/h, a plasma peak at 90 minutes, within the 1 to 2 hours product information gives) is **provisional** and named as such. The oral AUC does not depend on it; the oral peak does.

### Covariates

None in the source. The parameters take the default [fat-free-mass scaling](help:models/fat-free-mass) and are used as published with the switch off.

### Effect site

None; the glucocorticoid effect is genomic and takes hours. Only the plasma concentration is plotted. The shaded band (0.2 to 1.5 mcg/mL) covers what 40 to 125 mg produce over the first hours.

### Where to be careful

The succinate ester hydrolyses to the active steroid with a half-time of about four minutes, which the source absorbs into its parameters; the first minutes after a fast bolus are the only place that shows. Five healthy men is a small anchor, and no dependence on age, sex or disease is represented.
