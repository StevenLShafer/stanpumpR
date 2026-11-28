### The model

The intravenous pharmacokinetics measured by de Groot and colleagues in 6 nonpregnant women (*Int J Clin Pharmacol Ther* 1995;33:328-332, [PubMed](https://pubmed.ncbi.nlm.nih.gov/7582383/)): mean age 41 years, 61 kg, 167 cm. Each received 0.2 mg of methylergonovine maleate intravenously over 1 minute. Venous plasma was assayed by liquid chromatography for 9 hours.

**Intravenous.** Two compartments built from the women's clearance (22.2 L/h), steady-state volume (50.8 L) and half-lives (6 minutes and 1.94 h). The paper reports the free base, 0.152 mg of it per 0.2 mg of maleate, so the parameters are divided by 0.76 and doses are entered as the labelled maleate. The model draws the dose as given at once, so its first few minutes lie above what a 1-minute injection gives.

**Intramuscular, the weakest route.** The study gave no intramuscular dose. Bioavailability, 0.78, is the figure the **Methergine** prescribing information ([Drugs@FDA, 2025](https://www.accessdata.fda.gov/drugsatfda_docs/label/2025/006035s080lbl.pdf)) gives for injection during delivery, without saying how it was measured. The absorption rate was set to the label's time of peak, 0.41 h.

**No oral route.** The tablet is marketed, and the same study measured it, but oral doses are not offered. De Groot's later review (1996, [PubMed](https://pubmed.ncbi.nlm.nih.gov/8909954/), abstract) concludes that oral (methyl)ergometrine tablets are unstable in storage, that the drug's pharmacokinetics and effects by mouth are unpredictable, that it has not been shown to reduce blood loss after childbirth, and that it is no alternative to injected drugs for preventing haemorrhage in the third stage of labour. In the study, oral bioavailability ranged from 22% to 138% between subjects.

### How far to trust it

**The product label reports higher concentrations,** about three times these after injection. After 0.2 mg intramuscularly it gives a peak of 5.9 ng/mL, against 1.7 ng/mL here, and its clearance divided by bioavailability is 14.4 L/h, against 37 L/h here. The label names no assay. A review of older radioimmunoassay studies (Mäntylä and Kanto 1981, [PubMed](https://pubmed.ncbi.nlm.nih.gov/7298246/)) reports a clearance of 7 to 14 L/h, like the label's. An antibody that also reads metabolites would explain the gap, but that has not been shown. This model follows the specific assay and the intravenous data.

**Do not rely on the early peak after intravenous injection.** The paper plots the concentrations of one woman. Against hers, the model is about twice too high 3 to 5 minutes after injection and 1.4 times too high at 10 minutes, close at 20 and 30 minutes, and 25% to 35% too low from 1 hour on. The model's central volume, which sets the early concentrations, comes from the group's average values (a two-stage average) and is poorly determined. One woman cannot show whether it is wrong for women in general.

The study was small. The women's reported clearance predicts an intravenous area under the curve 14% above the one measured, which the model does not reconcile.

### Doses

Doses are methylergonovine **maleate**, as labelled: 0.2 mg intramuscularly, repeated as required every 2 to 4 hours. The label reserves intravenous injection for necessity, given over at least 60 seconds with blood pressure monitoring. It also gives the tablet, 0.2 mg three or four times daily after delivery for no more than a week, which is not simulated here.

### Body size

No covariate was fitted. With **Adjust to fat-free mass** on, the values belong to the study's typical woman (61 kg, 167 cm, 41 years) and are scaled from there: a 70 kg, 165 cm, 30-year-old woman is predicted a peak of 1.6 ng/mL after 0.2 mg intramuscularly, and the 70 kg reference man 1.4. With it off, everyone receives the study women's values. Scaled this way to the men of the same study, the women's model predicts their clearance within 8% and their volume within 5%.

### Effect and typical concentrations

The label gives the onset of the uterine effect (immediate intravenously, 2 to 5 minutes intramuscularly) but no time of peak effect, so no effect-site curve is drawn. No therapeutic concentration range is established, and the 1981 review found no apparent relation between plasma level and clinical effect, so no band is shown.

### Pregnancy

Methylergonovine is **contraindicated during pregnancy** because it contracts the uterus; it is used at delivery and after it. The women studied were not pregnant, and nothing is applied for pregnancy. What is reported:

- **At delivery,** the label gives an intramuscular bioavailability of 78%, without saying what it is compared with or how it was measured. The model uses it for every intramuscular dose.
- **After delivery,** the label says the peak after the tablet may be delayed to about 3 hours in postpartum women on continuous treatment. Oral doses are not simulated.
- **Clearance and volume:** no comparison with nonpregnant women was found.

### Where to be careful

The label contraindicates methylergonovine in hypertension and toxemia as well as pregnancy, warns against routine intravenous use because of sudden hypertensive and cerebrovascular accidents, and says strong and moderate CYP3A4 inhibitors should not be given with it. None of these effects or interactions is simulated. The label also advises against breastfeeding during treatment and for at least 12 hours after the last dose.
