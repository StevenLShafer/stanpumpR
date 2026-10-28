### The model

No pharmacokinetic study of **intravenous** ephedrine in humans has been published. The labels of the intravenous products say only that limited data after intravenous dosing support urinary excretion of the drug and its metabolite, and the FDA review of the first of them extrapolated from oral data. This model does the same, on Steven L. Shafer's decision (2026-10-28).

Csajka and colleagues (*Br J Clin Pharmacol* 2005;59:335-345) fitted a population model to ephedrine, its metabolite norephedrine, and caffeine in 24 healthy adults given single oral doses. Ephedrine is one compartment with first-order absorption, cleared mostly by the kidney, with a minor saturable conversion to norephedrine. Their reported apparent volume of distribution was 181 L and their apparent clearance 0.34 L/min — both already divided by bioavailability (V/F and CL/F).

Because those are apparent oral values (CL/F already folds in bioavailability), taking bioavailability as complete — Steven L. Shafer's decision — makes them the absolute volume and total clearance directly: a central volume of 181 L and a clearance of 0.34 L/min (20 L/h) at 70 kg, with a half-life of 6.1 hours. (An earlier draft of this page divided both again by the urinary-recovery fraction and gave 232 L and 0.44 L/min; that double-counted bioavailability and read about a fifth low.)

### The check

Gurley and colleagues (*Ther Drug Monit* 1998;20:439-445, abstract) gave 25 mg ephedrine and three herbal products to 10 subjects and reported, without a model, an apparent volume of 2.98 L/kg in men and 3.49 in women and an apparent clearance of 0.37 L/h per kg in men and 0.48 in women. This model, in base-dose terms at 70 kg, is 2.6 L/kg and 0.29 L/h per kg: the same order, running about a fifth below Gurley's, as two single-dose studies in different people with different assays may. Berlin and colleagues (*Eur J Clin Pharmacol* 2001;57:447) report a half-life near 6 hours.

### What is plotted

Plasma (−)-ephedrine base in ng/mL; norephedrine is not plotted. Doses are ephedrine sulfate, as the US intravenous products (Akovaz, Corphedra, Emerphed) are labelled, and are entered as the salt; the model carries the salt-to-base factor (0.771). Ephedrine hydrochloride (Rezipres) holds 6% more base per milligram.

### Covariates

None. The source prints litres and no mean weight, so 70 kg is assumed. With [the scaling switch](help:models/fat-free-mass) on, the 70-kg values are scaled to fat-free mass; with it off, the published values are used at every weight (the source fitted no weight covariate). Elimination is mostly renal and depends on urine pH — Csajka found clearance falling as urine became alkaline — and renal impairment slows it, but no renal or pH term is applied, and the Serum creatinine field does not affect this drug.

### Where to be careful

**The first minutes.** The model has no distribution phase, because oral data cannot show one — absorption hides it. After an intravenous bolus the real concentrations in the first minutes are higher than drawn; how much higher is not known. The curve is most trustworthy for an infusion and for the hours after a bolus, not for the peak.

There is no therapeutic concentration range, so no band is drawn. No time to peak pressor effect after an intravenous bolus has been measured with concentrations, so no effect site is drawn and no blood pressure is predicted. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
