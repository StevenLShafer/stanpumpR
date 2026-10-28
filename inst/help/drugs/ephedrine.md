### The model

No pharmacokinetic study of **intravenous** ephedrine in humans has been published. The labels of the intravenous products say only that limited data after intravenous dosing support urinary excretion of the drug and its metabolite, and the FDA review of the first of them extrapolated from oral data. This model does the same, on Steven L. Shafer's decision (2026-10-28).

Csajka and colleagues (*Br J Clin Pharmacol* 2005;59:335-345) fitted a population model to ephedrine, its metabolite norephedrine, and caffeine in 24 healthy adults given single oral doses. Ephedrine is one compartment with first-order absorption, cleared mostly by the kidney, with a minor saturable conversion to norephedrine. Their apparent volume was 181 L, their renal clearance 0.34 L/min, and the fraction of a dose the model had to load to account for the ephedrine recovered in urine was 0.78 for the herbal product (whose content was assayed as ephedrine base) and 0.59 for the pharmaceutical.

Because the model fits urine together with plasma, that renal clearance is the real one, and the loaded fraction is the bioavailability times the fraction excreted unchanged. Taking bioavailability as complete — what the FDA review assumed — gives a central volume of 232 L and a clearance of 0.44 L/min (26 L/h) at 70 kg, with a half-life of 6.1 hours. If bioavailability is less than complete the true volume and clearance are smaller in proportion, down to about a fifth less.

### The check

Gurley and colleagues (*Ther Drug Monit* 1998;20:439-445, abstract) gave 25 mg ephedrine and three herbal products to 10 subjects and reported, without a model, an apparent volume of 2.98 L/kg in men and 3.49 in women and an apparent clearance of 0.37 L/h per kg in men. This model, in base-dose terms at 70 kg, is 3.3 L/kg and 0.37 L/h per kg: the volume between the sexes and the clearance at the men's value. Berlin and colleagues (*Eur J Clin Pharmacol* 2001;57:447) report a half-life near 6 hours.

### What is plotted

Plasma (−)-ephedrine base in ng/mL; norephedrine is not plotted. Doses are ephedrine sulfate, as the US intravenous products (Akovaz, Corphedra, Emerphed) are labelled, and are entered as the salt; the model carries the salt-to-base factor (0.771). Ephedrine hydrochloride (Rezipres) holds 6% more base per milligram.

### Covariates

None. The source prints litres and no mean weight, so 70 kg is assumed. With [the scaling switch](help:models/fat-free-mass) on, the 70-kg values are scaled to fat-free mass; with it off, linearly in weight. Elimination is mostly renal and depends on urine pH — Csajka found clearance falling as urine became alkaline — and renal impairment slows it, but no renal or pH term is applied, and the Serum creatinine field does not affect this drug.

### Where to be careful

**The first minutes.** The model has no distribution phase, because oral data cannot show one — absorption hides it. After an intravenous bolus the real concentrations in the first minutes are higher than drawn; how much higher is not known. The curve is most trustworthy for an infusion and for the hours after a bolus, not for the peak.

There is no therapeutic concentration range, so no band is drawn. No time to peak pressor effect after an intravenous bolus has been measured with concentrations, so no effect site is drawn and no blood pressure is predicted. See [the vasoactive and resuscitation drugs](help:models/vasoactive).
