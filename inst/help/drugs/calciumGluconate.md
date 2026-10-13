### What is plotted

The curve is the **change in venous serum ionized calcium**, in mmol/L, above the patient's own baseline, exactly as for [calcium chloride](help:drugs/calciumChloride), whose page holds the source study, the population and the caveats. Doses are entered as grams or milligrams of calcium gluconate (the monohydrate, 448 g/mol; a 10% ampoule is 1 g in 10 mL, 2.2 mmol of calcium).

### How the two salts are linked

Ansari and colleagues (*Anesthesiology* 2025;142:121–31) randomized 34 parturients at cesarean delivery to calcium chloride 0.5 g or calcium gluconate 1.5 or 2 g by 10-minute infusion. The molecular weights of the hydrated salts predict that 3.05 g of calcium gluconate carries as much calcium as 1 g of calcium chloride; the trial's first 20 patients, dosed at a 4:1 ratio from the anhydrous weights, showed the gluconate raising the ionized calcium more, and the protocol's recalibration to 1.5 g produced curves that lay on top of the 0.5 g calcium chloride curves. The population model then estimated the ratio as a parameter: **3.11 g of calcium gluconate per gram of calcium chloride** (95% CI 2.77 to 3.48).

stanpumpR applies that ratio as the paper instructs: a dose of calcium gluconate is divided by 3.11 and run through the calcium chloride model, which it does by converting the dose to millimoles at 147.01 × 3.11 = 457.2 g per mole of calcium chloride equivalent, 2% above the stoichiometric 448 g/mol. So 3.11 g of calcium gluconate plots identically to 1 g of calcium chloride, and the usual 3 g dose over 10 minutes raises the ionized calcium by 0.375 mmol/L at the end of the infusion, 96% of calcium chloride's 0.389.

### Why it matters

The study was prompted by deaths from treating hyperkalemia with 1 g of calcium gluconate in place of 1 g of calcium chloride. Here, 1 g of calcium gluconate over 10 minutes raises the ionized calcium by 0.13 mmol/L, a third of what the same gram of calcium chloride gives. The authors also point out that the calcium gluconate package insert's maximal rate of 200 mg/min is only two-thirds of the calcium chloride insert's 100 mg/min: 300 mg/min of calcium gluconate is the equivalent. Load the [calcium chloride versus calcium gluconate](scenario:calcium-chloride-vs-gluconate) scenario to see the two salts side by side.

### Where to be careful

Everything on the [calcium chloride](help:drugs/calciumChloride) page applies: the model describes the first hour or two in parturients, is offered to every patient as an extrapolation, has no effect site and no band, and is scaled to fat-free mass by the library's assumption rather than the paper's. The dose ratio was estimated over 1.5 to 3 g of calcium gluconate given over 10 minutes; its confidence interval, 2.77 to 3.48, is the uncertainty in the gluconate curve relative to the chloride curve.
