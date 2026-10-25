### The model

Oral aripiprazole and its active metabolite, from Kim J-R and colleagues (*Br J Clin Pharmacol* 2008;66:802-810): 141 steady-state plasma concentrations from 80 Korean psychiatric patients on 10 to 30 mg/day. One compartment for each species, absorption fixed at 1.06 per hour, apparent volume 192 L. Aripiprazole forms [dehydroaripiprazole](help:drugs/dehydroaripiprazole).

Only the abstract could be read. It gives the structure, the absorption rate, a volume of 192 L (the implementation brief the model was built from gives 193 L from the paper's table) and the ratio of intermediate to normal clearance. The genotype-group clearances and the metabolite parameters come from the brief and **have not yet been checked**.

### CYP 2D6

Apparent clearance is 3.15 L/h in **normal** metabolisers (two functional alleles) and 1.83 L/h in **intermediate** metabolisers. Poor and ultrarapid metabolisers were **not studied**; they take the nearest studied value, which for a poor metaboliser overstates clearance and understates exposure. Kim also reported 2.66 and 2.27 L/h for one functional allele paired with a partly or wholly non-functional one.

### Oral only

All parameters are **apparent**, so aripiprazole is offered by mouth only: **mg PO**, or **mg PO qd** for a once-daily dose repeated to the end of the plot (see [The dose table](help:dose-table)).

### Effect site

Kim E and colleagues (*J Cereb Blood Flow Metab* 2012;32:759-768) measured striatal D2 occupancy with PET and serial plasma concentrations in 18 healthy men, and fitted an effect compartment driven by **parent** aripiprazole: ke0 0.725 per hour (a 57-minute equilibration half-time), EC50 8.63 ng/mL, Emax 100%. ke0 is supplied directly, so the effect-site curve on this row is that model's effect-site concentration, and occupancy = 100% × Ce / (8.63 + Ce): 50% at 8.63 ng/mL, 80% at 34.5 ng/mL. The EC50 was checked in the abstract; the ke0 and Emax were not. Dehydroaripiprazole must **not** be added to Ce for this calculation. The PET and pharmacokinetic studies used different populations. Occupancy is a receptor biomarker, not a dosing target, and is not the band.

The shaded band, **120 to 270 ng/mL** of parent aripiprazole with 195 as the typical value (its midpoint), is the serum therapeutic reference range for schizophrenia from the AGNP task force's systematic review and meta-analysis (Hart and colleagues, *Psychopharmacology* 2022;239:3377-3391, https://doi.org/10.1007/s00213-022-06233-2), which replaced the AGNP 2017 range of 100 to 350 ng/mL; the AGNP consensus guidelines' 2026 update (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5) adopts it, with a laboratory alert level of 540 ng/mL. It is a trough at steady state, to read against the plasma curve, not the effect site. The active moiety, aripiprazole plus [dehydroaripiprazole](help:drugs/dehydroaripiprazole), has its own range, 180 to 380 ng/mL. The review found that 10 mg a day brings most patients into the range, and 5 mg is enough for a known CYP2D6 poor metaboliser.

### Pregnancy

With **Pregnant** ticked, the parent's apparent clearance is multiplied by 2.1, on top of the CYP2D6 value; the volume, the absorption and the effect site are unchanged. Westin and colleagues (*Clin Pharmacol Ther* 2018;103:477-484) followed 14 aripiprazole pregnancies through routine drug monitoring and found the dose-adjusted active moiety (aripiprazole plus dehydroaripiprazole) 52% lower in the third trimester than the same women's own baseline, with aripiprazole alone falling similarly. Because all of the parent's clearance forms dehydroaripiprazole, its formation rises with it, while the metabolite's own clearance is not changed: so aripiprazole, and the D2 occupancy it drives, falls by 52% at steady state, but dehydroaripiprazole does not, and the plotted active moiety falls by about 38% rather than the measured 52%. Westin found the metabolite itself about 30% lower, which would need a change in its own clearance that has not been measured. See [Pregnancy](help:models/pregnancy).
