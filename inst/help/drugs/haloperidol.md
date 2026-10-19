### The model

Oral haloperidol, from Pilla Reddy and colleagues (*J Clin Psychopharmacol* 2013;33:731-739), who analysed 122 patients with schizophrenia. Two compartments, first-order absorption, no covariates: apparent clearance 88 L/h, central volume 669 L, peripheral volume 2500 L, intercompartmental clearance 233 L/h, absorption rate constant 0.236 per hour. Half-lives are 1.3 h and 31 h.

**These values have not yet been checked against the full paper**, which could not be obtained. The abstract confirms the two-compartment structure and the 122 subjects, and a later paper cites this model with a clearance of 88 L/h and a total volume of 3169 L, which agree with them.

### Oral only

The parameters are **apparent**, so this drug is offered as **mg PO** only. Intravenous haloperidol is a separate drug, [haloperidolIV](help:drugs/haloperidolIV), with absolute parameters from a different study; one is not the other times a bioavailability.

### CYP 2D6

The **CYP 2D6** field scales clearance. Park and colleagues (*J Clin Psychopharmacol* 2006;26:135-142) gave single 5 mg doses to healthy Korean men and found oral clearance 23% lower with CYP2D6\*10/\*10 than with \*1/\*1 (3.6 against 4.7 L/h/kg): **intermediate** metabolisers take 0.77 of the typical clearance. Only that ratio is used; Park's absolute clearance and 12.6 h half-life come from 72 h of sampling against a 31 h terminal half-life and are biased. Poor and ultrarapid metabolisers were not studied, so they take the nearest studied value (0.77 and 1).

### Interactions not modelled

- **Rifampin** lowered haloperidol troughs to 63% of baseline by day 3, 37% by day 7 and 30% by day 28; stopping it raised them 3.3-fold over about two weeks (Kim and colleagues, *J Clin Psychopharmacol* 1996;16:247-252). That is roughly a 3.3-fold rise in clearance that builds over 1 to 2 weeks.
- **Itraconazole** (a strong CYP3A4 inhibitor) lowered oral clearance to 0.67 of placebo, and to 0.56 in \*10/\*10 subjects (Park 2006).

Neither is applied to the curve.

### Covariates

None in the source. The published values are read as the reference adult's and take the library's [fat-free-mass scaling](help:models/fat-free-mass).

### Effect site

Lim and colleagues (*Pharm Res* 2013;30:683-693) measured striatal D2 occupancy with [18F]fallypride PET in eight healthy Korean men taking 0.5 to 3 mg daily, and fitted an effect compartment driven by plasma haloperidol: **ke0 0.213 per hour** (a 3.3 hour equilibration half-time) and half-maximal occupancy at **0.791 ng/mL** in the putamen (0.650 in the caudate). ke0 is supplied directly, so the effect-site curve on this row is that model's, and occupancy = 100% × Ce / (0.791 + Ce): 50% at 0.79 ng/mL, 80% at 3.2 ng/mL. Lim's own pharmacokinetic model, with a saturable binding compartment, cannot be run here, so the occupancy model is carried onto Pilla Reddy's kinetics. Occupancy is a receptor biomarker, not a target; Lim's simulations put 2 mg every 12 hours near a 65-80% window, which is an effect-site concentration of about 1.5 to 3.2 ng/mL.

There is **no band yet**. The AGNP 2017 consensus gives a therapeutic reference range of 1 to 10 ng/mL (Hiemke and colleagues, *Pharmacopsychiatry* 2018;51:9-62), and an excerpt of the 2026 update's table (Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, https://doi.org/10.1055/a-2860-7861) appears to narrow it to 1 to 5 ng/mL, 12 hours after a dose. Neither table has been read in full, and the two disagree, so the band waits until the 2026 table is read.

The same paper as the kinetics also fitted PANSS scores with a drug effect of 0.31 × Css / (3.58 ng/mL + Css), where Css is each patient's **average steady-state** concentration, inside a placebo-response and dropout model. That describes weeks of treatment, not the effect of a concentration at a moment, so it is not plotted, and 3.58 ng/mL is not a target.
