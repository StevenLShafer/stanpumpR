### The model

Meropenem's parameters come from the population model in the FDA's Clinical Pharmacology Review of Vabomere (meropenem and vaborbactam, NDA 209776, 2017). It was fitted to plasma and urine from healthy subjects, subjects with renal impairment or end-stage renal disease, and patients with complicated urinary tract or carbapenem-resistant infections: about 400 adults aged 18 to 92, with an MDRD eGFR of 4.5 to 338 mL/min/1.73 m². The model has two compartments:

- **Clearance:** a non-renal part of 3.78 L/h plus a renal part that rises in a sigmoid with eGFR, up to 6.60 L/h. The renal part is half-maximal at an eGFR of 40.8 mL/min/1.73 m², with a Hill coefficient of 1.94. Total clearance is multiplied by (age/58)<sup>−0.43</sup>.
- **Volumes and transfer:** central volume 17.4 × (weight/80)<sup>0.487</sup> L, intercompartmental clearance 1.52 L/h, and peripheral volume 2.50 × (weight/80)<sup>0.324</sup> L.

At or below an eGFR of 30 the non-renal part falls to 0.349 of its value, a 65% reduction. The review's prose calls it "a reduction ... by 35%", but its equation, its table and the published refit of the same trials all give 0.35. This is the source's model, an indicator rather than a smooth curve, so clearance drops by about 40% as the eGFR crosses 30.

### Checks

The published analysis of the same trials (Trang and colleagues, *Antimicrob Agents Chemother* 2021;65:e02606-20, doi 10.1128/AAC.02606-20) gives three figures, and the model matches each:

| Quantity | Trang 2021 | This model |
|---|---|---|
| Clearance, 58-year-old with an eGFR of 100 | "about 10 liters/h" | 9.4 L/h |
| Clearance at an eGFR of 40 | "approximately 7 liters/h" | 7.0 L/h |
| Renal fraction of clearance at an eGFR of 100 | "approximately 59%" | 60% |

A 35-year-old man with a normal creatinine clears 11.6 L/h, with half-lives of 0.7 and 1.5 hours. The Merrem label gives "approximately 1 hour".

Two sources the model was not fitted to: the label agrees within its ranges, and Christensson and colleagues (*Antimicrob Agents Chemother* 1992;36:1532-1537) measured a higher peak and a smaller area.

| Source | Dose | Measured | This model |
|---|---|---|---|
| Merrem label, healthy volunteers | 500 mg over 30 min | peak about 23 mg/L (14–26) | 25.3 mg/L |
| Merrem label, healthy volunteers | 1 g over 30 min | peak 49 mg/L (39–58) | 50.6 mg/L |
| Merrem label, healthy volunteers | 500 mg, at 6 h | about 1 mg/L | 0.8 mg/L |
| Christensson 1992, six men of 79 kg, GFR 100 | 500 mg over 30 min | peak 30.3 mg/L; AUC 36.0 mg·h/L | 24.0 mg/L; 40.6 mg·h/L |

### Covariates

The eGFR is the MDRD equation, as the source used it (per 1.73 m², without a race term). It is computed from the Patient Profile's **Serum creatinine**; a blank field means an assumed normal creatinine for age and sex. The creatinine is held at 0.5 mg/dL or more, as the source held it, and the age term at 18 years or more. The model was fitted in adults only.

Weight acts on the two volumes. With [the scaling switch](help:models/fat-free-mass) on, it is the pharmacokinetic weight; with it off, total body weight. Clearance has no weight term, because the source found none.

### The MIC line and the band

The line is free meropenem at the EUCAST susceptible breakpoint for Enterobacterales and *Pseudomonas aeruginosa*, **2 mg/L**. Meropenem is about 2% bound, so the line sits at 2 mg/L of total drug.

The band is free drug at the expected MBC of the common pathogens. The Merrem label puts bactericidal concentrations at "typically 1–2 times" the MIC. In Edwards and colleagues (1989) the MBC equalled the MIC in 59% of strains and was within twice it in 90%. The MBC50 is therefore taken as the MIC50, and the MBC90 as twice the MIC90. That gives:

- **Lower edge:** 0.03 mg/L, the Enterobacterales' MBC50.
- **Typical:** 0.12 mg/L, their MBC90.
- **Upper edge:** 16 mg/L, *P. aeruginosa*'s MBC90.

The *P. aeruginosa* MIC90, 8 mg/L, is at the CLSI resistant breakpoint. See [Antibiotics](help:models/antibiotics).

There is no consensus therapeutic range. The ESICM/ESCMID position paper (Abdul-Aziz and colleagues, *Intensive Care Med* 2020;46:1127-1153, doi 10.1007/s00134-020-06050-1) recommends monitoring the beta-lactams to a target of 100% of the dosing interval with free drug above the MIC. It recommends a trough sample "30 min or just before the next dosing", 24–48 hours into therapy, or a steady-state concentration above the MIC during a continuous infusion. Its Table 1 gives a carbapenem toxicity threshold of a trough above 44.5 mg/L, from meropenem data.

### Dose

Merrem IV vials contain "500 mg or 1 gram (as the trihydrate ...) of sterile meropenem powder", so doses are milligrams of meropenem.
