### A component of Coartem

Artemether is given in the United States only in [Coartem](help:drugs/Coartem), whose tablets contain 20 mg of artemether. It cannot be entered in the dose table on its own. Its curve comes from the Coartem you give, drawn with its active metabolite [dihydroartemisinin](help:drugs/dihydroartemisinin).

### The model

The parameters come from the joint artemether and dihydroartemisinin model of Ding and colleagues (*Br J Clin Pharmacol* 2026;92:589-605, doi 10.1002/bcp.70301, Table 2). It was fitted to 626 samples from the 79 densely sampled patients with uncomplicated falciparum malaria in the TRACII and TACT-CV trials (Bangladesh and Vietnam); children under 20 kg were not densely sampled. The parameters are apparent (oral) and are given for a typical 45 kg patient:

- **Absorption:** the dose passes through a depot and two transit compartments before reaching the circulation, with a mean transit time of 1.55 h. Each step runs at 3/1.55 = 1.94 per hour (see below).
- **Clearance:** CL/F 79.3 L/h × (weight / 45)<sup>0.75</sup> at the first dose.
- **Volumes:** central 141 L and peripheral 283 L, each × weight / 45.
- **Intercompartmental clearance:** Q/F 21.7 L/h × (weight / 45)<sup>0.75</sup>.

The weight exponents were fixed, not estimated. No other covariate was retained. The model assumes that all of artemether's elimination forms dihydroartemisinin.

### The clearance rises with each dose

The label notes that artemether's exposure falls over the course while dihydroartemisinin's rises. It attributes this to enzyme induction: the ratio of their areas falls from 1.2 after one dose to 0.3 after six. Ding's model describes this as a rise in clearance at each dose, CL × (1 + 0.551 × (dose number − 1)), so that by the sixth dose the clearance is 3.76 times the first. On the plot, each artemether peak is lower than the one before.

The authors say this empirical model "is not suitable for extrapolation" beyond the standard three days. stanpumpR therefore keeps the sixth dose's clearance for any later dose rather than raising it further. A second course weeks later also starts at that clearance. No source says how fast the induction wears off.

### Reading the transit chain

The paper reports two transit compartments with "an identical rate constant between compartments" but prints no equation. The Mahidol-Oxford group usually puts the dose into a depot ahead of the chain, with every step at (number of transit compartments + 1) / mean transit time. stanpumpR reads the model this way.

For the typical 45 kg patient, this reading gives a first-dose artemether peak of 239 ng/mL and a dihydroartemisinin peak of 128 ng/mL, both about 2 h after the dose. Ding's medians are 256 and 135 ng/mL, and the label says the peak comes "about 2 hours" after a dose. The other reading of the paper (two compartments at 2 / 1.55 h) gives lower peaks, 220 and 116 ng/mL.

### The terminal half-life

The model's slow phase has a half-life of 11.8 h for the typical patient at the first dose; Ding's median across patients is 12.1 h. The label gives about 2 h, and an earlier model from the same group (Hoglund 2015) rejected its own 12-hour phase as unstable and kept a single compartment.

Steven L. Shafer chose Ding (2026-11-23): "Typically later studies have more sensitive assays. More sensitive assays give longer half-lives." Almost all of each dose is cleared in the fast, roughly 2-hour phase. The slow phase leaves a few ng/mL between doses.

### Checks

For the typical 45 kg patient on the label's six doses of four tablets, this model gives:

- an artemether AUC of about 3,000 h·ng/mL, against Ding's median of 2,850;
- a dihydroartemisinin AUC of 1,890, against 1,870.

The engine was also checked against an independent step-by-step numerical solution of the same equations, including the rise in clearance at each dose. The two agree to one part in a million.

### Not modelled

- Food. The curve is for a fed patient; fasting lowers artemether's bioavailability two- to threefold.
- Interactions through CYP3A4. The label lists ketoconazole (artemether AUC ×2.3), rifampin (−89%), efavirenz (−50%) and lopinavir/ritonavir (about −40%).

### No band

No therapeutic range is established for artemether or dihydroartemisinin. The evidence linking their exposure to parasite clearance is inconsistent between studies.
