### The model

A second model of dapagliflozin, kept for the Japanese patients it was fitted to; the library's main model is [dapagliflozin](help:drugs/dapagliflozin) (Melin 2023), the source of this model's fixed volume and absorption rate.

Kobuchi and colleagues' analysis of 85 Japanese outpatients with type 2 diabetes on 5 mg once daily (*Int J Med Sci* 2025): one compartment, apparent clearance 229.3 L/day times (weight / 77 kg)<sup>0.41</sup>, with the weight at the pharmacokinetic weight. The volume (73.9 L) and absorption rate (57.4/day) were fixed from an earlier population analysis (Melin and colleagues, 2024), because every sample was a trough. Neither renal function nor sex changed clearance in these patients.

### Only the trough is fitted

Trough samples fix the average and trough concentration at steady state, which is the daily dose over the clearance. They do not fix the shape within the day. With the borrowed volume the half-life is about 5.4 hours, against the label's 12.9, so the peak and the fall after it are the fixed parameters', not this population's. All the patients were also taking metformin and a DPP-4 inhibitor.

### What is not plotted

Concentration only, in ng/mL. The same paper's HbA1c model is an endpoint of its own, over months, and is not drawn. See [Antidiabetic models and their limits](help:models/antidiabetics).
