### The model

Fokkink and colleagues' population analysis (*Clin Pharmacokinet* 2022) of 177 patients with Guillain–Barré syndrome (GBS), aged 5 to 89 years and weighing 18 to 122 kg, given Gammagard S/D 0.4 g/kg a day for 5 days, with 15 patients with chronic inflammatory demyelinating polyneuropathy; it was validated in 177 more patients with GBS. Two compartments, every parameter scaled to weight about 70 kg:

- clearance 0.28 L/day × (weight/70)^0.75;
- central volume 2.87 L × weight/70;
- intercompartmental clearance 0.25 L/day × (weight/70)^0.75, peripheral volume 2.65 L × weight/70.

At 70 kg the half-lives are 2.9 and 18 days. The Gammagard S/D label gives IgG a half-life of 21 to 25 days in healthy people and about 38 days in primary immunodeficiency, and notes that a high IgG concentration and fever shorten it.

### What is plotted

Total serum IgG in **g/L**: the patient's own plus the IgG infused. The patient's own is the Patient Profile's **IgG baseline**, shown while immunoglobulin is in the dose table. Left blank, it is the source population's, patients with GBS, 10.3 g/L (a normal adult's is about 7 to 16 g/L).

No band is drawn. No therapeutic range of serum IgG is established for intravenous immunoglobulin; the source's target in GBS is a rise of at least 7.3 g/L above the baseline two weeks after the course began, a rise and not a level.

### Covariates

- **Weight**, on every parameter, as fitted; with the fat-free-mass switch on, the pharmacokinetic weight.
- **Disease severity and steroids are held at the reference.** In the source, a severely affected patient cleared IVIG faster (×1.22 for a disability score of 3 or 4, ×1.83 for a ventilated patient), and so did a patient also given methylprednisolone (×1.31). The Patient Profile cannot carry either, so the curve is that of a mildly affected patient given no steroid; a sicker patient's IgG falls faster than drawn.
- **Who it describes.** The source is GBS. IVIG is also given in sepsis and in other inflammatory disease, where fever and inflammation would be expected to shorten the half-life further; no model of those uses is in the library.
- **Pregnancy**: no third-trimester pharmacokinetics have been reported. With the dose adjusted to weight, the IVIG exposure was unchanged from before pregnancy into the second trimester (Ensom and Stephenson 2011). Nothing is applied.

### Doses

`g/kg` or `g` as a dose, and `mg/kg/min` as an infusion rate (the label begins at 0.5 mL/kg/hour of the 5% solution, 0.42 mg/kg/min, and limits a patient at risk of renal failure or thrombosis to under 3.3 mg/kg/min).

### Not modelled

The patient's own IgG is held constant; its production, and its consumption in disease, are not drawn.
